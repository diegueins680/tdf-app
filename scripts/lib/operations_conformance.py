"""Synthetic Operations HTTP/PG regressions, run by test-booking-conformance.py.

Uses only the caller's owned disposable database and localhost HTTP server. Locks
are witnessed in pg_stat_activity; a sleep alone never establishes a race.
"""
import concurrent.futures
from contextlib import contextmanager
import json
import subprocess
import time
import uuid


def verify_operations(sql, request, check, env, database, output, actors):
    def q(value):
        return "'" + str(value).replace("'", "''") + "'"

    org = sql("INSERT INTO operations_organization(slug,display_name,operations_enabled) VALUES ('synthetic-ops','Synthetic operations',true) RETURNING id")
    branches = [sql("INSERT INTO operations_branch(organization_id,slug,display_name) VALUES (" + q(org) + "," + q('branch-' + str(i)) + ",'Synthetic') RETURNING id") for i in range(2)]
    for branch in branches:
        for role in ['admin', 'manager', 'accounting']:
            sql("INSERT INTO operations_scope_member(organization_id,branch_id,party_id) VALUES (" + q(org) + "," + q(branch) + "," + actors[role] + ")")

    def item(branch=branches[0]):
        return sql("INSERT INTO operations_work_item(organization_id,branch_id,source_system,source_channel,entity_type,uncorrelated,correlation_key,title_es,title_en,description_es,description_en,created_at) VALUES (" + q(org) + "," + q(branch) + ",'synthetic','manual','uncorrelated_inbound',true," + q(str(uuid.uuid4())) + ",'Synthetic','Synthetic','Synthetic','Synthetic',now()) RETURNING id")

    def command(kind, key, version=1, **changes):
        body = {'expectedVersion': version, 'reason': 'Synthetic conformance', 'requestId': str(uuid.uuid4()), 'sourceClient': 'conformance'}
        body.update({'priority': 'high'} if kind == 'priority' else {})
        body.update(changes)
        return lambda: request('/operations/work-items/' + key + '/' + kind, body, method='PATCH')

    def count(table, predicate):
        return int(sql('SELECT count(*) FROM ' + table + ' WHERE ' + predicate))

    def effects(key):
        return tuple(count(table, column + '=' + q(key)) for table, column in [
            ('operations_admin_audit', 'target_entity_id'), ('operations_work_item_event', 'work_item_id'),
            ('operations_stream_event', 'work_item_id')])

    @contextmanager
    def held(query):
        with (output / 'operations-barriers.log').open('a') as log:
            process = subprocess.Popen(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database],
                                       env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log, text=True)
            try:
                process.stdin.write("BEGIN; " + query + "; SELECT 'fixture_locked:' || pg_backend_pid();\n")
                process.stdin.flush()
                while True:
                    line = process.stdout.readline()
                    if not line: raise RuntimeError('Operations barrier exited before acquiring lock')
                    if line.startswith('fixture_locked:'):
                        barrier_pid = int(line.strip().split(':')[1])
                        break
                def release():
                    process.stdin.write('COMMIT;\n')
                    process.stdin.flush()
                yield release, barrier_pid
            finally:
                process.communicate('ROLLBACK;\n', timeout=15)
                if process.returncode: raise RuntimeError('Operations barrier failed')

    def blocked(number, barrier_pid):
        deadline = time.monotonic() + 12
        while time.monotonic() < deadline:
            actual = int(sql("WITH RECURSIVE blocked(pid) AS (SELECT " + str(barrier_pid) + " UNION SELECT activity.pid FROM pg_stat_activity activity JOIN blocked parent ON parent.pid=ANY(pg_blocking_pids(activity.pid)) WHERE activity.datname=current_database()) SELECT count(DISTINCT activity.pid) FROM blocked JOIN pg_stat_activity activity USING(pid) WHERE activity.pid<>" + str(barrier_pid) + " AND activity.wait_event_type='Lock' AND (activity.query LIKE '%operations_%' OR activity.query LIKE '%api_token%' OR activity.query LIKE '%party_security_role%')"))
            if actual >= number: return
            time.sleep(.02)
        raise AssertionError('Operations concurrency barrier did not observe ' + str(number) + ' blocked requests')

    def race(query, calls, before_release=None):
        with held(query) as (release, barrier_pid):
            with concurrent.futures.ThreadPoolExecutor(max_workers=len(calls)) as executor:
                futures = [executor.submit(call) for call in calls]
                try:
                    blocked(len(calls), barrier_pid)
                    if before_release: before_release()
                finally:
                    release()
                return [future.result() for future in futures]

    for kind, arguments in [('priority', {}), ('seen', {}), ('assignment', {'assigneePartyId': int(actors['manager'])}),
                            ('transition', {'targetStatus': 'resolved'})]:
        key = item()
        for invalid in [{'version': 0}, {'requestId': ''}, {'sourceClient': ''}]:
            check('operations ' + kind + ' rejects invalid ' + next(iter(invalid)),
                  command(kind, key, **arguments, **invalid)()[0] == 422 and effects(key) == (0, 0, 0))
        responses = race('SELECT id FROM operations_work_item WHERE id=' + q(key) + ' FOR UPDATE',
                         [command(kind, key, **arguments) for _ in range(8)])
        check('operations ' + kind + ' same-version race has one winner', sorted(r[0] for r in responses) == [200] + [409] * 7)
        check('operations ' + kind + ' commits exactly one version and evidence set',
              sql('SELECT version FROM operations_work_item WHERE id=' + q(key)) == '2' and effects(key) == (1, 1, 1))

    key = item()
    sql("INSERT INTO operations_sla_timer(organization_id,work_item_id,phase,starts_at,due_at) VALUES (" + q(org) + ',' + q(key) + ",'resolve',now(),now()+interval '1 day')")
    responses = race('UPDATE operations_work_item SET version=2 WHERE id=' + q(key),
                     [command('transition', key, targetStatus='resolved')])
    check('operations losing transition leaves SLA and evidence unchanged', responses[0][0] == 409 and effects(key) == (0, 0, 0)
          and sql('SELECT completed_at IS NULL FROM operations_sla_timer WHERE work_item_id=' + q(key)) == 't')

    # Force failure after primary row/timer writes, proving the whole transaction rolls back.
    sql("CREATE FUNCTION synthetic_ops_audit_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic private data must not be logged'; END $$")
    def audit_failure(call):
        sql('CREATE TRIGGER synthetic_ops_audit_failure BEFORE INSERT ON operations_admin_audit FOR EACH ROW EXECUTE FUNCTION synthetic_ops_audit_failure()')
        try: return call()
        finally: sql('DROP TRIGGER synthetic_ops_audit_failure ON operations_admin_audit')
    response = audit_failure(command('transition', key, version=2, targetStatus='resolved'))
    check('operations audit failure rolls back item, timers and stream', response[0] == 500 and effects(key) == (0, 0, 0)
          and sql('SELECT version FROM operations_work_item WHERE id=' + q(key)) == '2'
          and sql('SELECT completed_at IS NULL FROM operations_sla_timer WHERE work_item_id=' + q(key)) == 't')

    for label, table, predicate in [
        ('scope', 'operations_scope_member', 'party_id=' + actors['admin'] + ' AND branch_id=' + q(branches[0])),
        ('branch', 'operations_branch', 'id=' + q(branches[0])),
        ('role', 'party_security_role', 'party_id=' + actors['admin']),
        ('session', 'api_token', "token='fixture-admin'")]:
        key = item()
        try:
            responses = race('UPDATE ' + table + ' SET active=false WHERE ' + predicate, [command('priority', key)])
            check('operations revalidates ' + label + ' after revocation wins lock', responses[0][0] in [401, 403] and effects(key) == (0, 0, 0)
                  and sql('SELECT version FROM operations_work_item WHERE id=' + q(key)) == '1')
        finally: sql('UPDATE ' + table + ' SET active=true WHERE ' + predicate)

    # A grant inserted after the lock-query snapshot cannot authorize this command.
    sql("INSERT INTO operations_scope_member(organization_id,branch_id,party_id) VALUES (" + q(org) + "," + q(branches[0]) + "," + actors['reception'] + ")")
    key = item()
    body = {'expectedVersion': 1, 'priority': 'high', 'reason': 'Synthetic grant race', 'requestId': 'grant-race', 'sourceClient': 'conformance'}
    grant = []
    def insert_manager():
        grant.append(sql("INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT " + actors['reception'] + ",id,'bootstrap',true FROM security_role WHERE code='manager' RETURNING id"))
    try:
        replies = race("SELECT id FROM security_role WHERE code='reception' FOR UPDATE",
                       [lambda: request('/operations/work-items/' + key + '/priority', body, token='fixture-reception', method='PATCH')],
                       before_release=insert_manager)
        check('operations excludes a role grant inserted after locked snapshot', replies[0][0] == 403 and effects(key) == (0, 0, 0))
    finally:
        for grant_id in grant: sql('DELETE FROM party_security_role WHERE id=' + q(grant_id))

    def approval_body(**changes):
        body = {'organizationId': org, 'branchId': branches[0], 'actionType': 'refund', 'targetEntityType': 'payment',
                'targetEntityId': 'synthetic-payment', 'amountMinor': 10, 'currency': 'USD', 'reason': 'Synthetic approval',
                'idempotencyKey': str(uuid.uuid4()), 'requestId': str(uuid.uuid4()), 'sourceClient': 'conformance'}
        body.update(changes)
        return body

    def create(body, token='fixture-admin'):
        return request('/operations/approvals', body, token=token, method='POST')

    def decide(key, **changes):
        body = {'decision': 'approved', 'reason': 'Synthetic decision', 'expectedDecision': 'pending',
                'requestId': str(uuid.uuid4()), 'sourceClient': 'conformance'}
        body.update(changes)
        return lambda: request('/operations/approvals/' + key + '/decision', body, token='fixture-manager', method='PATCH')

    sql('UPDATE operations_branch SET active=false WHERE id=' + q(branches[0]))
    try:
        default_scope = create(approval_body(branchId=None))
        check('operations omitted branch selects an enabled membership', default_scope[0] == 201 and json.loads(default_scope[1])['branchId'] == branches[1])
    finally: sql('UPDATE operations_branch SET active=true WHERE id=' + q(branches[0]))

    body = approval_body(expiresAt='2035-01-01T00:00:00.123456789Z')
    response = create(body)
    check('operations accepts canonical approval', response[0] == 201)
    approval = json.loads(response[1])['id']
    check('operations fine-precision timestamp replay retains same response', create(body) == response)
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
        replies = list(executor.map(lambda _: create(body), range(8)))
    check('operations simultaneous approval replays return same result', all(reply == response for reply in replies))
    check('operations replay appends no duplicate audit', count('operations_admin_audit', 'approval_request_id=' + q(approval)) == 1)
    before_rejections = (count('operations_admin_audit', 'true'), count('operations_approval_request', 'true'))
    for changes in [{'branchId': branches[1]}, {'amountMinor': 11}, {'currency': 'EUR'}, {'reason': 'Changed'},
                    {'targetEntityId': 'changed'}, {'actionType': 'payment_void'}, {'expiresAt': None}]:
        check('operations rejects bound approval payload change ' + next(iter(changes)), create({**body, **changes})[0] == 409)
    check('operations rejects key replay by another requester', create(body, 'fixture-manager')[0] == 409)
    check('operations refuses linked item outside branch', create(approval_body(workItemId=item(branches[1])))[0] == 404)
    for changes in [{'requestId': ''}, {'sourceClient': ''}, {'amountMinor': -1}, {'currency': 'usd'}, {'idempotencyKey': ' '}, {'expiresAt': '2000-01-01T00:00:00Z'}]:
        check('operations rejects invalid approval ' + next(iter(changes)), create(approval_body(**changes))[0] == 422)
    check('operations rejected approval creates and replays persist no effects', before_rejections == (count('operations_admin_audit', 'true'), count('operations_approval_request', 'true')))
    self_decision = {'decision': 'approved', 'reason': 'Synthetic', 'expectedDecision': 'pending', 'requestId': 'self', 'sourceClient': 'conformance'}
    check('operations requester cannot approve own request', request('/operations/approvals/' + approval + '/decision', self_decision, method='PATCH')[0] == 409)
    response = audit_failure(decide(approval))
    check('operations decision audit failure rolls back decision', response[0] == 500
          and sql('SELECT decision FROM operations_approval_request WHERE id=' + q(approval)) == 'pending')
    replies = race('SELECT id FROM operations_approval_request WHERE id=' + q(approval) + ' FOR UPDATE', [decide(approval) for _ in range(8)])
    check('operations concurrent decisions commit once', sorted(reply[0] for reply in replies) == [200] + [409] * 7
          and count('operations_admin_audit', 'approval_request_id=' + q(approval)) == 2)
    check('operations terminal approval cannot be reopened', decide(approval, decision='rejected', expectedDecision='approved')()[0] == 409)
    expired = json.loads(create(approval_body())[1])['id']
    replies = race("UPDATE operations_approval_request SET expires_at=clock_timestamp()+interval '1 second' WHERE id=" + q(expired),
                   [decide(expired)], before_release=lambda: time.sleep(1.1))
    check('operations decision checks real clock after lock wait', replies[0][0] == 409
          and sql('SELECT decision FROM operations_approval_request WHERE id=' + q(expired)) == 'pending'
          and count('operations_admin_audit', 'approval_request_id=' + q(expired)) == 1)
    sql('DROP FUNCTION synthetic_ops_audit_failure()')
