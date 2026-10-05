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
        sql("INSERT INTO operations_business_hours(organization_id,branch_id,iso_weekday,opens_at,closes_at) SELECT " + q(org) + "," + q(branch) + ",day,'00:00','23:59' FROM generate_series(1,7) day")
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

    # Aggregates must not reveal payments excluded from the specialist's list.
    for role in ['teacher', 'engineer']:
        sql("INSERT INTO operations_scope_member(organization_id,branch_id,party_id) VALUES (" + q(org) + "," + q(branches[0]) + "," + actors[role] + ")")
    payment = item()
    sql("UPDATE operations_work_item SET entity_type='payment',amount_minor=12345,currency='USD',payment_state='completed' WHERE id=" + q(payment))
    assigned = item()
    sql("UPDATE operations_work_item SET entity_type='booking',assignee_party_id=" + actors['teacher'] + " WHERE id=" + q(assigned))
    metrics_path = '/operations/metrics?organizationId=' + org + '&branchId=' + branches[0]
    for role, expected_bookings in [('teacher', 1), ('engineer', 0)]:
        response = request(metrics_path, token='fixture-' + role)
        check('operations ' + role + ' metrics use visible assigned items only', response[0] == 200
              and json.loads(response[1])['revenueReceivedTodayMinor'] == 0
              and json.loads(response[1])['unassignedWork'] == 0
              and json.loads(response[1])['reservationsAwaitingConfirmation'] == expected_bookings)
        check('operations ' + role + ' cannot read hidden payment detail',
              request('/operations/work-items/' + payment, token='fixture-' + role)[0] == 404)
    response = request(metrics_path)
    check('operations manager metrics retain authorized financial totals', response[0] == 200
          and json.loads(response[1])['revenueReceivedTodayMinor'] == 12345)

    failure_ids = []
    for index, branch in enumerate(branches):
        for _ in range(index + 1):
            failure_ids.append((branch, sql("INSERT INTO operations_integration_failure(organization_id,branch_id,provider,direction,source_record_type,source_record_id,failure_code,redacted_summary,retryable) VALUES (" + q(org) + "," + q(branch) + ",'synthetic','internal','synthetic','private-reference','synthetic','Synthetic private',true) RETURNING id")))
    for role in ['admin', 'manager', 'teacher', 'engineer']:
        response = request(metrics_path, token='fixture-' + role)
        check('operations failure count is branch and role scoped ' + role, response[0] == 200
              and json.loads(response[1])['integrationFailures'] == (1 if role in ['admin', 'manager'] else 0))
    sql('UPDATE operations_scope_member SET active=false WHERE party_id=' + actors['manager'] + ' AND branch_id=' + q(branches[1]))
    try:
        response = request('/operations/integration-failures?organizationId=' + org, token='fixture-manager')
        check('operations failure list excludes other branches', response[0] == 200
              and {row['id'] for row in json.loads(response[1])} == {failure_ids[0][1]})
    finally:
        sql('UPDATE operations_scope_member SET active=true WHERE party_id=' + actors['manager'] + ' AND branch_id=' + q(branches[1]))

    for kind, arguments in [('priority', {}), ('seen', {}), ('assignment', {'assigneePartyId': int(actors['manager'])}),
                            ('transition', {'targetStatus': 'resolved'})]:
        key = item()
        for invalid in [{'version': 0}, {'requestId': ''}, {'sourceClient': ''}]:
            check('operations ' + kind + ' rejects invalid ' + next(iter(invalid)),
                  command(kind, key, **arguments, **invalid)()[0] == 422 and effects(key) == (0, 0, 0))
        responses = race('SELECT id FROM operations_work_item WHERE id=' + q(key) + ' FOR UPDATE',
                         [command(kind, key, **arguments) for _ in range(8)])
        (output / ('operations-' + kind + '-responses.json')).write_text(json.dumps(responses, indent=2) + '\n')
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
        for grant_id in grant: sql('UPDATE party_security_role SET active=false WHERE id=' + q(grant_id))

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
    # One read policy drives list, detail, stream and aggregates; read-only grants
    # must not broaden a separate, narrower write grant.
    domains = ['booking', 'course_registration', 'maintenance_ticket', 'manual', 'payment', 'marketplace_order', 'social_event', 'intern_project', 'security_incident', 'synthetic_future']
    policy_items = {domain: item() for domain in domains}
    for domain, key in policy_items.items():
        sql('UPDATE operations_work_item SET entity_type=' + q(domain) + ' WHERE id=' + q(key))
    expected = {
        'read-only': set(domains) - {'security_incident'},
        'teacher': {'booking', 'course_registration', 'social_event', 'intern_project'},
        'engineer': {'booking', 'maintenance_ticket', 'social_event', 'intern_project'},
        'maintenance': {'booking', 'maintenance_ticket', 'manual'},
        'reception': {'booking', 'course_registration', 'manual', 'payment', 'social_event'},
        'producer': set(), 'a-and-r': set()}
    check('operations policy fixture contains every required role', set(expected) <= set(actors))
    for role, visible in expected.items():
        sql('INSERT INTO operations_scope_member(organization_id,branch_id,party_id) VALUES (' + q(org) + ',' + q(branches[0]) + ',' + actors[role] + ') ON CONFLICT DO NOTHING')
        sql('UPDATE operations_work_item SET assignee_party_id=' + actors[role] + ' WHERE id IN (' + ','.join(q(key) for key in policy_items.values()) + ')')
        listed = request('/operations/work-items?organizationId=' + org + '&branchId=' + branches[0] + '&limit=100', token='fixture-' + role)
        check('operations list accepts scoped actor ' + role, listed[0] == 200)
        listed_ids = {row['id'] for row in json.loads(listed[1])['items']}
        for domain, key in policy_items.items():
            allowed = domain in visible
            check('operations list/detail agree for ' + role + ' ' + domain,
                  (key in listed_ids) == allowed and request('/operations/work-items/' + key, token='fixture-' + role)[0] == (200 if allowed else 404))
    if 'read-only' in actors:
        extra = sql("INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT " + actors['teacher'] + ",id,'bootstrap',true FROM security_role WHERE code='read-only' RETURNING id")
        try:
            key = policy_items['payment']
            check('operations ReadOnly plus Teacher permits broad read', request('/operations/work-items/' + key, token='fixture-teacher')[0] == 200)
            response = request('/operations/work-items/' + key + '/seen', {'expectedVersion': 1, 'reason': None, 'requestId': 'mixed-role', 'sourceClient': 'conformance'}, token='fixture-teacher', method='PATCH')
            check('operations read-only grant cannot expand teacher writes', response[0] == 404 and sql('SELECT version FROM operations_work_item WHERE id=' + q(key)) == '1')
        finally: sql('UPDATE party_security_role SET active=false WHERE id=' + q(extra))

    # Manual keys are receipt identifiers, never trusted source correlation keys.
    def manual(**changes):
        body = {'organizationId': org, 'branchId': branches[0], 'entityType': 'uncorrelated_inbound',
                'entityId': None, 'uncorrelated': True, 'correlationKey': str(uuid.uuid4()),
                'titleEs': 'Synthetic manual', 'titleEn': 'Synthetic manual',
                'descriptionEs': 'Synthetic', 'descriptionEn': 'Synthetic', 'priority': 'normal',
                'responsibleTeam': None, 'customerPartyId': None, 'serviceKey': None,
                'amountMinor': None, 'currency': None, 'metadata': {}, 'requestId': str(uuid.uuid4()), 'sourceClient': 'conformance'}
        body.update(changes)
        return body

    def post_manual(body, token='fixture-admin'):
        return request('/operations/work-items', body, token=token, method='POST')

    private = item(branches[1])
    sql("UPDATE operations_work_item SET entity_type='payment',amount_minor=777,currency='USD',correlation_key='payment:synthetic-private' WHERE id=" + q(private))
    snapshot = sql('SELECT row_to_json(item)::text FROM operations_work_item item WHERE id=' + q(private))
    check('operations manual denies unassigned specialist creation',
          post_manual(manual(correlationKey='payment:synthetic-private'), 'fixture-teacher')[0] == 403)
    check('operations manual rejects lifecycle control metadata', post_manual(manual(metadata={'terminal': True}))[0] == 422)
    body = manual(correlationKey='payment:synthetic-private')
    response = post_manual(body)
    check('operations manual cannot overwrite existing source correlation', response[0] == 201
          and json.loads(response[1])['id'] != private and json.loads(response[1])['correlationKey'].startswith('manual:')
          and snapshot == sql('SELECT row_to_json(item)::text FROM operations_work_item item WHERE id=' + q(private)))
    manual_id = json.loads(response[1])['id']
    manual_effects = effects(manual_id)
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as executor:
        replies = list(executor.map(lambda _: post_manual({**body, 'requestId': str(uuid.uuid4())}), range(8)))
    check('operations identical manual replays return one item and evidence set', all(reply == response for reply in replies)
          and effects(manual_id) == manual_effects == (1, 1, 1))
    for changes in [{'branchId': branches[1]}, {'titleEn': 'Changed'}, {'priority': 'high'}, {'amountMinor': 1, 'currency': 'USD'}]:
        check('operations binds manual replay ' + next(iter(changes)), post_manual({**body, **changes})[0] == 409)
    check('operations binds manual replay actor', post_manual(body, 'fixture-manager')[0] == 409)
    raced_body = manual()
    replies = race('SELECT id FROM operations_organization WHERE id=' + q(org) + ' FOR UPDATE',
                   [lambda i=i: post_manual({**raced_body, 'titleEn': 'Version ' + str(i)}) for i in range(8)])
    check('operations conflicting manual retries commit once', sorted(reply[0] for reply in replies) == [201] + [409] * 7)

    future_key = 'payment:synthetic-future'
    future_manual = post_manual(manual(correlationKey=future_key))
    check('operations future source key is not preclaimed by manual creation', future_manual[0] == 201
          and json.loads(future_manual[1])['correlationKey'] != future_key)
    def enqueue(correlation, branch, entity='payment', metadata='{}'):
        source_id = sql("INSERT INTO payment(party_id,method,amount_cents,currency,received_at) VALUES (" + actors['admin'] + ",'CashM',1,'USD',now()) RETURNING id") if entity == 'payment' else str(uuid.uuid4())
        return sql("INSERT INTO operations_domain_event(organization_id,branch_id,event_type,aggregate_type,aggregate_id,source_system,source_channel,correlation_key,deduplication_key,occurred_at,payload) VALUES ("
                   + q(org) + ',' + q(branch) + ",'synthetic.source'," + q(entity) + ',' + q(source_id)
                   + ",'synthetic','synthetic'," + q(correlation) + ',' + q(str(uuid.uuid4()))
                   + ",now(),jsonb_build_object('titleEs','Synthetic source','titleEn','Synthetic source','metadata'," + q(metadata) + "::jsonb)) RETURNING id")
    source_event = enqueue(future_key, branches[1])
    check('operations trusted source after manual key uses a distinct item',
          sql("SELECT processed||'|'||failed FROM operations_process_outbox_batch(1,'synthetic'," + q(source_event) + '::uuid)') == '1|0'
          and sql('SELECT branch_id::text FROM operations_work_item WHERE organization_id=' + q(org) + ' AND correlation_key=' + q(future_key)) == branches[1])
    poison = enqueue(future_key, branches[0], 'booking')
    check('operations projection rejects cross-domain and branch key collision',
          sql("SELECT processed||'|'||failed FROM operations_process_outbox_batch(1,'synthetic'," + q(poison) + '::uuid)') == '0|1'
          and sql('SELECT entity_type FROM operations_work_item WHERE organization_id=' + q(org) + ' AND correlation_key=' + q(future_key)) == 'payment')
    unrelated = enqueue('payment:unrelated-pending', branches[1])
    check('operations manual projects only its requested event', post_manual(manual())[0] == 201
          and sql('SELECT status FROM operations_outbox WHERE event_id=' + q(unrelated)) == 'pending')
    before = tuple(count(table, 'true') for table in ['operations_domain_event', 'operations_work_item', 'operations_outbox'])
    check('operations failed manual projection rolls back receipt and item', audit_failure(lambda: post_manual(manual()))[0] == 409
          and before == tuple(count(table, 'true') for table in ['operations_domain_event', 'operations_work_item', 'operations_outbox']))
    legacy_body = manual()
    sql("INSERT INTO operations_domain_event(organization_id,branch_id,event_type,aggregate_type,aggregate_id,source_system,source_channel,correlation_key,deduplication_key,occurred_at,payload) VALUES ("
        + q(org) + ',' + q(branches[0]) + ",'manual.created','manual','legacy','tdf-hq','manual'," + q(legacy_body['correlationKey'])
        + ",encode(digest(" + q(legacy_body['correlationKey'] + ':manual-created') + ",'sha256'),'hex'),now(),'{}')")
    check('operations refuses unbound legacy manual replay', post_manual(legacy_body)[0] == 409)

    # Exercise the installed WhatsApp capture trigger, not a handwritten DTO.
    sender = 'synthetic-identity-' + uuid.uuid4().hex
    thread_ids = []
    for party in ['NULL', actors['admin'], 'NULL']:
        external = 'synthetic-message-' + uuid.uuid4().hex
        sql("INSERT INTO whats_app_message(external_id,sender_id,direction,created_at,party_id,text) VALUES (" + q(external) + ',' + q(sender) + ",'inbound',now()," + party + ",'Synthetic private message')")
        event_id = sql('SELECT id FROM operations_domain_event WHERE provider_event_id=' + q(external))
        check('operations WhatsApp source trigger produces one event', bool(event_id) and '\n' not in event_id)
        check('operations WhatsApp identity progression projects',
              sql("SELECT processed||'|'||failed FROM operations_process_outbox_batch(1,'synthetic'," + q(event_id) + '::uuid)') == '1|0')
        row = json.loads(sql("SELECT row_to_json(item)::text FROM operations_work_item item WHERE correlation_key=" + q('whatsapp:' + sender)))
        thread_ids.append(row['id'])
        if len(thread_ids) > 1:
            check('operations WhatsApp known identity remains bound', row['entity_type'] == 'party' and row['entity_id'] == actors['admin'] and not row['uncorrelated'])
    check('operations WhatsApp identity retains a single thread', len(set(thread_ids)) == 1)

    # Appending a note has no expectedVersion, but current ownership is still locked.
    note_item = item()
    note_body = {'body': 'Synthetic note', 'mentionedPartyIds': [], 'requestId': 'synthetic-note', 'sourceClient': 'conformance'}
    def post_note(token='fixture-admin'):
        return request('/operations/work-items/' + note_item + '/notes', note_body, token=token, method='POST')
    check('operations note appends with atomic audit', post_note()[0] == 201
          and count('operations_note', 'work_item_id=' + q(note_item)) == 1)
    check('operations note audit failure rolls back the note', audit_failure(post_note)[0] == 500
          and count('operations_note', 'work_item_id=' + q(note_item)) == 1)
    for label, query, restore in [
        ('scope', 'UPDATE operations_scope_member SET active=false WHERE party_id=' + actors['admin'] + ' AND branch_id=' + q(branches[0]), 'UPDATE operations_scope_member SET active=true WHERE party_id=' + actors['admin']),
        ('session', "UPDATE api_token SET active=false WHERE token='fixture-admin'", "UPDATE api_token SET active=true WHERE token='fixture-admin'")]:
        try:
            replies = race(query, [post_note])
            check('operations note denies revoked ' + label, replies[0][0] in [401, 403]
                  and count('operations_note', 'work_item_id=' + q(note_item)) == 1)
        finally: sql(restore)
    sql("UPDATE operations_work_item SET entity_type='booking',assignee_party_id=" + actors['teacher'] + ' WHERE id=' + q(note_item))
    replies = race('UPDATE operations_work_item SET assignee_party_id=NULL WHERE id=' + q(note_item),
                   [lambda: post_note('fixture-teacher')])
    check('operations note denies lost assignment after waiting', replies[0][0] == 404
          and count('operations_note', 'work_item_id=' + q(note_item)) == 1)

    failure_id = failure_ids[0][1]
    replay_body = {'reason': 'Synthetic retry', 'requestId': 'synthetic-retry', 'sourceClient': 'conformance'}
    def replay():
        return request('/operations/integration-failures/' + failure_id + '/replay', replay_body, method='POST')
    try:
        replies = race('UPDATE operations_branch SET active=false WHERE id=' + q(branches[0]), [replay])
        check('operations failure replay rejects a disabled branch after waiting', replies[0][0] == 403
              and sql('SELECT attempt_count FROM operations_integration_failure WHERE id=' + q(failure_id)) == '0')
    finally: sql('UPDATE operations_branch SET active=true WHERE id=' + q(branches[0]))
    check('operations failure replay audit error rolls back state', audit_failure(replay)[0] == 500
          and sql('SELECT status FROM operations_integration_failure WHERE id=' + q(failure_id)) == 'open')
    sql("UPDATE operations_integration_failure SET status='dead_letter' WHERE id=" + q(failure_id))
    replies = race('SELECT id FROM operations_integration_failure WHERE id=' + q(failure_id) + ' FOR UPDATE', [replay for _ in range(8)])
    check('operations failure retry admits exactly one request', sorted(reply[0] for reply in replies) == [202] + [409] * 7
          and sql('SELECT attempt_count FROM operations_integration_failure WHERE id=' + q(failure_id)) == '1')
    check('operations failure audit retains actual previous status', sql("SELECT previous_value->>'status' FROM operations_admin_audit WHERE correlation_id=" + q(failure_id)) == 'dead_letter')

    view = {'organizationId': org, 'name': 'Synthetic owned view', 'shared': False, 'filters': {},
            'columns': ['status'], 'widgets': [], 'subscribedEventTypes': [], 'requestId': 'view', 'sourceClient': 'conformance'}
    push = {'organizationId': org, 'platform': 'web', 'deviceToken': 'synthetic-device-token-for-conformance',
            'requestId': 'push', 'sourceClient': 'conformance'}
    def save_view(body=view): return request('/operations/saved-views', body, method='POST')
    def save_push(body=push): return request('/operations/push-subscriptions', body, method='POST')
    response = save_view()
    check('operations saves owned typed view', response[0] == 201)
    view_id = json.loads(response[1])['id']
    check('operations repeated owned view replaces same row', json.loads(save_view()[1])['id'] == view_id)
    check('operations malformed view shape is rejected', save_view({**view, 'columns': {'status': True}})[0] == 422)
    check('operations failed view audit rolls back replacement', audit_failure(lambda: save_view({**view, 'shared': True}))[0] == 500
          and sql('SELECT shared FROM operations_saved_view WHERE id=' + q(view_id)) == 'f')
    response = save_push()
    check('operations registers encrypted owned push token', response[0] == 201)
    push_id = json.loads(response[1])['id']
    check('operations stores recoverable encrypted token without public token field',
          push['deviceToken'] not in response[1]
          and sql("SELECT pgp_sym_decrypt(encrypted_device_token,'synthetic-operations-conformance-key') FROM operations_push_subscription WHERE id=" + q(push_id)) == push['deviceToken'])
    check('operations same actor token upsert retains identity', json.loads(save_push()[1])['id'] == push_id)
    check('operations failed push audit rolls back replacement', audit_failure(lambda: save_push({**push, 'platform': 'ios'}))[0] == 500
          and sql('SELECT platform FROM operations_push_subscription WHERE id=' + q(push_id)) == 'web')
    for label, call in [('view', save_view), ('push', save_push)]:
        try:
            replies = race("UPDATE api_token SET active=false WHERE token='fixture-admin'", [call])
            check('operations ' + label + ' denies session revoked before commit', replies[0][0] == 401)
        finally: sql("UPDATE api_token SET active=true WHERE token='fixture-admin'")

    sql('DROP FUNCTION synthetic_ops_audit_failure()')
