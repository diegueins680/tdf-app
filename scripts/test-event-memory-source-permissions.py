#!/usr/bin/env python3
"""Real permission-revocation interleavings inside the runner's owned test DB."""
import argparse
import concurrent.futures
import json
import os
import queue
import shlex
import subprocess
import threading
import time
import urllib.parse
import uuid

if not __debug__:
    raise SystemExit('Permission concurrency assertions require normal Python mode')

parser = argparse.ArgumentParser()
parser.add_argument('--database-url', required=True)
parser.add_argument('--only', default='')
args = parser.parse_args()
if any(os.environ.get(key) for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE')):
    raise SystemExit('Refusing libpq routing overrides')
if args.database_url.startswith(('postgresql://', 'postgres://')):
    parsed = urllib.parse.urlsplit(args.database_url)
    if parsed.query or parsed.fragment:
        raise SystemExit('Refusing additional URI routing options')
    host = parsed.hostname
else:
    settings = dict(token.split('=', 1) for token in shlex.split(args.database_url))
    if set(settings) - {'host', 'port', 'user', 'dbname'}:
        raise SystemExit('Refusing unsupported libpq routing options')
    host = settings.get('host')
if not (host in ('127.0.0.1', 'localhost', '::1') or (
        host == 'postgres' and os.environ.get('GITHUB_ACTIONS') == 'true')):
    raise SystemExit('Refusing a non-test database host')
psql = ['psql', '-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', args.database_url]


def sql(statement):
    result = subprocess.run(psql, input="SET statement_timeout='12s';\n" + statement,
                            capture_output=True, text=True, timeout=16)
    assert result.returncode == 0, result.stderr[-2000:]
    return result.stdout.strip()


if sql('SELECT current_database();') != 'tdf_event_memory_source_moderation_test':
    raise SystemExit('Refusing a database not owned by the source moderation runner')


def literal(value):
    return "'" + str(value).replace("'", "''") + "'"


class Session:
    def __init__(self):
        self.process = subprocess.Popen(psql, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE, text=True, bufsize=1)
        self.lines = queue.Queue()
        def pump():
            for line in self.process.stdout:
                self.lines.put(line.strip())
            self.lines.put(None)
        threading.Thread(target=pump, daemon=True).start()
        self.send("SET statement_timeout='12s'; BEGIN; SELECT pg_backend_pid();")
        self.pid = int(self.read())

    def send(self, statement):
        self.process.stdin.write(statement + '\n')
        self.process.stdin.flush()

    def read(self):
        value = self.lines.get(timeout=14)
        assert value is not None, self.process.stderr.read()[-2000:]
        return value

    def close(self, commit=False):
        if self.process.poll() is None:
            self.send(('COMMIT;' if commit else 'ROLLBACK;') + '\n\\q')
            try:
                self.process.wait(timeout=15)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=5)
            assert self.process.returncode == 0, self.process.stderr.read()[-2000:]


sql('''
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
 SELECT n,'Synthetic source permission '||n,false,now() FROM generate_series(919147001,919147003) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
 SELECT n,'source-permission-'||n,'not-a-login-hash',true FROM generate_series(919147001,919147003) n;
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 919147003,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919147001,NULL,'Synthetic source permission race',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":true}' FROM event_type WHERE code='concert';
INSERT INTO event_moment(id,event_id,author_party_id,author_name,caption,media_url,media_type,created_at,updated_at)
 VALUES(919147001,919147001,'919147002','Synthetic author','Synthetic permission memory','','text',now(),now());
''')
target = sql("SELECT interaction_register('event_moment','919147001',919147003);")
role = sql("SELECT id FROM security_role WHERE code='admin';")


def snapshot():
    return json.loads(sql(f"""SELECT jsonb_build_object('state',m.source_moderation_state,
     'version',t.version,'sourceRevision',m.updated_at::text,
     'audit',(SELECT count(*) FROM interaction_audit WHERE target_id=t.id),
     'requests',(SELECT count(*) FROM interaction_request WHERE target_id=t.id))
     FROM event_moment m JOIN interaction_target t ON t.entity_key=m.id::text
     WHERE t.id={literal(target)};"""))


def command(payload, key):
    return f'SELECT interaction_command(919147003,{literal(target)},{literal(key)},{literal(json.dumps(payload))});'


def observed_block(future, application, blocker):
    deadline = time.monotonic() + 6
    while time.monotonic() < deadline:
        if future.done():
            future.result()
            return False
        if sql(f"SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND application_name={literal(application)} AND {blocker}=ANY(pg_blocking_pids(pid)));") == 't':
            return True
        time.sleep(.01)
    raise AssertionError('Expected permission-row lock was not observed')


def require_block(future, application, holder, case):
    if not observed_block(future, application, holder.pid):
        holder.close(commit=True)
        active = sql(f"SELECT active FROM party_security_role WHERE party_id=919147003 AND role_id={literal(role)};")
        raise AssertionError('UNSERIALIZED committed interleaving: ' + json.dumps({'case':case,'assignmentActive':active,'source':snapshot()}))


passed = []
grant = 'assignment'
revoke = f"UPDATE party_security_role SET active=false,version=version+1 WHERE party_id=919147003 AND role_id={literal(role)};"
for operation in ('source.remove', 'source.restore', 'source.hide', 'source.report.resolve'):
    for ordering in ('action-first', 'revocation-first'):
        case = grant + '/' + operation + '/' + ordering
        if args.only and args.only != case:
            continue
        sql(f"UPDATE party_security_role SET active=true,version=version+1 WHERE party_id=919147003 AND role_id={literal(role)};")
        initial = 'removed' if operation == 'source.restore' else 'visible'
        sql(f"UPDATE event_moment SET source_moderation_state='{initial}',updated_at=clock_timestamp() WHERE id=919147001;")
        if operation == 'source.report.resolve':
            sql(f"""INSERT INTO interaction_report(target_id,comment_id,reporter_id,reason,reported_version)
              VALUES({literal(target)},NULL,919147001,'Synthetic permission review',1)
              ON CONFLICT(target_id,reporter_id) WHERE comment_id IS NULL DO UPDATE SET state='open';""")
        current = json.loads(sql(f"SELECT interaction_summary(919147003,'event_moment','919147001');"))
        payload = {'operation':operation,'reason':'Synthetic permission serialization',
                   'expectedVersion':current['version'],'expectedSourceRevision':current['sourceModeration']['sourceRevision']}
        if operation == 'source.report.resolve':
            payload['decision'] = 'reviewed'
        key = str(uuid.uuid4())
        before = snapshot()
        holder = Session()
        pool = concurrent.futures.ThreadPoolExecutor(max_workers=1)
        try:
            application = 'source-permission-' + uuid.uuid4().hex
            if ordering == 'action-first':
                holder.send(command(payload, key) + " SELECT 'action-held';")
                result = json.loads(holder.read())
                assert 'error' not in result, result
                assert holder.read() == 'action-held'
                waiting = pool.submit(sql, 'SET application_name=' + literal(application) + '; ' + revoke)
                require_block(waiting, application, holder, case)
                holder.close(commit=True)
                waiting.result(timeout=16)
                after = snapshot()
                assert after['audit'] == before['audit'] + 1 and after['requests'] == before['requests'] + 1
                replay = json.loads(sql(command(payload, key)))
                assert replay.get('error') in ('forbidden','unavailable','invalid'), replay
                assert snapshot() == after, 'Revoked replay must not change state or audit'
            else:
                holder.send(revoke + " SELECT 'revocation-held';")
                assert holder.read() == 'revocation-held'
                waiting = pool.submit(sql, 'SET application_name=' + literal(application) + '; ' + command(payload, key))
                require_block(waiting, application, holder, case)
                holder.close(commit=True)
                result = json.loads(waiting.result(timeout=16))
                assert result.get('error') in ('forbidden','unavailable','invalid'), result
                assert snapshot() == before, 'Revocation-first must leave source, version, audit and ledger unchanged'
        finally:
            holder.close()
            pool.shutdown(wait=True)
        passed.append(case)
        print(json.dumps({'status':'PASS','case':case,'actualPostgres':True}), flush=True)
assert len(passed) == (1 if args.only else 8), 'Every expected interleaving must execute'
print(json.dumps({'status':'PASS','interleavings':len(passed),'source':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()}))
