#!/usr/bin/env python3
"""Real service HTTP/lock tests in a newly owned, completely migrated test DB.

No operator credential is used. The only secrets are disposable synthetic values.
Every request uses loopback; the server receives no inherited provider settings.
"""
import argparse
import concurrent.futures
import datetime
import hashlib
import json
import os
import pathlib
import selectors
import signal
import subprocess
import sys
import time
import urllib.error
import urllib.parse
import urllib.request


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', required=True)
    parser.add_argument('--database-url', required=True)
    parser.add_argument('--output', required=True)
    args = parser.parse_args()
    root = pathlib.Path(__file__).resolve().parent.parent
    binary = pathlib.Path(args.binary).resolve(strict=True)
    # Reuse the canonical routing validator BEFORE database creation, SQL, or
    # cleanup ownership. Its CI-host opt-in is only the actual GHA environment.
    validate = """import {disposablePostgresUrl} from './scripts/lib/disposable-postgres-url.mjs';
    console.log(disposablePostgresUrl(process.argv[1],
      {ci:process.env.GITHUB_ACTIONS==='true'}).href);"""
    database_url = subprocess.check_output(
        ['node', '--input-type=module', '-e', validate, args.database_url],
        cwd=root, text=True).strip()
    parsed = urllib.parse.urlsplit(database_url)
    database = parsed.path[1:]
    maintenance_url = urllib.parse.urlunsplit(parsed._replace(path='/postgres'))
    output = pathlib.Path(args.output).resolve()
    output.mkdir(mode=0o700, parents=False, exist_ok=False)
    env = {k: v for k, v in os.environ.items()
           if k in ('HOME', 'PATH', 'USER', 'LOGNAME', 'LANG', 'LC_CTYPE', 'TZ', 'TMPDIR')}
    # Never inherit any PG* routing, options, or password-file override.
    if os.environ.get('TDF_TEST_POSTGRES_PASSWORD'):
        env['PGPASSWORD'] = os.environ['TDF_TEST_POSTGRES_PASSWORD']
    env['PGCONNECT_TIMEOUT'] = '5'
    env['PGPASSFILE'] = os.devnull
    report = {'source': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip(),
              'dirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=root)),
              'binarySha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
              'assertions': [], 'requests': [], 'lockWitnesses': [], 'boundaryChecks': [],
              'externalProviderCalls': 0, 'liveCredentialsUsed': False}
    manifest = json.loads((root / 'scripts/production-migrations.json').read_text())
    paths = ['scripts/test-event-watch-http.py', 'scripts/production-migrations.json',
             'tdf-hq/src/TDF/EventWatch.hs', 'tdf-hq/src/TDF/Auth.hs',
             'tdf-hq/src/TDF/API.hs', 'tdf-hq/src/TDF/Server.hs',
             'tdf-hq/src/TDF/Server/EventResearch.hs', 'tdf-hq/tdf-hq.cabal',
             'tdf-hq/test/sql/event_watch_service_auth.sql',
             'tdf-hq/test/sql/event_watch_archived_actor.sql',
             'scripts/lib/disposable-postgres-url.mjs',
             'scripts/__tests__/fixtures/production-schema-20260814.sql',
             'scripts/__tests__/fixtures/catalog-production-source-fixture.sql',
             'scripts/render-production-migration-batch.mjs', 'scripts/lib/production-release.mjs']
    paths += [entry['path'] for entry in manifest['migrations']]
    report['inputs'] = {p: hashlib.sha256((root / p).read_bytes()).hexdigest() for p in paths}
    owned = False
    server = None
    holders = []
    futures = concurrent.futures.ThreadPoolExecutor(max_workers=4)
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    server_log = None

    def terminate(_signum, _frame):
        raise InterruptedError('Owned event-watch fixture interrupted')

    signal.signal(signal.SIGTERM, terminate)

    def run(command, data=None, check=True):
        result = subprocess.run(command, input=data, capture_output=True,
                                cwd=root, env=env, timeout=180)
        if check and result.returncode:
            raise AssertionError('Owned fixture command failed: ' + result.stderr.decode()[-1500:])
        return result

    def sql(command, check=True):
        return run(['psql', '-XwqAt', '-v', 'ON_ERROR_STOP=1', '--dbname', database_url],
                   command.encode(), check)

    def value(command):
        return sql(command).stdout.decode().strip()

    def apply(path):
        run(['psql', '-Xwq', '-v', 'ON_ERROR_STOP=1', '--dbname', database_url,
             '-f', str(path)])

    def check(condition, label):
        assert condition, label
        report['assertions'].append(label)

    def boundary(condition, label):
        # Collect all independently relevant boundary observations, then fail
        # the suite if ANY is false. Never convert an observed violation to PASS.
        report['boundaryChecks'].append({'label': label, 'passed': bool(condition)})
        if condition:
            report['assertions'].append(label)

    def wait_for(predicate, label, seconds=15):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            if predicate():
                return
            time.sleep(.05)
        raise AssertionError(label)

    def hold(statement):
        proc = subprocess.Popen(['psql', '-XwqAt', '-v', 'ON_ERROR_STOP=1',
                                 '--dbname', database_url], stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                cwd=root, env=env, bufsize=0)
        holders.append(proc)
        proc.stdin.write(("BEGIN; " + statement + "; SELECT 'owned-lock-ready';\n").encode())
        proc.stdin.flush()
        selector = selectors.DefaultSelector()
        selector.register(proc.stdout, selectors.EVENT_READ)
        deadline = time.monotonic() + 15
        ready = False
        try:
            while time.monotonic() < deadline:
                if selector.select(timeout=.2):
                    line = proc.stdout.readline()
                    if line.strip() == b'owned-lock-ready':
                        ready = True
                        break
                assert proc.poll() is None, 'Owned barrier connection exited'
        finally:
            selector.close()
        assert ready, 'Owned SQL barrier was not established'
        return proc

    def release(proc):
        stdout, stderr = proc.communicate(b'COMMIT;\n\\q\n', timeout=15)
        assert proc.returncode == 0, stderr.decode()[-1000:]

    def blocked(fragment, event):
        # Only count this owned database's backend wait; no SQL text, credentials,
        # other-database process information, or arbitrary process data is logged.
        literal = fragment.replace("'", "''")
        result = value("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() "
                       "AND pid<>pg_backend_pid() AND wait_event_type='Lock' "
                       f"AND lower(query) LIKE '%{literal}%' "
                       + ("AND wait_event='advisory'" if event == 'advisory' else ''))
        return int(result) > 0

    def witness(fragment, event, label):
        wait_for(lambda: blocked(fragment, event), label)
        report['lockWitnesses'].append({'label': label, 'actualOwnedDatabaseWaitObserved': True,
                                        'event': event})

    def request(path, token=None, body=None, method='GET', expected=200):
        headers = {'Content-Type': 'application/json'} if body is not None else {}
        if token:
            headers['Authorization'] = 'Bearer ' + token
        req = urllib.request.Request(origin + path,
                                     data=json.dumps(body).encode() if body is not None else None,
                                     headers=headers, method=method)
        try:
            response = opener.open(req, timeout=25)
        except urllib.error.HTTPError as error:
            response = error
        raw = response.read()
        report['requests'].append({'method': method, 'path': path, 'status': response.status})
        detail = '<credential response redacted>' if path == '/integrations/event-watch/token' else raw[:250]
        if expected is not None:
            assert response.status == expected, (method, path, response.status, expected, detail)
        payload = json.loads(raw) if 'json' in response.headers.get('Content-Type', '') and raw else raw
        return payload, response.headers

    def exchange(client, secret, expected=200):
        payload, headers = request('/integrations/event-watch/token', body={
            'grant_type': 'client_credentials', 'client_id': client,
            'client_secret': secret}, method='POST', expected=expected)
        if expected != 200:
            return None
        assert payload['token_type'] == 'Bearer' and 0 < payload['expires_in'] <= 900
        assert len(payload['access_token']) == 64 and set(payload['access_token']) <= set('0123456789abcdef')
        assert 'no-store' in headers.get('Cache-Control', '') and headers.get('Pragma') == 'no-cache'
        return payload['access_token']

    def import_body(key, title='Synthetic event watch candidate'):
        now = datetime.datetime.now(datetime.timezone.utc).isoformat()
        source = 'https://official.example.test/events/' + key
        return {'runKey': key, 'materialize': False, 'publish': False, 'candidate': {
            'erCandidateProvider': 'event-watch:beta-watch', 'erCandidateExternalId': key,
            'erCandidateRunId': '999999999', 'erCandidateSourceId': '999999999',
            'erCandidateReviewState': 'draft', 'erCandidateTitle': title,
            'erCandidateStartTime': None, 'erCandidateEndTime': None,
            'erCandidateTimezone': 'America/Guayaquil', 'erCandidateVenueName': None,
            'erCandidateCity': None, 'erCandidateProvince': None, 'erCandidateCountryCode': 'EC',
            'erCandidateSourceUrl': source, 'erCandidateInfoUrl': None, 'erCandidatePurchaseUrl': None,
            'erCandidatePayload': {}, 'erCandidateEvidence': [{'erEvidenceUrl': source,
                'erEvidenceKind': 'official_source', 'erEvidenceConsultedAt': now, 'erEvidenceNotes': None}],
            'erCandidateConfidence': 'low', 'erCandidateManagedFields': [], 'erCandidateVerifiedAt': now}}

    def imports(token, body, expected=200):
        return request('/integrations/event-watch/import', token, body, 'PUT', expected)[0]

    def counts():
        return value("SELECT jsonb_build_array((SELECT count(*) FROM event_research_run),"
                     "(SELECT count(*) FROM event_research_candidate),"
                     "(SELECT count(*) FROM event_research_change),(SELECT count(*) FROM social_event))")

    try:
        run(['createdb', '-w', '--encoding=UTF8', '--template=template0',
             '--maintenance-db', maintenance_url, database])
        owned = True  # A pre-existing database creation failure never owns cleanup.
        apply(root / 'scripts/__tests__/fixtures/production-schema-20260814.sql')
        apply(root / 'scripts/__tests__/fixtures/catalog-production-source-fixture.sql')
        batch = run(['node', 'scripts/render-production-migration-batch.mjs']).stdout
        (output / 'migrations.sql').write_bytes(batch)
        apply(output / 'migrations.sql')
        apply(output / 'migrations.sql')
        actual_ids = json.loads(value("SELECT jsonb_agg(migration_id ORDER BY migration_id) FROM tdf_schema_migration"))
        expected_ids = sorted(entry['id'] for entry in manifest['migrations'])
        check(actual_ids == expected_ids, 'Complete canonical migration manifest applies and replays exactly')
        report['migrationCount'] = len(actual_ids)
        report['migrationBatchSha256'] = hashlib.sha256(batch).hexdigest()
        # Original SQL constraints/revocation witnesses run unchanged, on real schema.
        sql("INSERT INTO party(id,display_name,is_org,created_at) VALUES(910001,'Event watch original fixture',false,now())")
        apply(root / 'tdf-hq/test/sql/event_watch_service_auth.sql')
        check(value("SELECT count(*) FROM event_watch_client WHERE client_id='test-watch'") == '0',
              'Original SQL witnesses execute and roll back their fixture changes')
        guard_fixture = root / 'tdf-hq/test/sql/event_watch_archived_actor.sql'
        # Controlled absent-guard counterexample on the same canonical schema.
        # The deliberately failing connection rolls back its DROP and all data;
        # the unchanged fixture must then pass with the real migration installed.
        baseline = sql(guard_fixture.read_text().replace('BEGIN;\n',
            'BEGIN;\nDROP TRIGGER identity_archive_reference_guard ON event_watch_client;\n', 1), check=False)
        check(baseline.returncode != 0 and b'archived actor acquired a new credential' in baseline.stderr,
              'Unchanged archive fixture detects missing service credential guard')
        apply(guard_fixture)
        report['assertions'].append('Archived actor insert/reassignment denied; historical update preserved; dynamic merge dependency retained')
        parties = [int(value("INSERT INTO party(display_name,is_org,created_at) "
                             f"VALUES('Synthetic event watch {label}',false,now()) RETURNING id"))
                   for label in ('alpha', 'beta')]
        secrets = {'alpha-watch': 'a' * 64, 'beta-watch': 'b' * 64}
        for client, actor in zip(secrets, parties):
            digest = hashlib.sha256(secrets[client].encode()).hexdigest()
            sql(f"INSERT INTO event_watch_client(client_id,secret_hash,actor_party_id,expires_at) "
                f"VALUES('{client}','{digest}',{actor},clock_timestamp()+interval '1 day')")
        import socket
        with socket.socket() as sock:
            sock.bind(('127.0.0.1', 0))
            http_port = sock.getsockname()[1]
        origin = 'http://127.0.0.1:' + str(http_port)
        assets = output / 'assets'
        assets.mkdir()
        server_env = {**env, 'APP_ENV': 'test', 'DATABASE_URL': database_url,
                      'APP_PORT': str(http_port), 'RUN_MIGRATIONS': 'false', 'RESET_DB': 'false',
                      'AUTO_APPLY_PRODUCTION_MIGRATIONS': 'false', 'SEED_DB': 'false',
                      'DEFAULT_LOCALE': 'es', 'HQ_ASSETS_DIR': str(assets),
                      'EVENT_DISCOVERY_ENABLED': 'false', 'ARTIST_ENRICHMENT_ENABLED': 'false',
                      'EVENT_LOGISTICS_RECHECK_ENABLED': 'false', 'OPERATIONS_WORKER_ENABLED': 'false'}
        server_log = (output / 'backend.log').open('wb')
        server = subprocess.Popen([str(binary)], cwd=root, env=server_env, stdin=subprocess.DEVNULL,
                                  stdout=server_log, stderr=subprocess.STDOUT, start_new_session=True)

        def healthy():
            assert server.poll() is None, 'Actual server exited during startup'
            try:
                return opener.open(origin + '/health', timeout=1).status == 200
            except OSError:
                return False
        wait_for(healthy, 'Actual server startup deadline', seconds=60)
        exchange('alpha-watch', 'c' * 64, 401)
        exchange('missing-watch', 'c' * 64, 401)
        alpha = exchange('alpha-watch', secrets['alpha-watch'])
        beta = exchange('beta-watch', secrets['beta-watch'])
        request('/integrations/event-watch/token', body={
            'grant_type': 'client_credentials', 'client_id': 'alpha-watch',
            'client_secret': secrets['alpha-watch'], 'scope': 'admin'}, method='POST', expected=400)
        digest = hashlib.sha256(alpha.encode()).hexdigest()
        check(value(f"SELECT token_hash<>'{alpha}' AND token_hash='{digest}' FROM event_watch_session WHERE client_id='alpha-watch'") == 't',
              'Only session digest stored; token exchange rejects incorrect/unknown credentials and emits no-store')
        request('/parties', alpha, expected=401)
        check(request('/session', alpha)[0] is None,
              'Service token does not become an interactive session')
        request('/parties', secrets['alpha-watch'], expected=401)
        request('/integrations/event-watch/candidates', secrets['alpha-watch'], expected=401)
        interactive = 'e' * 64
        sql(f"INSERT INTO api_token(token,party_id,label,active,issued_at_epoch,expires_at_epoch) "
            f"VALUES('{interactive}',{parties[0]},'password-login:synthetic-watch',true,"
            "floor(extract(epoch FROM clock_timestamp()))::bigint,"
            "floor(extract(epoch FROM clock_timestamp()))::bigint+600)")
        check(request('/session', interactive)[0] is not None,
              'Control interactive token authenticates through the real session route')
        request('/integrations/event-watch/candidates', interactive, expected=401)
        check(value(f"SELECT count(*) FROM api_token WHERE token='{alpha}'") == '0',
              'Service token and client secret grant no interactive authority')
        imported = imports(alpha, import_body('alpha-only'))
        record = imported['candidateRecord']
        check(record['erCandidateProvider'] == 'event-watch:alpha-watch'
              and record['erCandidateSourceId'] is None
              and record['erCandidateRunId'] != '999999999',
              'Client-supplied provider/run/source are replaced by server-owned identity')
        check(value(f"SELECT created_by_party_id='{parties[0]}' FROM event_research_run WHERE id={int(record['erCandidateRunId'])}") == 't',
              'Actual curator Party and namespaced run recorded')
        check(request('/integrations/event-watch/candidates', beta)[0] == [],
              'Second client cannot read first-client candidates')
        second = imports(beta, import_body('alpha-only'))
        check(second['candidateRecord']['erCandidateProvider'] == 'event-watch:beta-watch'
              and second['candidateRecord']['erCandidateId'] != record['erCandidateId'],
              'Same external identifier in second client creates a separate candidate')
        first_beta = beta
        for _ in range(5):
            beta = exchange('beta-watch', secrets['beta-watch'])
        request('/integrations/event-watch/candidates', first_beta, expected=401)
        check(value("SELECT count(*) FROM event_watch_session WHERE client_id='beta-watch'") == '4',
              'Actual credential exchange caps each client at four sessions and retires the oldest')
        before = counts()
        denied = import_body('not-authorized')
        denied.update(materialize=True, publish=True)
        imports(alpha, denied, 403)
        invalid = import_body('invalid-evidence')
        invalid['candidate']['erCandidateEvidence'] = []
        imports(alpha, invalid, 400)
        check(counts() == before, 'Denied publication and invalid candidate roll back run/candidate/audit effects')
        # An explicit grant alone cannot bypass the existing pilot/evidence checks.
        sql("UPDATE event_watch_client SET allow_publish=true WHERE client_id='alpha-watch'")
        request('/integrations/event-watch/candidates', alpha, expected=401)
        alpha = exchange('alpha-watch', secrets['alpha-watch'])
        imports(alpha, denied, 409)
        check(counts() == before, 'Granted publication still requires existing research eligibility; failed materialization is atomic')
        # Expiry while auth waits on a client lock must reject after the real wait.
        token_hash = hashlib.sha256(alpha.encode()).hexdigest()
        sql(f"UPDATE event_watch_session SET issued_at=clock_timestamp()-interval '890 seconds',expires_at=clock_timestamp()+interval '8 seconds' WHERE token_hash='{token_hash}'")
        lock = hold("SELECT client_id FROM event_watch_client WHERE client_id='alpha-watch' FOR UPDATE")
        pending = futures.submit(request, '/integrations/event-watch/candidates', alpha, None, 'GET', 401)
        witness('event_watch_client', 'client-row', 'Actual HTTP authentication waits for client row lock')
        wait_for(lambda: value(f"SELECT clock_timestamp()>=expires_at FROM event_watch_session WHERE token_hash='{token_hash}'") == 't', 'Session must actually expire during client-lock wait')
        release(lock)
        pending.result(timeout=25)
        check(counts() == before, 'Expired session rejected after actual client-lock wait, without writes')
        # Expiry after admission but before mutation must roll back all action writes.
        alpha = exchange('alpha-watch', secrets['alpha-watch'])
        token_hash = hashlib.sha256(alpha.encode()).hexdigest()
        sql(f"UPDATE event_watch_session SET issued_at=clock_timestamp()-interval '890 seconds',expires_at=clock_timestamp()+interval '8 seconds' WHERE token_hash='{token_hash}'")
        lock = hold("SELECT pg_advisory_xact_lock(hashtextextended('event-watch:alpha-watch',0))")
        pending = futures.submit(imports, alpha, import_body('expiry-rollback'), 401)
        witness('pg_advisory_xact_lock', 'advisory', 'Admitted HTTP import waits on owned per-client serialization lock')
        wait_for(lambda: value(f"SELECT clock_timestamp()>=expires_at FROM event_watch_session WHERE token_hash='{token_hash}'") == 't', 'Session must actually expire during action-lock wait')
        release(lock)
        pending.result(timeout=25)
        check(counts() == before, 'Post-admission expiry rolls back candidate, run, audit and event writes')
        # A queued rotation cannot overtake an already admitted transactional import.
        alpha = exchange('alpha-watch', secrets['alpha-watch'])
        lock = hold("SELECT pg_advisory_xact_lock(hashtextextended('event-watch:alpha-watch',0))")
        pending = futures.submit(imports, alpha, import_body('before-rotation'))
        witness('pg_advisory_xact_lock', 'advisory', 'Import admitted before rotation is serialized')
        new_secret = 'd' * 64
        new_digest = hashlib.sha256(new_secret.encode()).hexdigest()
        rotation = futures.submit(sql, f"UPDATE event_watch_client SET secret_hash='{new_digest}' WHERE client_id='alpha-watch'")
        witness('update event_watch_client', 'client-row', 'Actual concurrent rotation waits for admitted import transaction')
        release(lock)
        pending.result(timeout=25)
        rotation.result(timeout=25)
        request('/integrations/event-watch/candidates', alpha, expected=401)
        exchange('alpha-watch', secrets['alpha-watch'], 401)
        alpha = exchange('alpha-watch', new_secret)
        check(len(request('/integrations/event-watch/candidates', beta)[0]) == 1,
              'Rotation revokes old token/secret after commit while another client remains valid')
        # Concurrent same-client retries serialize to one candidate/run identity.
        retry_body = import_body('concurrent-retry')
        pair = [futures.submit(imports, alpha, retry_body) for _ in range(2)]
        responses = [item.result(timeout=25) for item in pair]
        check(responses[0]['candidateRecord']['erCandidateId'] == responses[1]['candidateRecord']['erCandidateId']
              and value("SELECT count(*) FROM event_research_run WHERE run_key='event-watch:alpha-watch:concurrent-retry'") == '1',
              'Concurrent HTTP retries preserve one client-owned candidate and run')
        sql("UPDATE event_watch_client SET active=false WHERE client_id='alpha-watch'")
        request('/integrations/event-watch/candidates', alpha, expected=401)
        exchange('alpha-watch', new_secret, 401)
        check(len(request('/integrations/event-watch/candidates', beta)[0]) == 1,
              'Committed deactivation denies session and new exchange without revoking another client')
        sql("UPDATE event_watch_client SET expires_at=clock_timestamp()-interval '1 second' WHERE client_id='beta-watch'")
        request('/integrations/event-watch/candidates', beta, expected=401)
        exchange('beta-watch', secrets['beta-watch'], 401)
        sql("UPDATE event_watch_client SET expires_at=clock_timestamp()+interval '1 day' WHERE client_id='beta-watch'")
        restored_beta = exchange('beta-watch', secrets['beta-watch'])
        request('/integrations/event-watch/candidates', beta, expected=401)
        sql(f"UPDATE event_watch_client SET actor_party_id={parties[0]} WHERE client_id='beta-watch'")
        request('/integrations/event-watch/candidates', restored_beta, expected=401)
        beta = exchange('beta-watch', secrets['beta-watch'])
        old_creator = value("SELECT created_by_party_id FROM event_research_run WHERE run_key='event-watch:beta-watch:alpha-only'")
        check(old_creator != str(parties[0]), 'Reused-run test has a genuinely different historical curator')
        before_reused = counts()
        reused = import_body('alpha-only', 'Current actor must not write an old curator batch')
        request('/integrations/event-watch/import', beta, reused, 'PUT', expected=None)
        reused_status = report['requests'][-1]['status']
        report['reusedRunCreatorProbe'] = {'actualStatus': reused_status,
            'historicalCreator': old_creator, 'currentCurator': parties[0],
            'beforeCounts': json.loads(before_reused), 'afterCounts': json.loads(counts())}
        boundary(reused_status == 409 and counts() == before_reused,
                 'Curator reassignment cannot attribute new writes to a historical run creator')
        changed_actor = imports(beta, import_body('changed-curator'))
        run_id = int(changed_actor['candidateRecord']['erCandidateRunId'])
        check(value(f"SELECT created_by_party_id='{parties[0]}' FROM event_research_run WHERE id={run_id}") == 't',
              'Expired-client/reactivation and actor changes revoke old sessions; new session uses current curator')
        # Exercise the existing pilot-only materializer positively, without
        # altering its business gates. These candidates are captured while the
        # disposable pilot is unapproved. Only a separate owned operator fixture
        # then approves it through the canonical admin HTTP route; the service
        # token cannot call that route or acquire the operator's role.
        high = []
        high_records = []
        for offset, key in [(2, 'materialize-draft'), (3, 'materialize-public'),
                            (5, 'private-other-organizer'), (3, 'same-client-alias'),
                            (7, 'reference-rebind')]:
            body = import_body(key)
            candidate = body['candidate']
            candidate.update(erCandidateConfidence='high', erCandidateCity='Quito',
                erCandidateVenueName='Event watch synthetic venue',
                erCandidateStartTime=(datetime.datetime.now(datetime.timezone.utc)
                    + datetime.timedelta(days=offset)).isoformat(),
                erCandidatePurchaseUrl=candidate['erCandidateSourceUrl'],
                erCandidatePayload={'eventType': 'concert', 'lineup': ['Event watch synthetic artist']})
            candidate['erCandidateEvidence'][0]['erEvidenceKind'] = 'official_sale'
            saved = imports(beta, body)
            check(saved['candidateRecord']['erCandidateIsPilot'] is True,
                  'Eligible controlled materialization candidate retains preapproval pilot identity: ' + key)
            high.append(body)
            high_records.append(saved['candidateRecord'])
        # Distinct namespaces may deliberately share one audit Party. A matching
        # organizer alone must never give alpha access to beta's materialization.
        sql("UPDATE event_watch_client SET active=true WHERE client_id='alpha-watch'")
        alpha = exchange('alpha-watch', new_secret)
        shared_actor = json.loads(json.dumps(high[1]))
        shared_actor['runKey'] = 'shared-actor-collision'
        candidate = shared_actor['candidate']
        candidate.update(erCandidateExternalId='shared-actor-collision',
                         erCandidateTitle='Owned manual title',
                         erCandidateSourceUrl='https://official.example.test/events/shared-actor-collision',
                         erCandidatePurchaseUrl='https://official.example.test/events/shared-actor-collision')
        candidate['erCandidateEvidence'][0]['erEvidenceUrl'] = candidate['erCandidateSourceUrl']
        saved = imports(alpha, shared_actor)
        check(saved['candidateRecord']['erCandidateIsPilot'] is True,
              'Shared-audit-actor client has its own valid eligible candidate')
        request('/social-events/event-research/pilot', beta, expected=401)
        request('/social-events/event-research/pilot/approve', beta,
                {'erPilotApprovalReference': 'Not an authorized operator'}, 'POST', 401)
        operator = int(value("INSERT INTO party(display_name,is_org,created_at) "
                             "VALUES('Owned event watch operator fixture',false,now()) RETURNING id"))
        operator_token = 'f' * 64
        sql(f"INSERT INTO party_security_role(party_id,role_id,approval_mode,active) "
            f"SELECT {operator},id,'bootstrap',true FROM security_role WHERE code='admin';"
            f"INSERT INTO api_token(token,party_id,label,active,issued_at_epoch,expires_at_epoch) "
            f"VALUES('{operator_token}',{operator},'password-login:synthetic-watch-operator',true,"
            "floor(extract(epoch FROM clock_timestamp()))::bigint,"
            "floor(extract(epoch FROM clock_timestamp()))::bigint+600)")
        approved, _ = request('/social-events/event-research/pilot/approve', operator_token,
            {'erPilotApprovalReference': 'Owned HTTP operator fixture'}, 'POST')
        check(approved['erPilotApproved'] is True
              and value(f"SELECT count(*) FROM event_research_pilot_audit WHERE approved_by_party_id='{operator}'") == '1',
              'Distinct disposable operator approves pilot through canonical HTTP and audit path')
        wrong_purpose = '0' * 64
        sql(f"INSERT INTO api_token(token,party_id,label,active) "
            f"VALUES('{wrong_purpose}',{operator},'event-watch:synthetic',true)")
        request('/parties', wrong_purpose, expected=401)
        check(request('/session', wrong_purpose)[0] is None,
              'Even an Admin-owned general token with service-purpose label is denied interactive access')
        draft = high[0]
        draft['materialize'] = True
        materialized = imports(beta, draft)
        draft_event = int(materialized['candidateRecord']['erCandidateEventId'])
        check(materialized['eventLink'] is not None
              and value(f"SELECT (metadata::jsonb->>'isPublic')='false' AND end_time IS NULL FROM social_event WHERE id={draft_event}") == 't',
              'Ungrantable publication client can materialize eligible draft while preserving unknown end time')
        publish = high[1]
        publish.update(materialize=True, publish=True)
        imports(beta, publish, 403)
        sql("UPDATE event_watch_client SET allow_publish=true WHERE client_id='beta-watch'")
        request('/integrations/event-watch/candidates', beta, expected=401)
        beta = exchange('beta-watch', secrets['beta-watch'])
        materialized = imports(beta, publish)
        public_event = int(materialized['candidateRecord']['erCandidateEventId'])
        check(value(f"SELECT (metadata::jsonb->>'isPublic')='true' AND organizer_party_id='{parties[0]}' FROM social_event WHERE id={public_event}") == 't',
              'Explicit current grant plus eligible pilot candidate creates the scoped synthetic public event')
        alias = high[3]
        alias.update(materialize=True, publish=True)
        same_namespace = imports(beta, alias)
        check(int(same_namespace['candidateRecord']['erCandidateEventId']) == public_event,
              'Same client can deduplicate a second external identity against its authorized namespace')
        # Stable replay must preserve manually edited fields on the existing event.
        sql(f"UPDATE social_event SET title='Owned manual title' WHERE id={public_event}")
        replay = imports(beta, publish)
        check(int(replay['candidateRecord']['erCandidateEventId']) == public_event
              and value(f"SELECT title FROM social_event WHERE id={public_event}") == 'Owned manual title',
              'Materialized replay preserves event identity and manual event fields')
        later = json.loads(json.dumps(publish))
        later.update(materialize=False, publish=False, runKey='after-approval')
        candidate = later['candidate']
        candidate['erCandidateExternalId'] = 'after-approval'
        candidate['erCandidateSourceUrl'] = 'https://official.example.test/events/after-approval'
        candidate['erCandidatePurchaseUrl'] = candidate['erCandidateSourceUrl']
        candidate['erCandidateEvidence'][0]['erEvidenceUrl'] = candidate['erCandidateSourceUrl']
        candidate['erCandidateStartTime'] = (datetime.datetime.now(datetime.timezone.utc)
            + datetime.timedelta(days=4)).isoformat()
        saved = imports(beta, later)
        check(saved['candidateRecord']['erCandidateIsPilot'] is False,
              'New postapproval candidate retains canonical non-pilot identity')
        before_later = counts()
        later.update(materialize=True, publish=True)
        refused = imports(beta, later, 409)
        check('only pilot candidates can be materialized' in str(refused)
              and counts() == before_later,
              'Even a granted client cannot materialize new non-pilot candidates after approval')
        check(value(f"SELECT count(*) FROM party_security_role WHERE party_id IN ({parties[0]},{parties[1]})") == '0',
              'Neither service curator acquires operator or interactive security-role grants')
        # A service principal must not inherit the admin materializer's power
        # over another organizer's private event through a dedupe match.
        foreign = high[2]
        candidate = foreign['candidate']
        start = candidate['erCandidateStartTime']
        private_event = int(value("INSERT INTO social_event(organizer_party_id,title,start_time,"
            "venue_id,event_type_id,workflow_state_id,timezone,metadata,created_at,updated_at) "
            f"SELECT '{operator}','Synthetic event watch candidate','{start}'::timestamptz,"
            "venue_id,event_type_id,workflow_state_id,timezone,'{\"isPublic\":false}',now(),now() "
            f"FROM social_event WHERE id={draft_event} RETURNING id"))
        before_private = value(f"SELECT jsonb_build_array((SELECT count(*) FROM event_artist WHERE event_id={private_event}),"
            f"(SELECT count(*) FROM external_event_ref WHERE event_id={private_event}))")
        foreign['materialize'] = True
        foreign['publish'] = False
        request('/integrations/event-watch/import', beta, foreign, 'PUT', expected=None)
        status = report['requests'][-1]['status']
        after_private = value(f"SELECT jsonb_build_array((SELECT count(*) FROM event_artist WHERE event_id={private_event}),"
            f"(SELECT count(*) FROM external_event_ref WHERE event_id={private_event}))")
        report['ownershipBoundaryProbe'] = {'actualStatus': status,
            'beforeArtistAndExternalReferences': json.loads(before_private),
            'afterArtistAndExternalReferences': json.loads(after_private),
            'distinctOrganizerParty': operator, 'serviceCuratorParty': parties[0]}
        boundary(status == 403 and after_private == before_private,
              'Service materialization cannot mutate another organizer private event through deduplication')
        admin_result, _ = request('/social-events/event-research/candidates/'
            + high_records[2]['erCandidateId'] + '/materialize', operator_token,
            {'erMaterializationRunId': high_records[2]['erCandidateRunId'],
             'erMaterializationPublish': False}, 'POST')
        check(int(admin_result['erMaterializationEventId']) == private_event
              and value(f"SELECT count(*) FROM event_artist WHERE event_id={private_event}") == '1',
              'Canonical administrative materialization retains its explicit cross-organizer authority')
        private_after_admin = value(f"SELECT jsonb_build_array((SELECT count(*) FROM event_artist WHERE event_id={private_event}),"
            f"(SELECT count(*) FROM external_event_ref WHERE event_id={private_event}))")
        request('/integrations/event-watch/import', beta, foreign, 'PUT', expected=None)
        linked_status = report['requests'][-1]['status']
        boundary(linked_status == 403 and value(f"SELECT jsonb_build_array((SELECT count(*) FROM event_artist WHERE event_id={private_event}),"
              f"(SELECT count(*) FROM external_event_ref WHERE event_id={private_event}))") == private_after_admin,
              'A candidate-linked private foreign event cannot bypass service authority on replay')
        before_namespace = value("SELECT jsonb_build_array("
            "(SELECT count(*) FROM external_event_ref WHERE provider='event-watch:alpha-watch'),"
            "(SELECT count(*) FROM external_artist_ref WHERE provider='event-watch:alpha-watch'),"
            "(SELECT count(*) FROM external_venue_ref WHERE provider='event-watch:alpha-watch'))")
        shared_actor.update(materialize=True, publish=False)
        request('/integrations/event-watch/import', alpha, shared_actor, 'PUT', expected=None)
        namespace_status = report['requests'][-1]['status']
        after_namespace = value("SELECT jsonb_build_array("
            "(SELECT count(*) FROM external_event_ref WHERE provider='event-watch:alpha-watch'),"
            "(SELECT count(*) FROM external_artist_ref WHERE provider='event-watch:alpha-watch'),"
            "(SELECT count(*) FROM external_venue_ref WHERE provider='event-watch:alpha-watch'))")
        report['sharedAuditActorNamespaceProbe'] = {'actualStatus': namespace_status,
            'sharedAuditParty': parties[0], 'beforeEventArtistVenueRefs': json.loads(before_namespace),
            'afterEventArtistVenueRefs': json.loads(after_namespace)}
        boundary(namespace_status == 403 and after_namespace == before_namespace,
              'Distinct service clients sharing one audit Party cannot acquire each other event namespace')
        # An exact provider reference is read before the canonical event lock.
        # An alias granting authority to A must not validate a stale read when
        # the selected reference is removed during that actual lock wait.
        # Direct event_id rebinding is already serialized by the canonical pilot
        # trigger; DELETE exercises the distinct, supported lifecycle without
        # disabling that trigger or the deferred directory-sync trigger.
        reference = high[4]
        reference.update(materialize=True, publish=False)
        sql("INSERT INTO external_event_ref(provider,external_id,event_id,city,country_code,"
            "last_seen_at,missing_runs,source_status) "
            f"VALUES('event-watch:beta-watch','reference-rebind',{public_event},'Quito','EC',clock_timestamp(),0,'on_sale')")
        lock = hold(f"SELECT id FROM social_event WHERE id={public_event} FOR UPDATE")
        pending = futures.submit(request, '/integrations/event-watch/import', beta, reference, 'PUT', None)
        witness('social_event', 'event-row', 'Provider reference selected before actual materialization event-lock wait')
        sql("SET lock_timeout='2s'; DELETE FROM external_event_ref "
            "WHERE provider='event-watch:beta-watch' AND external_id='reference-rebind'")
        aliases = int(value(f"SELECT count(*) FROM external_event_ref WHERE provider='event-watch:beta-watch' AND event_id={public_event}"))
        check(value("SELECT count(*) FROM external_event_ref WHERE provider='event-watch:beta-watch' AND external_id='reference-rebind'") == '0' and aliases > 0,
              'Independent owned actor removes exact reference while a real same-provider alias still grants event A')
        release(lock)
        pending.result(timeout=25)
        reference_status = report['requests'][-1]['status']
        linked_target = value("SELECT event_id FROM event_research_candidate WHERE provider='event-watch:beta-watch' AND external_id='reference-rebind'")
        report['providerReferenceRebindingProbe'] = {'actualStatus': reference_status,
            'selectedBeforeWait': public_event, 'exactReferenceRemovedDuringWait': True,
            'candidateLinkedAfterWait': linked_target or None,
            'sameProviderAliasesStillReferencingA': aliases}
        denied_rebind = reference_status == 409 and linked_target == ''
        boundary(denied_rebind, 'A same-provider alias cannot authorize a stale exact-reference target after rebinding')
        if denied_rebind:
            sql("INSERT INTO external_event_ref(provider,external_id,event_id,city,country_code,"
                "last_seen_at,missing_runs,source_status) "
                f"VALUES('event-watch:beta-watch','reference-rebind',{draft_event},'Quito','EC',clock_timestamp(),0,'on_sale')")
            retried = imports(beta, reference)
            check(int(retried['candidateRecord']['erCandidateEventId']) == draft_event,
                  'Fresh retry follows the current authorized reference B without creating a duplicate')
        lock = hold("SELECT pg_advisory_xact_lock(hashtextextended('event-watch:beta-watch',0))")
        pending = futures.submit(request, '/integrations/event-watch/import', beta, publish, 'PUT', None)
        witness('pg_advisory_xact_lock', 'advisory', 'Ownership changes after HTTP admission but before materialization event lock')
        sql(f"UPDATE social_event SET organizer_party_id='{operator}' WHERE id={public_event}")
        release(lock)
        pending.result(timeout=25)
        transfer_status = report['requests'][-1]['status']
        report['ownershipTransferReplayProbe'] = {'actualStatus': transfer_status,
            'ownershipTransferCommittedWhileRequestWaited': True}
        boundary(transfer_status == 403 and value(f"SELECT title='Owned manual title' AND organizer_party_id='{operator}' FROM social_event WHERE id={public_event}") == 't',
              'Former curator cannot replay against a transferred published event or disclose its associations')
        failures = [entry['label'] for entry in report['boundaryChecks'] if not entry['passed']]
        assert not failures, 'Service materialization authority violations: ' + '; '.join(failures)
        report['result'] = 'PASS_ACTUAL_HTTP_POSTGRES'
    except BaseException as error:
        report['result'] = 'FAIL'
        report['primaryFailure'] = {'type': type(error).__name__, 'message': str(error)[:1000]}
        raise
    finally:
        # Release only processes/DB actually created by this invocation, including
        # failures between a lock acquisition and its normal release.
        primary_active = sys.exc_info()[0] is not None
        cleanup_errors = []
        for holder in holders:
            if holder.poll() is None:
                try:
                    holder.communicate(b'ROLLBACK;\n\\q\n', timeout=5)
                except subprocess.TimeoutExpired:
                    holder.kill()
                    holder.communicate(timeout=5)
                except (OSError, ValueError) as error:
                    cleanup_errors.append(type(error).__name__)
                    if holder.poll() is None:
                        holder.kill()
                        holder.communicate(timeout=5)
        futures.shutdown(wait=True, cancel_futures=True)
        if server and server.poll() is None:
            try:
                os.killpg(server.pid, signal.SIGTERM)
                try:
                    server.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(server.pid, signal.SIGKILL)
                    server.wait(timeout=10)
            except ProcessLookupError:
                server.wait(timeout=10)
        if server_log:
            server_log.close()
        # Cleanup failures are reported without replacing an already raised
        # assertion, process failure, or interrupt with a less useful exception.
        try:
            cleanup = run(['dropdb', '-w', '--maintenance-db', maintenance_url, database], check=False) if owned else None
        except BaseException as error:
            report['cleanupFailure'] = type(error).__name__
            cleanup = None
        report['ownedDatabaseDropped'] = bool(cleanup and cleanup.returncode == 0)
        report['ownedServerStopped'] = server is None or server.poll() is not None
        report['ownedBarrierProcessesStopped'] = all(holder.poll() is not None for holder in holders)
        report['cleanupErrors'] = cleanup_errors
        (output / 'result.json').write_text(json.dumps(report, indent=2) + '\n')
        if owned and not primary_active:
            assert cleanup and cleanup.returncode == 0, 'Owned database cleanup failed'
            assert not cleanup_errors, 'Owned process cleanup reported errors'
    print(json.dumps({'result': report['result'], 'assertions': len(report['assertions']),
                      'requests': len(report['requests']), 'migrations': report['migrationCount']}))


if __name__ == '__main__':
    main()
