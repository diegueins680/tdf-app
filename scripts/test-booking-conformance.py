#!/usr/bin/env python3
"""Real HTTP/PostgreSQL booking scope, projection and concurrency checks.

Creates and drops only its own nonce database on an explicitly selected loopback
PostgreSQL test server. Source fixtures and fake actors never contact providers.
"""
import concurrent.futures
import base64
from datetime import datetime, timedelta, timezone
import getpass
import hashlib
import hmac
import json
import os
from pathlib import Path
import socket
import subprocess
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid

ROOT = Path(__file__).resolve().parent.parent
HOST = os.environ.get('TDF_BOOKING_TEST_PG_HOST', '127.0.0.1')
assert HOST == '127.0.0.1' or (HOST == 'postgres' and os.environ.get('CI') == 'true')
PORT = int(os.environ['TDF_BOOKING_TEST_PG_PORT'])
assert 0 < PORT < 65536
ROLE = os.environ.get('TDF_BOOKING_TEST_PG_ROLE', getpass.getuser())
PASSWORD = os.environ.get('TDF_BOOKING_TEST_PG_PASSWORD', '')
BINARY = Path(os.environ['TDF_BOOKING_TEST_SERVER_BIN']).resolve(strict=True)
OUTPUT = Path(os.environ['TDF_BOOKING_TEST_OUTPUT']).resolve()
OUTPUT.mkdir(mode=0o700)  # Never overwrite an earlier result.
NAME = 'tdf_booking_conformance_' + uuid.uuid4().hex[:12] + '_test'
ENV = {'PATH': os.environ['PATH'], 'PGHOST': HOST, 'PGPORT': str(PORT),
       'PGUSER': ROLE, 'PGPASSWORD': PASSWORD, 'PGCONNECT_TIMEOUT': '5',
       'PGOPTIONS': '-c statement_timeout=20000 -c lock_timeout=15000'}
URL = 'postgresql://' + urllib.parse.quote(ROLE, safe='') + ':' + urllib.parse.quote(PASSWORD, safe='') + '@' + HOST + ':' + str(PORT) + '/' + NAME
owned = False
server = None
checks = []
dirty = bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT, text=True).strip())
revision = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()


def run(args, **kwargs):
    result = subprocess.run(args, env=ENV, **kwargs)
    if result.returncode:
        # All data and credentials in this fixture are synthetic. Retain captured
        # database diagnostics privately instead of losing the cause on cleanup.
        with (OUTPUT / 'fixture-errors.log').open('a') as log:
            log.write(Path(args[0]).name + ' exited ' + str(result.returncode) + '\n')
            for value in [result.stdout, result.stderr]:
                if isinstance(value, str): log.write(value)
        raise RuntimeError('Fixture command failed: ' + Path(args[0]).name)
    return result


def sql(query):
    return run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', NAME, '-c', query],
               capture_output=True, text=True).stdout.strip()


def check(name, condition):
    if not condition:
        raise AssertionError(name)
    checks.append(name)
    print('PASS ' + name, flush=True)


def request(path, payload=None, token='fixture-admin', method=None, idempotency=None, signed=False):
    body = None if payload is None else json.dumps(payload).encode()
    req = urllib.request.Request('http://127.0.0.1:' + str(http_port) + path, data=body,
        method=method or ('GET' if body is None else 'PUT'),
        headers={**({'Authorization': 'Bearer ' + token} if token else {}), 'Content-Type': 'application/json', **({'Idempotency-Key': idempotency} if idempotency else {}),
                 **({'X-Hub-Signature-256': 'sha256=' + hmac.new(b'synthetic-local-webhook-secret', body, hashlib.sha256).hexdigest()} if signed else {})})
    try:
        with urllib.request.urlopen(req, timeout=30) as response:
            return response.status, response.read().decode()
    except urllib.error.HTTPError as error:
        return error.code, error.read().decode()


def actor(role, identity=None):
    identity = identity or role
    key = sql("INSERT INTO party(display_name,is_org,created_at) VALUES ('Fixture " + identity + "',false,now()) RETURNING id")
    sql("INSERT INTO party_security_role(party_id,role_id,approval_mode,active) SELECT " + key + ",id,'bootstrap',true FROM security_role WHERE code='" + role + "'")
    sql("INSERT INTO api_token(token,party_id,label,active) VALUES ('fixture-" + identity + "'," + key + ",'Synthetic booking conformance',true)")
    return key


def booking(owner, title, start, end, engineer=None, status='Confirmed'):
    key = sql("INSERT INTO booking(title,party_id,engineer_party_id,starts_at,ends_at,status,notes,created_at) VALUES ('" + title + "'," + owner + "," + (engineer or 'NULL') + ",'2035-01-01 " + start + "+00','2035-01-01 " + end + "+00','" + status + "','PRIVATE_SYNTHETIC_NOTE',now()) RETURNING id")
    sql("INSERT INTO booking_resource(booking_id,resource_id,role) VALUES (" + key + "," + resource + ",'primary')")
    return key


try:
    run(['createdb', NAME], capture_output=True, text=True)
    owned = True
    with (OUTPUT / 'schema.log').open('w') as log:
        for file in ['scripts/__tests__/fixtures/production-schema-20260814.sql',
                     'scripts/__tests__/fixtures/catalog-production-source-fixture.sql']:
            run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME, '-f', str(ROOT / file)], stdout=log, stderr=log)
    with (OUTPUT / 'migrations.sql').open('w') as output:
        subprocess.run(['node', 'scripts/render-production-migration-batch.mjs'], cwd=ROOT,
            env={**ENV, 'SOURCE_COMMIT': revision}, stdout=output, check=True)
    with (OUTPUT / 'migrations.log').open('w') as log:
        for _ in range(2):
            run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME, '-f', str(OUTPUT / 'migrations.sql')], stdout=log, stderr=log)
    check('canonical migration batch applies twice', sql('SELECT count(*) FROM tdf_schema_migration') == str(len(json.loads((ROOT / 'scripts/production-migrations.json').read_text())['migrations'])))
    interval_cases = """WITH points AS (
      SELECT '2035-01-01'::timestamptz + n * interval '1 microsecond' AS t FROM generate_series(0,8) n
    ), windows AS (SELECT a.t AS s,b.t AS e FROM points a CROSS JOIN points b WHERE a.t<b.t)
    SELECT bool_and((a.s < b.e AND a.e > b.s) = (tstzrange(a.s,a.e,'[)') && tstzrange(b.s,b.e,'[)'))),
           bool_and((a.s <= b.e AND a.e >= b.s) = (tstzrange(a.s,a.e,'[)') && tstzrange(b.s,b.e,'[)')))
    FROM windows a CROSS JOIN windows b"""
    check('half-open overlap matches PostgreSQL ranges; inclusive mutation fails on adjacent windows', sql(interval_cases) == 't|f')
    roles = sql('SELECT code FROM security_role WHERE active ORDER BY code').splitlines()
    assert all(all(c.islower() or c == '-' for c in role) for role in roles)
    actors = {role: actor(role) for role in roles}
    owner = actor('artist', 'owner')
    resource = sql("INSERT INTO resource(name,slug,resource_type,capacity,active) VALUES ('Fixture room','booking-conformance-room','Room',1,true) RETURNING id")
    first = booking(owner, 'First', '10:00', '11:00', engineer=owner)
    second = booking(owner, 'Second', '12:00', '13:00')
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0)); http_port = sock.getsockname()[1]
    assets = OUTPUT / 'assets'; assets.mkdir()
    with (OUTPUT / 'backend.log').open('w') as log:
        server = subprocess.Popen([str(BINARY)], cwd=OUTPUT, stdout=log, stderr=log,
            env={'PATH': ENV['PATH'], 'APP_ENV': 'test', 'DATABASE_URL': URL,
                 'APP_PORT': str(http_port), 'RESET_DB': 'false', 'RUN_MIGRATIONS': 'false',
                 'AUTO_APPLY_PRODUCTION_MIGRATIONS': 'false', 'SEED_DB': 'false',
                 'HQ_ASSETS_DIR': str(assets), 'DEFAULT_LOCALE': 'es',
                 'FACEBOOK_APP_SECRET': 'synthetic-local-webhook-secret',
                 'DDEX_STORAGE_BACKEND': 'local-private', 'DDEX_PRIVATE_STORAGE_ROOT': str(OUTPUT / 'ddex-private'),
                 'EVENT_DISCOVERY_ENABLED': 'false', 'ARTIST_ENRICHMENT_ENABLED': 'false',
                 'EVENT_LOGISTICS_RECHECK_ENABLED': 'false', 'REPUTATION_AGGREGATION_WORKER_ENABLED': 'false'})
    for _ in range(120):
        if server.poll() is not None: raise RuntimeError('Fixture backend exited')
        try:
            status, body = request('/health')
            if status == 200 and json.loads(body).get('db') == 'ok': break
        except urllib.error.URLError: pass
        time.sleep(.25)
    else: raise RuntimeError('Fixture backend did not become ready')

    # OpenAPI requires bearer authentication for party radio presence. A removed
    # shadow public declaration must never turn listener state into anonymous data.
    sql("INSERT INTO party_radio_presence(party_id,stream_url,station_name,updated_at) VALUES (" + owner + ",'https://example.invalid/synthetic-stream','SYNTHETIC_RADIO_STATION',now())")
    for token in [None, 'fixture-invalid']:
        status, body = request('/radio/presence/' + owner, token=token)
        check('radio presence rejects ' + str(token), status == 401 and 'SYNTHETIC_RADIO_STATION' not in body)
    status, body = request('/radio/presence/' + owner, token='fixture-artist')
    check('authenticated radio listener presence remains readable', status == 200 and json.loads(body)['rpPartyId'] == int(owner))
    check('invalid radio identity rejected', request('/radio/presence/0', token='fixture-artist')[0] == 400)

    # Real HTTP transport checks: NoContent is an empty payload, not an implicit
    # 204 override of Servant's declared 200. Empty webhook entries cannot enqueue
    # messages or trigger a provider call; the only signing key is synthetic.
    for path, payload in [('/facebook/webhook', {'object': 'page', 'entry': [{}]}),
                          ('/webhooks/whatsapp', {'entry': []})]:
        check('webhook rejects unsigned transport ' + path,
              request(path, payload, token=None, method='POST')[0] == 401)
        check('signed empty webhook returns 200 with empty body ' + path,
              request(path, payload, token=None, method='POST', signed=True) == (200, ''))
    check('radio reset rejects anonymous caller', request('/radio/presence', token=None, method='DELETE')[0] == 401)
    check('radio reset returns 200 with empty body', request('/radio/presence', token='fixture-owner', method='DELETE') == (200, ''))
    check('radio reset removes own row', sql('SELECT count(*) FROM party_radio_presence WHERE party_id=' + owner) == '0')
    check('unfollow returns 200 with empty body', request('/fans/me/follows/' + owner, token='fixture-fan', method='DELETE') == (200, ''))
    notification = sql("INSERT INTO notification(recipient_party_id,notif_type,title,body,is_read,created_at) VALUES (" + actors['fan'] + ",'directory.invitation','Synthetic','Synthetic',false,now()) RETURNING id")
    check('mark notification read returns 200 with empty body', request('/fans/me/notifications/' + notification + '/read', {}, token='fixture-fan', method='POST') == (200, ''))
    check('mark notification read commits own state', sql('SELECT is_read FROM notification WHERE id=' + notification) == 't')
    check('legacy Stripe remains unavailable', request('/marketplace/cart/synthetic/stripe/payment-intent',
          {'mcrBuyerName': 'Synthetic', 'mcrBuyerEmail': 'synthetic@example.invalid'}, token=None, method='POST')[0] == 503)

    xml = (ROOT / 'tdf-hq/test/fixtures/ddex/ern-v432/single-valid.xml').read_bytes()
    upload = {'uploadFileName': 'synthetic-conformance.xml', 'uploadContentType': 'application/xml',
              'uploadContentBase64': base64.b64encode(xml).decode()}
    schema_sql = subprocess.check_output(['node', '--input-type=module', '-e',
        "import {buildSchemaVerificationSql} from './scripts/lib/production-release.mjs'; process.stdout.write(buildSchemaVerificationSql());"], cwd=ROOT, env=ENV, text=True)
    for legacy in ['severity', 'layer']:
        probe = subprocess.run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME], env=ENV,
            input='BEGIN; ALTER TABLE ddex_validation_issue ALTER COLUMN ' + legacy + ' SET NOT NULL;\n' + schema_sql + '\nROLLBACK;', capture_output=True, text=True)
        check('schema rejects incompatible DDEX legacy NOT NULL ' + legacy, probe.returncode != 0 and 'nullable retained legacy severity and layer' in probe.stderr)
    check('DDEX upload denies unprivileged actor', request('/ddex/documents', upload, token='fixture-fan', method='POST')[0] == 403)
    status, body = request('/ddex/documents', upload, method='POST')
    check('DDEX upload is implemented with 200 JSON ' + str((status, body[:180])), status == 200)
    document = json.loads(body); document_id = str(document['ddexDocumentId'])
    partner = {'partnerName': 'Synthetic contract partner', 'partnerDpid': None,
               'partnerAllowedStandardVersionIds': [document['ddexDocumentStandardVersionId']]}
    status, body = request('/ddex/partners', partner, method='POST')
    check('DDEX partner accepts Mobile canonical version IDs', status == 200 and json.loads(body)['ddexPartnerAllowedStandardVersions'][0]['ddexStandardVersionId'] == document['ddexDocumentStandardVersionId'])
    check('DDEX partner rejects removed Mobile request field', request('/ddex/partners',
        {'partnerName': 'Synthetic', 'partnerDpid': None, 'partnerAllowedVersions': ['4.3']}, method='POST')[0] == 400)
    check('DDEX upload uses canonical workflow references', bool(document['ddexDocumentWorkflowStateId']) and 'ddexDocumentStatus' not in document)
    status, body = request('/ddex/documents/' + document_id + '/raw')
    check('DDEX private source uses JSON base64 envelope', status == 200 and base64.b64decode(json.loads(body)['downloadContentBase64']) == xml)
    status, body = request('/ddex/documents/' + document_id + '/preview')
    check('DDEX preview is implemented with 200 JSON ' + str((status, body[:180])), status == 200 and bool(json.loads(body)['previewWarnings']))
    # Faults after each write stage must roll back run, issue and document state.
    def validation_state():
        return sql("SELECT (SELECT count(*) FROM ddex_validation_run),(SELECT count(*) FROM ddex_validation_issue),(SELECT workflow_state_id FROM ddex_document WHERE id=" + document_id + ")")
    before_validation = validation_state()
    for table, event in [('ddex_validation_run', 'INSERT'), ('ddex_validation_issue', 'INSERT'), ('ddex_validation_run', 'UPDATE')]:
        sql("CREATE FUNCTION fixture_ddex_reject() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'SYNTHETIC_VALIDATION_REJECTION'; END $$; CREATE TRIGGER fixture_ddex_reject AFTER " + event + " ON " + table + " FOR EACH ROW EXECUTE FUNCTION fixture_ddex_reject()")
        try:
            check('DDEX injected failure rejects ' + table + ' ' + event,
                  request('/ddex/documents/' + document_id + '/validation-runs', {}, method='POST')[0] == 500)
            check('DDEX rejected validation rolls back every effect ' + table + ' ' + event, validation_state() == before_validation)
        finally:
            sql('DROP TRIGGER fixture_ddex_reject ON ' + table + '; DROP FUNCTION fixture_ddex_reject()')
    status, body = request('/ddex/documents/' + document_id + '/validation-runs', {}, method='POST')
    check('DDEX structural validation completes with 200 JSON ' + str((status, body[:180])), status == 200 and 'validationRunWorkflowStateId' in json.loads(body))
    run_response = json.loads(body)
    stored_times = json.loads(sql("SELECT json_build_object('started',started_at,'finished',finished_at) FROM ddex_validation_run WHERE id=" + str(run_response['validationRunId'])))
    parse_time = lambda value: datetime.fromisoformat(value.replace('Z', '+00:00'))
    check('DDEX response timestamps equal committed record', parse_time(run_response['validationRunStartedAt']) == parse_time(stored_times['started']) and parse_time(run_response['validationRunFinishedAt']) == parse_time(stored_times['finished']))
    status, body = request('/ddex/documents/' + document_id + '/validation-runs/latest')
    report = json.loads(body)
    check('DDEX structural completion is not official profile validity', status == 200 and not report['reportIsValid'] and any(x['issueCode'] == 'PROFILE_VALIDATION_REQUIRED' for x in report['reportIssues']))
    check('DDEX validation result uses canonical reference only', sql('SELECT count(*) FROM ddex_validation_run WHERE result IS NOT NULL OR result_id IS NULL OR finished_at IS NULL') == '0')
    for mutation in ["severity='error'", "layer='xml'", 'severity_id=NULL', 'layer_id=NULL']:
        probe = subprocess.run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME], env=ENV,
            input='BEGIN; UPDATE ddex_validation_issue SET ' + mutation + '; ROLLBACK;', capture_output=True, text=True)
        check('DDEX canonical issue guard rejects ' + mutation, probe.returncode != 0 and 'active canonical severity and layer IDs' in probe.stderr)
    for path, payload, method in [('/ddex/documents/' + document_id + '/import-plans', {}, 'POST'),
            ('/ddex/import-plans/1', {'resolutionPlanId': 1, 'resolutionConflicts': []}, 'PATCH'),
            ('/ddex/import-plans/1/commit', {}, 'POST'), ('/ddex/exports/1/download', None, 'GET')]:
        check('DDEX guarded capability returns 503 ' + path, request(path, payload, method=method)[0] == 503)

    sql("INSERT INTO subject(name,active) VALUES ('VISIBLE_SUBJECT',true),('INACTIVE_SUBJECT',false)")
    for suffix in ['', '?includeInactive=true']:
        status, body = request('/trials/v1/subjects' + suffix, token=None)
        check('public subject catalog stays active-only ' + suffix, status == 200 and 'VISIBLE_SUBJECT' in body and 'INACTIVE_SUBJECT' not in body)
    requirements = json.loads((ROOT / 'formal/system/requirements.json').read_text())['requirements']
    school_roles = set(next(r for r in requirements if r['id'] == 'COURSE-SUBJECT-001')['state']['managementRoles'])
    check('school management roles are canonical', school_roles <= set(roles))
    for role in roles:
        status, body = request('/trials/v1/subjects/catalog?includeInactive=true', token='fixture-' + role)
        if role in school_roles:
            check(role + ' can explicitly list inactive school subjects', status == 200 and 'INACTIVE_SUBJECT' in body)
        else:
            check(role + ' cannot list inactive school subjects', status == 403 and 'INACTIVE_SUBJECT' not in body)
    for token in [None, 'fixture-invalid']:
        check('protected subject list rejects ' + str(token), request('/trials/v1/subjects/catalog?includeInactive=true', token=token)[0] == 401)
    status, body = request('/trials/v1/subjects/catalog', token='fixture-admin')
    check('protected subject list defaults active-only', status == 200 and 'VISIBLE_SUBJECT' in body and 'INACTIVE_SUBJECT' not in body)
    check('protected subject filter rejects invalid boolean', request('/trials/v1/subjects/catalog?includeInactive=invalid', token='fixture-admin')[0] == 400)

    policy = json.loads((ROOT / 'formal/system/booking-policy.json').read_text())
    staff = {role['code'] for role in policy['studioWideRoles']}
    check('all specified staff roles are canonical', staff <= set(roles))
    for state, projection in policy['checkoutProjection'].items():
        check('runtime booking projection ' + state, sql("SELECT service_booking_projected_booking_status('" + state + "')") == projection['booking'])
    for role in roles:
        status, body = request('/bookings?bookingId=' + first, token='fixture-' + role)
        if role in staff:
            check(role + ' permitted detail read', status == 200 and 'PRIVATE_SYNTHETIC_NOTE' in body)
        else:
            check(role + ' cannot read foreign detail', status in (200, 401, 403) and 'PRIVATE_SYNTHETIC_NOTE' not in body)
            status, body = request('/bookings', token='fixture-' + role)
            check(role + ' cannot read foreign unfiltered calendar', status in (200, 401, 403) and 'PRIVATE_SYNTHETIC_NOTE' not in body)
            status, _ = request('/bookings/' + first, {'ubNotes': 'UNAUTHORIZED'}, token='fixture-' + role)
            check(role + ' cannot mutate foreign booking', status in (401, 403, 404))
    for selector in ['partyId=' + owner, 'engineerPartyId=' + owner, 'partyId=' + owner + '&engineerPartyId=' + owner]:
        status, body = request('/bookings?' + selector)
        check('filtered calendar positive control ' + selector, status == 200 and 'PRIVATE_SYNTHETIC_NOTE' in body)
        status, body = request('/bookings?' + selector, token='fixture-artist')
        check('foreign filtered calendar denied ' + selector, status == 200 and 'PRIVATE_SYNTHETIC_NOTE' not in body)
    status, body = request('/bookings?bookingId=' + first, token='fixture-owner')
    check('customer owner can read own booking', status == 200 and 'PRIVATE_SYNTHETIC_NOTE' in body)
    check('denied role mutations have no side effects', sql('SELECT notes FROM booking WHERE id=' + first) == 'PRIVATE_SYNTHETIC_NOTE')
    sql('UPDATE booking SET engineer_party_id=' + actors['engineer'] + ' WHERE id=' + first)
    status, body = request('/bookings?bookingId=' + first, token='fixture-engineer')
    check('explicitly assigned engineer can read', status == 200 and 'PRIVATE_SYNTHETIC_NOTE' in body)
    status, _ = request('/bookings/' + first, {'ubNotes': 'ASSIGNED'}, token='fixture-engineer')
    check('explicitly assigned engineer can update', status == 200 and sql('SELECT notes FROM booking WHERE id=' + first) == 'ASSIGNED')
    status, _ = request('/bookings/' + second, {'ubEngineerPartyId': int(actors['student'])}, token='fixture-student')
    check('request cannot confer assignment authority', status == 404 and sql('SELECT engineer_party_id IS NULL FROM booking WHERE id=' + second) == 't')

    # CRM admission must not expose foreign booking projections through either
    # customer/engineer lists or a class-session link. Use an actual matching row
    # and a distinct booking status so a missing filter cannot pass vacuously.
    crm_booking = booking(owner, 'CRM_PRIVATE_BOOKING', '20:00', '21:00', engineer=owner, status='Cancelled')
    subject = sql("INSERT INTO subject(name,active) VALUES ('Synthetic CRM subject',true) RETURNING id")
    class_id = sql("INSERT INTO class_session(student_id,teacher_id,subject_id,start_at,end_at,room_id,booking_id,attended) VALUES (" + owner + "," + actors['teacher'] + "," + subject + ",'2035-01-01 20:00+00','2035-01-01 21:00+00'," + resource + "," + crm_booking + ",false) RETURNING id")
    def crm_projection(token):
        status, body = request('/parties/' + owner + '/related', token=token)
        check('CRM projection request admitted ' + token, status == 200)
        projection = json.loads(body)
        sessions = [row for row in projection['prClassSessions'] if str(row['prcClassSessionId']) == class_id]
        check('CRM class positive fixture exists ' + token, len(sessions) == 1)
        return projection, sessions[0]
    projection, session = crm_projection('fixture-producer')
    check('CRM admission cannot expose foreign customer or engineer booking', projection['prBookings'] == [])
    check('CRM class cannot expose foreign booking identity or status', session['prcBookingId'] is None and session['prcStatus'] == 'programada')
    projection, session = crm_projection('fixture-admin')
    check('staff CRM retains both booking relationships', len([row for row in projection['prBookings'] if str(row['prbBookingId']) == crm_booking]) == 2)
    check('staff CRM retains linked booking identity and status', str(session['prcBookingId']) == crm_booking and session['prcStatus'] == 'cancelada')
    sql('UPDATE booking SET engineer_party_id=' + actors['producer'] + ' WHERE id=' + crm_booking)
    projection, session = crm_projection('fixture-producer')
    check('assigned CRM actor retains authorized booking projection', any(str(row['prbBookingId']) == crm_booking for row in projection['prBookings']) and str(session['prcBookingId']) == crm_booking and session['prcStatus'] == 'cancelada')

    status, _ = request('/bookings/' + second, {'ubStartsAt': '2035-01-01T10:30:00Z', 'ubEndsAt': '2035-01-01T11:30:00Z'})
    check('overlapping move conflicts at HTTP boundary', status == 409)
    check('conflicting edit rolls back both records', sql("SELECT b.starts_at=a.starts_at AND b.ends_at=a.ends_at AND b.starts_at='2035-01-01 12:00+00' FROM booking b JOIN service_booking_resource_allocation a ON a.booking_id=b.id WHERE b.id=" + second) == 't')
    check('cancellation succeeds', request('/bookings/' + first, {'ubStatus': 'Cancelled'})[0] == 200)
    check('cancellation releases resource', sql('SELECT allocation_status FROM service_booking_resource_allocation WHERE booking_id=' + first) == 'released')
    check('reactivation succeeds', request('/bookings/' + first, {'ubStatus': 'Confirmed'})[0] == 200)
    check('reactivation reacquires resource', sql('SELECT allocation_status FROM service_booking_resource_allocation WHERE booking_id=' + first) == 'reserved')
    third = booking(owner, 'Third', '14:00', '15:00')
    gate = threading.Barrier(2)
    def compete(key):
        gate.wait(timeout=10)
        return request('/bookings/' + key, {'ubStartsAt': '2035-01-01T16:00:00Z', 'ubEndsAt': '2035-01-01T17:00:00Z'})[0]
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        results = list(executor.map(compete, [second, third]))
    check('concurrent resource moves have one winner', sorted(results) == [200, 409])
    check('every tested projection matches its booking', sql("SELECT count(*) FROM booking b JOIN service_booking_resource_allocation a ON a.booking_id=b.id WHERE b.id IN (" + ','.join([first, second, third]) + ") AND (b.starts_at<>a.starts_at OR b.ends_at<>a.ends_at)") == '0')
    # Hold the row while both HTTP operations queue. The handler must read the
    # booking only after acquiring its lock, or a notes-only write resurrects it.
    fourth = booking(owner, 'Concurrent cancellation', '18:00', '19:00')
    with (OUTPUT / 'barrier.log').open('w') as log:
        barrier = subprocess.Popen(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', NAME],
            env=ENV, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log, text=True)
        try:
            barrier.stdin.write("BEGIN; SET LOCAL idle_in_transaction_session_timeout='20s'; SELECT id FROM booking WHERE id=" + fourth + " FOR UPDATE; SELECT 'LOCKED';\n")
            barrier.stdin.flush()
            for _ in range(4):
                if barrier.stdout.readline().strip() == 'LOCKED': break
            else: raise RuntimeError('Barrier did not acquire row lock')
            def waiters(expected):
                for _ in range(100):
                    count = int(sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND state='active' AND wait_event_type='Lock' AND query ILIKE '%booking%'") or 0)
                    if count >= expected: return
                    time.sleep(.05)
                raise RuntimeError('HTTP requests did not reach the row lock')
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
                cancel = executor.submit(request, '/bookings/' + fourth, {'ubStatus': 'Cancelled'})
                waiters(1)
                edit = executor.submit(request, '/bookings/' + fourth, {'ubNotes': 'CONCURRENT_NOTES'})
                waiters(2)
                barrier.stdin.write('COMMIT;\n'); barrier.stdin.flush(); barrier.stdin.close()
                barrier.wait(timeout=5)
                check('concurrent cancellation and notes both accepted', cancel.result()[0] == 200 and edit.result()[0] == 200)
            check('notes-only write cannot resurrect cancellation', sql('SELECT b.status,b.notes,a.allocation_status FROM booking b JOIN service_booking_resource_allocation a ON a.booking_id=b.id WHERE b.id=' + fourth) == 'Cancelled|CONCURRENT_NOTES|released')
        finally:
            if barrier.poll() is None:
                barrier.terminate(); barrier.wait(timeout=5)

    with (OUTPUT / 'revocation-barrier.log').open('w') as log:
        barrier = subprocess.Popen(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', NAME],
            env=ENV, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log, text=True)
        try:
            barrier.stdin.write("BEGIN; SET LOCAL idle_in_transaction_session_timeout='20s'; UPDATE api_token SET active=false WHERE token='fixture-owner'; SELECT 'LOCKED';\n")
            barrier.stdin.flush()
            for _ in range(4):
                if barrier.stdout.readline().strip() == 'LOCKED': break
            else: raise RuntimeError('Revocation barrier did not acquire token lock')
            with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
                mutation = executor.submit(request, '/bookings/' + first, {'ubNotes': 'REVOKED'}, 'fixture-owner')
                for _ in range(100):
                    waiting = int(sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND state='active' AND wait_event_type='Lock' AND query ILIKE '%api_token%'") or 0)
                    if waiting: break
                    time.sleep(.05)
                else: raise RuntimeError('Mutation did not wait on revocation fence')
                barrier.stdin.write('COMMIT;\n'); barrier.stdin.flush(); barrier.stdin.close(); barrier.wait(timeout=5)
                check('revocation winning session lock denies in-flight mutation', mutation.result()[0] == 401)
        finally:
            if barrier.poll() is None:
                barrier.terminate(); barrier.wait(timeout=5)
    check('revoked session cannot retry mutation', request('/bookings/' + first, {'ubNotes': 'REVOKED'}, token='fixture-owner')[0] == 401)
    check('revoked retry leaves note unchanged', sql('SELECT notes FROM booking WHERE id=' + first) == 'ASSIGNED')
    offering = sql("SELECT o.id FROM service_offering o JOIN workflow_state w ON w.id=o.workflow_state_id WHERE o.active AND o.deprecated_at IS NULL AND NOT o.requires_engineer AND w.code='published' ORDER BY o.code LIMIT 1")
    check('fixture published offering exists', bool(offering))
    create = {'cbTitle': 'Owned create', 'cbStartsAt': '2035-01-02T10:00:00Z',
              'cbEndsAt': '2035-01-02T11:00:00Z', 'cbStatus': 'Confirmed',
              'cbServiceOfferingId': offering, 'cbResourceIds': ['booking-conformance-room']}
    status, body = request('/bookings', {**create, 'cbPartyId': int(owner)}, 'fixture-artist', 'POST')
    check('non-staff cannot create for foreign customer', status == 403)
    status, body = request('/bookings', create, 'fixture-artist', 'POST')
    check('non-staff omitted customer becomes self', status == 200 and str(json.loads(body)['partyId']) == actors['artist'])
    for payload in ({}, {'ubNotes': None}, {'ubResourceIds': []}, {'ubPartyId': int(owner)}):
        check('invalid update rejected ' + json.dumps(payload), request('/bookings/' + first, payload)[0] == 400)

    # A synthetic checkout creates no payment attempt and calls no provider.
    sql("WITH legacy AS (INSERT INTO service_catalog(name,kind,pricing_model,default_rate_cents,active) VALUES ('Synthetic checkout','Recording','Hourly',2000,true) RETURNING id) UPDATE service_offering SET legacy_service_catalog_id=(SELECT id FROM legacy) WHERE id='" + offering + "'")
    sql("UPDATE service_booking_commerce_policy SET active=false WHERE service_offering_id='" + offering + "'")
    sql("INSERT INTO service_booking_commerce_policy(service_offering_id,policy_version,currency,rate_minor,rate_unit_minutes,tax_bps,deposit_bps,hold_minutes,min_duration_minutes,max_duration_minutes,duration_step_minutes,terms_version,terms_summary,approval_status,active,approved_at,approved_by) VALUES ('" + offering + "','booking-conformance','USD',2000,60,0,5000,15,60,120,60,'booking-conformance','Synthetic test','approved',true,now(),'isolated-test')")
    checkout_start = (datetime.now(timezone.utc) + timedelta(days=30)).replace(hour=10, minute=0, second=0, microsecond=0)
    payload = {'pbcFullName': 'Synthetic checkout', 'pbcEmail': 'booking-conformance@persona.test',
               'pbcServiceOfferingId': offering, 'pbcStartsAt': checkout_start.isoformat(),
               'pbcDurationMinutes': 60, 'pbcResourceIds': ['booking-conformance-room'], 'pbcTermsAccepted': True}
    status, body = request('/bookings/public/checkout', payload, method='POST', idempotency='booking-conformance-checkout-0001')
    check('synthetic checkout accepted without provider', status == 200)
    bound = sql("SELECT booking_id FROM service_booking_checkout_runtime WHERE create_idempotency_key='booking-conformance-checkout-0001'")
    check('bound checkout exists', bool(bound))
    check('generic edit cannot change checkout snapshot', request('/bookings/' + bound, {'ubStartsAt': (checkout_start + timedelta(minutes=15)).isoformat()})[0] == 409)
    check('generic edit cannot cancel checkout lifecycle', request('/bookings/' + bound, {'ubStatus': 'Cancelled'})[0] == 409)
    check('metadata edit preserves bound checkout', request('/bookings/' + bound, {'ubNotes': 'BOUND_NOTE'})[0] == 200)
    migration = (ROOT / 'tdf-hq/sql/2026-10-05_booking_calendar_projection.sql').read_text()
    # Each mutation is isolated by an explicit transaction and connection close;
    # failure must be the named reconciliation guard, not SQL/tool failure.
    for label, mutation in {
        'released': "UPDATE service_booking_resource_allocation SET allocation_status='released' WHERE booking_id=" + bound,
        'missing': 'DELETE FROM service_booking_resource_allocation WHERE booking_id=' + bound,
        'stale window': "UPDATE service_booking_resource_allocation SET starts_at=starts_at+interval '1 minute' WHERE booking_id=" + bound,
    }.items():
        probe = subprocess.run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME], env=ENV,
            input='BEGIN;\n' + mutation + ';\n' + migration.replace('BEGIN;\n', '', 1).rsplit('COMMIT;', 1)[0] + '\nROLLBACK;\n', capture_output=True, text=True)
        (OUTPUT / ('migration-control-' + label.replace(' ', '-') + '.log')).write_text(probe.stdout + probe.stderr)
        check('migration rejects bound allocation ' + label, probe.returncode != 0 and 'Existing checkout allocation divergence requires reconciliation' in probe.stderr)
    check('migration control mutations rolled back', sql('SELECT allocation_status FROM service_booking_resource_allocation WHERE booking_id=' + bound) == 'holding')
    sql("UPDATE service_booking_checkout_runtime SET fulfillment_status='expired' WHERE booking_id=" + bound)
    check('runtime expiry projects booking and resource atomically', sql('SELECT b.status,a.allocation_status FROM booking b JOIN service_booking_resource_allocation a ON a.booking_id=b.id WHERE b.id=' + bound) == 'Cancelled|released')
    check('checkout verification performed no payment attempt', sql('SELECT count(*) FROM commerce_payment_attempt') == '0')
    schema_sql = subprocess.check_output(['node', '--input-type=module', '-e',
        "import {buildSchemaVerificationSql} from './scripts/lib/production-release.mjs'; process.stdout.write(buildSchemaVerificationSql());"], cwd=ROOT, env=ENV, text=True)
    run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME], input=schema_sql, capture_output=True, text=True)
    check('complete schema verifier accepts candidate', True)
    for label, definition in {
        'when-false': 'AFTER UPDATE OF starts_at, ends_at, status, service_offering_id ON booking FOR EACH ROW WHEN (false) EXECUTE FUNCTION service_booking_sync_legacy_booking_allocation()',
        'wrong-function': 'AFTER UPDATE OF starts_at, ends_at, status, service_offering_id ON booking FOR EACH ROW EXECUTE FUNCTION booking_control_noop()',
        'before-event': 'BEFORE UPDATE OF starts_at, ends_at, status, service_offering_id ON booking FOR EACH ROW EXECUTE FUNCTION service_booking_sync_legacy_booking_allocation()',
    }.items():
        probe = subprocess.run(['psql', '-X', '-v', 'ON_ERROR_STOP=1', '-d', NAME], env=ENV,
            input="BEGIN; CREATE FUNCTION booking_control_noop() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RETURN NEW; END $$; DROP TRIGGER trg_service_booking_sync_legacy_allocation ON booking; CREATE TRIGGER trg_service_booking_sync_legacy_allocation " + definition + ';\n' + schema_sql + '\nROLLBACK;', capture_output=True, text=True)
        (OUTPUT / ('schema-control-' + label + '.log')).write_text(probe.stdout + probe.stderr)
        check('schema rejects ineffective booking trigger ' + label, probe.returncode != 0 and 'Booking calendar update and checkout correspondence contract is missing' in probe.stderr)
    concurrent_xml = xml.replace(b'MSG-20260804-001', b'MSG-CONFORMANCE-CONCURRENT')
    status, body = request('/ddex/documents', {**upload, 'uploadContentBase64': base64.b64encode(concurrent_xml).decode()}, method='POST')
    check('DDEX concurrency fixture upload accepted', status == 200)
    concurrent_id = str(json.loads(body)['ddexDocumentId'])
    before_runs = int(sql('SELECT count(*) FROM ddex_validation_run'))
    barrier = threading.Barrier(2)
    def validate_concurrently(_):
        barrier.wait(timeout=10)
        return request('/ddex/documents/' + concurrent_id + '/validation-runs', {}, method='POST')[0]
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        statuses = list(executor.map(validate_concurrently, range(2)))
    check('DDEX concurrent validators serialize with deliberate lifecycle results ' + str(statuses), 200 in statuses and set(statuses) <= {200, 409})
    check('DDEX creates exactly one completed run per accepted concurrent request', int(sql('SELECT count(*) FROM ddex_validation_run')) == before_runs + statuses.count(200) and sql('SELECT count(*) FROM ddex_validation_run WHERE finished_at IS NULL OR result_id IS NULL') == '0')

    # REC-CATALOG-001: a rejected multi-row reorder has no committed prefix.
    catalog_fixture = json.loads(sql("SELECT json_build_object('id',g.id,'catalog',c.code,'key',c.id) FROM genre g JOIN catalog_definition c ON c.id=g.catalog_id WHERE c.active ORDER BY g.id LIMIT 1"))
    catalog_path = '/catalog/' + catalog_fixture['catalog'] + '/reorder'
    def catalog_state():
        return json.loads(sql("SELECT json_build_object('revision',c.cache_revision,'items',(SELECT json_agg(json_build_array(g.id,g.sort_order,g.version,g.updated_at) ORDER BY g.id) FROM genre g WHERE g.catalog_id=c.id),'audit',(SELECT count(*) FROM catalog_audit_event a WHERE a.catalog_id=c.id)) FROM catalog_definition c WHERE c.id='" + catalog_fixture['key'] + "'"))
    def reorder_payload(ids, revision):
        return {'orderedItemIds': ids, 'expectedCatalogRevision': revision,
                'reason': 'Synthetic atomic reorder test', 'correlationId': str(uuid.uuid4())}
    before = catalog_state()
    payload = reorder_payload([catalog_fixture['id'], str(uuid.uuid4())], before['revision'])
    check('catalog missing reorder member rejected with 409', request(catalog_path, payload, method='POST')[0] == 409)
    check('catalog rejected reorder rolls back every row, revision and audit', catalog_state() == before)
    foreign_id = str(uuid.uuid4())
    foreign_catalog = sql("SELECT id FROM catalog_definition WHERE id <> '" + catalog_fixture['key'] + "' ORDER BY id LIMIT 1")
    sql("INSERT INTO genre(id,catalog_id,code,name_es,active,sort_order,version,created_at,updated_at) VALUES ('" + foreign_id + "','" + foreign_catalog + "','fixture-foreign','Fixture foreign',true,900,1,now(),now())")
    check('catalog foreign reorder member rejected with 409', request(catalog_path, reorder_payload([catalog_fixture['id'], foreign_id], before['revision']), method='POST')[0] == 409)
    check('catalog foreign-member rejection leaves target unchanged', catalog_state() == before)
    check('catalog foreign member is not updated', sql("SELECT sort_order || ':' || version FROM genre WHERE id='" + foreign_id + "'") == '900:1')
    valid = reorder_payload([catalog_fixture['id']], before['revision'])
    check('catalog fan cannot reorder', request(catalog_path, valid, token='fixture-fan', method='POST')[0] == 403)
    check('catalog unauthenticated reorder denied', request(catalog_path, valid, token=None, method='POST')[0] == 401)
    check('catalog authorization denial has no effect', catalog_state() == before)
    sql("CREATE FUNCTION fixture_reorder_audit_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic reorder audit failure'; END $$; CREATE TRIGGER fixture_reorder_audit_failure AFTER INSERT ON catalog_audit_event FOR EACH ROW WHEN (NEW.operation='reordered') EXECUTE FUNCTION fixture_reorder_audit_failure()")
    check('catalog audit failure is not reported as success', request(catalog_path, valid, method='POST')[0] == 500)
    check('catalog audit failure rolls back item and revision', catalog_state() == before)
    sql('DROP TRIGGER fixture_reorder_audit_failure ON catalog_audit_event; DROP FUNCTION fixture_reorder_audit_failure()')
    barrier = threading.Barrier(2)
    def reorder_concurrently(index):
        payload = reorder_payload([catalog_fixture['id']], before['revision'])
        barrier.wait(timeout=10)
        return request(catalog_path, payload, method='POST')[0]
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        statuses = list(executor.map(reorder_concurrently, range(2)))
    check('catalog concurrent identical expected revisions yield one commit and one conflict ' + str(statuses), sorted(statuses) == [200, 409])
    after = catalog_state()
    check('catalog successful reorder advances revision and audit exactly once', after['revision'] == before['revision'] + 1 and after['audit'] == before['audit'] + 1)
    old_item = next(item for item in before['items'] if item[0] == catalog_fixture['id'])
    new_item = next(item for item in after['items'] if item[0] == catalog_fixture['id'])
    check('catalog successful reorder advances selected item once', new_item[1] == 0 and new_item[2] == old_item[2] + 1)
    check('catalog unchanged members remain unchanged', [x for x in after['items'] if x[0] != catalog_fixture['id']] == [x for x in before['items'] if x[0] != catalog_fixture['id']])
    check('catalog stale retry returns conflict', request(catalog_path, valid, method='POST')[0] == 409)
    check('catalog stale retry has no effect', catalog_state() == after)

    # Anonymous assistant retrieval must not treat the internal index as public.
    # No OPENAI_API_KEY is present; local embeddings and fallback reply are used.
    embedding = [0] * 1536
    word_hash = 5381
    for character in 'hola': word_hash = word_hash * 33 + ord(character)
    embedding[word_hash % 1536] = 1
    def rag_chunk(source, identity, content):
        sql("INSERT INTO rag_chunk(source,source_id,chunk_index,content,metadata,embedding) VALUES ('" + source + "','" + identity + "',0,'" + content + "','{}','" + json.dumps(embedding) + "'::vector)")
    def knowledge():
        status, body = request('/ads/assist', {'aarMessage': 'hola'}, token=None, method='POST')
        check('anonymous assistant remains available', status == 200)
        return json.loads(body)['aasKnowledgeUsed']
    for source in ['availability', 'studio_brain', 'campaign', 'ad', 'resource', 'service', 'unknown']:
        rag_chunk(source, 'private-' + source, 'PRIVATE_SENTINEL_' + source)
    rag_chunk('course', 'missing-course', 'MISSING_COURSE_SENTINEL')
    check('private and unknown RAG sources excluded', knowledge() == [])
    sql("INSERT INTO course(slug,title,price_cents,currency,capacity,updated_at) VALUES ('public-conformance-course','PUBLIC_BEFORE',15000,'USD',12,now())")
    rag_chunk('course', 'public-conformance-course', 'POISONED_CACHED_PRIVATE_TEXT')
    sql("UPDATE course SET title='PUBLIC_CURRENT',updated_at=now()+interval '1 minute' WHERE slug='public-conformance-course'")
    content = ' '.join(knowledge())
    check('RAG renders current public course rather than cached private text', 'PUBLIC_CURRENT' in content and all(value not in content for value in ['PRIVATE_SENTINEL', 'POISONED_CACHED_PRIVATE_TEXT', 'PUBLIC_BEFORE']))
    sql("DELETE FROM course WHERE slug='public-conformance-course'")
    check('deleted course cannot survive through stale index', knowledge() == [])
    result = {'revision': revision, 'workingTreeDirty': dirty, 'binarySha256': hashlib.sha256(BINARY.read_bytes()).hexdigest(), 'checks': checks, 'status': 'passed'}
finally:
    if server is not None:
        server.terminate()
        try: server.wait(timeout=10)
        except subprocess.TimeoutExpired: server.kill(); server.wait(timeout=5)
    if owned:
        run(['dropdb', NAME], capture_output=True, text=True)

result['ownedDatabaseDropped'] = True
(OUTPUT / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
