"""PAY-MUSIC-OUTCOME-001 on an admitted synthetic PostgreSQL database.

Provider evidence is a fixture; this checks database projection, not transport,
capture admission, ledger posting, receipts or automated reconciliation.
"""
import concurrent.futures
from contextlib import contextmanager
import json
import hashlib
import re
import selectors
import subprocess
import time
import uuid


def verify_music_payment_outcomes(sql, check, env, database, output, root, request=None):
    fixture = (root / 'tdf-hq/test/sql/music_payment_state_fixture.sql').read_text()
    flags_before = sql("SELECT coalesce(jsonb_object_agg(flag_key || ':' || environment,enabled), '{}') "
                       "FROM revenue_feature_flag WHERE flag_key LIKE 'music_releases.%'")
    verify_music_payment_controls(sql, check, env, database, output, root)

    def make_fixture():
        value = json.loads(sql(fixture + '\nSELECT row_to_json(f) '
                              'FROM pg_temp.music_payment_state_fixture() f;'))
        return {key: str(uuid.UUID(value[key])) for key in ['order_id', 'checkout_id', 'attempt_id']}

    def failure(row):
        return ("UPDATE commerce_payment_attempt SET status='failed',failure_code='fixture_failure' WHERE id='"
                + row['attempt_id'] + "' AND status <> 'succeeded'; "
                "UPDATE commerce_checkout_session SET status='failed' WHERE id='"
                + row['checkout_id'] + "' AND status IN ('awaiting_payment','processing');")

    def payment(row):
        return ("UPDATE commerce_payment_attempt SET status='succeeded',failure_code=NULL WHERE id='"
                + row['attempt_id'] + "'; UPDATE commerce_checkout_session "
                "SET status='paid',paid_minor=total_minor,paid_at=NOW() WHERE id='"
                + row['checkout_id'] + "';")

    @contextmanager
    def held(body, label):
        with (output / 'music-payment-barriers.log').open('a') as log:
            process = subprocess.Popen(
                ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database],
                env={**env, 'PGAPPNAME': label}, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=log, text=True)
            released = False
            try:
                process.stdin.write("BEGIN ISOLATION LEVEL READ COMMITTED; " + body
                                    + " SELECT 'locked:' || pg_backend_pid();\n")
                process.stdin.flush()
                with selectors.DefaultSelector() as selector:
                    selector.register(process.stdout, selectors.EVENT_READ)
                    if not selector.select(timeout=15):
                        raise AssertionError('Music payment holder did not acquire its lock')
                    marker = process.stdout.readline().strip()
                if not marker.startswith('locked:'):
                    raise AssertionError('Music payment holder exited before its lock marker')
                pid = int(marker.split(':')[1])

                def release():
                    nonlocal released
                    if not released:
                        process.stdin.write('COMMIT;\n')
                        process.stdin.flush()
                        released = True

                yield release, pid
            finally:
                try:
                    process.communicate('ROLLBACK;\n', timeout=6)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.communicate(timeout=5)
                    raise AssertionError('Music payment holder did not exit')
                if process.returncode:
                    raise AssertionError('Music payment holder failed')

    def waiting(label, pid):
        deadline = time.monotonic() + 8
        while time.monotonic() < deadline:
            if sql("SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() "
                   "AND application_name='" + label + "' AND wait_event_type='Lock' AND "
                   + str(pid) + '=ANY(pg_blocking_pids(pid))') == '1':
                return
            time.sleep(.02)
        raise AssertionError('Music payment waiter was not witnessed blocked by the holder')

    def execute(body, label):
        result = subprocess.run(
            ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database,
             '-c', 'BEGIN ISOLATION LEVEL READ COMMITTED; ' + body + ' COMMIT;'],
            env={**env, 'PGAPPNAME': label}, capture_output=True, text=True, timeout=25)
        if result.returncode:
            with (output / 'music-payment-barriers.log').open('a') as log:
                log.write(result.stdout + result.stderr)
            raise AssertionError('Music payment waiter failed')

    for kind in ['failure-first', 'success-first', 'cancel-first', 'expiry-first', 'failure-before-retry',
                 'failure-before-late-binding', 'failure-before-late-pending']:
        row = make_fixture()
        holder = failure(row) if kind == 'failure-first' else payment(row)
        waiter = failure(row) if kind == 'success-first' else payment(row)
        if kind in ['cancel-first', 'expiry-first']:
            status = 'cancelled' if kind == 'cancel-first' else 'expired'
            holder = ("UPDATE commerce_checkout_session SET status='" + status
                      + "' WHERE id='" + row['checkout_id'] + "';")
        if kind in ['failure-before-retry', 'failure-before-late-binding', 'failure-before-late-pending']:
            holder = failure(row)
            waiter = ("DO $$ BEGIN IF music_checkout_payment_attempt_allowed('"
                      + row['checkout_id'] + "') THEN RAISE EXCEPTION "
                      "'stale music retry admitted'; END IF; END $$;")
        if kind == 'failure-before-late-binding':
            waiter = ("UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id='"
                      + row['checkout_id'] + "'; " + waiter)
        if kind == 'failure-before-late-pending':
            waiter = ("UPDATE commerce_payment_attempt SET status='processing' WHERE id='"
                      + row['attempt_id'] + "'; UPDATE commerce_checkout_session SET status='processing' WHERE id='"
                      + row['checkout_id'] + "'; " + waiter)
        label = 'music-outcome-' + uuid.uuid4().hex
        with held(holder, label + '-holder') as (release, pid):
            executor = concurrent.futures.ThreadPoolExecutor(max_workers=1)
            future = executor.submit(execute, waiter, label)
            try:
                try:
                    waiting(label, pid)
                    check('music payment lock witnessed: ' + kind, not future.done())
                finally:
                    # Release before waiting for/cancelling a blocked waiter,
                    # including when observation or an assertion raises.
                    release()
                future.result(timeout=25)
            finally:
                release()
                executor.shutdown(wait=True, cancel_futures=True)
        state = json.loads(sql(
            "SELECT json_build_object('order',p.state,'checkout',c.status,'active',"
            "(SELECT count(*) FROM music_entitlement e WHERE e.purchase_order_id=p.id "
            "AND e.status='active'),'exceptions',(SELECT count(*) FROM "
            "commerce_reconciliation_exception r WHERE r.internal_reference=c.id::text "
            "AND r.exception_type='music_paid_closed_order' AND r.status='open')) "
            "FROM music_purchase_order p JOIN commerce_checkout_session c ON c.id=p.checkout_id "
            "WHERE p.id='" + row['order_id'] + "'"))
        expected = ({'order': 'cancelled', 'checkout': 'paid', 'active': 0, 'exceptions': 1}
                    if kind in ['cancel-first', 'expiry-first'] else
                    {'order': 'paid', 'checkout': 'paid', 'active': 1, 'exceptions': 0})
        if kind.startswith('failure-before-'):
            status = {'failure-before-retry': 'failed', 'failure-before-late-binding': 'awaiting_payment',
                      'failure-before-late-pending': 'processing'}[kind]
            expected = {'order': 'awaiting_payment', 'checkout': status, 'active': 0, 'exceptions': 0}
        check('music payment serialized outcome: ' + kind, state == expected)
        if kind in ['cancel-first', 'expiry-first'] and request is not None:
            path = ('/admin/commerce/reconciliation-exceptions?environment=sandbox&status=open'
                    '&checkoutId=' + row['checkout_id'])
            status, body = request(path)
            entries = json.loads(body).get('crrEntries', []) if status == 200 else []
            check('music closed-order exception links through administrative API: ' + kind,
                  status == 200 and len(entries) == 1
                  and entries[0].get('creCheckoutId') == row['checkout_id']
                  and entries[0].get('crePaymentAttemptId') == row['attempt_id']
                  and entries[0].get('creReason') == 'music_paid_closed_order'
                  and entries[0].get('creExpectedMinor') == '250'
                  and entries[0].get('creActualMinor') == '250'
                  and 'music-state-fixture' not in body)
            check('music reconciliation denies anonymous and unrelated actors: ' + kind,
                  request(path, token='')[0] == 401
                  and request(path, token='fixture-fan')[0] == 403)

    if request is not None:
        row = make_fixture()
        token = 'music-binding-' + uuid.uuid4().hex
        sql("INSERT INTO api_token(token,party_id,label,active) SELECT '" + token
            + "',buyer_party_id,'Synthetic music binding replay',true FROM music_purchase_order WHERE id='"
            + row['order_id'] + "'; INSERT INTO commerce_provider_binding(payment_attempt_id,provider,environment,"
            "merchant_account_ref,resource_type,provider_resource_id,merchant_reference,amount_minor,currency) "
            "SELECT id,provider,environment,merchant_account_ref,'order','FIXTURE-ORDER-'||id,'" + row['order_id']
            + "',amount_minor,currency FROM commerce_payment_attempt WHERE id='" + row['attempt_id'] + "'")
        saved = sql("SELECT coalesce(json_agg(row_to_json(flag)), '[]') FROM revenue_feature_flag flag "
                    "WHERE flag_key='music_releases.commerce' AND environment='sandbox'")
        try:
            sql("INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason) VALUES "
                "('music_releases.commerce',true,'sandbox','synthetic fixture') "
                "ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true")
            path = '/music/purchases/' + row['order_id'] + '/paypal/create'
            code, body = request(path, token=token, method='POST', idempotency='fixture-existing-binding')
            check('music existing binding is exercised without a provider call',
                  code == 200 and json.loads(body).get('paypalOrderId') == 'FIXTURE-ORDER-' + row['attempt_id'])
            for status in ['failed', 'awaiting_payment', 'processing']:
                if status == 'failed':
                    sql(failure(row))
                else:
                    sql("UPDATE commerce_checkout_session SET status='" + status + "' WHERE id='" + row['checkout_id'] + "'")
                    if status == 'processing':
                        sql("UPDATE commerce_payment_attempt SET status='processing' WHERE id='" + row['attempt_id'] + "'")
                code, body = request(path, token=token, method='POST', idempotency='fixture-existing-binding')
                check('music existing binding denies quarantined replay: ' + status,
                      code == 409 and 'requires reconciliation' in body and 'FIXTURE-ORDER-' not in body)
            check('music denied binding replays create no payment attempt', sql(
                "SELECT count(*) FROM commerce_payment_attempt WHERE checkout_id='" + row['checkout_id'] + "'") == '1')
        finally:
            # Only the admitted disposable fixture is changed; never a runtime activation.
            sql("DELETE FROM revenue_feature_flag WHERE flag_key='music_releases.commerce' "
                "AND environment='sandbox'; INSERT INTO revenue_feature_flag "
                "SELECT * FROM json_populate_recordset(NULL::revenue_feature_flag, '" + saved.replace("'", "''") + "')")

    check('music outcome tests preserve all feature flags', flags_before == sql(
        "SELECT coalesce(jsonb_object_agg(flag_key || ':' || environment,enabled), '{}') "
        "FROM revenue_feature_flag WHERE flag_key LIKE 'music_releases.%'"))


def verify_music_payment_controls(sql, check, env, database, output, root):
    migration = (root / 'tdf-hq/sql/2026-10-15_music_payment_outcome_consistency.sql').read_text()
    functions = re.findall(
        r'CREATE OR REPLACE FUNCTION music_sync_verified_checkout\(\).*?\$\$;',
        migration, flags=re.S)
    if len(functions) != 1:
        raise AssertionError('Unsupported music outcome migration function shape')
    function = functions[0]
    body = re.fullmatch(
        r'CREATE OR REPLACE FUNCTION music_sync_verified_checkout\(\)\s+'
        r'RETURNS TRIGGER LANGUAGE plpgsql AS \$\$(.*)\$\$;', function, flags=re.S)
    if body is None:
        raise AssertionError('Unsupported music outcome function body shape')
    fingerprint_query = ("SELECT prosrc FROM pg_proc WHERE "
                         "oid='music_sync_verified_checkout()'::regprocedure")
    installed = sql(fingerprint_query)
    check('music outcome installed trigger matches candidate source', installed == body.group(1).strip())
    retry_functions = re.findall(
        r'CREATE OR REPLACE FUNCTION music_checkout_payment_attempt_allowed\(checkout_key UUID\).*?\$\$;',
        migration, flags=re.S)
    if len(retry_functions) != 1:
        raise AssertionError('Unsupported music retry admission function shape')
    retry_function = retry_functions[0]
    retry_body = re.fullmatch(
        r'CREATE OR REPLACE FUNCTION music_checkout_payment_attempt_allowed\(checkout_key UUID\)\s+'
        r'RETURNS BOOLEAN LANGUAGE plpgsql AS \$\$(.*)\$\$;', retry_function, flags=re.S)
    if retry_body is None:
        raise AssertionError('Unsupported music retry admission body shape')
    retry_fingerprint = ("SELECT prosrc FROM pg_proc WHERE "
                         "oid='music_checkout_payment_attempt_allowed(uuid)'::regprocedure")
    installed_retry = sql(retry_fingerprint)
    check('music retry admission matches candidate source', installed_retry == retry_body.group(1).strip())
    tests = (root / 'tdf-hq/test/sql/music_payment_outcomes.sql').read_text()
    fixture = (root / 'tdf-hq/test/sql/music_payment_state_fixture.sql').read_text()
    include = r'\ir music_payment_state_fixture.sql'
    if tests.count(include) != 1:
        raise AssertionError('Unsupported music outcome test include shape')
    tests = tests.replace(include, fixture)

    def replace_once(value, before, after):
        if value.count(before) != 1:
            raise AssertionError('Unsupported music outcome mutation target')
        return value.replace(before, after)

    cancel = replace_once(function, "ELSIF NEW.status IN ('cancelled','expired') THEN",
                          "ELSIF NEW.status IN ('cancelled','expired','failed') THEN")
    grant_closed = replace_once(function,
        "IF purchase.state NOT IN ('pending','awaiting_payment','paid') THEN", 'IF FALSE THEN')
    dropped, changed = re.subn(
        r'      INSERT INTO commerce_reconciliation_exception\(.*?ON CONFLICT DO NOTHING;',
        '      NULL;', function, flags=re.S)
    if changed != 1:
        raise AssertionError('Unsupported reconciliation mutation target')
    wrong_reference = replace_once(function,
        'NEW.id::TEXT, provider_reference_value, purchase.gross_minor,',
        'purchase.id::TEXT, provider_reference_value, purchase.gross_minor,')
    allow_failed, changed = re.subn(
        r"RETURN FOUND AND checkout_status IN .*?;", 'RETURN FOUND;', retry_function, flags=re.S)
    if changed != 1:
        raise AssertionError('Unsupported full admission mutation target')
    drop_fence, changed = re.subn(
        r'\n    AND NOT EXISTS \(.*?\n    \)', '', retry_function, flags=re.S)
    if changed != 1:
        raise AssertionError('Unsupported durable failure mutation target')
    variants = [
        ('intended', function, None),
        ('cancel-on-failure', cancel, 'attempt failure does not cancel a payable order'),
        ('grant-closed', grant_closed, 'verified money never reopens closed fulfillment: cancelled'),
        ('drop-exception', dropped,
         'closed-order capture has one observable reconciliation case: cancelled'),
        ('wrong-reference', wrong_reference,
         'closed-order capture has one observable reconciliation case: cancelled'),
        ('allow-failed-retry', allow_failed + '\n' + function,
         'failed checkout denies a new or replayed provider operation'),
        ('drop-failure-fence', drop_fence + '\n' + function,
         'late binding cannot clear the failed-attempt quarantine')]
    results = []
    for name, variant, expected in variants:
        path = output / ('music-outcome-' + name + '.sql')
        with path.open('x') as target:
            target.write('BEGIN ISOLATION LEVEL READ COMMITTED;\n' + variant + '\n' + tests)
        result = subprocess.run(
            ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f', str(path)],
            env=env, capture_output=True, text=True, timeout=30)
        log = result.stdout + result.stderr
        (output / ('music-outcome-' + name + '.log')).write_text(log)
        # A named assertion must fail. Syntax, transport, timeout and unrelated
        # database errors cannot satisfy a negative control.
        matches = expected is not None and re.search(
            r'ERROR:\s+music payment outcome assertion failed: ' + re.escape(expected)
            + r'\s*$', log, flags=re.M) is not None
        check('music outcome SQL control: ' + name,
              result.returncode == 0 if expected is None else result.returncode != 0 and matches)
        check('music outcome control restores trigger: ' + name, sql(fingerprint_query) == installed)
        check('music outcome control restores admission: ' + name, sql(retry_fingerprint) == installed_retry)
        results.append({'variant': name, 'exit': result.returncode,
                        'expectedAssertion': expected,
                        'logSha256': hashlib.sha256(log.encode()).hexdigest()})
    (output / 'music-outcome-controls.json').write_text(json.dumps({
        'functionBodySha256': hashlib.sha256(installed.encode()).hexdigest(),
        'migrationSha256': hashlib.sha256(migration.encode()).hexdigest(),
        'scope': 'Transactional SQL mutations of the actual projection function; synthetic provider evidence',
        'results': results}, indent=2) + '\n')
