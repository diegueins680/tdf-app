"""Invoice/receipt HTTP boundary checks on the caller's owned synthetic DB.

No provider submission, payment or customer notification is performed. Admission
races require observed PostgreSQL lock waits, not elapsed sleep as race evidence.
"""
import concurrent.futures
from contextlib import contextmanager
from pathlib import Path
import json
import subprocess
import time


def verify_invoice_receipts(sql, request, check, env, database, output, actors):
    def invoice(generate=False, token='fixture-admin'):
        return request('/invoices', {'ciCustomerId': int(actors['artist']), 'ciCurrency': 'USD',
            'ciGenerateReceipt': generate, 'ciLineItems': [{'cilDescription': 'Synthetic receipt item',
            'cilQuantity': 2, 'cilUnitCents': 12345, 'cilTaxBps': 1500}]}, token=token, method='POST')

    def new_invoice():
        response = invoice()
        check('invoice creates through current canonical UUID role admission', response[0] == 200)
        return json.loads(response[1])['invId']

    def receipt(iid, token='fixture-admin', **overrides):
        return request('/receipts', {'crInvoiceId': iid, **overrides}, token=token, method='POST')

    @contextmanager
    def held(query):
        with (output / 'invoice-receipt-barriers.log').open('a') as log:
            process = subprocess.Popen(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database],
                env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=log, text=True)
            try:
                process.stdin.write("BEGIN; " + query + "; SELECT 'fixture_locked:' || pg_backend_pid();\n")
                process.stdin.flush()
                while True:
                    line = process.stdout.readline()
                    if not line: raise RuntimeError('Invoice barrier exited before locking')
                    if line.startswith('fixture_locked:'):
                        pid = int(line.strip().split(':')[1]); break
                def release():
                    process.stdin.write('COMMIT;\n'); process.stdin.flush()
                yield release, pid
            finally:
                process.communicate('ROLLBACK;\n', timeout=15)
                if process.returncode: raise RuntimeError('Invoice barrier failed')

    def blocked(number, pid):
        deadline = time.monotonic() + 12
        while time.monotonic() < deadline:
            actual = int(sql("WITH RECURSIVE blocked(pid) AS (SELECT " + str(pid)
                + " UNION SELECT a.pid FROM pg_stat_activity a JOIN blocked b ON b.pid=ANY(pg_blocking_pids(a.pid)) WHERE a.datname=current_database()) SELECT count(DISTINCT pid) FROM blocked WHERE pid<>" + str(pid)))
            if actual >= number: return
            time.sleep(.02)
        raise AssertionError('Receipt race failed to observe blocked requests')

    def race(query, calls):
        with held(query) as (release, pid):
            with concurrent.futures.ThreadPoolExecutor(max_workers=len(calls)) as executor:
                futures = [executor.submit(call) for call in calls]
                try: blocked(len(calls), pid)
                finally: release()
                return [future.result() for future in futures]

    iid = new_invoice()
    check('receipt cannot relabel USD amounts as EUR', receipt(iid, crCurrency='EUR')[0] == 422)
    check('currency rejection creates no receipt', sql('SELECT count(*) FROM receipt WHERE invoice_id=' + str(iid)) == '0')
    responses = race('SELECT id FROM invoice WHERE id=' + str(iid) + ' FOR UPDATE',
        [lambda: receipt(iid, crBuyerName='Synthetic Buyer', crBuyerEmail='synthetic@example.invalid', crNotes='Immutable synthetic note') for _ in range(8)])
    check('eight same-invoice concurrent requests all succeed', all(r[0] == 200 for r in responses))
    bodies = [json.loads(r[1]) for r in responses]
    check('same-invoice replays return exactly one immutable receipt', len({json.dumps(b, sort_keys=True) for b in bodies}) == 1)
    first = bodies[0]
    check('receipt preserves invoice currency and exact minor units',
        (first['currency'], first['subtotalCents'], first['taxCents'], first['totalCents']) == ('USD', 24690, 3703, 28393))
    check('omitted replay overrides retain issued snapshot', json.loads(receipt(iid)[1]) == first)
    for field, value in [('crBuyerName', 'Changed buyer'), ('crBuyerEmail', 'changed@example.invalid'), ('crNotes', 'Changed notes')]:
        check('changed explicit replay ' + field + ' conflicts', receipt(iid, **{field: value})[0] == 409)
    check('currency mismatch remains rejected on replay', receipt(iid, crCurrency='EUR')[0] == 422)
    check('anonymous receipt denied', receipt(iid, token=None)[0] == 401)
    check('artist cannot issue studio receipt', receipt(iid, token='fixture-artist')[0] == 403)

    ids = [new_invoice() for _ in range(8)]
    responses = race('SELECT * FROM receipt_number_counter FOR UPDATE',
        [lambda key=key: receipt(key) for key in ids])
    check('eight distinct-invoice concurrent receipts succeed', all(r[0] == 200 for r in responses))
    check('annual allocator never duplicates concurrent receipt numbers',
        len({json.loads(r[1])['receiptNumber'] for r in responses}) == 8)

    # Old auth middleware may have read a committed grant. Recheck under the
    # same lock held by its revocation, then reject after observing its commit.
    for name, revoke, restore, expected in [
        ('session', "UPDATE api_token SET active=false WHERE token='fixture-admin'", "UPDATE api_token SET active=true WHERE token='fixture-admin'", 401),
        ('role assignment', 'UPDATE party_security_role SET active=false WHERE party_id=' + actors['admin'], 'UPDATE party_security_role SET active=true WHERE party_id=' + actors['admin'], 403),
        ('module permission', "UPDATE role_permission SET active=false WHERE role_id=(SELECT id FROM security_role WHERE code='admin')", "UPDATE role_permission SET active=true WHERE role_id=(SELECT id FROM security_role WHERE code='admin')", 403),
    ]:
        try:
            responses = race(revoke, [lambda: receipt(iid)])
            check('revoked ' + name + ' denies receipt replay after witnessed lock wait', responses[0][0] == expected)
        finally: sql(restore)

    for label, mutation in [
        ('header disagreement', 'UPDATE invoice SET subtotal_cents=1'),
        ('line disagreement', 'UPDATE invoice_line SET total_cents=1'),
        ('negative quantity', 'UPDATE invoice_line SET quantity=-2'),
        ('multiplication overflow', 'UPDATE invoice_line SET quantity=9223372036854775807'),
    ]:
        key = new_invoice()
        sql(mutation + (' WHERE id=' if mutation.startswith('UPDATE invoice SET') else ' WHERE invoice_id=') + str(key))
        check('receipt rejects stored ' + label, receipt(key)[0] == 422)
        check('invalid snapshot leaves no receipt ' + label, sql('SELECT count(*) FROM receipt WHERE invoice_id=' + str(key)) == '0')

    key = new_invoice()
    before = sql('SELECT sum(last_number) FROM receipt_number_counter')
    sql("CREATE FUNCTION receipt_fixture_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'synthetic private receipt data'; END $$; CREATE TRIGGER receipt_fixture_failure BEFORE INSERT ON receipt_line FOR EACH ROW EXECUTE FUNCTION receipt_fixture_failure()")
    try:
        check('failed receipt line insertion returns fixed failure', receipt(key) == (500, 'Internal server error'))
        check('failed line rolls back header and number allocation', sql('SELECT count(*) FROM receipt WHERE invoice_id=' + str(key)) == '0' and sql('SELECT sum(last_number) FROM receipt_number_counter') == before)
        invoice_count = sql('SELECT count(*) FROM invoice')
        check('combined invoice receipt failure rejects', invoice(generate=True)[0] == 500)
        check('combined failure rolls back invoice and lines', sql('SELECT count(*) FROM invoice') == invoice_count)
    finally:
        sql('DROP TRIGGER receipt_fixture_failure ON receipt_line; DROP FUNCTION receipt_fixture_failure()')
    check('failed transaction can safely retry after recovery', receipt(key)[0] == 200)
    generated = invoice(generate=True)
    check('invoice with generated receipt commits together', generated[0] == 200 and json.loads(generated[1])['receiptId'] is not None)

    # Savepoint-backed direct SQL controls prove constraints protect alternative
    # writers; neither a known-invalid insert nor a control persists fixture data.
    def rejected(statement):
        result = subprocess.run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database,
            '-c', 'BEGIN; ' + statement + '; ROLLBACK;'], env=env, capture_output=True, text=True)
        return result.returncode != 0
    check('database rejects issued invoice currency rewrite', rejected("UPDATE invoice SET currency='EUR' WHERE id=" + str(iid)))
    check('database rejects issued invoice amount rewrite', rejected('UPDATE invoice SET total_cents=1 WHERE id=' + str(iid)))
    check('database rejects receipt currency relabeling', rejected("UPDATE receipt SET currency='EUR' WHERE invoice_id=" + str(iid)))
    check('database rejects duplicate invoice receipts', rejected("INSERT INTO receipt(invoice_id,number,issue_date,issued_at,buyer_name,currency,subtotal_cents,tax_cents,total_cents,created_at) SELECT invoice_id,'synthetic-duplicate',issue_date,issued_at,buyer_name,currency,subtotal_cents,tax_cents,total_cents,created_at FROM receipt WHERE invoice_id=" + str(iid)))

    verification = subprocess.check_output(['node', 'scripts/render-production-schema-verification.mjs'],
        cwd=Path(__file__).resolve().parents[2], text=True)
    for constraint in ['unique_receipt_invoice', 'receipt_invoice_snapshot', 'receipt_nonnegative_snapshot']:
        result = subprocess.run(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database],
            input='BEGIN; ALTER TABLE receipt DROP CONSTRAINT ' + constraint + ';\n' + verification + '\nROLLBACK;',
            env=env, capture_output=True, text=True)
        check('schema gate detects missing receipt authority ' + constraint,
            result.returncode != 0 and 'Invoice receipt snapshot authority is missing or changed' in result.stderr)

    migration = (Path(__file__).resolve().parents[2]
        / 'tdf-hq/sql/2026-10-05_invoice_receipt_snapshot.sql').read_text()
    receipt_year = first['receiptNumber'].split('-')[1]
    for broken in [True, False]:
        key = new_invoice()
        number = 9999 if broken else 10000
        writer = "INSERT INTO receipt(invoice_id,number,issue_date,issued_at,buyer_name,currency,subtotal_cents,tax_cents,total_cents,created_at) SELECT id,'R-" + receipt_year + "-" + str(number) + "','2026-10-05',now(),'Synthetic legacy writer',currency,subtotal_cents,tax_cents,total_cents,now() FROM invoice WHERE id=" + str(key)
        source = migration
        if broken:
            source = source.replace('LOCK TABLE invoice IN SHARE ROW EXCLUSIVE MODE;', '').replace('LOCK TABLE receipt IN SHARE ROW EXCLUSIVE MODE;', '')
        with held(writer) as (release, pid):
            process = subprocess.Popen(['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', database],
                env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
            try:
                process.stdin.write('BEGIN; ' + source + '; COMMIT;\n'); process.stdin.close()
                if broken:
                    process.wait(timeout=10)
                    check('unlocked seed mutation misses uncommitted legacy number', process.returncode == 0 and int(sql('SELECT last_number FROM receipt_number_counter WHERE receipt_year=' + receipt_year)) < number)
                else:
                    blocked(1, pid)
                release()
                process.wait(timeout=15)
                check('migration fixture succeeds after legacy writer completion', process.returncode == 0)
                if not broken:
                    check('locked migration seeds max issued number after observed legacy wait', sql('SELECT last_number FROM receipt_number_counter WHERE receipt_year=' + receipt_year) == str(number))
            finally:
                if process.poll() is None: process.kill(); process.wait(timeout=5)
                for pipe in [process.stdout, process.stderr]: pipe.close()
    check('gapped legacy numbering does not reuse row counts', receipt(new_invoice())[0] == 200)
