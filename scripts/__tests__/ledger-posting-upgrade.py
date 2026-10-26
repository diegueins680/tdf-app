"""Legacy admission and reversal concurrency on the owned integration database."""
import os
from pathlib import Path
import select
import subprocess
import time
from urllib.parse import urlparse
import uuid

url = os.environ['TDF_LEDGER_POSTING_DATABASE_URL']
parsed = urlparse(url)
assert parsed.hostname in (['127.0.0.1', 'localhost'] + (['postgres'] if os.environ.get('CI') == 'true' else []))
assert parsed.path.endswith('_test') and not parsed.query and not parsed.fragment
assert not any(os.environ.get(k) for k in ['PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE'])
root = Path(__file__).resolve().parents[2]
migration = root / 'tdf-hq/sql/2026-10-12_ledger_posting_integrity.sql'
base = ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', url]
prefix = 'ledger-upgrade-' + uuid.uuid4().hex


def sql(statement):
    return subprocess.check_output(base + ['-c', statement], text=True).strip()


def create(posted=True):
    identity = str(uuid.uuid4())
    sql("INSERT INTO commerce_ledger_transaction(id,transaction_type,source_type,source_id,status,"
        "effective_at,correlation_id,created_by) VALUES ('" + identity + "','payment_capture','" + prefix
        + "','" + identity + "','draft',now(),'" + identity + "','synthetic-upgrade');"
        "INSERT INTO commerce_ledger_entry(transaction_id,account_code,currency,amount_minor) VALUES ('"
        + identity + "','synthetic.cash','USD',1),('" + identity + "','synthetic.revenue','USD',-1);"
        + ("UPDATE commerce_ledger_transaction SET status='posted' WHERE id='" + identity + "';" if posted else ''))
    return identity


def cleanup():
    sql("BEGIN; ALTER TABLE commerce_ledger_entry DISABLE TRIGGER trg_commerce_ledger_entry_immutable;"
        "DELETE FROM commerce_ledger_entry WHERE transaction_id IN (SELECT id FROM commerce_ledger_transaction "
        "WHERE source_type='" + prefix + "'); ALTER TABLE commerce_ledger_entry ENABLE TRIGGER trg_commerce_ledger_entry_immutable;"
        "ALTER TABLE commerce_ledger_transaction DISABLE TRIGGER trg_commerce_ledger_posting;"
        "UPDATE commerce_ledger_transaction SET status='draft', reversal_of=NULL WHERE source_type='" + prefix + "';"
        "ALTER TABLE commerce_ledger_transaction ENABLE TRIGGER trg_commerce_ledger_posting;"
        "DELETE FROM commerce_ledger_transaction WHERE source_type='" + prefix + "'; COMMIT;")


def migrate():
    result = subprocess.run(base + ['-f', str(migration)], text=True, capture_output=True, timeout=30)
    if result.returncode != 0:
        return result
    # Replay the additive successor too: replaying the predecessor alone would
    # leave the obsolete lock-only entry function installed for later suites.
    result = subprocess.run(base + ['-f', str(root / 'tdf-hq/sql/2026-10-16_ledger_entry_write_fence.sql')],
                            text=True, capture_output=True, timeout=30)
    if result.returncode != 0:
        return result
    return subprocess.run(base + ['-f', str(root / 'tdf-hq/sql/2026-10-24_ledger_guard_namespace.sql')],
                          text=True, capture_output=True, timeout=30)


before = sql("SELECT pg_get_functiondef('commerce_validate_ledger_posting()'::regprocedure)")
try:
    for case in ['empty', 'unbalanced', 'self', 'draft', 'reversed']:
        original, reverser, final = create(), create(case != 'draft'), create()
        drift = {
            'empty': "DELETE FROM commerce_ledger_entry WHERE transaction_id='" + original + "';",
            'unbalanced': "UPDATE commerce_ledger_entry SET amount_minor=2 WHERE transaction_id='" + original + "' AND amount_minor=1;",
            'self': "UPDATE commerce_ledger_transaction SET status='reversed',reversal_of=id WHERE id='" + original + "';",
            'draft': "UPDATE commerce_ledger_transaction SET status='reversed',reversal_of='" + reverser + "' WHERE id='" + original + "';",
            'reversed': "UPDATE commerce_ledger_transaction SET status='reversed',reversal_of='" + reverser + "' WHERE id='" + original + "';"
                        "UPDATE commerce_ledger_transaction SET status='reversed',reversal_of='" + final + "' WHERE id='" + reverser + "';",
        }[case]
        sql('BEGIN; ALTER TABLE commerce_ledger_transaction DISABLE TRIGGER trg_commerce_ledger_posting;'
            'ALTER TABLE commerce_ledger_entry DISABLE TRIGGER trg_commerce_ledger_entry_immutable;' + drift
            + 'ALTER TABLE commerce_ledger_entry ENABLE TRIGGER trg_commerce_ledger_entry_immutable;'
            'ALTER TABLE commerce_ledger_transaction ENABLE TRIGGER trg_commerce_ledger_posting; COMMIT;')
        result = migrate()
        expected = 'empty or unbalanced' if case in ['empty', 'unbalanced'] else 'distinct posted reverser'
        assert result.returncode != 0 and expected in result.stderr, (case, result.stderr)
        assert sql("SELECT pg_get_functiondef('commerce_validate_ledger_posting()'::regprocedure)") == before
        print('PASS legacy ' + case + ' refuses migration without replacing guards', flush=True)
        cleanup()
    for _ in range(2):
        result = migrate()
        assert result.returncode == 0, result.stderr
    print('PASS clean migration remains idempotent', flush=True)

    def start(isolation):
        p = subprocess.Popen(base, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, text=True, bufsize=1)
        p.stdin.write('BEGIN ISOLATION LEVEL ' + isolation + '; '
                      'SELECT pg_backend_pid() FROM commerce_ledger_transaction LIMIT 1;\n')
        p.stdin.flush()
        assert select.select([p.stdout], [], [], 10)[0], 'session did not establish snapshot'
        pid = int(p.stdout.readline().strip())
        return p, pid

    for isolation in ['READ COMMITTED', 'REPEATABLE READ']:
        original, reverser, final = create(), create(), create()
        target, target_pid = start(isolation)
        holder, holder_pid = start('READ COMMITTED')
        try:
            holder.stdin.write("UPDATE commerce_ledger_transaction SET status='reversed',reversal_of='"
                               + reverser + "' WHERE id='" + original + "'; SELECT 'held';\n")
            holder.stdin.flush()
            assert select.select([holder.stdout], [], [], 10)[0], 'reversal did not acquire fence'
            assert holder.stdout.readline().strip() == 'held'
            target.stdin.write("UPDATE commerce_ledger_transaction SET status='reversed',reversal_of='"
                               + final + "' WHERE id='" + reverser + "'; COMMIT;\n")
            target.stdin.flush()
            deadline = time.monotonic() + 10
            while sql('SELECT ' + str(holder_pid) + '=ANY(pg_blocking_pids(' + str(target_pid) + '))') != 't':
                assert time.monotonic() < deadline, 'reversal did not wait on shared write fence'
                time.sleep(.02)
            holder.communicate('COMMIT;\n', timeout=10)
            assert holder.returncode == 0
            _, error = target.communicate(timeout=10)
            expected = 'posted reverser' if isolation == 'READ COMMITTED' else 'could not serialize access'
            assert target.returncode != 0 and expected in error, error
            assert sql("SELECT status FROM commerce_ledger_transaction WHERE id='" + reverser + "'") == 'posted'
            print('PASS concurrent reversal preserves posted reference under ' + isolation, flush=True)
        finally:
            for p in [holder, target]:
                if p.poll() is None:
                    p.communicate('ROLLBACK;\n', timeout=10)
            cleanup()
finally:
    cleanup()
