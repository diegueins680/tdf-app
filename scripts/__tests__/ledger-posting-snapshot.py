"""SYS-MONEY-003: witnessed MVCC schedules on a fully migrated synthetic DB.

No backend substitute: commands exercise the installed PostgreSQL triggers.
The optional controls replace only the entry fence, then restore its exact bytes.
"""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import select
import subprocess
import time
import uuid

ROOT = Path(__file__).resolve().parents[2]
URL = os.environ['TDF_LEDGER_POSTING_DATABASE_URL']
# Reuse the canonical full-authority parser BEFORE connecting or issuing SQL.
subprocess.run(['node', '--input-type=module', '-e',
                "import {disposablePostgresUrl} from './scripts/lib/disposable-postgres-url.mjs';"
                "disposablePostgresUrl(process.argv[1], {ci:process.env.CI==='true'});", URL],
               cwd=ROOT, check=True)
BASE = ['psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-v', 'VERBOSITY=verbose', '-d', URL]
PREFIX = 'ledger-snapshot-' + uuid.uuid4().hex
RESULTS = []


def require(condition, detail):
    if not condition:
        raise AssertionError(detail)


def sql(statement, ok=True):
    result = subprocess.run(BASE + ['-c', statement], text=True, capture_output=True, timeout=20)
    if ok:
        require(result.returncode == 0, result.stderr)
    return result


def value(statement):
    return sql(statement).stdout.strip()


def record(name, **facts):
    RESULTS.append({'case': name, **facts})
    print(json.dumps(RESULTS[-1], sort_keys=True), flush=True)


def draft():
    identity = str(uuid.uuid4())
    sql("INSERT INTO commerce_ledger_transaction(id,transaction_type,source_type,source_id,status,"
        "effective_at,correlation_id,created_by) VALUES ('" + identity + "','payment_capture','" + PREFIX
        + "','" + identity + "','draft',now(),'" + identity + "','synthetic-snapshot');"
        "INSERT INTO commerce_ledger_entry(transaction_id,account_code,currency,amount_minor) VALUES ('"
        + identity + "','synthetic.cash','USD',1),('" + identity + "','synthetic.revenue','USD',-1);")
    return identity


def state(identity):
    return value("SELECT status || '|' || COALESCE(sum(amount_minor::numeric),0)::text "
                 "FROM commerce_ledger_transaction t LEFT JOIN commerce_ledger_entry e "
                 "ON e.transaction_id=t.id WHERE t.id='" + identity + "' GROUP BY t.status")


def cleanup():
    # Fixtures alone are removed, transactionally. DDL is limited to the owned test DB.
    sql("BEGIN; ALTER TABLE commerce_ledger_entry DISABLE TRIGGER trg_commerce_ledger_entry_immutable;"
        "DELETE FROM commerce_ledger_entry WHERE transaction_id IN "
        "(SELECT id FROM commerce_ledger_transaction WHERE source_type='" + PREFIX + "');"
        "ALTER TABLE commerce_ledger_entry ENABLE TRIGGER trg_commerce_ledger_entry_immutable;"
        "DELETE FROM commerce_ledger_transaction WHERE source_type='" + PREFIX + "'; COMMIT;")


class Session:
    def __init__(self, isolation):
        self.process = subprocess.Popen(BASE, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE, text=True, bufsize=1)
        self.send('BEGIN ISOLATION LEVEL ' + isolation + "; SET LOCAL statement_timeout='15s'; "
                  'SELECT pg_backend_pid() FROM commerce_ledger_transaction LIMIT 1;')
        # Reading a real table fixes the RR/SERIALIZABLE snapshot before the other writer.
        self.pid = int(self.line())

    def send(self, statement):
        self.process.stdin.write(statement + '\n')
        self.process.stdin.flush()

    def line(self):
        require(select.select([self.process.stdout], [], [], 10)[0], 'session did not acknowledge')
        result = self.process.stdout.readline().strip()
        require(result, 'session closed without acknowledgement')
        return result

    def finish(self, statement=None):
        output, error = self.process.communicate(statement, timeout=20)
        return self.process.returncode, output.strip(), error

    def close(self):
        if self.process.poll() is None:
            self.finish('ROLLBACK;\n')


def blocked(waiter, holder):
    deadline = time.monotonic() + 10
    while value('SELECT ' + str(holder.pid) + '=ANY(pg_blocking_pids(' + str(waiter.pid) + '))') != 't':
        require(time.monotonic() < deadline, 'expected blocking edge was never observed')
        require(waiter.process.poll() is None, 'waiter exited before acquiring the shared fence')
        time.sleep(.02)


def posting(identity):
    return "UPDATE commerce_ledger_transaction SET status='posted' WHERE id='" + identity + "';"


def mutation(identity, operation):
    if operation == 'insert':
        return ("INSERT INTO commerce_ledger_entry(transaction_id,account_code,currency,amount_minor) "
                "VALUES ('" + identity + "','synthetic.extra','USD',1);")
    if operation == 'update':
        return ("UPDATE commerce_ledger_entry SET amount_minor=2 WHERE transaction_id='" + identity
                + "' AND amount_minor=1;")
    return "DELETE FROM commerce_ledger_entry WHERE transaction_id='" + identity + "' AND amount_minor=-1;"


def entry_first(isolation, operation, overlap, control=False, rollback=False):
    identity = draft()
    target, holder = Session(isolation), Session('READ COMMITTED')
    try:
        holder.send(mutation(identity, operation) + "SELECT 'held';")
        require(holder.line() == 'held', 'entry mutation did not acquire fence')
        if not overlap:
            require(holder.finish('COMMIT;\n')[0] == 0, 'entry writer failed')
        target.send(posting(identity) + 'COMMIT;')
        if overlap:
            blocked(target, holder)
            require(holder.finish('ROLLBACK;\n' if rollback else 'COMMIT;\n')[0] == 0,
                    'entry writer completion failed')
        code, _, error = target.finish()
        actual = state(identity)
        if rollback:
            require(code == 0 and actual == 'posted|0', (code, error, actual))
            outcome = 'posted after entry rollback'
        elif control:
            require(code == 0 and actual == 'posted|1', ('missing counterexample', code, error, actual))
            outcome = 'counterexample: committed posted balance 1'
        else:
            expected = '23514' if isolation == 'READ COMMITTED' else '40001'
            require(code != 0 and expected in error and actual == 'draft|1', (code, error, actual))
            outcome = expected
        record('entry-first', isolation=isolation, operation=operation, overlap=overlap,
               rollback=rollback, control=control, outcome=outcome, state=actual)
    finally:
        holder.close()
        target.close()
        cleanup()


def post_first(isolation):
    identity = draft()
    target, holder = Session(isolation), Session('READ COMMITTED')
    try:
        holder.send(posting(identity) + "SELECT 'posted-held';")
        require(holder.line() == 'posted-held', 'posting did not acquire fence')
        target.send(mutation(identity, 'insert') + 'COMMIT;')
        blocked(target, holder)
        require(holder.finish('COMMIT;\n')[0] == 0, 'posting writer failed')
        code, _, error = target.finish()
        expected = '23514' if isolation == 'READ COMMITTED' else '40001'
        require(code != 0 and expected in error and state(identity) == 'posted|0', (code, error))
        record('post-first', isolation=isolation, outcome=expected, state=state(identity))
    finally:
        holder.close()
        target.close()
        cleanup()


def upgrade_preflight(function):
    migration = ROOT / 'tdf-hq/sql/2026-10-16_ledger_entry_write_fence.sql'
    for case in ['empty', 'unbalanced']:
        identity = draft()
        try:
            sql(posting(identity))
            drift = ("DELETE FROM commerce_ledger_entry WHERE transaction_id='" + identity + "';"
                     if case == 'empty' else
                     "UPDATE commerce_ledger_entry SET amount_minor=2 WHERE transaction_id='" + identity
                     + "' AND amount_minor=1;")
            sql('BEGIN; ALTER TABLE commerce_ledger_entry DISABLE TRIGGER trg_commerce_ledger_entry_immutable;'
                + drift + 'ALTER TABLE commerce_ledger_entry ENABLE TRIGGER trg_commerce_ledger_entry_immutable; COMMIT;')
            before = state(identity)
            result = subprocess.run(BASE + ['-f', str(migration)], text=True, capture_output=True, timeout=20)
            require(result.returncode != 0 and '23514' in result.stderr
                    and 'empty or unbalanced' in result.stderr, result.stderr)
            require(state(identity) == before, 'migration rewrote financial history')
            require(value("SELECT pg_get_functiondef('commerce_protect_posted_ledger()'::regprocedure)") == function,
                    'failed preflight changed the installed guard')
            record('additive-migration-preflight', history=case, outcome='23514; history and guard retained')
        finally:
            cleanup()
    for _ in range(2):
        result = subprocess.run(BASE + ['-f', str(migration)], text=True, capture_output=True, timeout=20)
        require(result.returncode == 0, result.stderr)
    record('additive-migration-replay', applications=2, outcome='accepted clean history')


def run(controls):
    version = value('SELECT version()')
    manifest = json.loads((ROOT / 'scripts/production-migrations.json').read_text())
    expected = {entry['id'] for entry in manifest['migrations']}
    applied = set(value('SELECT migration_id FROM tdf_schema_migration').splitlines())
    require(expected == applied, ('test DB must have the complete candidate manifest',
                                  sorted(expected - applied), sorted(applied - expected)))
    function = value("SELECT pg_get_functiondef('commerce_protect_posted_ledger()'::regprocedure)")
    upgrade_preflight(function)
    for isolation in ['READ COMMITTED', 'REPEATABLE READ', 'SERIALIZABLE']:
        for operation in ['insert', 'update', 'delete']:
            for overlap in [True, False]:
                entry_first(isolation, operation, overlap)
        post_first(isolation)
        entry_first(isolation, 'insert', True, rollback=True)
    if controls:
        write_fence = ("UPDATE commerce_ledger_transaction SET created_at = created_at\n"
                       "    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id)\n"
                       "    RETURNING status INTO transaction_status;")
        lock_only = ("SELECT status INTO transaction_status FROM commerce_ledger_transaction\n"
                     "    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id) FOR NO KEY UPDATE;")
        require(function.count(write_fence) == 1, 'entry fence shape changed; review control translation')
        try:
            sql(function.replace(write_fence, lock_only))
            for operation in ['insert', 'update', 'delete']:
                for overlap in [True, False]:
                    entry_first('REPEATABLE READ', operation, overlap, control=True)
        finally:
            sql(function)
        require(value("SELECT pg_get_functiondef('commerce_protect_posted_ledger()'::regprocedure)") == function,
                'control did not restore the installed function')
    sources = [Path(__file__).relative_to(ROOT).as_posix(),
               'tdf-hq/sql/2026-08-13_unified_checkout_core.sql',
               'tdf-hq/sql/2026-10-12_ledger_posting_integrity.sql',
               'tdf-hq/sql/2026-10-16_ledger_entry_write_fence.sql',
               'scripts/production-migrations.json',
               'scripts/__tests__/fixtures/production-schema-20260814.sql',
               'scripts/__tests__/fixtures/catalog-production-source-fixture.sql',
               'formal/event-operations/LedgerPosting.tla']
    print(json.dumps({'kind': 'postgresql-integration-receipt',
                      'timestamp': datetime.datetime.now(datetime.timezone.utc).isoformat(),
                      'sourceRevision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT,
                                                               text=True).strip(),
                      'server': version, 'cases': len(RESULTS),
                      'appliedMigrations': len(applied),
                      'dirtyWorktree': bool(subprocess.check_output(['git', 'status', '--porcelain'],
                                                                   cwd=ROOT, text=True)),
                      'sourceHashes': {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest() for p in sources},
                      'installedEntryFunctionSha256': hashlib.sha256(function.encode()).hexdigest(),
                      'claim': 'Named finite SQL schedules only; complete refinement remains UNPROVEN'},
                     sort_keys=True), flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--negative-controls', action='store_true')
    args = parser.parse_args()
    try:
        run(args.negative_controls)
    finally:
        cleanup()
