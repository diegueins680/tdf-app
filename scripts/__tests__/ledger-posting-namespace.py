"""Exercise non-owner namespace attacks against the installed ledger triggers.

Each session rolls back its synthetic role, ledger rows, namespace and optional
function mutation. No production role or privilege policy is changed.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess
import uuid

ROOT = Path(__file__).resolve().parents[2]
URL = os.environ['TDF_LEDGER_POSTING_DATABASE_URL']
subprocess.run(['node', '--input-type=module', '-e',
                "import {disposablePostgresUrl} from './scripts/lib/disposable-postgres-url.mjs';"
                "disposablePostgresUrl(process.argv[1], {ci:process.env.CI==='true'});", URL],
               cwd=ROOT, check=True)
BASE = ['psql', '-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', URL]


def require(condition, problem):
    # Verification must remain fail-closed under python -O / PYTHONOPTIMIZE.
    if not condition:
        raise AssertionError(problem)


def sql(statement):
    return subprocess.run(BASE, input=statement, text=True, capture_output=True, timeout=30)


def value(statement):
    result = sql(statement)
    require(result.returncode == 0, result.stderr)
    return result.stdout.strip()


def definitions():
    return value("SELECT string_agg(pg_get_functiondef(oid), E'\\n' ORDER BY proname) "
                 "FROM pg_proc WHERE oid IN ('public.commerce_validate_ledger_posting()'::regprocedure,"
                 "'public.commerce_protect_posted_ledger()'::regprocedure)")


def fixture(control=None):
    token = uuid.uuid4().hex
    role = 'ledger_namespace_' + token
    bad, posted, valid, empty = [str(uuid.uuid4()) for _ in range(4)]
    prefix = 'namespace-' + token
    setup = f"""
BEGIN;
SET LOCAL lock_timeout = '5s';
CREATE ROLE {role} NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS;
GRANT USAGE ON SCHEMA public TO {role};
GRANT SELECT, INSERT, UPDATE, DELETE ON public.commerce_ledger_transaction,
  public.commerce_ledger_entry TO {role};
-- The existing identity-reference trigger reads this synthetic fixture table
-- before the ledger trigger. Preserve that guard and grant only its needed read.
GRANT SELECT ON public.identity_party_archive TO {role};
"""
    if control:
        setup += f'ALTER FUNCTION public.{control}() RESET search_path;\n'
    # Owner fixtures are created before switching to the ordinary role.
    for identity, balanced in [(bad, False), (posted, True), (valid, True), (empty, None)]:
        setup += f"""
INSERT INTO public.commerce_ledger_transaction
  (id, transaction_type, source_type, source_id, status, effective_at, correlation_id, created_by)
VALUES ('{identity}', 'payment_capture', '{prefix}', '{identity}', 'draft', now(), '{identity}', 'synthetic');
"""
        if balanced is not None:
            setup += f"""
INSERT INTO public.commerce_ledger_entry(transaction_id, account_code, currency, amount_minor)
VALUES ('{identity}', 'synthetic.cash', 'USD', 1)
""" + (f", ('{identity}', 'synthetic.revenue', 'USD', -1)" if balanced else '') + ';\n'
    setup += f"""
UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='{posted}';
SET LOCAL ROLE {role};
DO $$ BEGIN
  IF (SELECT rolsuper OR rolcreaterole OR rolbypassrls FROM pg_roles WHERE rolname=current_user)
     OR has_schema_privilege(current_user, 'public', 'CREATE')
     OR NOT has_database_privilege(current_user, current_database(), 'TEMP')
     OR (SELECT pg_get_userbyid(relowner)=current_user FROM pg_class
         WHERE oid='public.commerce_ledger_transaction'::regclass) THEN
    RAISE EXCEPTION 'Fixture requires a non-owner role with TEMP and without public CREATE';
  END IF;
END $$;
"""

    def refused(statement, label):
        return f"""
DO $$ DECLARE rejected boolean := false; BEGIN
  BEGIN
    {statement}
  EXCEPTION WHEN check_violation THEN rejected := true;
  END;
  IF NOT rejected THEN RAISE EXCEPTION '{label}'; END IF;
END $$;
"""

    post_bad = f"UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='{bad}';"
    update_posted = f"UPDATE public.commerce_ledger_entry SET amount_minor=2 WHERE transaction_id='{posted}' AND amount_minor=1;"
    # Baselines ensure the privileges suffice to reach the real guards.
    setup += refused(post_bad, 'ordinary unbalanced posting accepted')
    setup += refused(update_posted, 'ordinary posted mutation accepted')
    setup += f"""
CREATE TEMP TABLE commerce_ledger_entry(transaction_id uuid, currency text, amount_minor bigint);
INSERT INTO pg_temp.commerce_ledger_entry VALUES ('{bad}', 'USD', 1), ('{bad}', 'USD', -1),
  ('{empty}', 'USD', 1), ('{empty}', 'USD', -1);
"""
    setup += refused(post_bad, 'temp entry shadow accepted unbalanced posting')
    setup += refused(f"UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='{empty}';",
                     'temp entry shadow accepted empty posting')
    # Valid real entries remain usable even when the shadow contains none.
    setup += f"UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='{valid}';\n"
    setup += f"""
CREATE TEMP TABLE commerce_ledger_transaction(id uuid, status text, created_at timestamptz);
INSERT INTO pg_temp.commerce_ledger_transaction VALUES ('{posted}', 'draft', now());
"""
    for operation, statement in [
        ('update', update_posted),
        ('insert', f"INSERT INTO public.commerce_ledger_entry(transaction_id,account_code,currency,amount_minor) VALUES ('{posted}','synthetic.extra','USD',1);"),
        ('delete', f"DELETE FROM public.commerce_ledger_entry WHERE transaction_id='{posted}';"),
    ]:
        setup += refused(statement, f'temp parent shadow accepted posted {operation}')
    # A caller-controlled permanent schema ahead of public must not change
    # lookup either. The synthetic role owns only this isolated fixture schema.
    setup += f"""
RESET ROLE;
CREATE SCHEMA {role} AUTHORIZATION {role};
SET LOCAL ROLE {role};
CREATE TABLE {role}.commerce_ledger_entry (LIKE pg_temp.commerce_ledger_entry);
INSERT INTO {role}.commerce_ledger_entry SELECT * FROM pg_temp.commerce_ledger_entry;
CREATE TABLE {role}.commerce_ledger_transaction (LIKE pg_temp.commerce_ledger_transaction);
INSERT INTO {role}.commerce_ledger_transaction SELECT * FROM pg_temp.commerce_ledger_transaction;
DROP TABLE pg_temp.commerce_ledger_entry, pg_temp.commerce_ledger_transaction;
SET LOCAL search_path = {role}, public;
"""
    setup += refused(post_bad, 'caller schema accepted unbalanced posting')
    setup += refused(update_posted, 'caller schema accepted posted mutation')
    setup += f"""
RESET ROLE;
DO $$ BEGIN
  IF (SELECT status FROM public.commerce_ledger_transaction WHERE id='{bad}') <> 'draft'
     OR (SELECT status FROM public.commerce_ledger_transaction WHERE id='{empty}') <> 'draft'
     OR (SELECT status FROM public.commerce_ledger_transaction WHERE id='{valid}') <> 'posted'
     OR (SELECT sum(amount_minor::numeric) FROM public.commerce_ledger_entry WHERE transaction_id='{posted}') <> 0 THEN
    RAISE EXCEPTION 'Real ledger state changed across rejected namespace attacks';
  END IF;
END $$;
ROLLBACK;
"""
    return setup, role, prefix


parser = argparse.ArgumentParser()
parser.add_argument('--negative-controls', action='store_true')
args = parser.parse_args()
before = definitions()
controls = [(None, None)]
if args.negative_controls:
    controls += [
        ('commerce_validate_ledger_posting', 'temp entry shadow accepted unbalanced posting'),
        ('commerce_protect_posted_ledger', 'temp parent shadow accepted posted update'),
    ]
for function, expected in controls:
    statement, role, prefix = fixture(function)
    result = sql(statement)
    if function:
        require(result.returncode != 0 and expected in result.stderr, (function, result.stderr))
    else:
        require(result.returncode == 0, result.stderr)
    require(definitions() == before, 'Fixture did not restore exact function definitions')
    require(value(f"SELECT count(*) FROM pg_roles WHERE rolname='{role}'") == '0', 'Role leaked')
    require(value(f"SELECT count(*) FROM pg_namespace WHERE nspname='{role}'") == '0', 'Schema leaked')
    require(value(f"SELECT count(*) FROM public.commerce_ledger_transaction WHERE source_type='{prefix}'") == '0',
            'Ledger fixture leaked')
    print(json.dumps({'case': function or 'trusted-ledger-namespace',
                      'result': 'expected-counterexample' if function else 'passed',
                      'guardsRestoredExactly': True, 'fixtureRolledBack': True}), flush=True)
