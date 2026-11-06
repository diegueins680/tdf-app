// Posted source identities are replay/reconciliation anchors, not editable labels.
import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';
import { buildSchemaVerificationSql } from '../lib/production-release.mjs';

const database = process.env.TDF_LEDGER_POSTING_DATABASE_URL;
disposablePostgresUrl(database, { ci: process.env.CI === 'true' });
assert.ok(!process.env.PGHOSTADDR && !process.env.PGSERVICE && !process.env.PGSERVICEFILE,
  'Inherited libpq routing overrides are forbidden');
const run = sql => spawnSync('psql', ['-XqAt', '-v', 'ON_ERROR_STOP=1', '-d', database], {
  input: sql, encoding: 'utf8', timeout: 30000,
  env: { ...process.env, PGOPTIONS: '-c statement_timeout=15000 -c lock_timeout=10000' },
});
const pass = (sql, label) => {
  const result = run(sql);
  assert.equal(result.status, 0, `${label}: ${result.error ?? result.stderr}`);
  return result.stdout.trim();
};
const snapshot = () => pass(`SELECT jsonb_build_object(
  'function', pg_get_functiondef('public.commerce_protect_posted_ledger_header()'::regprocedure),
  'trigger', (SELECT pg_get_triggerdef(oid)||'|'||tgenabled::text FROM pg_trigger
    WHERE tgrelid='public.commerce_ledger_transaction'::regclass
      AND tgname='trg_commerce_ledger_header_immutable'),
  'receipts', (SELECT jsonb_agg(to_jsonb(m) ORDER BY migration_id) FROM public.tdf_schema_migration m));`, 'guard snapshot');

function fixture(status = 'posted') {
  const id = randomUUID();
  const reverser = randomUUID();
  const role = `ledger_identity_${id.replaceAll('-', '')}`;
  return `BEGIN;
    CREATE ROLE ${role} NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE;
    GRANT USAGE ON SCHEMA public TO ${role};
    GRANT SELECT, UPDATE ON public.commerce_ledger_transaction TO ${role};
    INSERT INTO public.commerce_ledger_transaction
      (id,transaction_type,source_type,source_id,status,effective_at,correlation_id,created_by)
      VALUES ('${id}','payment_capture','ledger_identity_probe','${id}','draft',now(),'${id}','synthetic');
    INSERT INTO public.commerce_ledger_entry
      (transaction_id,account_code,currency,amount_minor)
      VALUES ('${id}','cash.identity_probe','USD',100),('${id}','liability.identity_probe','USD',-100);
    ${status !== 'draft' ? `UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='${id}';` : ''}
    ${status === 'reversed' ? `
      INSERT INTO public.commerce_ledger_transaction
        (id,transaction_type,source_type,source_id,status,effective_at,correlation_id,created_by)
        VALUES ('${reverser}','payment_reversal','ledger_header_reverser','${reverser}','draft',now(),'${reverser}','synthetic');
      INSERT INTO public.commerce_ledger_entry(transaction_id,account_code,currency,amount_minor)
        VALUES ('${reverser}','cash.identity_probe','USD',-100),('${reverser}','liability.identity_probe','USD',100);
      UPDATE public.commerce_ledger_transaction SET status='posted' WHERE id='${reverser}';
      UPDATE public.commerce_ledger_transaction SET status='reversed',reversal_of='${reverser}' WHERE id='${id}';
    ` : ''}
    SET LOCAL ROLE ${role};
    SELECT CASE WHEN NOT (SELECT rolsuper FROM pg_roles WHERE rolname=current_user)
      AND NOT pg_has_role(current_user,(SELECT relowner FROM pg_class
        WHERE oid='public.commerce_ledger_transaction'::regclass),'USAGE')
      AND NOT has_schema_privilege(current_user,'public','CREATE')
      AND NOT has_table_privilege(current_user,'public.commerce_ledger_entry','UPDATE')
      THEN 'ordinary-header-writer' ELSE 'invalid-fixture-privileges' END;
  `;
}

for (const status of ['posted', 'reversed']) {
  for (const [column, value] of [
    ['transaction_type', 'relabelled_capture'],
    ['source_type', 'relabelled_source'],
    ['source_id', 'relabelled_event'],
    ['effective_at', '2000-01-01T00:00:00Z'],
    ['correlation_id', 'relabelled_correlation'],
    ['created_by', 'relabelled_actor'],
    ['created_at', '2000-01-01T00:00:00Z'],
    ['id', randomUUID()],
  ]) {
    const result = run(`${fixture(status)}
      UPDATE public.commerce_ledger_transaction SET ${column}='${value}'
        WHERE source_type='ledger_identity_probe' RETURNING status,transaction_type,source_type,source_id;
      ROLLBACK;`);
    assert.match(result.stdout, /ordinary-header-writer/, result.error ?? result.stderr);
    assert.notEqual(result.status, 0,
      `posted ledger header changed: ${status}/${column}: ${result.stdout}`);
    assert.match(result.stderr, /Posted ledger header is immutable/,
      `wrong rejection for ${status}/${column}: ${result.error ?? result.stderr}`);
  }
}

// Draft edits remain possible; a value-preserving posted UPDATE is also valid.
for (const status of ['draft', 'posted', 'reversed']) {
  const mutation = status === 'draft' ? "source_id='edited_before_post'" : 'source_id=source_id';
  const result = run(`${fixture(status)}
    UPDATE public.commerce_ledger_transaction SET ${mutation},created_at=created_at WHERE source_type='ledger_identity_probe';
    ROLLBACK;`);
  assert.equal(result.status, 0, result.error ?? result.stderr);
  assert.match(result.stdout, /ordinary-header-writer/);
}
console.log('ledger-event-identity: passed (16 identity/source/audit mutations rejected; draft edits and identical posted/reversed updates permitted)');

const before = snapshot();
const verification = buildSchemaVerificationSql({ includePsqlHeader: false });
pass(verification, 'full-schema startup baseline');
for (const mutation of [
  'ALTER TABLE public.commerce_ledger_transaction DISABLE TRIGGER trg_commerce_ledger_header_immutable;',
  'DROP TRIGGER trg_commerce_ledger_header_immutable ON public.commerce_ledger_transaction;',
  'ALTER FUNCTION public.commerce_protect_posted_ledger_header() SECURITY DEFINER;',
  'ALTER FUNCTION public.commerce_protect_posted_ledger_header() RESET search_path;',
]) {
  const result = run(`BEGIN; ${mutation}\n${verification}\nROLLBACK;`);
  assert.notEqual(result.status, 0, `startup accepted ${mutation}`);
  assert.match(result.stderr, /Posted ledger header immutability guard is missing or changed/,
    result.error ?? result.stderr);
  assert.equal(snapshot(), before, 'startup mutation did not restore exactly');
}

if (process.argv.includes('--negative-controls')) {
  // Disable only this new guard in the rollback-scoped owner setup. The same
  // ordinary writer must reproduce the original header-identity violation.
  const setup = fixture().replace('SET LOCAL ROLE',
    'ALTER TABLE public.commerce_ledger_transaction DISABLE TRIGGER trg_commerce_ledger_header_immutable; SET LOCAL ROLE');
  const result = run(`${setup}
    UPDATE public.commerce_ledger_transaction SET source_id='control_changed_event'
      WHERE source_type='ledger_identity_probe' RETURNING status,source_id;
    ROLLBACK;`);
  assert.equal(result.status, 0, result.error ?? result.stderr);
  assert.match(result.stdout, /ordinary-header-writer/);
  assert.match(result.stdout, /posted\|control_changed_event/);
  assert.equal(snapshot(), before, 'enforcement control did not restore exactly');
}
console.log('ledger-event-identity: startup rejects 4 guard mutations; exact definitions and receipts restored');
