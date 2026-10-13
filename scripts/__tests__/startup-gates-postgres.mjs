// DEPLOY-STARTUP-001 at the PostgreSQL boundary: the capability-gate policy of
// the startup schema verification (buildSchemaVerificationSql), run through
// psql against the migrated fixture for every asserted production flag in each
// of its three states (missing, disabled, enabled). The policy is a conjunction
// of per-flag rules, so every flag is varied alone over an approved baseline;
// the approved baseline itself and the two joint approved extremes (every
// either-state gate enabled, every one disabled) are run as well.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';
import { buildSchemaVerificationSql } from '../lib/production-release.mjs';

const db = process.env.TDF_STARTUP_GATES_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const args = [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'];
const run = sql => spawnSync('psql', args, { input: sql, encoding: 'utf8' });
const pass = (sql, label) => { const r = run(sql); assert.equal(r.status, 0, `${label}: ${r.stderr}`); return r.stdout.trim(); };

// The policy, as the requirement states it (AUTHORITY-050 for the provider gates).
const policy = {
  'checkout.bank_transfer': { allowed: ['enabled'], message: /Manual settlement capability flags are missing or disabled/ },
  'checkout.cash': { allowed: ['enabled'], message: /Manual settlement capability flags are missing or disabled/ },
  'checkout.pos': { allowed: ['enabled'], message: /Manual settlement capability flags are missing or disabled/ },
  'commerce.marketplace_sales': { allowed: ['enabled'], message: /marketplace sales and rentals capability gates must exist enabled/ },
  'commerce.marketplace_rentals': { allowed: ['enabled'], message: /marketplace sales and rentals capability gates must exist enabled/ },
  'commerce.service_bookings': { allowed: ['disabled'], message: /service booking capability gate must exist disabled/ },
  'commerce.courses': { allowed: ['disabled'], message: /course checkout and recurring billing gates must exist disabled/ },
  'commerce.course_recurring_billing': { allowed: ['disabled'], message: /course checkout and recurring billing gates must exist disabled/ },
  'checkout.paypal.webhooks': { allowed: ['enabled', 'disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
  'checkout.paypal.refunds': { allowed: ['enabled', 'disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
  'checkout.datafast.webhooks': { allowed: ['enabled', 'disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
  'checkout.datafast.refunds': { allowed: ['enabled', 'disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
  'checkout.placetopay.webhooks': { allowed: ['disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
  'checkout.payphone.notifications': { allowed: ['disabled'], message: /provider capability gates must exist; PlaceToPay and Payphone must remain staged/ },
};
const flags = Object.keys(policy);
const states = ['missing', 'disabled', 'enabled'];

const dir = mkdtempSync(join(tmpdir(), 'startup-gates-'));
const verification = join(dir, 'verify.sql');
writeFileSync(verification, buildSchemaVerificationSql({ includePsqlHeader: true }));
const verify = () => spawnSync('psql', [db, '-X', '-v', 'ON_ERROR_STOP=1', '-f', verification], { encoding: 'utf8' });

const snapshot = JSON.parse(pass(`SELECT coalesce(json_agg(json_build_object('flag_key', flag_key, 'enabled', enabled, 'reason', reason, 'updated_by', updated_by, 'updated_at', updated_at)), '[]')
  FROM revenue_feature_flag WHERE environment='production' AND flag_key IN (${flags.map(f => `'${f}'`).join(',')});`, 'snapshot'));
const set = (flag, state) => pass(state === 'missing'
  ? `DELETE FROM revenue_feature_flag WHERE environment='production' AND flag_key='${flag}';`
  : `INSERT INTO revenue_feature_flag (flag_key, enabled, environment, reason) VALUES ('${flag}', ${state === 'enabled'}, 'production', 'startup gate test')
     ON CONFLICT (flag_key, environment) DO UPDATE SET enabled = EXCLUDED.enabled;`, `set ${flag} ${state}`);
const restore = () => {
  pass(`DELETE FROM revenue_feature_flag WHERE environment='production' AND flag_key IN (${flags.map(f => `'${f}'`).join(',')});`, 'restore: clear');
  for (const row of snapshot) {
    pass(`INSERT INTO revenue_feature_flag (flag_key, enabled, environment, reason, updated_by, updated_at) VALUES ('${row.flag_key}', ${row.enabled}, 'production', ${row.reason === null ? 'NULL' : `'${String(row.reason).replaceAll("'", "''")}'`}, ${row.updated_by ?? 'NULL'}, '${row.updated_at}');`, 'restore: row');
  }
};
const baseline = () => { for (const flag of flags) set(flag, policy[flag].allowed[0] === 'enabled' ? 'enabled' : 'disabled'); };

const results = [];
try {
  // The approved baseline and the two joint extremes of the either-state gates pass.
  baseline();
  assert.equal(verify().status, 0, 'approved baseline passes startup verification');
  for (const either of ['enabled', 'disabled']) {
    for (const flag of flags) if (policy[flag].allowed.length === 2) set(flag, either);
    const r = verify();
    assert.equal(r.status, 0, `every either-state provider gate ${either}: ${r.stderr}`);
  }
  baseline();

  // Each flag alone, in each state.
  for (const flag of flags) {
    for (const state of states) {
      set(flag, state);
      const r = verify();
      const allowed = policy[flag].allowed.includes(state);
      results.push([flag, state, allowed, r.status]);
      if (allowed) assert.equal(r.status, 0, `${flag} ${state} must be accepted: ${r.stderr}`);
      else {
        assert.notEqual(r.status, 0, `${flag} ${state} must be rejected`);
        assert.match(r.stderr, policy[flag].message, `${flag} ${state} fails with its fixed message`);
      }
      set(flag, policy[flag].allowed[0] === 'enabled' ? 'enabled' : 'disabled');
    }
  }
  // Verification never writes a gate.
  assert.equal(pass(`SELECT count(*) FROM revenue_feature_flag WHERE environment='production' AND reason='startup gate test' AND flag_key NOT IN (${flags.map(f => `'${f}'`).join(',')});`, 'no stray rows'), '0');
} finally {
  restore();
  rmSync(dir, { recursive: true, force: true });
}
assert.equal(results.length, flags.length * states.length);
console.log(`startup-gates-postgres: ok (${results.length} flag states, ${results.filter(r => r[2]).length} accepted, ${results.filter(r => !r[2]).length} rejected)`);
