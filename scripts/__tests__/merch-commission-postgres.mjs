import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const db = process.env.TDF_MERCH_RUNTIME_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const run = sql => spawnSync('psql', [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'], { input: sql, encoding: 'utf8' });
const cap = (1n << 63n) - 1n;
// Existing synthetic fixture supplies the required store/recipient/policy FKs.
// Each boundary insert and each deliberately invalid constraint rolls back.
function insert(subtotal, bps, commission) {
  const id = randomUUID();
  return `INSERT INTO merch_order SELECT (jsonb_populate_record(NULL::merch_order,
    (SELECT to_jsonb(o) FROM merch_order o WHERE id='98000000-0000-4000-8000-000000000004')
    || jsonb_build_object('id','${id}','order_number','TDF-MERCH-MAXMONEY01',
      'checkout_id',NULL,'cart_id',NULL,'lookup_token_hash','${id}',
      'create_idempotency_key','${id}','product_subtotal_minor',${subtotal}::bigint,
      'discount_minor',0,'tax_minor',0,'shipping_minor',0,'processor_fee_minor',0,
      'tdf_commission_bps',${bps},'tdf_commission_minor',${commission}::bigint,
      'seller_net_minor',${subtotal - commission}::bigint,'total_minor',${subtotal}::bigint))).*;
    SELECT count(*) FROM merch_order WHERE id='${id}';`;
}
for (const subtotal of [1n,9999n,10001n,cap-1n,cap]) {
  for (const bps of [0n,1n,3333n,5000n,9999n,10000n]) {
    const commission = subtotal * bps / 10000n;
    const r = run(`BEGIN; ${insert(subtotal,bps,commission)} ROLLBACK;`);
    assert.equal(r.status,0,r.stderr);
    assert.equal(r.stdout.trim(),'1','actual storage must admit the exact oracle result');
  }
}
const valid = cap * 5000n / 10000n;
const wrong = run(`BEGIN; ${insert(cap,5000n,valid+1n)} COMMIT;`);
assert.notEqual(wrong.status,0);
assert.match(wrong.stderr,/merch_order_commission_exact/);
const missing = run(`BEGIN; ALTER TABLE merch_order DROP CONSTRAINT merch_order_commission_exact;
  ${insert(cap,5000n,valid+1n)} ROLLBACK;`);
assert.equal(missing.status,0,missing.stderr);
assert.equal(missing.stdout.trim(),'1','missing constraint must admit the invalid commission');
for (const [label, expression] of [
  ['fixed-width intermediate','((product_subtotal_minor-discount_minor)*tdf_commission_bps)/10000'],
  ['rounded numeric division','trunc((product_subtotal_minor::numeric-discount_minor::numeric)*tdf_commission_bps::numeric/10000)'],
]) {
  const r = run(`BEGIN; ALTER TABLE merch_order DROP CONSTRAINT merch_order_commission_exact;
    ALTER TABLE merch_order ADD CONSTRAINT synthetic_wrong_commission CHECK(tdf_commission_minor=${expression});
    ${insert(cap,5000n,valid)} ROLLBACK;`);
  assert.notEqual(r.status,0,label);
  assert.match(r.stderr,/bigint out of range|synthetic_wrong_commission/,label);
}
console.log('Merch commission PostgreSQL: 30 exact BigInt-oracle boundary inserts, incorrect-result denial, and three constraint negative controls passed.');
