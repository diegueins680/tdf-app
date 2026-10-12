import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { randomInt, randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';
import { buildSchemaVerificationSql } from '../lib/production-release.mjs';

const db = process.env.TDF_CHECKOUT_AMOUNT_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const migration = readFileSync(new URL('../../tdf-hq/sql/2026-10-04_checkout_amount_correspondence.sql', import.meta.url), 'utf8');
const body = migration.replace(/^BEGIN;$/m, '').replace(/^COMMIT;$/m, '');
const fixedMigration = readFileSync(new URL('../../tdf-hq/sql/2026-10-12_checkout_line_snapshot_fixed.sql', import.meta.url), 'utf8');
const fixedBody = fixedMigration.replace(/^BEGIN;$/m, '').replace(/^COMMIT;$/m, '');
const dropFixed = 'DROP TRIGGER trg_commerce_checkout_line_fixed ON commerce_checkout_line_item;';
const args = [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'];
const run = sql => spawnSync('psql', args, { input: sql, encoding: 'utf8' });
const pass = (sql, label) => { const r = run(sql); assert.equal(r.status, 0, `${label}: ${r.stderr}`); return r.stdout.trim(); };
const reject = (sql, pattern, label) => { const r = run(sql); assert.notEqual(r.status, 0, label); assert.match(r.stderr, pattern, label); };
const header = (id, total = '100', fee = '0') => `INSERT INTO commerce_checkout_session
  (id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,fee_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)
  VALUES('${id}','synthetic_amount','${id}','holding','sandbox','USD',${total}::bigint-${fee}::bigint,${fee},${total},'synthetic@example.test','${id}','${id}',now()+interval '1 hour');`;
const line = (id, amount = '100', number = 1) => `INSERT INTO commerce_checkout_line_item
  (checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot)
  VALUES('${id}',${number},'synthetic','synthetic','1','Synthetic amount fixture',1,${amount},${amount},${amount},'{}');`;
const drop = `DROP TRIGGER IF EXISTS trg_commerce_checkout_total ON commerce_checkout_session;
 DROP TRIGGER IF EXISTS trg_commerce_checkout_line_total ON commerce_checkout_line_item;
 DROP TRIGGER IF EXISTS trg_commerce_checkout_money_immutable ON commerce_checkout_session;
 DROP TRIGGER IF EXISTS trg_commerce_checkout_line_fixed ON commerce_checkout_line_item;`;

// All DDL controls and deliberately invalid historical data roll back. This also
// tests the migration when the full manifest has already installed its triggers.
const oldBad = randomUUID();
reject(`BEGIN; ${drop} ${header(oldBad)} ${body} COMMIT;`, /preflight failed/, 'historical missing lines fail closed');
assert.equal(pass(`SELECT count(*) FROM commerce_checkout_session WHERE id='${oldBad}';`, 'preflight rollback'), '0');
const noPreflight = body.replace(/DO \$preflight\$[\s\S]*?\$preflight\$;/, '');
assert.notEqual(noPreflight, body);
pass(`BEGIN; ${drop} ${header(oldBad)} ${noPreflight} SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: omitted preflight admits old incomplete snapshot');
pass(`BEGIN; ${drop} ${body} ${fixedBody} COMMIT;`, 'additive migrations on valid prior state');
const schemaCheck = buildSchemaVerificationSql({ includePsqlHeader: false });
pass(schemaCheck, 'full production schema gate accepts intended schema');
reject(`BEGIN; ALTER TABLE merch_order DROP CONSTRAINT merch_order_commission_exact; ${schemaCheck} COMMIT;`,
  /Exact merchandise commission constraint is missing or changed/, 'commission constraint drift fails closed');
for (const [table, trigger] of [
  ['commerce_checkout_session', 'trg_commerce_checkout_total'],
  ['commerce_checkout_line_item', 'trg_commerce_checkout_line_total'],
  ['commerce_checkout_session', 'trg_commerce_checkout_money_immutable'],
  ['commerce_checkout_line_item', 'trg_commerce_checkout_line_fixed'],
  ['commerce_checkout_session', 'trg_commerce_checkout_created'],
]) {
  reject(`BEGIN; ALTER TABLE ${table} DISABLE TRIGGER ${trigger}; ${schemaCheck} COMMIT;`, /correspondence triggers are missing or disabled/, 'schema drift fails closed');
}
reject(`BEGIN; DROP TRIGGER trg_commerce_checkout_total ON commerce_checkout_session;
 CREATE CONSTRAINT TRIGGER trg_commerce_checkout_total AFTER INSERT ON commerce_checkout_session
 DEFERRABLE INITIALLY DEFERRED FOR EACH ROW WHEN (false) EXECUTE FUNCTION commerce_check_checkout_line_total();
 ${schemaCheck} COMMIT;`, /correspondence triggers are missing or disabled/, 'conditional trigger bypass rejected');
reject(`BEGIN; DROP TRIGGER trg_commerce_checkout_money_immutable ON commerce_checkout_session;
 CREATE TRIGGER trg_commerce_checkout_money_immutable BEFORE UPDATE OF updated_at ON commerce_checkout_session
 FOR EACH ROW EXECUTE FUNCTION commerce_protect_checkout_money();
 ${schemaCheck} COMMIT;`, /correspondence triggers are missing or disabled/, 'column-restricted trigger bypass rejected');

for (const [label, total, amounts] of [
  ['missing lines', '100', []],
  ['mismatch', '100', ['99']],
  ['fixed-width overflow', '1', ['9223372036854775807', '9223372036854775807', '3']],
]) {
  const id = randomUUID();
  reject(`BEGIN; ${header(id, total)} ${amounts.map((a, i) => line(id, a, i + 1)).join('\n')} COMMIT;`, /does not match its nonempty line snapshot/, label);
  assert.equal(pass(`SELECT count(*) FROM commerce_checkout_session WHERE id='${id}';`, label), '0', `${label} rolls back header`);
}
const valid = randomUUID(), shipping = randomUUID(), maximum = randomUUID();
pass(`BEGIN; ${header(valid)} ${line(valid)} COMMIT;`, 'valid canonical checkout');
pass(`BEGIN; ${header(shipping, '125', '25')} ${line(shipping)} ${line(shipping, '25', 2)} COMMIT;`, 'valid merchandise shipping');
pass(`BEGIN; ${header(maximum, '9223372036854775807')} ${line(maximum, '9223372036854775807')} COMMIT;`, 'exact Int64 maximum');
pass(`BEGIN; UPDATE commerce_checkout_session SET status='awaiting_payment',updated_at=now() WHERE id='${valid}'; COMMIT;`, 'ordinary lifecycle remains legal');
reject(`BEGIN; UPDATE commerce_checkout_session SET currency='EUR' WHERE id='${valid}'; COMMIT;`, /monetary terms are immutable/, 'currency binding immutable');
reject(`BEGIN; UPDATE commerce_checkout_session SET subtotal_minor=101,total_minor=101 WHERE id='${valid}'; COMMIT;`, /monetary terms are immutable/, 'amount snapshot immutable');
reject(`BEGIN; ${line(valid, '1', 2)} COMMIT;`, /lines are created with their checkout/, 'positive append rejected before the sum check');
reject(`BEGIN; ${dropFixed} ${line(valid, '1', 2)} COMMIT;`, /does not match/, 'positive append rejected by the sum check alone');
for (const command of [`UPDATE commerce_checkout_line_item SET total_minor=101,subtotal_minor=101,unit_amount_minor=101 WHERE checkout_id='${valid}';`, `DELETE FROM commerce_checkout_line_item WHERE checkout_id='${valid}';`]) {
  reject(`BEGIN; ${command} COMMIT;`, /immutable/, 'existing line immutability retained');
}

// Each control must admit the exact invalid operation that the intended schema
// rejects. Restore original triggers automatically through transaction rollback.
const control = randomUUID();
pass(`BEGIN; DROP TRIGGER trg_commerce_checkout_total ON commerce_checkout_session; ${header(control)} SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: missing parent trigger');
pass(`BEGIN; ${dropFixed} DROP TRIGGER trg_commerce_checkout_line_total ON commerce_checkout_line_item; ${line(valid, '1', 2)} SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: missing append triggers');
pass(`BEGIN; DROP TRIGGER trg_commerce_checkout_money_immutable ON commerce_checkout_session; UPDATE commerce_checkout_session SET currency='EUR' WHERE id='${valid}'; SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: mutable currency');

const concurrent = sql => new Promise((resolve, rejectPromise) => {
  const child = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  let stderr = '';
  child.stderr.on('data', data => { stderr += data; });
  child.stdout.resume();
  child.on('error', rejectPromise);
  child.on('close', status => resolve({ status, stderr }));
  child.stdin.end(sql);
});
// The 2026-10-04 sum check alone must still refuse racing positive appends:
// the six transactions below run in a window without the line snapshot rule.
pass(`BEGIN; ${dropFixed} COMMIT;`, 'window without the line snapshot rule');
for (const isolation of ['READ COMMITTED', 'REPEATABLE READ', 'SERIALIZABLE']) {
  const gate = randomInt(1, 2147483647), application = `checkout_amount_${randomUUID()}`;
  const holder = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  const ready = new Promise((resolve, rejectPromise) => {
    let output = '';
    holder.stdout.on('data', data => { output += data; if (output.includes('gate_ready')) resolve(); });
    holder.on('error', rejectPromise);
    holder.on('close', status => rejectPromise(new Error(`gate holder exited: ${status}`)));
  });
  holder.stderr.resume();
  holder.stdin.write(`SELECT pg_advisory_lock(20261004,${gate}); SELECT 'gate_ready';\n`);
  await ready;
  const attempts = [2, 3].map(n => concurrent(`SET application_name='${application}'; BEGIN ISOLATION LEVEL ${isolation};
    SET LOCAL statement_timeout='15s'; SELECT total_minor FROM commerce_checkout_session WHERE id='${valid}';
    ${line(valid, '1', n)} SELECT pg_advisory_xact_lock_shared(20261004,${gate}); COMMIT;`));
  let observed = false;
  try {
    for (let poll = 0; poll < 100; poll++) {
      observed = pass(`SELECT count(*) FROM pg_stat_activity WHERE datname=current_database()
        AND application_name='${application}' AND wait_event_type='Lock' AND wait_event='advisory';`, 'race barrier') === '2';
      if (observed) break;
      await new Promise(resolve => setTimeout(resolve, 50));
    }
  } finally {
    holder.stdin.end(`SELECT pg_advisory_unlock(20261004,${gate});\n`);
  }
  const results = await Promise.all(attempts);
  assert.ok(observed, `${isolation}: both appends must reach the commit barrier concurrently`);
  for (const r of results) { assert.notEqual(r.status, 0); assert.match(r.stderr, /does not match|could not serialize/); }
  assert.equal(pass(`SELECT sum(total_minor) FROM commerce_checkout_line_item WHERE checkout_id='${valid}';`, isolation), '100');
}
pass(`BEGIN; ${fixedBody} COMMIT;`, 'restore the line snapshot rule');
// Reproduce a pre-enforcement transaction retaining an incomplete RR snapshot.
// Another transaction completes the old snapshot before migration preflight.
// Only the new visibility fence distinguishes that old view from current truth.
const withoutFence = body.replace(/  IF NOT EXISTS \(SELECT 1 FROM commerce_checkout_amount_boundary WHERE singleton\) THEN[\s\S]*?  END IF;/, '');
assert.notEqual(withoutFence, body);
for (const broken of [false, true]) {
  const old = randomUUID();
  pass(`BEGIN; ${drop} DROP TABLE commerce_checkout_amount_boundary;
    ${header(old)} ${line(old, '50')} COMMIT;`, 'synthetic pre-enforcement state');
  const child = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  let output = '', stderr = '';
  child.stderr.on('data', data => { stderr += data; });
  const done = new Promise((resolve, rejectPromise) => {
    child.on('error', rejectPromise); child.on('close', status => resolve({ status, stderr }));
  });
  const ready = new Promise((resolve, rejectPromise) => {
    child.stdout.on('data', data => { output += data; if (output.includes('old_snapshot_ready')) resolve(); });
    child.on('error', rejectPromise);
    child.on('close', () => rejectPromise(new Error('old-snapshot transaction exited before barrier')));
  });
  child.stdin.write(`BEGIN ISOLATION LEVEL REPEATABLE READ;
    SET LOCAL statement_timeout='15s'; SELECT sum(total_minor) FROM commerce_checkout_line_item WHERE checkout_id='${old}';
    SELECT 'old_snapshot_ready';\n`);
  try {
    await ready;
    pass(line(old, '50', 2), 'old writer completes snapshot before preflight');
    pass(`BEGIN; ${broken ? withoutFence : body} COMMIT;`, 'preflight sees complete current state');
    child.stdin.end(`${line(old, '50', 3)} ${broken ? 'SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;' : 'COMMIT;'}\n`);
    const result = await done;
    if (broken) assert.equal(result.status, 0, `negative control must admit old snapshot: ${result.stderr}`);
    else { assert.notEqual(result.status, 0); assert.match(result.stderr, /snapshot predates monetary enforcement/); }
  } finally {
    if (!child.stdin.writableEnded) child.stdin.end('ROLLBACK;\n');
    await done;
  }
  assert.equal(pass(`SELECT sum(total_minor) FROM commerce_checkout_line_item WHERE checkout_id='${old}';`, 'stale snapshot rollback'), '100');
  pass(`BEGIN; ${drop} ${body} ${fixedBody} COMMIT;`, 'restore intended constraints after control');
}

// Lines are created only in the transaction that creates their checkout
// (2026-10-12): the sum check alone admitted zero-total appends to a
// committed checkout. The header insert records the checkout id in a
// transaction-local setting; a line is accepted only for a recorded id.
const fixedCheckout = randomUUID(), unknownCheckout = randomUUID(), rolledBack = randomUUID(), reinserted = randomUUID();
pass(`BEGIN; SELECT count(*) FROM commerce_checkout_session; ${header(fixedCheckout, '125', '25')}
  UPDATE commerce_checkout_session SET updated_at=now() WHERE id='${fixedCheckout}'; ${line(fixedCheckout)}
  SAVEPOINT shipping; ${line(fixedCheckout, '25', 2)} RELEASE SAVEPOINT shipping; COMMIT;`, 'header and lines in one transaction, with a read first, a self-update and a subtransaction');
pass(`BEGIN; SAVEPOINT retry; ${header(reinserted)} ROLLBACK TO SAVEPOINT retry; ${header(reinserted)} ${line(reinserted)} COMMIT;`, 'header re-inserted after a subtransaction rollback');
reject(`BEGIN; ${line(fixedCheckout, '0', 3)} COMMIT;`, /lines are created with their checkout/, 'zero-total append rejected');
reject(line(fixedCheckout, '0', 3), /lines are created with their checkout/, 'zero-total append rejected as the first statement of a transaction');
reject(`BEGIN; UPDATE commerce_checkout_session SET updated_at=now() WHERE id='${fixedCheckout}'; ${line(fixedCheckout, '0', 3)} COMMIT;`, /lines are created with their checkout/, 'zero-total append rejected after a header update in the same transaction');
reject(`BEGIN ISOLATION LEVEL REPEATABLE READ; ${line(fixedCheckout, '0', 3)} COMMIT;`, /lines are created with their checkout/, 'zero-total append rejected under repeatable read');
reject(`BEGIN; ${line(unknownCheckout, '0', 1)} COMMIT;`, /lines are created with their checkout/, 'line for an unknown checkout rejected');
reject(`BEGIN; SAVEPOINT h; ${header(rolledBack)} ROLLBACK TO SAVEPOINT h; ${line(rolledBack)} COMMIT;`, /lines are created with their checkout/, 'header rolled back to a subtransaction is not a created checkout');
assert.equal(pass(`SELECT count(*) FROM commerce_checkout_line_item WHERE checkout_id='${fixedCheckout}';`, 'fixed snapshot'), '2');
// ON CONFLICT on an existing header inserts nothing and registers nothing;
// the same statement on a new id is an ordinary creation. A header and its
// line in one statement (a data-modifying CTE) is refused: registration is an
// AFTER ROW trigger, run at the end of the statement. No writer uses it.
const upsert = (id, action) => header(id).replace(/;\s*$/, ` ON CONFLICT (id) DO ${action};`);
const upserted = randomUUID();
reject(`BEGIN; ${upsert(fixedCheckout, 'NOTHING')} ${line(fixedCheckout, '0', 3)} COMMIT;`, /lines are created with their checkout/, 'append after an upsert that inserted nothing rejected');
reject(`BEGIN; ${upsert(fixedCheckout, 'UPDATE SET updated_at=now()')} ${line(fixedCheckout, '0', 3)} COMMIT;`, /lines are created with their checkout/, 'append after an upsert that updated the header rejected');
pass(`BEGIN; ${upsert(upserted, 'NOTHING')} ${line(upserted)} COMMIT;`, 'upsert of a new header with its line commits');
reject(`BEGIN; WITH created AS (${header(randomUUID()).replace(/;\s*$/, ' RETURNING id')})
  INSERT INTO commerce_checkout_line_item (checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot)
  SELECT id,1,'synthetic','synthetic','1','Synthetic amount fixture',1,100,100,100,'{}' FROM created; COMMIT;`, /lines are created with their checkout/, 'header and line in one statement refused (documented)');
assert.equal(pass(`SELECT count(*) FROM commerce_checkout_line_item WHERE checkout_id IN ('${fixedCheckout}','${upserted}');`, 'snapshots after upserts'), '3');
assert.equal(pass(`SELECT coalesce(current_setting('tdf.commerce_checkouts_created', true), '');`, 'setting is transaction-local'), '');
pass(`BEGIN; ${dropFixed} ${line(fixedCheckout, '0', 3)} SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: the sum check alone admits a zero-total append');
pass(`BEGIN; ${dropFixed} UPDATE commerce_checkout_session SET updated_at=now() WHERE id='${fixedCheckout}'; ${line(fixedCheckout, '0', 3)} SET CONSTRAINTS ALL IMMEDIATE; ROLLBACK;`, 'negative control: the sum check alone admits a zero-total append after a header update');
const session = () => {
  const child = spawn('psql', args, { stdio: ['pipe', 'pipe', 'pipe'] });
  let output = '', errors = '';
  child.stderr.on('data', data => { errors += data; });
  const closed = new Promise((resolve, rejectPromise) => { child.on('error', rejectPromise); child.on('close', resolve); });
  const until = marker => new Promise(resolve => { child.stdout.on('data', data => { output += data; if (output.includes(marker)) resolve(); }); });
  return { child, closed, until, errors: () => errors };
};
{
  // Another session's uncommitted header is not a checkout this transaction created;
  // that session still commits with its own line.
  const pendingCheckout = randomUUID();
  const holder = session();
  const ready = holder.until('header_ready');
  holder.child.stdin.write(`BEGIN; ${header(pendingCheckout)} SELECT 'header_ready';\n`);
  await ready;
  reject(`SET statement_timeout='15s'; ${line(pendingCheckout, '0', 2)}`, /lines are created with their checkout/, "line for another session's uncommitted header rejected");
  holder.child.stdin.end(`${line(pendingCheckout)} COMMIT;\n`);
  assert.equal(await holder.closed, 0, `creating transaction still commits: ${holder.errors()}`);
  assert.equal(pass(`SELECT count(*) FROM commerce_checkout_line_item WHERE checkout_id='${pendingCheckout}';`, 'pending checkout committed with its own line'), '1');
}
{
  // A session that began, and wrote, before the checkout was created and
  // committed cannot append to it either (transaction order is irrelevant).
  const earlier = randomUUID(), later = randomUUID();
  const holder = session();
  const ready = holder.until('earlier_ready');
  holder.child.stdin.write(`BEGIN; ${header(earlier)} ${line(earlier)} SELECT 'earlier_ready';\n`);
  await ready;
  pass(`BEGIN; ${header(later)} ${line(later)} COMMIT;`, 'checkout created and committed after the earlier session began');
  holder.child.stdin.end(`SET LOCAL statement_timeout='15s'; ${line(later, '0', 2)} COMMIT;\n`);
  assert.notEqual(await holder.closed, 0, 'earlier session must not append to the later checkout');
  assert.match(holder.errors(), /lines are created with their checkout/);
  assert.equal(pass(`SELECT count(*) FROM commerce_checkout_line_item WHERE checkout_id='${later}';`, 'later checkout keeps its snapshot'), '1');
  assert.equal(pass(`SELECT count(*) FROM commerce_checkout_session WHERE id='${earlier}';`, 'earlier session rolled back'), '0');
}
console.log('Checkout amount PostgreSQL: migration preflight/rollback, commit-time bounds, shipping compatibility, immutability, seven negative controls, structural drift rejection, six concurrent append transactions, pre-migration snapshot fence and the fixed line snapshot (header update, upsert, earlier session, subtransactions) passed.');
