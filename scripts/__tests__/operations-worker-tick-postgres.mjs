// OPS-WORKER-001 at the PostgreSQL boundary: the maintenance tick that
// TDF.Operations.Worker runs every second (outbox batch, SLA tick, archive in
// one transaction), mirrored statement by statement against the migrated
// fixture. A domain event for an entity that no longer exists is the poison:
// operations_work_item_entity_reference is a deferred constraint trigger, so
// without immediate checking its error surfaces at commit, outside the batch
// function's per-row handling, and the whole tick rolls back.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { randomInt, randomUUID } from 'node:crypto';
import { disposablePostgresUrl } from '../lib/disposable-postgres-url.mjs';

const db = process.env.TDF_OPERATIONS_WORKER_DATABASE_URL;
disposablePostgresUrl(db, { ci: process.env.CI === 'true' });
const args = [db, '-X', '-qAt', '-v', 'ON_ERROR_STOP=1'];
const run = sql => spawnSync('psql', args, { input: sql, encoding: 'utf8' });
const pass = (sql, label) => { const r = run(sql); assert.equal(r.status, 0, `${label}: ${r.stderr}`); return r.stdout.trim(); };
const reject = (sql, pattern, label) => { const r = run(sql); assert.notEqual(r.status, 0, label); assert.match(r.stderr, pattern, label); return r; };

const worker = 'tdf-hq-operations-worker';
// The three statements of operationsMaintenanceTick, in one transaction.
const tickBody = `SELECT 'batch:'||processed||','||failed||','||dead_lettered FROM operations_process_outbox_batch(250, '${worker}');
SELECT 'sla:'||reminders_created||','||breached_created FROM operations_tick_sla(now());
WITH archived AS (UPDATE operations_work_item SET status = 'archived', archived_at = now(), updated_at = now(), version = version + 1
  WHERE status = 'resolved' AND resolved_at < now() - interval '90 days' RETURNING id, organization_id, branch_id),
 stream AS (INSERT INTO operations_stream_event (organization_id, branch_id, event_type, work_item_id, payload)
  SELECT organization_id, branch_id, 'work_item.archived', id, jsonb_build_object('workItemId', id, 'reason', 'retention_90_days') FROM archived),
 audit AS (INSERT INTO operations_admin_audit (organization_id, branch_id, acting_role, source_client, action, target_entity_type, target_entity_id, new_value, request_id, correlation_id, reason)
  SELECT organization_id, branch_id, 'system', '${worker}', 'auto_archive', 'operations_work_item', id::text, jsonb_build_object('status', 'archived'), gen_random_uuid()::text, id::text, 'resolved for 90 days' FROM archived)
SELECT 'archived:'||count(*) FROM archived;`;
const previousTick = `BEGIN; ${tickBody} COMMIT;`;
const tick = `BEGIN; SET CONSTRAINTS ALL IMMEDIATE; ${tickBody} COMMIT;`;

const scope = pass(`SELECT organization_id||','||coalesce(branch_id::text, '') FROM operations_domain_event WHERE branch_id IS NOT NULL ORDER BY recorded_at LIMIT 1;`, 'scope').split(',');
const [org, branch] = [scope[0], scope[1] ? `'${scope[1]}'` : 'NULL'];
const liveParty = pass(`SELECT id FROM party ORDER BY id LIMIT 1;`, 'live party');
assert.match(liveParty, /^\d+$/, 'the fixture must hold a party');
// A fresh missing id per run: the batch keeps aggregate order, so a new event
// for an aggregate with a dead letter would wait behind it.
const missingParty = String(999000000000 + randomInt(1, 999999999));
const enqueue = (aggregateId, key) => pass(`INSERT INTO operations_domain_event(organization_id,branch_id,event_type,aggregate_type,aggregate_id,source_system,source_channel,correlation_key,deduplication_key,occurred_at,payload)
  VALUES ('${org}',${branch},'synthetic.source','party','${aggregateId}','synthetic','synthetic','${key}','${randomUUID()}',now(),
  jsonb_build_object('titleEs','Synthetic worker','titleEn','Synthetic worker','metadata','{}'::jsonb)) RETURNING id;`, `enqueue ${aggregateId}`);
const outbox = id => pass(`SELECT status||','||attempt_count||','||coalesce(last_error,'')||','||(next_attempt_at > now()) FROM operations_outbox WHERE event_id='${id}';`, `outbox ${id}`);
const workItems = key => pass(`SELECT count(*) FROM operations_work_item WHERE correlation_key='${key}';`, `items ${key}`);

let healthy = null, poison = null, healthyKey = null, poisonKey = null;
try {
  // Park every pre-existing pending outbox row so only the synthetic rows are in play.
  pass(`UPDATE operations_outbox SET next_attempt_at = now() + interval '1 day' WHERE status = 'pending';`, 'park existing rows');
  healthyKey = `synthetic:worker:${randomUUID()}`; poisonKey = `synthetic:worker:${randomUUID()}`;
  healthy = enqueue(liveParty, healthyKey); poison = enqueue(missingParty, poisonKey);
  assert.equal(outbox(healthy), 'pending,0,,false'); assert.equal(outbox(poison), 'pending,0,,false');

  // Negative control, the previous tick: the deferred trigger fails the commit,
  // nothing is processed, no attempt is recorded, and the next tick repeats it.
  reject(previousTick, new RegExp(`operations work item references missing party\\.${missingParty}`), 'previous tick fails as a whole');
  assert.equal(outbox(healthy), 'pending,0,,false', 'healthy row untouched by the failed tick');
  assert.equal(outbox(poison), 'pending,0,,false', 'poisoned row records no attempt');
  assert.equal(workItems(healthyKey), '0');

  // The tick with immediate constraint checking: the poisoned row fails alone with
  // its attempt, backoff and error code; the healthy row is projected; SLA and
  // archive run; everything commits.
  let out = pass(tick, 'tick');
  assert.match(out, /^batch:1,1,0\nsla:\d+,\d+\narchived:\d+$/);
  assert.equal(workItems(healthyKey), '1');
  assert.equal(outbox(healthy).split(',')[0], 'processed');
  assert.equal(outbox(poison), 'pending,1,23503: projection failed,true');
  assert.equal(pass(`SELECT count(*) FROM operations_work_item WHERE correlation_key='${poisonKey}';`, 'no item for the missing entity'), '0');

  // Backoff: the poisoned row is not retried before next_attempt_at (pinned
  // explicitly: the recorded backoff is 2 s plus jitter, too short to rely on).
  pass(`UPDATE operations_outbox SET next_attempt_at = now() + interval '1 hour' WHERE event_id='${poison}';`, 'pin backoff');
  out = pass(tick, 'tick during backoff');
  assert.match(out, /^batch:0,0,0\n/);
  assert.equal(outbox(poison), 'pending,1,23503: projection failed,true');

  // Seven recorded attempts later the eighth dead-letters it with a private
  // integration failure, and the tick keeps completing.
  pass(`UPDATE operations_outbox SET attempt_count = 7, next_attempt_at = now() WHERE event_id='${poison}';`, 'advance attempts');
  out = pass(tick, 'dead-letter tick');
  assert.match(out, /^batch:0,1,1\n/);
  assert.equal(outbox(poison), 'dead_letter,8,23503: projection failed,true');
  assert.equal(pass(`SELECT status||','||attempt_count||','||retryable||','||failure_code||','||redacted_summary FROM operations_integration_failure WHERE provider='internal_outbox' AND source_record_type='party' AND source_record_id='${missingParty}' ORDER BY created_at DESC LIMIT 1;`, 'failure row'),
    'dead_letter,8,true,23503,Projection failed; private diagnostics withheld', 'the dead letter keeps the SQLSTATE and a fixed summary, no entity detail');
  // The dead letter is itself a domain event; the next tick projects it into a
  // work item and never retries the dead-lettered row.
  out = pass(tick, 'tick after dead letter');
  assert.match(out, /^batch:1,0,0\n/, 'the integration failure event is projected');
  assert.equal(pass(`SELECT count(*) FROM operations_work_item WHERE entity_type='integration_failure' AND entity_id=(SELECT id::text FROM operations_integration_failure WHERE provider='internal_outbox' AND source_record_type='party' AND source_record_id='${missingParty}' ORDER BY created_at DESC LIMIT 1);`, 'failure item'), '1');
  assert.equal(outbox(poison), 'dead_letter,8,23503: projection failed,true', 'a dead letter is never retried');
  assert.equal(pass(`SELECT count(*) FROM operations_work_item WHERE correlation_key='${poisonKey}';`, 'still no item for the missing entity'), '0');
} finally {
  // Domain events and work-item history are append-only, so the synthetic rows
  // stay, all terminal (processed, dead-lettered, projected). The pre-existing
  // pending rows parked above are made due again.
  pass(`UPDATE operations_outbox SET next_attempt_at = now() WHERE status = 'pending' AND next_attempt_at > now() + interval '23 hours';`, 'cleanup');
}
console.log('operations-worker-tick-postgres: ok');
