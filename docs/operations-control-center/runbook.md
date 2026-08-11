# Deployment, monitoring, recovery, and rollback

## Pre-deployment

1. Back up PostgreSQL and confirm at least 2× the estimated operations-table growth is free.
2. Run the normal schema generator/migrations in a disposable clone, then apply `tdf-hq/sql/2026-08-09_admin_operations_control_center.sql` twice with `ON_ERROR_STOP=1`.
3. Confirm all seeded provider rows and `operations_organization.operations_enabled` are false.
4. Set organization/branch timezone, currency, hours, holidays, and membership. Configure `tdf.push_encryption_key` through the database/session secret mechanism if push is in the canary.
5. Build Haskell, web, and mobile from the regenerated OpenAPI types and generated mobile registry. Verify the production bundle contains no `operations` mock fallback.

## Backfill

```sh
TDF_OPERATIONS_DATABASE_URL='postgresql://…' scripts/operations-backfill.sh \
  --run-key operations-v1-production

TDF_OPERATIONS_DATABASE_URL='postgresql://…' scripts/operations-backfill.sh \
  --apply --run-key operations-v1-production --batch-size 500
```

The first command is dry-run. Eligibility includes only unresolved/actionable records; terminal history is excluded. Original business creation/update timestamps are preserved. `stock_item`, which has no source timestamp, records `timestampBasis=backfill_run`. A run can stop between batches and resume with the same key. Re-running cannot duplicate domain events or threads.

After each batch compare `operations_backfill_run.inserted_count`, domain-event count, outbox processed/dead-letter counts, and work correlation count. Drain outbox before feature enablement.

## Staged rollout

1. Enable the organization for Admin/Manager canaries; keep every provider disabled.
2. Verify KPI/inbox/detail, direct-ID denial, two-browser optimistic concurrency, and event replay after disconnect.
3. Add Accounting and Reception, then assigned Teachers/Engineers/Maintenance. Run cross-role source-link checks.
4. Activate one provider/configuration row at a time only after its official sandbox and credential checklist passes.
5. Expand organizations/branches only after tenant-isolation smoke tests.

## Health and metrics

The existing application health/readiness endpoints remain authoritative for process/database health. Operations readiness additionally requires `to_regprocedure('operations_process_outbox_batch(integer,text)')` and a successful read of organization config.

Recommended metrics/alerts:

| Signal | Warning | Critical/action |
| --- | --- | --- |
| oldest pending outbox age | >30 s | >120 s; page on-call, inspect locks/dead letter |
| dead-letter increase | any in 15 min | >10 in 15 min or financial/security event; page |
| outbox failed rate | >1%/15 min | >5%/5 min |
| p95 projection latency | >2 s | >3 s |
| integration retry queue | >25/provider | >100/provider or oldest >30 min |
| SLA breached active work | any urgent | growth >10% hour-over-hour |
| stream replay connections/errors | error >1% | reconnect storm >20/min/user |
| outbound delivery failure | >2%/provider | >10%/5 min; disable provider only |
| worker heartbeat/log silence | >30 s | >120 s |

Useful SQL:

```sql
SELECT count(*) AS pending, max(now() - created_at) AS oldest
FROM operations_outbox WHERE status IN ('pending','processing','failed');

SELECT provider, status, count(*), max(now() - created_at) AS oldest
FROM operations_integration_failure
WHERE status <> 'resolved' GROUP BY provider, status;

SELECT organization_id, priority, count(*)
FROM operations_work_item
WHERE sla_breached_at IS NOT NULL AND status NOT IN ('resolved','archived')
GROUP BY organization_id, priority;
```

Logs are structured with component and aggregate counts. Never add raw event/delivery payloads. Correlate by request ID, work-item ID, domain-event ID, provider event ID digest, and outbox ID. Forward application errors to the repository's configured error tracker and traces; redact PII/token/card/tax fields at ingestion.

## Partially processed outbox recovery

1. Disable only the affected provider if external; do not disable core capture.
2. Find stale `processing` leases and verify no worker currently owns them.
3. Move only the exact stale records to `failed` with `next_attempt_at=now()` in a reviewed transaction, retaining attempt/error fields.
4. Let `operations_process_outbox_batch` replay. Uniqueness on event/thread and per-aggregate order prevent duplicate visible effects.
5. Reconcile domain event → outbox → thread event counts and append an operator audit/reason for any manual replay.

## Rollback and forward-fix

Preferred rollback is feature/provider disablement. For application rollback, deploy the previous binaries and apply `tdf-hq/sql/2026-08-09_admin_operations_control_center_rollback.sql`. It disables organizations/providers and removes capture triggers, but deliberately retains all tables, work, delivery attempts, approvals, and immutable audit evidence. Do not drop operations tables.

To forward-fix, deploy corrected binaries, reapply the additive main migration (which recreates capture idempotently), reconcile events committed during the disabled interval with a new dry-run/backfill key, then re-enable the organization. A provider can remain disabled throughout.

## Smoke tests

- Create a course registration and verify one event/thread within three seconds.
- Replay its provider/business event and verify one thread.
- Open with Reception and deny with an unrelated/out-of-scope party.
- Assign in one browser and confirm stale mutation returns `409` in another.
- Put work into external Waiting and verify SLA pause; internal Waiting must continue.
- Disconnect/reconnect web updates and verify `afterId` fills the gap once.
- Create/reject a dual approval with two parties; self-decision must fail.
- Run the same backfill key again and verify zero new threads.
- Disable one provider and verify the core inbox remains functional.
