# ADR 0120: Durable, fenced payment status-query recovery

Date: 2026-09-15 UTC. Status: implemented behind disabled gates in a dependent
draft after #358. This is not provider or production activation.

## Scope and decision

Recover a missing callback only for a PlaceToPay session or PayPhone sale whose
create operation has an immutable resource binding and ambiguous outcome.
Unknown-resource creates are excluded: neither elapsed time nor retry exhaustion
proves no charge. The worker only uses the existing adapter **query** operation;
it cannot create, capture, refund, void, pay out or change provider.

```text
Qualified account + exact environment flag + configured adapter + process switch
  → enqueue one durable job per known ambiguous create operation (unique FK)
  → lock one due job / expired lease (SKIP LOCKED)
  → reserve shared provider/environment query budget; commit new lease
  → authenticate and query the original resource outside the SQL transaction
  → lock current lease; check expiry AFTER lock acquisition; recheck authority
  → caller-owned transaction
      ├─ revalidate binding and apply query under ADR 0119 savepoint
      ├─ payment / intent / ledger / receipt / audit
      └─ finish job or record redacted review exception
    commit together, or roll back together
```

The job references `commerce_provider_operation.id`; it does not fabricate a
callback, signature, inbox payload or provider event. Audit correlation is
`provider-query-job:<operation UUID>`. Existing callback correlation and public
wire contracts remain unchanged. Canonical payment states remain the source
for existing web/mobile/admin consumers; job status is not a payment status.

## Gates, scheduling and quota

- `COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED` must be exactly `true`. Unset/other
  values start no worker. Enabling after boot requires a process restart; a
  running worker rereads the switch each tick and before financial application.
- The exact `checkout.provider_query_recovery` database flag must be enabled in
  the configured checkout environment, including sandbox. Migration seeds both
  environments **false**, preserving existing/operator-edited flags.
- The original merchant alias must still match an enabled account with ready
  status, approved contract, validated credentials and USD settlement. Runtime
  adapter configuration must also load successfully. These database claims are
  qualification prerequisites, not proof that a real sandbox passed.
- Set `COMMERCE_CHECKOUT_ENV` explicitly for rollout. The shared existing
  loader defaults to sandbox if absent and rejects unknown values. No environment
  is inferred from a callback. General checkout/new-charge gates stay separate
  from this explicitly authorized read-only recovery gate.
- Enqueue at most 100 eligible operations per provider/tick, with a 30-second
  initial delay. One job per provider is claimed per five-second worker loop.
  Two-minute UUID leases fence results; a superseded or expired response makes
  no financial or job-completion writes. Reclaiming preserves the same attempt.
- Callback and scheduled queries reserve from one PostgreSQL UPSERT budget per
  provider/environment: one reservation per ten seconds, across app replicas.
  Merchant-alias rotation cannot reset this key. Contention does not increment
  scheduled-job attempts. Reservations are conservative and are not refunded
  after transport failure or a terminal preflight result.
- Retry delay is 30 seconds doubled per claimed attempt, capped at one hour.
  At most 24 claims; an expired final lease is dead-lettered without querying.
  Pending, transport failure and configuration revocation are not no-charge
  evidence. Dead-lettering creates an open reconciliation exception, never a
  second payment attempt or automatic provider fallback.

The ten-second budget is an intentionally conservative engineering choice, not a
provider SLA or a configurable pricing/capability catalog. It bounds this
database's consumers, not another database or external integration sharing the
same credentials. Confirm quota enforcement scope and coordinate those consumers
before activation. Callback-inbox quota contention currently uses the inherited
retry policy; a heavily contended inbox may reach its retry cap. Monitor it along
with scheduled jobs; this patch does not promise fair scheduling or zero latency.

## Threat controls and limitations

The final transaction locks the lease before the financial savepoint and retains
shared account/flag locks through commit. A revocation that commits before this
check prevents application; one arriving after these locks waits for that
transaction. This is the explicit linearization boundary, not instantaneous
cancellation of an already authorized commit. The process switch is read just
before final SQL; it is not a cross-process transactional revocation mechanism.

Duplicate scheduling/replicas converge on one job and bounded query reservations.
Database time determines deadlines. SQL errors leave the prior committed lease
recoverable and roll back financial writes with job completion. Terminal jobs
and identity/attempt history cannot be rewritten/deleted through normal SQL;
privileged database owners still require operational access controls and backups.
No raw response or credential is stored by this worker. Error codes and log
messages are fixed and redacted; audit contains internal correlation and lease
IDs, attempt count and outcome code, not provider payloads.

A callback can complete the payment while the query is in flight. An identical
terminal result remains idempotent. A conflicting or stale pending result goes
to operator review without downgrading the terminal payment. An already terminal
operation found before querying closes the job without another remote request.
Late success on an expired checkout remains a review case; the worker cannot
reallocate released stock, seats or booking holds. Unknown IDs and terminal
dead-letter reprocessing need a separately reviewed operational flow. No direct
job mutation/retry HTTP endpoint or new admin UI is introduced here.

The inherited PayPhone status-2 canceled/declined naming mismatch described in
ADR 0119 is not repaired by this change. Existing failed intent history must
remain replay-compatible when that separate correction is implemented.

## Official evidence

Accessed 2026-09-15 UTC; high confidence in these documented contracts, no claim
of TDF merchant qualification or provider sandbox success:

- [PlaceToPay notifications](https://docs.placetopay.dev/en/checkout/notification/):
  session callbacks are not retried, motivating independent recovery.
- [PlaceToPay session query](https://docs.placetopay.dev/en/checkout/api/reference/session):
  authenticated status lookup against the stored session resource.
- [PayPhone API Sale](https://docs.payphone.app/api-sale): authenticated status
  lookup, documented 30-per-minute query limit; enforcement scope requires
  provider confirmation. This patch makes no claim about higher commercial quotas.
- [PostgreSQL 16 locking](https://www.postgresql.org/docs/16/explicit-locking.html):
  row locks and savepoint rollback semantics supporting the fencing design.

## Compatibility and migration

`2026-09-15_provider_query_recovery.sql` adds only two tables, indexes, a guard
trigger/function and false flag seeds. No financial backfill or historical
provider reference changes. The production manifest records the introducing
commit and appends the migration after provider execution. Apply schema before
new callback/worker binaries: callbacks now require the budget table even when
the scheduled worker is disabled. Missing schema fails closed, not unmetered.

Old callback binaries do not reserve the shared budget. Drain/replace old query
consumers before worker activation; a mixed deployment does not have a complete
quota guarantee. Keep all new gates disabled during this cutover and qualify
the exact deployed source and sandbox account afterward.

Disable the worker database flag and process switch before image rollback.
Retain tables, audit and references on a used installation. The provided rollback
locks the new tables and refuses when either contains rows. Empty rollback drops
only the new schema and deletes only untouched false flag seeds. Never delete
jobs or budget rows to force rollback; use a forward repair. Reverting to an old
callback binary also loses the shared budget guarantee.

See [verification and operational procedure](../payments/query-recovery-2026-09-15.md).
