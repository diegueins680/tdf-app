# ADR 0122: Read-only payment-query recovery observability

Date: 2026-09-15 UTC. Status: implemented; local runtime verification passed,
staging/provider qualification blocked.
Depends on ADRs 0120 and 0121 / root PRs #363 and #371. No deployment, provider
activation, charge, payout or retry authorization is granted by this ADR.

## Gap and decision

The durable recovery worker records job status, attempt count, leases, outcomes
and shared query budgets, but the existing admin screen exposes only provider
events and financial summaries. Operators cannot distinguish a missing recovery
schema from an empty queue, or inspect a missed-callback job without SQL access.
An inbox replay is not an equivalent replacement for a scheduled status query.

Add a strict-admin-only `GET /admin/commerce/provider-queries`, an additive
OpenAPI contract, matching generated web/mobile types, and a read-only panel in
the existing web payment-operations screen. Do not add a replay/reset action:
terminal query jobs and their attempt history remain immutable, and retrying a
status query must never be confused with authorizing another charge.

```text
Authenticated strict Admin
  → validated environment/status/page
  → read-only database transaction, bounded statements
  → explicit availability + redacted jobs/budgets
  → no-store response + environment-checked, refreshed admin panel
```

## Boundaries

- Reuse `hasStrictAdminAccess`; ordinary customers, fans, managers, webmasters,
  and an Admin combined with disallowed operational roles cannot read the report.
  Authorization precedes application filter validation and database access.
- Default to sandbox, never a combined environment. Accept only sandbox or
  production, known queue states, limit 1–100 (default 25), and offset 0–10,000.
  SQL filters are parameterized. Sort by creation time and operation UUID for a
  deterministic page order; fetch one extra row for `hasMore`, not an unbounded
  row count. Concurrent queue changes can shift offset pages: this is a live
  report, not a consistent financial export.
- Use `SET TRANSACTION READ ONLY` and a three-second **per-statement** timeout.
  No schema, flag, job, budget, lease, audit row or financial state is changed.
  A timeout/database error returns a fixed 503 response with no SQL or diagnostic
  text. Missing recovery tables return `schemaReady=false`, not empty-queue proof.
- Successful reports and database-error (503) responses use `Cache-Control: no-store`. The panel hides cached
  rows during a refresh, after failures and after an environment mismatch. It
  uses separate environment/status/page query keys and discards unused cache.
- Expose only internal operation/checkout/attempt UUIDs, provider, queue state,
  operation state/certainty, counts, timestamps and known diagnostic tokens.
  Do not expose merchant aliases, provider references, URLs, lookup tokens,
  lease tokens, encrypted data, raw payloads, customer data or arbitrary errors.
  Unknown stored diagnostic strings become `unrecognized`; a regex-valid token
  is not automatically safe to disclose. The OpenAPI diagnostic enum and Haskell
  projection must match, enforced by a contract test and catalog review.
- An enabled **database** recovery flag is not proof of a live worker, enabled
  process switch, matching checkout environment, qualified merchant or usable
  credentials. A shared budget timestamp is an earliest slot, not a scheduled
  promise. The panel states both limitations explicitly.
- A completed status-query job is not a successful payment. Show original
  operation state and outcome certainty separately, with no success-colored job
  badge or charge/replay control. Unknown-resource operations that cannot enter
  this worker's bound-resource queue are outside this report; do not interpret
  its absence of rows as absence of unresolved payments.

## Security evidence

Official sources accessed 2026-09-15 UTC:

- [OWASP Authorization Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authorization_Cheat_Sheet.html):
  deny by default, least privilege, check every request and test access boundaries.
- [OWASP API4: Unrestricted Resource Consumption](https://api-security.owasp.org/editions/2023/en/0xa4-unrestricted-resource-consumption/):
  bound resource use and returned records. The row and statement limits implement
  this guidance; they do not establish a measured production throughput/SLA.

These are engineering controls, not legal, accounting, PCI or regulatory
certification. No provider or market availability claim is added by this report.

## Compatibility and rollout

No new migration, backfill or data mutation is needed. Keep all parent migration
checksums and historical records unchanged. Deploy the reviewed backend contract
before the panel; an older backend/404 produces an explicit unavailable state.
The original overview/event/replay routes remain compatible. Generated mobile
types add the read-only endpoint but do not add a native administrative screen.

Rollback the new UI and endpoint without changing job history. Never delete or
reset a terminal job to hide an exception. Parent worker/provider gates remain
separate. Production-sized read latency, authenticated staging operation, safe
manual recovery, unknown-resource queries and operator audit-detail navigation
remain later qualifications/implementation, not implied by these tests.

See [verification and handoff](../payments/query-observability-2026-09-15.md).
