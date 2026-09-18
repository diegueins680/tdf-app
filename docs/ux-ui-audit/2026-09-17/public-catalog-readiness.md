# Public catalog readiness — UX-260917-039

Observed18September2026 on API5c11577a5: `/marketplace` returned200 after38.924s;
a second request timed out at15s without bytes. The web client aborts at30s.
`/records/feed?locale=es` took10.566s and10.249s. Health returned200 in0.299s.
These are lab observations, not p75 field data or conversion measurements.
See [production receipt](evidence/public-catalog-production-before.json).

The list performed a separate asset lookup and separate approved rental terms
transaction per listing. Batch both inputs, preserve the existing shared DTO and
per-item/checkout terms query, and retain no cache. Records keeps the same queries,
filters, ordering and limits, with the fixed reads in one transaction. Locale and
missing workflow/state guards remain outside that transaction. No schema, API,
prices, payment setting or publication flag changes are required.

`node scripts/__tests__/public-catalog-http-runtime.mjs` requires an explicit local
`*_test` database and compiled server. Eighty generated listing combinations cover
inactive listings, active/inactive approval, optional weekly prices and authoritative
rental fields. Four representative detail responses must equal their list DTOs.
The Records collection IDs must equal a read-only published/active SQL oracle.
A TCP observer counts actual catalog SQL executions including transactions, without
retaining SQL or credentials. Background-worker transactions are excluded by table
classification. The budget is16marketplace and35Records executions.

On the same isolated database after cleaning the160owned fixtures left by the initial
failed cleanup,80generated listings yield64published. The old binary executed259/60 respectively and fails the budget; the
corrected binary executes5/28 and passes all value/visibility assertions. Local
corrected list time23ms does not predict production latency. Initial test cleanup
missed the immutable terms-history FK; scoped history cleanup was corrected. An
initial unfiltered observer included worker traffic (32 instead of5); domain
transaction classification removes that contamination. Those attempts are not
passing evidence. [Final receipt](evidence/public-catalog-query-budget.json).

Stack compilation passed. Full stack tests and hosted gates are pending. Existing
bounded payment/rental models remain applicable to unchanged consequential handlers;
query batching is covered by the executable persistence and wire contract, not a
claim of a new formal proof. No production-after or field-performance claim yet.

Release requires reviewed source, green applicable checks, immutable image, current
coordination/lease, pre-release snapshot and migration compatibility, the merged438
recovery guard, canary and actual public journey verification. Preserve all current
flags, especially the disabled onboarding experiment. No migration rollback needed;
a binary recovery must satisfy the existing authentication and schema compatibility
floor. The source change is not deployed merely because CI or `/health` succeeds.

Review follow-up: READ COMMITTED allowed concurrent sale fulfillment to deactivate
a selected listing before the later active-listing JOIN. The old candidate then
returned its base price instead of approved rental terms. A deterministic actual
PostgreSQL-wire interleaving reproduces10001 versus2001minor units. Binding the
already-selected IDs fixes it while retaining active/approved term predicates; a
subsequent request excludes the now-inactive row. Empty-ID batches are total. The
normal80fixture/64published query budget remains5; Records remains28. Final Stack
build and actual HTTP regression pass; full pinned models pass with a meaningful
unsafe selection-join control. The earlier2587-example full run applies to641876;
this successor still requires its complete hosted gates and renewed exact-head review.
