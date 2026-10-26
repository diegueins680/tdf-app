# PlaceToPay / PayPhone full-schema validation repair

Follow-up PR #596 is stacked on PR #551 at
`9d7551353a7c278cd0937eb57d04a94241318d42`. The earlier 119/304 diagnostic
against 207 migrations remains historical failing evidence; reduced fixtures
never qualified these rails for the production domain guards.

The new additive migration is introduced by
`456be073c944dd3d76eb9e3c2169fe7fbb4041ef`. It appends to the unchanged
208-entry manifest prefix, giving 209 migrations. The existing four triggers
now use a shared exact-binding predicate. PlaceToPay `session` and PayPhone
`sale` references must match their persisted create operation, rather than the
internal business order. Account, environment, checkout/intent, resource ID,
amount and currency must agree. Existing provider resource rules, independent
manual approval and Domo accepted active hold admission remain enforced.
Authenticated provider-query validation in the application is unchanged.

The provider runtime runner now applies/replays the complete manifest using the
same production-shaped baseline as the migration lane. Its synthetic fixtures
include canonical event types and workflow states, approved policies, complete
runtime snapshots and real immutable commercial/provider identities. Paid
recovery setup must pass the verified payment path. Invalid historical writes
are rejected at their actual database boundary; unrelated SQL errors do not
count as successful negative controls. No domain trigger is disabled.

Local PostgreSQL 16.15 evidence, before hosted compiled-suite qualification:

- The 209-entry batch applied and replayed on an owned fresh UTF-8 database.
- 224 provider/domain matrix cases passed: new rail positives, missing/mismatched
  evidence controls, and Datafast/PayPal/Stripe/independently-approved bank
  transfer regression cases across all four domains.
- 16 races passed with observed PostgreSQL blocking at READ COMMITTED and
  REPEATABLE READ; serialization failures retry in a fresh transaction and
  successful replay preserves paid state and fulfillment.
- Four event fee/tax fixtures also verify that order/header/line/runtime money
  components agree, including the remainder-cent discount.
- Ten controls witness the original four guards rejecting valid new rails and
  a removed-reference comparison admitting the named forbidden case.

Run `sh scripts/test-provider-retry-runtime.sh` with Stack, Node, psql and
Python psycopg2 available. The runner requires an empty disposable database
named `tdf_provider_retry_test` (or provisions its own Docker PostgreSQL).
It first runs the independent SQL admission controls, then the actual Haskell
provider/refund persistence suite against that same full schema. Exact current
head hosted checks and independent review remain necessary. Finite PostgreSQL
and bounded-model results do not establish a universal refinement proof.

No production migration or production deployment, provider activation, merchant request or
real-money transaction is part of this repair. Retain the admission migration
across application rollback; do not rewrite evidence or disable validation to
revert the application. Follow the existing protected review/FIFO merge and
separate production release prerequisites.

## Current-main integration, 26 October

The integration candidate retains all222 migrations from ledger candidate
`f3e3217ac12a04be2a221ee6ac8bea67875d46c5` in order, appending only the
original provider migration as223. PR551 is merged; PR621 remains a dependency
until protected admission completes. Earlier209-schema evidence above retains its
historical identity and does not qualify this composition. Fresh compiled
provider/refund and complete-schema verification is required before publication.

The former Python matrix used22 optimization-sensitive assertions, including
SQL transition calls. An independent extracted-helper control reproduces three
false accepts under Python -O: wrong SQLSTATE, wrong guard message and a changed
rollback snapshot. Explicit failure branches preserve every check and operation
under optimization. The canonical runner executes four harness controls normally
and with -O, then the actual PostgreSQL matrix with -O. These synthetic helper
controls establish harness detection, separately from database behavior.

The first223-schema compiled run at6566630f156fa35b9bea683d393162c24a34ab6a
executed316 cases with three fixture-precondition failures. The paid-but-unverified
refund test must construct historical legacy paid state before attaching its
canonical runtime; otherwise the unpaid-order400 branch fires before provider
selection. Its409 expectation is retained and the exact rejection body is checked;
an additional case verifies the unpaid400 branch without creating refund rows.
Late-capture review admission must complete actual ticket fulfillment after
recordVerifiedPayment; that store only synchronizes canonical payment status.
Fixtures now call the existing finalizePaidTicketOrder operation, retaining all
payment/admission guards. The failed receipt and logs remain distinct evidence.

## Protected-main reconciliation after ledger merge

PR621 merged as78fa351b469c1d2031df3232204dd722ac480e3b with all41 statuses
passing/skipped and approval on950c3f7c3f341cd05a48c237e7377cca66e35188.
This candidate incorporates protected main2e01bbe1263e8761fa2692bd951f828c01142449
without rewriting any of its222 registry entries or historical SQL. Only the
original provider migration is appended as223. The old draft's backend failure
was the historical saved-search prefix/readiness mismatch; the current main
contains its complete-batch checkpoint correction.

The317-case native result retains cb777 source identity. Current main changes
MusicRelease and SocialEventsHandlers, so the integrated current head requires
fresh compiled provider/refund execution. No whole-backend source equality or
current-head result is inferred from that older run. A controlled missing Hspec
group reproduces successful empty discovery without --fail-on=empty and failure
with it; the canonical provider runner now requires that flag.
