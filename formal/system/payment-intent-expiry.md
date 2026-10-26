# Fresh payment-intent expiry: PAY-INTENT-EXPIRY-001

Normative safety property: a fresh intent is admitted only when the locked
checkout expiry is strictly greater than database wall time sampled **after**
acquiring the checkout row lock. Equality refuses. Request/audit time is not
admission authority. Otherwise valid live checkouts must not be refused solely
because the supplied audit timestamp is later than expiry.

The existing `createPaymentIntent` expiry rejection and PAY-RETRY-001 denial of
new payments on closed checkouts supply authority. This repair changes no hold
duration, product eligibility, provider activation or replay policy.

## Counterexample and correction

Starting from main `512d590e902760130f54ca2927620c001a754b85`, a request timestamp
predating checkout expiry passes the SQL expression `expires_at > ?` even when
the database clock has passed expiry. Actual Haskell/PostgreSQL execution admitted
fresh PlaceToPay, PayPhone, PayPal and bank-transfer intents in this condition.
Two additional schedules witnessed a blocked checkout reader through
`pg_blocking_pids`, waited until the database observed expiry, released the
unchanged row lock, and admitted the expired intent at READ COMMITTED and
REPEATABLE READ. These are synthetic failures; no customer charge is claimed.

`PaymentIntentStore.createPaymentIntent` now selects the economic binding and
expiry under `FOR UPDATE`, then executes a separate `SELECT clock_timestamp()`.
It compares those values before inserting the intent, component or history.
`picOccurredAt` remains the audit timestamp. Replacing request time with `now()`
would retain the transaction-start defect. Even a wall-clock read before the
lock can become stale while blocked.

Preconditions: ordinary PostgreSQL locking, no privileged DDL/trigger bypass,
the database wall clock is authoritative, and the caller still owns transaction
commit/rollback. The timestamp sample is the admission linearization point.
The postcondition for expiry refusal is no new intent or amount component.
Provider HTTP completion, commit before expiry, future clock corrections,
every legacy writer, and complete caller authentication are outside this claim.
No liveness/fairness assumption or eventual provider-result claim is made.

## Executable and bounded verification

The existing owned fixture runner executes:

```sh
sh scripts/test-provider-retry-runtime.sh
```

It runs `scripts/verify-payment-intent-expiry.py`, then the existing retry suite.
The verifier runs seven Hspec examples against the actual store and three
independently compiled source mutations: request clock, transaction-start clock,
and wall clock sampled before acquiring the checkout lock. A compile failure,
empty test discovery or missing expected assertions cannot count as detection.
The probe validates local/CI routing and the dedicated fixture database name.
The verifier records the exact Git revision, source hashes and Stack/GHC/PostgreSQL
versions. It compiles retained snapshots of the Commerce modules and verifies
persistent source freshness afterward; this is not hostile-runner attestation.

For an already owned/migrated provider-retry fixture, run:

```sh
TDF_PROVIDER_RETRY_DATABASE_URL=postgresql://postgres@127.0.0.1:55475/tdf_provider_retry_test \
  python3 scripts/verify-payment-intent-expiry.py --output /tmp/payment-expiry.json
```

`PaymentIntentExpiry.tla` models one request, arbitrary caller timestamp in 0..3,
database time in 0..3 and expiry 2, with separately interleaved waiting, locking,
clock sampling and time advancement. Its positive configuration checks type,
expired-admission denial and live-admission acceptance. Four mutations use request
time, transaction time, pre-lock time, or an inclusive expiry bound. Each must
violate `ExpiredAdmissionDenied`. Positive exploration finds 164 distinct states
(188 generated); no universal SQL/Haskell refinement follows from these bounds.
The model assumes the implemented row-lock boundary and a monotone database clock.

The existing formal CI runner registers all five configurations. Replay all models:

```sh
JAVA_BIN=/path/to/java TLA2TOOLS_JAR=/path/to/pinned/tla2tools.jar \
  ALLOY_JAR=/path/to/pinned/alloy.jar npm run verify:event-operations:formal
```

The scoped evidence package contains original failing execution, repaired
execution/mutation results and named TLC traces. Statuses are **TESTED** for the
store and **BOUNDED_VERIFIED** for this finite abstraction. Full requirement and
implementation-refinement status remains **PARTIAL/UNKNOWN**, never PROVEN.

## Historical reconciliation and remaining priority

GitHub inspection on 2026-10-25 found #548 (music isolation/recovery), #581
(bounded models/certificate/report admission) and #598 (client diagnostics privacy)
merged. Their old receipts retain their original source identities. #551 remains
draft with ledger posting, checkout terms history and PayPal fallback fixes;
#555, #554 and #569 retain source history consolidated by that successor.
#596 separately repairs hosted-provider domain validation and depends on #551.
#593 extends arithmetic caller-precondition checking. These pending improvements
must not be counted as integrated into this baseline.

The next high-value financial task is independent current-head qualification and
protected integration of #551, preserving all six migration introduction ancestors
and SQL bytes. Current main still has its documented direct-posted/append ledger
enforcement gaps. Provider activation and production migration/deployment require
their existing separate approval and guarded operational process.
