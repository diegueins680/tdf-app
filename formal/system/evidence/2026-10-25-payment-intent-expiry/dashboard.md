# Verification cycle: payment-intent expiry

Source candidate: `5ca62256af0a8eecd2ec4929a7d0eb01a96425b5`.
Main baseline: `512d590e902760130f54ca2927620c001a754b85`.
Exact tools, hashes, commands and computed metrics: [receipt.json](receipt.json).
This dated package is current only for its recorded source fingerprints.

| Measurement | Evidence |
| --- | --- |
| Registered requirements | 137 total; 111 critical |
| Complete requirements | 0 proven; 137 unproven; all retain PARTIAL conformance |
| Implementation/refinement obligations | 137 + 137 unproven |
| Conditional source-derived SMT claims | 4 arithmetic, 3 authorization, 4 recovery PROVEN under their existing translator assumptions |
| Critical PAY/AUTH domain requirements | 18 / 11; no complete requirement proven |
| New scoped boundary | PAY-INTENT-EXPIRY-001: TESTED implementation; BOUNDED_VERIFIED abstraction |
| Executable regressions | 7 failed on original source; 7 pass after repair |
| Model properties | 2 semantic safety/admission properties; 164 distinct states; no liveness claim |
| Mutation detection | 3 compiled implementation controls + 4 named model controls |
| Confirmed defects | 1 repaired locally; 0 integrated at this evidence checkpoint |
| Current evidence coverage in this cycle | 1 scoped requirement; no whole-system coverage percentage inferred |
| Release | BLOCKED; independent review, current-head hosted CI and protected FIFO admission remain required |

The original failure is [retained](counterexample-original.log), alongside the
[actual-store/mutation receipt](store-mutations.json), [model traces](bounded-model.json)
and [canonical assurance matrix](assurance-report.md). The original seven examples
failed against unchanged main implementation; the fresh repaired executions are
bound to the clean source commit above. The original main lacks the test file;
its current probe hash identifies the augmented test harness.

The existing registry still has broader financial and authorization obligations.
The next financial integration priority is [#551](https://github.com/diegueins680/tdf-app/pull/551):
ledger posting fences, retained checkout terms history and PayPal capture uncertainty.
Hosted-provider domain settlement repair [#596](https://github.com/diegueins680/tdf-app/pull/596)
depends on it. Qualify each exact merged candidate and preserve migration ancestry.
Provider truthfulness, external settlement, commit/completion deadlines, clock
rollback and whole-program refinement remain unresolved by this cycle.

No production migration, deployment, provider activation or real charge occurred.
