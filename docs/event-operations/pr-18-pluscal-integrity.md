# PR 18 — Verify exact reservation-model regeneration

## Scope and dependency

Dependent on draft [PR 368](https://github.com/diegueins680/tdf-app/pull/368), branch
`fix/fanhub-authoritative-onboarding`, exact base
`ff3cd1676a6db7b6ceef56d44407b0846a4f2071`. Delivery branch:
`fix/reservation-pluscal-integrity`. This is phase-2 evidence hardening, not a booking feature
or completion of the end-to-end event system.

The [PC-01–06 contract](pluscal-integrity-contract.md) maps EO-057 to an exact-regeneration
gate. The prior reservation warning came from two stripped trailing spaces, not different
TLA+ expressions. Fixed `-lineWidth 120` regeneration removes the wrapping that emitted those
spaces. The only model diff is the generated checksum and two `UNCHANGED` list wraps.
PlusCal source, safety invariants, configurations, bounds and fairness are unchanged.

The checker validates the official JAR hash, copies each PlusCal model to its own temporary
directory and compares the whole regenerated file byte for byte. Failure does not repair
the source. It requires ReservationRace, discovers additional PlusCal files, rejects absent
or malformed pairs and runs before TLC. Real-translator negative controls and explicit Node
22 setup are part of the existing formal CI job; both PR and main path filters include the
new checker/test. No CI rules or existing model assertions were weakened.

## Completed local verification (2026-09-15)

Host Node `v24.8.0`, OpenJDK `21.0.12.1`, checksum-pinned TLA+ tools `1.7.2`
(`pcal.trans` 1.11, TLC reports 2.17), Alloy `6.2.0`, `sat4j`.

The exact complete command, run with the final model, checker, tests and shell runner:

```bash
env \
  JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Result: **exit 0**, final success sentinel observed. Exact PlusCal match, **13/13** integrity
tests (no skips), **16** passing positive TLC configurations, **30** expected named TLC
counterexamples, **2** SAT Alloy scenarios and **11** UNSAT assertion checks. No PlusCal
translation warning appeared in this complete run. ReservationRace and ReservationOverride
each retained **7 generated / 5 distinct states / depth 3**. Other finite counts are unchanged
from the [formal results](../../formal/event-operations/README.md). These are finite checks,
not universal proofs or reservation implementation verification.

The 13 executable tests cover valid non-mutating regeneration, stale algorithm, stale generated
semantics, either forged checksum, whitespace drift, the exact previous formatting defect,
missing/duplicate/orphaned blocks, syntax/process/missing-tool failure, discovery/mandatory
model, toolchain pin, rejected CLI options and CI wiring. A zero-exit process that does not
actually translate cannot pass. The completed final embedded test run took 4.741 seconds.

Additional completed checks:

- `npm run test:ci-pipeline`: **23/23 pass**, also repeated within the repository gate.
- `npm run quality:repo`: **exit 0**, **143 tests** total (8 + 42 + 4 + 61 + 23 + 2 + 3).
  Its separate heuristic audit reports 9,598 findings, 0 critical, 0 errors, 355 warnings;
  that heuristic is not TLC or Alloy. Its Git/merge/push/release fixtures use disposable
  local repositories and test assets, not production application operations.
- `bash -n scripts/verify-event-operations-formal.sh`, Node checker syntax and
  `git diff --check`: pass.
- Byte comparison against the independently regenerated diagnostic copy: identical.
- Diff against the exact parent: no changes in `tdf-hq`, `tdf-hq-ui`, `tdf-mobile`,
  `package-lock.json` or the production migration manifest. Repository fixture generation
  produced no unrelated worktree changes.

An earlier run printed the expected model results but ended with a shell read/syntax error
after the launcher was edited while it was still executing. That run is **not** counted as
a completed verification. The final run above used stable scripts throughout and completed
with both exit 0 and the success sentinel. The initial standalone 13-test run also passed;
the final embedded run includes strengthened zero-exit and mandatory-model assertions.
The host emitted Perl locale-fallback warnings; they are unrelated to PlusCal integrity and
are not suppressed or represented as fixed.

## Schema, privacy and rollback

No schema/migrations, application behavior, permissions, API/client, mobile, provider,
financial or deployment settings change. No production action, live charge, refund, payout,
credential change or merge is performed. Temporary outputs are removed only from directories
created by the checker. A reviewed revert restores the old tooling; it also restores the
known warning, so rollback must not be used to claim verified readiness.

## Limitations and remaining work

- Regeneration trusts the pinned translator; it does not prove its correctness, arbitrary
  source edits, or refinement of atomic booking actions into real SQL/HTTP transactions.
- The finite model still abstracts one exclusive resource and two overlapping requests.
  Buffers, capacity, multi-resource bookings and complete hiring/payment integration remain.
- No fresh UI/browser/mobile/backend/database migration tests or full UI lint/build were run
  for this tooling-only change. Prior global UI lint, CourseRegistrations and bundle-budget
  limitations are not cleared by these results. Mobile remains an uninitialized submodule.
- No hosted CI result is claimed here; verify the exact published head separately. The PR
  must remain draft and dependent, without merge or production activation.
- Next product work remains the canonical event/task workspace and missing task/RACI commands,
  followed by logistics/templates, engagement, payments, collaboration/offline and mobile.
  Legal/accounting/provider review gates still apply before production use.
