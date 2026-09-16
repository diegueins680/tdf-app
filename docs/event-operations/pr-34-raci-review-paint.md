# PR 34 — measurable RACI review paint readiness

## Scope, approval and dependency

Branch `fix/event-raci-review-contrast`, dependent on draft #411 at
`f2f01194090a241777c451d06cb0848af7aa759a`. Clean starting worktree. The user approved
the preceding CI investigation plan with Continue; `gh-fix-ci` guided log/trace
inspection and the bounded correction. The unavailable auxiliary plan skill was
replaced by the written plan. No application, API, SQL, schema, generated client,
permission, production manifest, provider, credentials or feature flag changed.

## Evidence and contract before implementation

[RP01–05](raci-review-paint-contract.md) define the executable presentation contract.
The [parent persona job](https://github.com/diegueins680/tdf-app/actions/runs/35134774913/job/104924440709)
failed one Linux WebKit light-theme RACI review: text contrast 4.01:1, cancel 2.59:1
and confirmation 2.36:1 against 4.5:1. 108 passed / 12 pre-existing platform skips.
The failure artifact and trace were downloaded and the screenshot visually
inspected. Its later opaque appearance does not disprove the measured violation.
Trace snapshots retain a 225ms opacity transition and target inline opacity 1.
Those are not computed paint values; focus after React's entered callback also
does not establish that all ancestor transitions have finished.

Three unchanged local macOS WebKit repetitions passed. Therefore this PR does not
claim to reproduce or prove the original Linux engine root cause. It closes the
test's measurable-readiness gap: collect computed styles, require full visible
opacity and no active/pending/paused animations in the review subtree and ancestor
chain, then run axe once. Normal expectation timeout stays unchanged. No fixed
delay, forced finish/cancel in the application journey, CSS override, axe retry,
rule suppression or reduced coverage. Stuck translucency fails readiness; stable
low contrast still fails axe. The synthetic negative control alone drives its own
owned animation through intermediate/finished states.

## Verification executed (2026-09-16)

```sh
env PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-raci-webkit.HwYQyR/baseline \
  npx playwright test e2e/web/event-raci-editor.spec.mjs \
  --project=webkit-critical --grep 'RACI light' --repeat-each=3
env PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-raci-webkit.HwYQyR/paint \
  npx playwright test e2e/web/event-raci-editor.spec.mjs --workers=2
node --test scripts/__tests__/event-raci-web-editor.test.mjs \
  scripts/__tests__/local-api-fixture.test.mjs scripts/__tests__/ci-pipeline.test.mjs
node --check e2e/web/event-raci-editor.spec.mjs
node --check e2e/web/helpers/review-paint.mjs
git diff --check
```

- Baseline: **3 passed**, no application changes; not reproduction evidence.
- Corrected browser suite: **20 passed**, no skips/retries/flaky cases, across
  Chromium desktop/phone/tablet, Firefox and macOS WebKit. Both themes retain safe
  keyboard focus, explicit confirmation, exact uncertain retry and document-wide
  serious/critical axe assertions. Two negative controls per profile still detect
  their deliberately bad conditions.
- **20 runner/fixture/CI tests passed**. Initial new source guard incorrectly
  matched `===` as assignment; corrected its regex to reject assignment only,
  without changing readiness or browser assertions. Syntax/whitespace checks passed.
- First sandbox browser startup failed EPERM binding loopback; approved runtime
  rerun completed. Docker image discovery failed with daemon EOF, so no local
  Linux container reproduction is claimed and Docker was not restarted.
- No full web/backend build, new TLC/Alloy run, database/migration test, native
  mobile, payment or production execution. Existing domain models are unchanged;
  parent hosted formal and all event PostgreSQL jobs passed. Parent real RACI
  browser also passed (40m42s); none of those is a fresh check for this branch.

All browser API data is synthetic, foreign requests are blocked and unknown API
calls fail closed. Diagnostic attachments are test artifacts, never production
task data. The helper only reads DOM/style/animation state.

## Limitations and rollback

Hosted Linux verification remains required; local passes alone do not close the
original failure. Catalog audit still reports 179 unreviewed candidates and nine
stale decisions in the parent; no decision is waived or automatically refreshed.
Catalog review is the next independently reviewable change, not mixed into this
test synchronization PR. Parent external provider internals were not inspected.

Rollback reverts only helper/tests/docs. No persisted data, accepted agreements,
audit history or migration is affected. No merge, deployment, production activation
or real-money action was performed. The broader event-operations mission remains
incomplete.

## Subsequent hosted checkpoint

Draft #413 exact `cbbb3da9f20c2314c73a5114b8c0aa943647054e` passed the
[Linux persona job](https://github.com/diegueins680/tdf-app/actions/runs/35143458319/job/104953603099):
**114 passed / 12 existing skips / zero unexpected or flaky results**, 232.2s.
Formal `verify` and aggregate `quality` also passed. The original catalog failure
remains separate; this is not an all-checks-green claim.

Downloaded JSON attachments provide concrete evidence of the synchronization gap:
in **dark Linux WebKit**, after cancel focus, `raci-review-paint-before` recorded
the dialog container's computed opacity **0** and an animation with
`playState=running`, `pending=true`, `endTime=225`. The final attachment recorded
opacity **1**, no animations and readiness true. Axe then passed. The light journey
was already opaque in its first measurement and passed as well. Thus the run
demonstrates focus can precede paint readiness, but does not reproduce the exact
original light-theme contrast sample or establish a universal engine root cause.
No animations or CSS were altered by the readiness helper.
