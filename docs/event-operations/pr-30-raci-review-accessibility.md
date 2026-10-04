# PR 30 — actionable RACI accessibility evidence

## Dependency and contract before changes

Parent draft #399, exact head `d4e7ed0505a87a03706b765fc304ffa50362b8fb`.
The worktree was clean. `gh-fix-ci` guides Actions diagnosis; its unavailable
auxiliary `plan` skill is replaced by this written plan. Safe reversible fixes and
dependent draft PRs are authorized by the user's standing instructions. No merges,
production operations, credentials or real-money actions are authorized.

The parent UI, formal verification, PostgreSQL checks and real RACI browser job
passed. Its [persona browser job](https://github.com/diegueins680/tdf-app/actions/runs/35114462810/job/104856379054)
reported 98 passed, 12 existing skips and one failure: WebKit RACI review returned
`color-contrast` from axe. The old assertion discards node details. The downloaded
failure screenshot was visually inspected; it alone does not identify the root
cause or establish whether a transition or stable style is responsible.

Plan: preserve full axe violation diagnostics; reproduce against the existing
isolated API fixture; identify the exact affected node and computed styles; make
only the necessary presentation/test synchronization repair; verify desktop,
phone and WebKit without suppressing assertions or increasing retry counts.

| Contract | Executable obligation |
| --- | --- |
| RA01 | Retain document-wide serious/critical axe assertions, including contrast; expose affected nodes on failure |
| RA02 | Review remains keyboard accessible, with cancel focus and no write before explicit confirmation |
| RA03 | Exact uncertain retry still uses the same body and key; validated success only |
| RA04 | No changes to domain authorization, RACI transitions, API schema, SQL or financial behavior |
| RA05 | Both explicit light/dark themes pass the journey; a known low-contrast blank-page control must still fail the same axe severity filter with a named node |

These presentation contracts refine existing EW03–07. Domain formal models remain
unchanged; passing parent finite checks are not a new model-checking execution or
a universal accessibility proof. Any domain behavior change would require its
applicable models to pass before implementation.

## Verification and remaining scope

The original contrast violation has **not been reproduced locally**: with only
full axe diagnostics added, the original WebKit journey passed three sequential
repetitions (44.1 seconds) and ten more with two workers (1.8 minutes), without
retries. This is macOS WebKit evidence, not proof about hosted Linux WebKit.
The first sandboxed run could not bind the local server (`EPERM`); the actual
browser runs required approved local runtime access. That failed startup is not
counted as a browser pass.

No application styles or state transitions were changed. The test now asserts
the existing `onEntered` cancel-focus contract before scanning, retains full
violation nodes in a JSON attachment and assertion, and checks explicit light
and dark themes. This semantic readiness assertion is not a fixed delay, a
retry of axe, or evidence that an animation caused the original failure.
A separate owned blank-page negative control requires axe to find a known
low-contrast paragraph and identify its selector/failure summary; it never
modifies the application or exempts any node from the journey's scan.

Executed commands (2026-09-16):

```sh
# Original journey, diagnostics only, before theme/focus/control additions:
env PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-raci-contrast.0yQuEB/baseline \
  npx playwright test e2e/web/event-raci-editor.spec.mjs --project=webkit-critical --repeat-each=3
env PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-raci-contrast.0yQuEB/concurrent \
  npx playwright test e2e/web/event-raci-editor.spec.mjs --project=webkit-critical --repeat-each=10 --workers=2
# Final expanded suite:
env PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-raci-contrast.0yQuEB/verified \
  npx playwright test e2e/web/event-raci-editor.spec.mjs --workers=2
node --test scripts/__tests__/event-raci-web-editor.test.mjs
npm run test:persona-program
node --test scripts/__tests__/local-api-fixture.test.mjs scripts/__tests__/ci-pipeline.test.mjs
```

Three RACI verification-wiring tests, three persona-program tests (26 personas,
78 stories, 17 epics) and 16 fixture/CI tests passed. The final expanded browser
suite passed **15/15 in 93.8 seconds**, zero skipped/unexpected/flaky/retries:
two themes plus one negative contrast control across Chromium desktop, phone,
tablet, Firefox and WebKit. All ten journey attachments contain an empty
serious/critical violation array; the five negative controls identified the
deliberately low-contrast node. The JSON report was independently read. The
light WebKit and dark phone screenshots were visually inspected; request IDs
wrap and confirmation controls remain readable. The owned port 4173 listener
was absent after the runner completed.

The existing fixture blocks foreign requests and unknown APIs; all identities,
reasons and mutation receipts are fictional. Attachments must never be reused
against real private task data. No TLC/Alloy, backend build, migration or full UI
suite was rerun for this test-only change; parent checks remain separate evidence.

A fresh read of the parent catalog log confirms **179 unreviewed candidates and
nine stale decisions**, unchanged from its ancestors. No catalog decision was
waived or automatically refreshed. The parent Vercel and Cloudflare status checks
now report success ([Vercel](https://vercel.com/diego-saas-projects/tdf-app-tdf-hq-ui/Ef2db6kFkbXA1af6D3oSbt7Lrm9f),
[Cloudflare](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/8edd0b16-7650-417e-b6f9-f72a7ef8ed5c));
their external internals were not inspected under `gh-fix-ci`.
No production deployment was initiated by this work.

Catalog governance, the original Linux-only observation, hosted verification and
the complete event-operations/native-mobile requirements remain separate gaps.
Rollback reverts only this increment's tests/documentation; no migration or
persisted-data rollback is required.
