# PR 29 — preserve lazy validation and the initial bundle budget

## Dependency, diagnosis and contract before changes

Branch `fix/event-ui-lazy-validation`, based on draft #398 at
`5ad2333c2a6ce379579a1cffa2708d56cbb21f5f`. `gh-fix-ci` guided Actions
diagnosis; its unavailable auxiliary `plan` skill is replaced by this written
plan. The user authorized safe reversible implementation and draft PR delivery.
No merges, production settings, provider operations or deployment are authorized.

On that exact head, [UI quality](https://github.com/diegueins680/tdf-app/actions/runs/35057374057/job/104670336108)
passed all 210 Jest suites / 2,132 tests, then failed its production artifact gate:
`initial JS 424395 bytes gzip exceeds 419840`. The aggregate quality failure is
downstream of that failure. The persona web job passed. Parent #395's real RACI
browser job also passed: eight journeys, 28.6 seconds, owned database removed.

A no-env local production build at the unchanged head reproduced the budget
failure: 423,263 bytes gzip across the entry and five preloads, above 419,840.
Measure final files, not an intermediate `generateBundle` callback: Vite still
rewrites the entry's preload dependency map after that diagnostic callback.
The module graph places `zod/v3/types.js` (115,404 rendered bytes before final
compression) in eager `vendor`, even though its application consumers are lazy
pages/components/API modules. Other vendor coupling exists; this repair is only Zod.
Local Node 24 and hosted Node 22 totals are separate observations, not identical
byte-size claims or an attribution of their difference to one specific cause.

| Contract | Guard / effect | Executable evidence |
| --- | --- | --- |
| LB01 | Exact `node_modules/zod/` modules go to their own chunk; unrelated package names and application files do not | Loaded Vite config tests |
| LB02 | Zod is absent from initial module preloads; budget stays 410 KiB, maximum eight preloads | Existing artifact checker plus negative Zod-preload fixture |
| LB03 | Existing route-only, oversized, excess-preload and secret detection remain fail-closed | Disposable artifact fixtures executed against the actual checker |
| LB04 | Production-built RACI editor still loads its real validators and preserves explicit review, exact retry and validated task reads | Isolated Playwright suite served from built files, not Vite transforms |

Only bundling policy and its executable regression contracts change. No domain
state machine, authorization rule, money operation or validator implementation is
changed. Existing high-risk models passed on #398 before this repair; the packaging
contracts use executable checks and do not assert a new TLC/Alloy proof.

Plan: add tests first; observe precise expected failures; isolate the package;
extend the existing forbidden preload guard without changing limits; build and
check the actual artifact; exercise the production RACI route; document results.

## Verification

Executed on 2026-09-16 (America/Guayaquil):

- Added regression tests before the fix: five existing safety checks passed and
  exactly two expected tests failed (`vendor !== zod`, eager Zod accepted).
  After the fix, all seven passed. Thirteen CI-pipeline checks passed, including
  a new check that `quality:ui` runs these regressions and retains its artifact gate.
- Final-file local comparison using the same environment/configuration:
  **423,263 → 411,212 bytes gzip initial JavaScript**, a 12,051-byte reduction,
  under the unchanged 419,840-byte limit. Both builds have five module preloads.
  Traversal of the emitted manifest's static entry-import closure excludes Zod;
  `zod-DGXLLr8g.js` still exists (12,094 bytes gzip) for the route to load.
- Served the compiled `fixed` artifact with Vite **preview**, not a development
  transform server. **12/12** existing task/editor browser tests passed on desktop
  and phone Chromium in 23.8 seconds, zero skipped/unexpected/flaky/retries.
  Covered explicit keyboard review, identical uncertain retry, malformed successful
  responses, invalid/repeated task selectors, ES/EN, task-only reads and foreign
  request isolation. Existing axe assertions passed. Both production-bundle RACI
  review screenshots were generated and visually inspected. APIs remain fictional;
  no real command/financial action was sent by this production-bundle test.
- Independent report read confirmed 12 expected, zero unexpected/skipped/flaky.
  The owned preview listener closed and child exit status was propagated.
- `npm run quality:ui` completed with exit 0: seven bundle regressions, full
  ESLint, full TypeScript, **210 Jest suites / 2,132 tests**, and the canonical
  production build/artifact gate all passed. Jest took 234.48 seconds; Vite took
  17.91 seconds. The final canonical artifact independently reported **five
  preloads / 411,212 bytes gzip**, matching the isolated build above. Existing
  React/MUI test warnings and the large-chunk advisory remain; no warning,
  assertion, budget or validation was suppressed to obtain this result.

Commands:

```sh
node --test scripts/__tests__/ui-validation-bundle.test.mjs
node --test scripts/__tests__/ci-pipeline.test.mjs
npm run quality:ui
```

The diagnostic builds used the repository Vite configuration through `vite.build`
with `root: 'tdf-hq-ui'`, `envFile:false`, `envPrefix:[]`, `manifest:true` and distinct
owned temporary output directories `baseline` and `fixed` beneath
`/private/tmp/tdf-ui-bundle.gsedbT`. Baseline/fixed artifacts were never deployed.
The initial totals are computed from finalized HTML script/preload references and
`gzipSync` of their actual files, not intermediate module-size estimates.
The browser run used `vite.preview` with that fixed output, `127.0.0.1:4173`,
`strictPort:true`, no `.env`, then spawned:

```sh
playwright test e2e/web/event-raci-editor.spec.mjs e2e/web/event-task-view.spec.mjs \
  --project=chromium-desktop --project=chromium-phone
```

The local child used `CI=''` solely to reuse the owned preview listener under the
existing config and `PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-ui-bundle.gsedbT/runtime`.
Hosted configuration, retry policy and gates were not relaxed. The artifact JSON
and screenshots are local diagnostic evidence, not checked-in production captures.
The complete UI command was started only after checking this worktree has no `.env`
other than `.env.example`, and no inherited `VITE_*` variables.

A fresh remote read confirms #398's real RACI job completed successfully at
2026-09-16 05:33:17 UTC; #398 remains draft/open on its original head with no
auto-merge. Its aggregate/UI/catalog/external failures remain as recorded above;
the fix here is a child branch, not a rewrite of its historical check result.

## Rollback, security and remaining limitations

Revert only this branch's bundling/checker/tests/documentation changes and rebuild;
there is no schema migration, persisted-data change or generated API change.
No validation is removed, deferred within an operation or replaced with a mock:
the route must load its normal module dependency before executing the command.
Browser APIs are fictional and foreign network traffic must be blocked; this is
production-bundle runtime evidence, not a live provider or deployed-system test.

Catalog governance remains open (179 unreviewed candidates / nine stale decisions
on parent #398). External failed checks were not inspected or changed:
[Vercel](https://vercel.com/diego-saas-projects/tdf-app-tdf-hq-ui/HxpQsayGoSrsTXmeyf3M5NNWdH9j),
[Cloudflare](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/2d66cc32-1997-4862-8d0a-ee8e4877a19e).
Full event operations, native mobile, provider tests and final hosted CI are not
claimed complete by this increment.
