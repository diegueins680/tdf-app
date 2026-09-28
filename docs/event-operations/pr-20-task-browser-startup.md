# PR 20 — Task browser startup verification

## Scope and dependency

Depends on draft [PR 373](https://github.com/diegueins680/tdf-app/pull/373), branch
`feat/event-task-raci-view`, exact base `f03f366de44c8ad09248e9d399caa5994af06d5a`.
Branch: `test/event-task-browser-startup`. Test harness and documentation only; no feature
code, authorization, API, schema, dependency, generated client or mobile change.

The [fixture contract](task-browser-isolation-contract.md) was written before modifying the
harness. Existing TV-01–06 and bounded TaskView/TaskRead checks from PR 19 remain applicable.
No new state transition or model change; TLC/Alloy were not rerun in this test-only increment.
The prior finite results are not a universal proof or a production-performance guarantee.

## Diagnosis and measured experiment

Preserved PR 19's final failing artifacts before running the new experiment. Desktop traces
showed 407 requests per single navigation. The task API completed in 38–78 ms, but the task
module was requested at 9.034 seconds (RACI journey) and 10.936 seconds (English journey).
The startup delay preceded task handling; it was not evidence of a slow task endpoint.

The fixture used `page.route('**/*')`, sending every Vite module through a JavaScript callback
in the test runner. Inspection of installed Playwright 1.59.1's `RouteHandler` and
`PageDispatcher` confirmed that a serialized RegExp can filter in the driver; unmatched
requests continue there. The replacement excludes only the exact loopback `/src/` and
`/node_modules/` namespaces. No modules are preloaded, no authentication is bypassed and
browser HTTP caching remains governed by the existing routing setup.

| Observed local run | Result | Wall duration | Desktop English module request | Desktop English continue callbacks |
|---|---|---|---|---|
| PR 19 preserved baseline | 5 pass / 3 fail | 142.261 s | 10.936 s | 387 |
| First filtered run, original eight tests | 8 pass / 0 fail | 66.064 s | 2.255 s | 6 |

The RACI journey likewise reduced continue callbacks from 387 to 6; initial task module
request moved from 9.034 to 6.252 seconds. The total resource count did not disappear: 407
requests for the English page in both runs. This removes a concrete source of test-runner
overhead, not application functionality. Shared-host load and cache conditions are not a
controlled benchmark; these observations do not establish the sole cause of prior variability
or a production latency target.

## Verification

Executed first experiment:

```bash
./node_modules/.bin/playwright test --config=playwright.event-task.config.mjs --trace on
```

All eight tests passed with original 8-second assertions, zero retries, one worker and real
development StrictMode. Both keyboard refresh/denial/retry journeys retained exact read
counts, no parent/profile reads and zero serious/critical axe violations. Malformed task
responses, repeated/invalid selectors and English empty-policy behavior remained covered.

Three Node tests passed for local module namespaces, all task/privacy API paths, unknown
paths, hostile origin lookalikes/ports and invalid base URLs. They now run in `quality:repo`.
A new browser scenario exercises the real handler on an empty local document: a foreign
module-shaped URL must be aborted by the fixture and an unknown POST must get synthetic 404.

The final expanded run used the same command and passed **10/10 tests**, exit 0,
**76.023 seconds**, with no skips or retries. Its desktop English module request started
at 3.352 seconds. Both viewport keyboard-recovery/axe checks passed again. Final desktop
and phone screenshots were generated and visually inspected; task fields and the RACI
table are readable. The existing fixed radio bar and scrollable desktop content remain;
this is not a full accessibility audit.

`npm run quality:repo` passed, exit 0: **146 tests** (8 + 3 + 42 + 4 + 61 + 23 + 2 + 3).
Its tracked-source heuristic reported 9,634 findings, zero critical/errors and 355 warnings;
this is not TLC/Alloy. The Git/release tests use disposable local repositories and fixtures,
not production operations. Node syntax, shell syntax, staged whitespace and 74 relative
documentation links passed. No generated fixture drift occurred.

Local evidence (ignored outputs, not committed or uploaded):

- Preserved PR 19 baseline: `/private/tmp/tdf-task-startup-baseline.5G7w3b/`.
- First filtered run: `/private/tmp/tdf-task-startup-filtered.4PK56Y/`.
- Final JSON, traces and screenshots: `artifacts/event-task-playwright/`.

Every run starts a new local Vite server and browser context. Local Vite dependency caches
were not deleted; this does not claim a clean-cache benchmark. The experiment and final run
both completed successfully; no failed run was discarded or reclassified as a pass.

This is synthetic browser/API integration, not actual server authorization or database E2E.
No full application build, full Jest/UI lint, backend, migration or native mobile result is
claimed for this increment. Hosted checks must be evaluated at the published head separately.

## Security, rollback and remaining work

Remote/non-loopback fixture bases fail closed. Only local Vite modules bypass the test-runner
callback; canonical APIs and foreign URLs retain interception. The application, permissions,
task generation fence, server feature flags and provider configuration remain unchanged.
Revert this test-only increment to restore the old harness; no data rollback or migration.

PR 19's historical failures remain documented, not rewritten as successes. This follow-up
does not complete task editing, rich task fields, templates, full event workspaces, hiring,
payments, offline synchronization or mobile. Prior full-suite/build-budget limitations remain.
No merge, deployment, credential activation or real-money action is authorized or performed.
