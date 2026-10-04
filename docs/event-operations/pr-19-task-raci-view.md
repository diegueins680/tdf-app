# PR 19 — Scoped task/RACI web read view (browser gate still failing)

## Scope and dependency

Depends on draft [PR 372](https://github.com/diegueins680/tdf-app/pull/372), branch
`fix/reservation-pluscal-integrity`, exact base
`ec24b70b1b2319a875099cc118db0e68b62a5ed0`. Branch: `feat/event-task-raci-view`.
This increment is implemented and unit-tested but **not ready for acceptance**: the final
local browser run retains three startup timeout failures. Do not merge or activate it.

Historical checkpoint: [PR 20](pr-20-task-browser-startup.md) subsequently verifies a
test-harness correction with the original assertions. Results below remain the actual
PR 19 runs; the follow-up does not establish production or full-stack readiness.

[TV-01–06](task-view-contract.md) refine EO-023–028/045/052–055/058 using the existing
canonical task API. `/social/eventos/80?tarea=8000` opens a task-only subview; the existing
logistics schedule links to it. The unchanged registered event route dispatches through
`SocialEventWorkspacePage` before importing either the task view or the legacy overview.
Task links never mount the overview's parent-event, moments or ticket queries. Repeated or
empty task selectors fail closed instead of falling back to a broader reader.

`EventTaskPage` displays canonical status, separate activity/policy versions, RACI IDs and
explicit responsibility-attention state. It does not invent titles, identity details, dates,
dependency readiness or an aggregate write token. No assignment/completion/edit controls are
added. Spanish strings and English fallback reuse the existing locale system.

Session/target/reload generation fences hide old receipts synchronously, bind explicit bearer
requests to the captured session, cancel on cleanup and ignore late responses/errors. The
existing client gains optional bearer/signal arguments without changing existing calls or
runtime decoding. No React Query cache, local storage, offline queue or optimistic success
is added. The server still decides exact-task authority and its feature flag remains unchanged.

## Formal verification before feature implementation

Completed command on 2026-09-15:

```bash
env \
  JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Exit 0 and final success sentinel, before adding feature code. Exact PlusCal regeneration
and its 13 tests passed. All **17 positive TLC configurations**, **33 expected named
counterexamples**, **2 SAT Alloy scenarios** and **11 UNSAT assertions** passed within the
documented bounds. New `TaskView`: **2,617 generated, 898 distinct states, depth 7**;
`TaskViewLate/Retained/Invalid` violate `CurrentView` or `ValidatedView` as intended.
No translation warning, changed reservation rule, new fairness condition or universal proof.
The existing relational and server authorization models remain unchanged.

## Completed application verification

Final expanded command:

```bash
npm run test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/pages/SocialEventWorkspacePage.test.tsx \
  src/pages/EventTaskPage.test.tsx \
  src/pages/SocialEventDetailPage.test.tsx \
  src/api/eventOperations.test.ts \
  src/utils/eventTaskRoutes.test.ts \
  src/api/client.test.ts \
  src/features/featureRegistry.test.ts \
  src/routes/AppShell.test.tsx
```

**115 tests / 8 suites pass**, exit 0, 17.323 seconds. Covers exact captures, unsafe/ambiguous
links, strict malformed/cross-target DTO rejection, no broad reads, loading/logout, refresh
denial/retry, stale success/failure, account change, same-party token rotation, unmount,
invalidation before React rerenders and StrictMode's canceled initial read. The ordinary
event overview, shared HTTP transport, registered route and AppShell remain covered.

`npm run typecheck:ui` passed twice, including the final dispatcher. Focused ESLint over
all 11 changed/new TypeScript files passed with `--max-warnings=0`. The first lint run found
an overly broad interpolation type in the test translator; it was narrowed to string/number,
not suppressed. The initial wrapper's missing test-i18n warning disappeared when the ordinary
overview was left unchanged and routing moved to the lightweight dispatcher.

Node syntax checks for the browser/config, shell syntax and `git diff --check` passed.
`npm run quality:repo` completed with exit 0 and **143 tests** (8 + 42 + 4 + 61 + 23 + 2 + 3).
After staging the new files, `npm run audit:formal -- --fail-on error` was repeated so its
tracked-file scan included the new sources: **9,634 findings, 0 critical, 0 errors, 355
warnings**. This static heuristic is not TLC/Alloy. Repository Git/release fixtures operate
on disposable local repositories, not the application's remote branches or production.
All 73 relative links in the six checked specification/index documents resolved.
No current-head full UI build/full Jest/full UI lint, backend tests, migrations or native
mobile tests are claimed. No changes to backend, SQL, generated clients, shared feature
registry, package lock, mobile pointer or the legacy `SocialEventDetailPage` remain in the diff.

## Browser evidence and unresolved gate

```bash
./node_modules/.bin/playwright test --config=playwright.event-task.config.mjs
```

Uses the existing isolated local Vite runner on port 4191, synthetic data, blocked foreign
HTTP, one worker, desktop/phone Chromium, unchanged 8-second assertions and zero retries.
This is browser/API-fixture integration, **not real server/database authorization E2E**.

- Sandbox attempt could not bind the local port (`EPERM`); the authorized local run followed.
- First run: **2 passed, 6 failed** on initial-load timeouts. Several failure snapshots already
  contained the expected content. A trace recorded task GET completion in 33.616 ms after
  several seconds of application/module bootstrap; this does not establish the sole root cause.
- Separating the dispatcher from legacy overview imports: **5 passed, 3 failed**. One phone
  startup timeout remained; two failures were an incorrect fixed read count of 3 versus 4.
  Development StrictMode starts an aborted request before the mounted read. Its behavior was
  verified in a rendered unit test. Browser assertions now permit only 1–2 observed initial
  reads and require exactly one additional GET per refresh and per retry. StrictMode, privacy
  assertions, timeouts and retries were not disabled or relaxed.
- Final run: **5 passed, 3 failed**, exit 1, **142.261 seconds**, no skips or retries. All four
  phone scenarios passed. Desktop malformed-response rejection passed; desktop initial RACI,
  invalid/repeated selector and English-empty-policy journeys exceeded the original 8-second
  startup assertion. Timing variability remains unresolved; this is not a green browser claim.

The final phone keyboard-recovery journey passed its no-parent-read, exact refresh/retry
count and axe checks (zero serious/critical violations). Its screenshot was generated and
visually inspected at
`artifacts/event-task-playwright/test-results/event-task-view-task-only--cb75e-thout-parent-reads-critical-chromium-phone/event-task-raci.png`.
Artifacts are ignored local outputs, not committed/uploaded by this turn. They do not establish
full accessibility compliance, desktop acceptance, native mobile behavior or search performance.

## Security, migration, rollback and remaining work

The feature flag, server authority, database and providers remain unchanged; no schema or
client regeneration is needed. The task path exposes no parent/private profile data and
never surfaces raw errors. A displayed authorized receipt is not remotely revoked by push;
refresh reauthorizes. Cookie identity, shared auth-expiration notifications and unrelated
legacy logistics caches are outside this client fence's guarantee.

Rollback the web dispatcher/link, task component and optional client arguments as one reviewed
revert; leave the existing canonical read API and records intact. No data migration or financial
rollback is involved. No merge, deployment, credential/provider activation or real-money action.

Next: resolve and measure cold application startup under the unchanged browser gate before
accepting this increment; then add canonical rich task fields and authorized RACI/task commands
with aggregate concurrency. Full workspace, templates/logistics, engagement/contracts/payments,
offline collaboration and mobile remain incomplete. The mobile submodule is still uninitialized
at `53569fc4baa842a6882235d9a12c4ee68c44ff24`; this is not mobile verification. Prior full UI
CourseRegistrations, bundle-budget and global-lint limitations are not cleared here. Hosted CI
must be checked against the published head; no hosted pass is claimed in this report.
