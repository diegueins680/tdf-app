# PR 26 — scoped RACI web editor

## Scope and dependency

Branch `feat/event-raci-web-editor`, based on draft
[PR 387](https://github.com/diegueins680/tdf-app/pull/387), exact head
`13abde1cba82b7b79c21b6f35e87c91de676366e`. The worktree was clean at branching;
unrelated worktrees were not changed. No merge, deployment, credentials, provider or production
feature activation. No new backend, API, generated client, database migration or event domain.

Adds `EventRaciEditor` to the existing `/social/eventos/:eventId?tarea=:activityId` subview.
Only an explicit “Preparar reasignación” click loads PR 25's authorized context. Current
manager/readiness, unbounded source pairs and eligible recipient IDs drive native labeled
selects. Pages replace prior options and clear drafts. A required reason leads to a separate
review dialog with exact target, role, parties, revision, reason and request key.

Confirming sends the existing canonical command. No optimistic success, automatic retry,
permission expansion, parent-event query, profile lookup or last-write-wins rebase. Success
requires the strict API client's exact-bound receipt. The task table remains its previous
read until the user explicitly retrieves it again; success is not fabricated by changing it.

## Requirements, security and recovery

[EW01–07](raci-web-editor-contract.md) map to `RaciWebEditor`, existing scoped models and
[executable traceability](traceability-matrix.md). A synchronous phase guard prevents duplicate
confirmation before React rerenders. The generation-keyed parent remounts on task, session,
credential or refresh change. Explicit captured bearer and cancellation signal accompany requests;
late results cannot update a different session or task. Reasons/keys/receipts stay in memory,
not shared query caches, analytics or storage. Secure UUID generation fails closed if unavailable.

An initial HTTP 409 requires a fresh read and new review. Other errors or invalid receipts are
conservatively uncertain: retry only on a new user click, with the **same** key/body/revision.
A conflict after an uncertain attempt does not establish that the original request failed.
Pending/uncertain requests disable local navigation/refresh and cancellation, warn on browser
unload, display recovery details and explain the need for history review before a new request.
Other shell navigation, logout, browser crash and lost devices are not durable offline recovery;
session changes must still discard private data. These limitations are visible, not hidden.

IDs are deliberately shown without private names. This is organizer reassignment of an existing
role, not the recipient's consent, hiring, invitation, availability, notification or booking.
Only current unbounded assignments are editable. Already-visible same-role/source candidates
are excluded; unseen timed conflicts and all authority/state conditions are rechecked by SQL.

## Formal validation before feature code

The complete pinned suite passed: **23 positive TLC configurations, 53 named negative controls,
13 PlusCal integrity tests, 2 SAT Alloy scenarios and 13 UNSAT assertions**. New editor model:
**154 generated / 120 distinct states, depth 9**, with three generations, two revisions and
two abstract attempts. Five mutations expose missing confirmation/context/single-flight/exact
retry/receipt validation. Existing relational authorization models apply without new grants.

The contract records the exact command and initial parser/type/sandbox errors and corrections.
No failed model run was reclassified as a pass, no invariant weakened, and no feature code was
written before the complete successful run. This is finite evidence, not a universal proof,
network liveness guarantee, unbounded retry proof or automatic model-to-code refinement.

## Executed implementation verification (2026-09-15)

- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/EventTaskPage.test.tsx
  src/api/eventOperations.test.ts src/pages/SocialEventWorkspacePage.test.tsx
  src/pages/SocialEventDetailPage.test.tsx`: final **201 tests, four suites, zero failures**.
  Includes 16 new rendered editor cases: explicit opening/review/cancel, reason validation,
  read failure/retry, reader/non-ready states, page replacement, duplicate clicks, source/role
  filtering, missing secure UUID, exact command transport, initial/replay conflicts, uncertain
  exact retry, malformed success, logout/rotation/navigation/late results and English labels.
  Form/dialog axe checks have no serious/critical violations; JSDOM cannot assess contrast.
- `npm run typecheck:ui`: passed after final feature edits. Focused ESLint from `tdf-hq-ui`:
  `../node_modules/.bin/eslint src/components/events/EventRaciEditor.tsx
  src/pages/EventTaskPage.tsx src/pages/EventTaskPage.test.tsx src/i18n/locales/es.ts
  src/i18n/locales/en.ts --max-warnings=0`: passed. Initial lint findings prompted interface,
  cleanup-counter and explicit dialog-focus corrections, not rule suppression.
- `./node_modules/.bin/playwright test --config=playwright.event-raci.config.mjs --trace on`:
  final **12/12 passed** in Chromium desktop/phone, zero retries/skips. Owned loopback Vite
  server, synthetic session/context/command responses, foreign traffic blocked, unknown APIs
  fail closed. Tests include deliberate network loss and exact replay, keyboard confirmation,
  dialog axe (including browser contrast), task-read regressions, invalid selectors and English.
  An initial sandbox socket denial did not start a browser run; approved execution passed,
  and a second complete run verified the final lint/focus changes. No timeout was increased
  beyond the existing isolated config. Screenshots and traces were actually generated; both
  final review screenshots were visually inspected and are readable, with phone key wrapping.
  The shell's synthetic offline badge/radio bar remains visible; this is not production E2E.
- `node --test scripts/__tests__/event-raci-web-editor.test.mjs
  scripts/__tests__/event-raci-editor-context-runner.test.mjs`: **4 passed**. New formal trigger
  paths and named negative controls remain mandatory; isolated browser interception and prior
  task journeys remain selected. Default persona CI discovers the new `@critical` journey.
- `npm run quality:repo`: passed, **146 tests**, no generated fixture drift. This gate's
  final heuristic audit reported 9,765 findings, zero critical/errors, 355 warnings and 9,410
  informational findings; this is separate from TLC/Alloy. Release/loop tests use disposable fixtures,
  not live deployment or real main-branch reconciliation.

Ignored local evidence: `artifacts/event-raci-playwright/results.json` and `test-results/`
(traces, desktop/phone review and task screenshots). No screenshot upload or production capture.
No full UI build/full Jest/full lint, actual server/database browser E2E, backend recompile,
new migration run, native mobile build, payment sandbox or search/load benchmark was executed
for this UI-only increment. Parent SQL/API evidence remains separate and is not a new test run.

## Rollback and remaining work

Remove the editor import/render, local navigation lock and new component/labels to restore the
read-only view. Leave existing backend context/command APIs, accepted assignments, receipts,
revisions and immutable history intact. No schema rollback or feature flag change is needed.
Production activation remains separately reviewed and unauthorized in this task.

The inspected parent PR 387 head is open/draft/unmerged. Its formal and all event PostgreSQL
gates (including context/full schema), repository and API-contract gates succeeded; backend,
UI, persona browser, catalog list, aggregate quality, Cloudflare and Vercel checks failed.
Causes were not established here. No waiver or whole-project-green claim; current branch CI
must be reviewed separately after publication. Future increments must address integration
failures, full-stack verification, durable recovery, consent/notifications, timed responsibilities,
collaborator removal and native mobile. The overall end-to-end mission is not complete.
