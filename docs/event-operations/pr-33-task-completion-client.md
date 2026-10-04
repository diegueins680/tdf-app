# PR 33 — captured, validated task completion client

## Scope and dependency

Branch `feat/event-task-completion-client`, based on draft #410 at
`823da0fa801abe3bdb3b7679550f1315ad0946ed`. The worktree started clean. Adds
`EventOperations.completeTask` using the existing generated types, strict scalar
validators and shared POST transport. No new task domain, backend/API schema,
migration, production manifest, feature flag, provider, credential or UI change.

## Requirements and security

[CC01–05](task-completion-client-contract.md) refine EO-026–028, EO-045/051 and
EO-052–058. Reject unsafe IDs, noncanonical revisions, invalid keys, blank/oversize
text and unknown actor/override fields before dispatch. A fresh parsed scalar
snapshot binds the response to the original request even if the caller mutates
its object. Pin explicit credentials/key/signal, use no-store, validate strict
receipt shape, target, UUID, completed state and exact BIGINT expected+1 revision.
Invalid receipts return fixed Spanish diagnostics without raw response data.

Exactly one POST per invocation: no automatic read, retry, rebase or compensation.
Errors and cancellation propagate. Client rejection cannot establish server
rollback; callers must retain the exact original request/key for an explicit
authorized replay. No grant or current-readiness inference; the unchanged server
remains authoritative. An explicit second invocation is a distinct attempt.

## Formal checkpoint before feature code

Pinned TLC 1.7.2 / OpenJDK 21.0.12.1: `TaskCompletionClient.cfg` passed with
**44 generated / 32 distinct states, depth 4**. Four intentionally unsafe configs
each exited 12 with the required invariant: request capture → OriginalRequestSent;
shape and binding → ValidatedReceipt; retry → SingleDispatch. No unexpected
counterexample remains. The first sandbox attempt failed with exit 255 because
TLC could not open its local RMI socket; the approved rerun completed all checks.

Scope: one invocation, two request/receipt identities, valid/invalid shape and at
most two sends. No fairness, liveness or client rollback claim. Exact commands,
abstractions and limitations are in the contract. The unchanged server and Alloy
models reuse PR 32's recorded full-suite evidence; **the complete TLC/Alloy suite
was not rerun locally here**. The mandatory full CI runner now includes the new
model and four named negative controls (25 positive / 64 negative configurations).
Finite checking is not a universal proof or proof of JavaScript/SQL refinement.

## Executed verification (2026-09-16)

```sh
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/api/eventTaskCompletion.test.ts src/api/eventTaskCompletion.transport.test.ts \
  src/api/eventOperations.test.ts src/api/client.test.ts \
  src/pages/EventTaskPage.test.tsx src/pages/SocialEventWorkspacePage.test.tsx \
  src/pages/SocialEventDetailPage.test.tsx
node --test scripts/__tests__/event-task-completion-client.test.mjs \
  scripts/__tests__/event-task-completion-runner.test.mjs \
  scripts/__tests__/ci-change-scope.test.mjs scripts/__tests__/ci-pipeline.test.mjs
npm run typecheck --workspace=tdf-hq-ui
```

- **304 tests / seven suites passed**, including eight model-derived combinations
  of caller edits, response shape and receipt identity; exact limits, historical
  replay, foreign/malformed receipts, transport conflicts, explicit retry and
  cancellation. Earlier 245-test run is superseded, not additive.
- Four transport tests use the real shared client with **synthetic fetch**:
  synchronous caller mutation cannot change serialized body/credentials, conflict
  propagates, foreign receipt rejects, abort cleans pending activity. They are not
  live HTTP, browser or production checks.
- Existing page tests passed with React Transition `act(...)` warnings; these were
  not hidden, disabled or treated as accessibility evidence.
- **29 Node runner/CI guard tests passed**, no skips. Both workflow triggers watch
  new tests and the guard; the formal job requires the guard and new model checks.
- Full web TypeScript check passed (exit 0), with no compiler-option changes.
  Focused ESLint passed with `--max-warnings=0` for `eventOperations.ts` and both
  new completion test files, using `../node_modules/.bin/eslint` from `tdf-hq-ui`.
  Shell syntax, workflow YAML parsing and `git diff --check` also passed.
- No backend build, PostgreSQL migration/concurrency rerun, browser/mobile,
  accessibility, performance, payment sandbox or complete repository gate was
  executed in this client-only increment. Parent evidence is not a fresh result.

## Inherited hosted verification limitations

Inspected draft #410 at exact head `823da0fa801abe3bdb3b7679550f1315ad0946ed`:
formal, UI/repository, API contracts, safe install and complete-schema checks
passed, but these jobs failed:

- [Completion PostgreSQL](https://github.com/diegueins680/tdf-app/actions/runs/35131932026/job/104914780747).
- [Persona web E2E](https://github.com/diegueins680/tdf-app/actions/runs/35131932155/job/104915120130).
- [Catalog list audit](https://github.com/diegueins680/tdf-app/actions/runs/35131932145/job/104914780077).

Their root causes were not investigated in this increment. Backend and real RACI
browser were still running at this snapshot; mobile/migration jobs were skipped.
These are parent observations, not this branch's hosted results. No all-green,
waiver, review, merge, deployment or production-readiness claim is made.

## Rollback and next increment

Remove the unused helper/export aliases if needed; no data migration or destructive
rollback. Preserve accepted server receipts, history and completed task state;
never issue an inverse mutation to compensate for an invalid/uncertain response.

Resolve inherited verification failures and specify scoped completion readiness,
review, deliberate consent and current-view/session conflict handling before UI
activation. Approvals/evidence, override/reopening, live-event effects, native
mobile, durable offline queues and broader event operations remain unfinished.
