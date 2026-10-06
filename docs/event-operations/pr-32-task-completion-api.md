# PR 32 — authenticated preparation-task completion API

## Scope and dependency

Branch `feat/event-task-completion-api`, based on draft #407 at
`78bdd4df465d1c7d8a752f9df0ed714ecad2330d`. The worktree started clean. No unrelated
worktree, production manifest, migration, provider, credential or feature flag changed.

Adds `POST /event-operations/events/{eventId}/tasks/{activityId}/complete`, strict
Haskell command/receipt types, an in-transaction decoder, additive OpenAPI and
generated web declarations. Reuses the canonical SQL command and current session
fence. No second task domain, manual web command helper, UI control or mobile flow.

## Requirements, security and formal evidence

[CA01–05](task-completion-api-contract.md) refine EO-026–028, EO-045/051 and EO-052–058.
Only current authenticated identity selects the actor; the body cannot carry actor,
hash, clock, grants or overrides. The same transaction revalidates the session,
executes SQL and validates its receipt before COMMIT. Receipt event/task/key,
completed status, activity INTEGER range and exact expected+1 BIGINT revision are
checked. Strict malformed/foreign/unknown-error responses abort staged effects;
commit/SQL failures produce sanitized 503, never a success-shaped placeholder.

Current read permission is required for original historical replay; replay does
not re-certify current RACI or compare against a later task revision. New commands
still enforce strict policy, lifecycle, current A/R and completed prerequisites.
Handler responses are private/no-store. Framework parse/auth errors retain existing
behavior. No silent retries/rebase/compensation, authorization expansion or live
event effects. Recognized SQL no-write rejections remain trusted.

Before feature code, the complete pinned suite passed exit 0: **24 positive TLC
configurations, 60 named negative controls, 13 PlusCal integrity tests, 2 SAT Alloy
scenarios and 13 UNSAT assertions**. Models were reused unchanged: `TaskCompletion`,
`SessionFence`, `CommandBoundary`, receipts/revisions and scoped Alloy relations.
Their exact bounds, command and limitations are in the contract. This is bounded
evidence, not a universal proof or proof of application/database refinement.

## Executed verification (2026-09-16)

```sh
sh scripts/test-event-operations-http.sh
node --test scripts/__tests__/event-operations-http-runner.test.mjs \
  scripts/__tests__/event-task-completion-runner.test.mjs \
  scripts/__tests__/ci-change-scope.test.mjs scripts/__tests__/ci-pipeline.test.mjs
npm run generate:api:ui
npm test --workspace=tdf-hq-ui -- --runTestsByPath src/api/eventOperations.test.ts
npm run typecheck --workspace=tdf-hq-ui
npm run quality:repo
```

- Final HTTP run: **107 examples, zero failures**, including five 100-case
  QuickCheck properties, real Stack/GHC 9.10.3 auth/subrouter and disposable PG16.
  Covers strict inputs, permissions/privacy, current dependencies/RACI/lifecycle,
  exact replay, simultaneous retries, nine malformed post-write SQL receipts,
  verified rollback of task versions/status/audit/receipts, deferred COMMIT fault,
  revocation after real authentication for new completion and historical replay,
  feature disabled and missing SQL, real HTTP maximum BIGINT receipt/replay,
  authenticated cookie transport and legacy integer overflow. Existing session/
  isolation tests remain intact. Initial run passed 106 examples before the final
  boundary case was added; these totals are not additive.
- **32 runner/CI regression tests passed**; both local/hosted HTTP runners install
  the SQL prerequisite before the fixture. Reduced fixture adds only the real
  canonical `updated_at` column missing from its skeletal activity table.
- Generated web types with openapi-typescript 7.10.1. Structured OpenAPI parsing
  validated exact response descriptions and strict command/receipt schema bounds.
- Existing web API suite: **150 tests passed**. No hand-written client behavior
  changed. The complete web TypeScript check passed (exit 0); it was slow but
  completed without termination, skipped checks or compiler-option changes.
- Repository gate passed: **147 tests**, deterministic fixture verification and
  heuristic audit (9,775 findings: zero critical/errors, 356 warnings, 9,419 info).
  The heuristic audit is not TLC/Alloy model checking. HTTP containers were removed
  automatically; no external database was targeted.
- Shell syntax and `git diff --check` passed. No SQL production migration changed;
  #407 independently records the PG16 race/rollback and authoritative PG17
  complete-schema rehearsal. Those checks were not rerun as part of this API-only
  increment. No full application backend build, native mobile build/generation,
  new browser E2E, accessibility or performance audit was run here.

Parent #407's inspected exact head passed hosted formal, all PostgreSQL including
completion/full-schema, repository, migration and persona browser checks. Its
catalog-list audit failed; backend and real-RACI browser were still running at
that snapshot. These are not this branch's hosted results, and no all-green,
waiver, rerun, review or merge claim is made. Repository tests use synthetic local
Git/release fixtures, not production actions.

## Rollback and remaining work

Remove/disable the new route before removing its SQL prerequisite; absent SQL
returns 503 without falling back to legacy writes. Preserve completed work,
versions, keys, receipts and immutable audit. Existing read routes are unchanged.
Production activation requires separate reviewed migration/feature/least-privilege
DB grants; none is performed by this PR.

Next: request-capturing typed command client and explicit scoped review/completion
UX. Required approvals/evidence, justified override, reopening, live-event effects,
notifications, retention/removal, mobile and offline queues remain separate work.
An ambiguous network result cannot be undone by client rejection: retain the
original request/key for an authorized exact retry. No new duplicate-JSON-key,
global body/rate limiter or legal/financial behavior is claimed.

Example for an explicitly enabled **local disposable fixture only** (not executed
against a live account; token and IDs are placeholders):

```sh
curl --fail-with-body -X POST \
  -H 'Authorization: Bearer <test-session-token>' \
  -H 'Content-Type: application/json' \
  -H 'Idempotency-Key: 60000000-0000-4000-8000-000000000400' \
  --data '{"expectedRevision":"4","reason":"Preparación de prueba","correlationId":"local-completion"}' \
  http://127.0.0.1:8080/event-operations/events/83/tasks/8300/complete
```
