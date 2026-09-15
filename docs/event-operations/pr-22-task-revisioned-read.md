# PR 22 — coherent opt-in task revision read

## Scope and dependency

Branch: `feat/event-task-revisioned-read`.
Depends on DRAFT [PR 379](https://github.com/diegueins680/tdf-app/pull/379), branch
`feat/event-task-aggregate-revision`, inspected base commit
`3b1ce4d9dbd0efb6d1e354ffd9f58e92a6916320`. Do not merge or activate production.

Adds `GET /event-operations/events/{eventId}/tasks/{activityId}/revisioned`:
`{ "task": <unchanged existing task JSON>, "aggregateRevision": "4" }`.
Old clients reject extra JSON fields, so the new envelope is explicit opt-in. It reuses
the same canonical task, policy, RACI, scope predicate and projector, not a second domain.
No existing page switches endpoints and no editor/offline write queue is activated.

The aggregate token uses canonical positive decimal text through SQL, strict Haskell DTO,
OpenAPI/generated TypeScript and the validating `EventOperations.taskWithRevision` client.
The maximum signed BIGINT is supported without JavaScript rounding. IDs and old numeric
versions retain their existing safe-integer restriction. The token is not authority,
readiness or immutable history; time-window expiry does not itself advance stored metadata.

## Formal contract before feature code

[RR01–05](task-revisioned-read-contract.md) map coherent projection, post-wait authorization,
exact transport, old-client compatibility and side-effect-free reads to models and tests.
`TaskRevisionRead` models staged writes, metadata locking, revision capture and projection.
The unchanged `TaskRead`, `TaskReadStructure.als` and `SessionFence` cover scoped relations
and current identity. No new relation or permission is introduced.

The full pinned command in the contract completed with exit 0 before feature implementation:
19 positive TLC configurations, 37 expected named counterexamples, 13 PlusCal integrity
tests, 2 SAT Alloy scenarios and 11 UNSAT assertions. The new positive model generated
400 states / 192 distinct, depth 11. Its mixed-snapshot and early-authorization mutations
each returned exit 12 for their precise invariant. Finite scopes, MVCC assumptions and
absence of a new liveness claim are explicit; these results are not universal proofs.

## Security and database behavior

The server keeps existing bearer/cookie authentication and the same transaction-local
session fence; no client-supplied actor/time is accepted. Lock order is current session,
enabled feature, event authorization, then exact-task metadata `FOR SHARE`. Scope is
checked early and rechecked by the original canonical projector after the metadata wait.
The metadata fence prevents committed tracked mutations from splitting task/revision reads;
it does not lock the entire event's task-write fence or repair absent metadata on GET.

Wrong actor/event/task, missing records and unreadable tasks remain opaque. Database faults
and malformed envelopes are sanitized 503, never success. Every successful envelope has
`Cache-Control: private, no-store`; the web method preserves captured bearer and abort signal.
The revision adds no contacts, names, agreements or private task fields. Trusted SQL callers
remain responsible for server actor identity; SECURITY INVOKER is not database-user auth.

## Migration and rollback

Apply `2026-09-15_event_task_revisioned_read.sql` after foundation/API/task-commit/task-read
and `2026-09-15_event_task_revision.sql`, with existing rollout controls and review. The only
schema addition is one function. No records, indexes or ORM entities are rewritten.
Both HTTP fixture runners and the complete-schema rehearsal include that prerequisite order.
The production migration manifest is unchanged and provider flags/credentials are untouched.

Rollback: remove/disable the new route first, then apply
`2026-09-15_event_task_revisioned_read_rollback.sql`. It drops only the new function, leaving
old reads, monotonic counters/triggers, tasks and immutable records intact. If the route is
still called during DB rollback it returns unavailable; it never silently falls back. In a
full chain rollback, remove this function before prior revision/read functions. Roll-forward
does not reset counters or silently enable the feature.

Shared read locks can delay task writers; avoid high-frequency polling or long transactions.
READ COMMITTED uses the post-wait committed state; stale RR/SERIALIZABLE reads may fail with
40001. No automatic rebase/retry or guaranteed latency is added. Operational timeout tuning
and production load verification remain deployment prerequisites.

## Executed verification

All database tests below create their own disposable database; no live application DB or
financial provider was contacted.

- `sh scripts/test-event-task-revisioned-read-migration.sh`: PASS, exit 0. Exact envelope,
  opaque scope, absent metadata without repair, BIGINT boundary, no record mutation,
  double apply/down and reapply. Three writer-first races (RC/RR/Serializable), an instrumented
  inter-statement reader-first race and expiry during a metadata wait all used observed
  `pg_blocking_pids` barriers. An unrelated same-event task remains writable. RR/Serializable
  logs contain actual 40001 failures at metadata locking; RC returns the matching new state.
  Local logs: `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-task-revision-read.TTjLxD/`.
- `bash scripts/test-event-operations-schema-rehearsal.sh`: PASS, exit 0, authoritative
  PostgreSQL 17 schema without diagnostic workaround. Apply twice, rollback chain and
  reapply retain legacy records, audit/receipt/RACI history, counters and production ledger;
  final full-schema verifier and disabled-on-roll-forward assertions passed.
- `sh scripts/test-event-operations-http.sh`: PASS, exit 0, **73 examples, zero failures**,
  including three 100-case QuickCheck properties. Compiled real auth/router/handler with
  Stack/GHC **9.10.3**, resolver `lts-24.42`. Exact old/new JSON, 400/401/404/503 boundaries,
  lost authority, maximum revision and real in-flight post-auth revocation are exercised.
  This is the real scoped subrouter, not a full application build or deployed E2E test.
  The final rerun also passed all 73 examples after adding real HTTP rejection of an
  invalid nested task status on the new route; database diagnostics remained sanitized.
- `npm run generate:api:ui`: completed with openapi-typescript 7.10.1; generated web types
  contain only the additive route/schema/operation changes. No native mobile generation run.
- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/api/eventOperations.test.ts`:
  PASS, **78 tests**. Exact tokens, numeric/Unicode/overflow/newline rejection, nested strict
  validation, target binding, no-store/context, unchanged old route, no fallback or mutation.
- `node --test scripts/__tests__/event-task-revisioned-read-runner.test.mjs
  scripts/__tests__/event-task-revision-runner.test.mjs
  scripts/__tests__/event-operations-http-runner.test.mjs
  scripts/__tests__/event-operations-schema-rehearsal.test.mjs`: PASS, **16 tests**.

- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/EventTaskPage.test.tsx
  src/pages/SocialEventWorkspacePage.test.tsx src/pages/SocialEventDetailPage.test.tsx
  src/api/eventOperations.test.ts`: PASS, **113 tests in four suites** (includes the 78
  API cases above, not 113 additional tests). Existing task/overview routing remains intact.
- `npm run typecheck --workspace=tdf-hq-ui`: PASS, exit 0.
- From `tdf-hq-ui`, `../node_modules/.bin/eslint src/api/eventOperations.ts
  src/api/eventOperations.test.ts --max-warnings=0`: PASS, exit 0. The first attempt used
  a nonexistent workspace-local binary; the corrected command uses the installed root tool.
- `npm run quality:repo`: PASS, **146 tests**; heuristic audit 9,659 findings,
  zero critical/errors, 355 warnings and 9,304 informational findings. This heuristic is
  distinct from TLC/Alloy. Repository/release tests use local fixtures, not live deployments.
- Shell syntax, whitespace and workflow/OpenAPI YAML parsing checks passed. The OpenAPI
  revision pattern also rejects final newlines and leading zeroes. No generated fixture drift.
  All 92 checked relative links in the changed specification/evidence indexes resolved.

Hosted checks are separate from local evidence; no hosted pass, full backend build, mobile build, browser
screenshot, accessibility audit or production performance result is implied here.

Example (already authorized **local disposable/sandbox** event; requires separately reviewed
local feature activation; token is a placeholder, not a credential):

```sh
curl --fail-with-body \
  --header 'Authorization: Bearer <test-session-token>' \
  http://127.0.0.1:8080/event-operations/events/80/tasks/8000/revisioned
```

Remaining product work: authenticated/idempotent task and RACI commands, mutation audit,
approval/override workflows, editor conflict handling, native mobile adoption and offline
synchronization. Broader logistics, marketplace and financial phases remain incomplete.
No merge, production deployment, credential change, provider activation or money movement.
