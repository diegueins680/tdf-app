# PR 24 — authenticated RACI API and typed client

## Scope and dependency

Branch `feat/event-raci-reassignment-api`, based on DRAFT
[PR 383](https://github.com/diegueins680/tdf-app/pull/383), exact base
`f6ffb2a27b87ed42d4d49289024d5b8a612b94cf`. No merge, production deployment or activation.

Adds `POST /event-operations/events/{eventId}/tasks/{activityId}/raci/reassign`, strict
Haskell command/outcome DTOs, OpenAPI/generated web types and `EventOperations.reassignRaci`.
Reuses the canonical private SQL command and current authenticated session transaction.
No duplicate event domain, migration, UI control, mobile client, consent or notification flow.

## Requirements and security

[RA01–05](raci-api-contract.md) bind current identity, exact command receipt, transaction
completion, typed client context and backward compatibility. SQL response decoding happens
inside `SqlPersistT` before commit, including event/task/key/role/parties and exact expected+2
revision. Unknown errors, malformed/foreign receipts and commit faults become sanitized 503.
Accepted responses and handler-generated errors carry `private, no-store`; framework parse/auth
errors retain existing behavior. The body cannot select actor/time/hash or confer permissions.
The opaque revision stays decimal text; arithmetic uses Integer/BigInt, not binary float.

The full pinned formal suite passed before feature code: **21 positive TLC configurations,
45 named negative controls, 13 PlusCal integrity tests, 2 SAT Alloy scenarios and 11 UNSAT
assertions**. `CommandBoundary` generated 14 states, depth 4; early-commit/unbound-receipt
mutations both returned exit 12 for `ValidatedCommit`. Existing `SessionFence`,
`RaciReassignment` and relational models remain applicable. Bounds, trusted SQL assumptions,
absence of a new liveness claim and exact tool command are documented in the contract.
These checks are finite evidence, not universal or SQL refinement proofs.

## Executed local verification (2026-09-15)

- `sh scripts/test-event-operations-http.sh`: final **86 examples, zero failures**, including
  four 100-case QuickCheck properties. Real Stack/GHC 9.10.3 auth/subrouter and disposable
  PostgreSQL 16; valid command/replay/stale/forbidden/opaque target, strict fields, malformed
  post-write receipts with verified row/audit/receipt rollback, deferred COMMIT failure,
  simultaneous HTTP retries, unsupported lifecycle, disabled feature and missing SQL function.
  Real HTTP revocation after production authentication rejects both new RACI and accepted
  receipt replay. Existing event/task/session tests remain intact. The first compile attempt
  identified a missing `catchError` import; corrected builds passed, first 81, then 84, then
  the final 86 examples (not additive totals).
- `npm run generate:api:ui`: regenerated with openapi-typescript 7.10.1. Additive route,
  command/outcome schemas and error codes; no native mobile generation performed.
- `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/api/eventOperations.test.ts`:
  **110 tests passed**. Includes 32 new RACI cases: strict input/result binding, exact maximum
  BIGINT replay, original request capture, explicit bearer/signal/key and no automatic retries.
- Four-suite run adding `src/pages/EventTaskPage.test.tsx`,
  `src/pages/SocialEventWorkspacePage.test.tsx`, `src/pages/SocialEventDetailPage.test.tsx`:
  **145 tests passed**, including the preceding 110, not additional to them. No page switches
  to mutation behavior or bypasses the existing read API.
- `npm run typecheck --workspace=tdf-hq-ui`: passed. Focused ESLint from `tdf-hq-ui`,
  `../node_modules/.bin/eslint src/api/eventOperations.ts src/api/eventOperations.test.ts
  --max-warnings=0`: passed.
- `node --test scripts/__tests__/event-operations-http-runner.test.mjs
  scripts/__tests__/event-raci-reassignment-runner.test.mjs
  scripts/__tests__/event-task-revisioned-read-runner.test.mjs
  scripts/__tests__/event-operations-schema-rehearsal.test.mjs`: **17 tests passed**.
  Both HTTP runners include the private SQL prerequisite; formal pre-commit/binding controls
  remain mandatory. Existing CI selects the real backend/HTTP and generated-contract gates.
- `npm run quality:repo`: **146 tests passed**, no generated fixture drift. Separate heuristic
  audit: 9,698 findings, zero critical/errors, 355 warnings and 9,343 informational findings.
  Synthetic local release/loop fixtures are not production actions or model checking.

No full application backend build, native mobile build, new browser E2E, accessibility audit,
payment sandbox, performance benchmark or full-schema rehearsal was run for this API-only
increment. SQL migrations are unchanged; PR 383 records their independent PG16/PG17 evidence.

## Rollback and manual steps

Disable/remove the new route/client entry point before rolling back the private SQL function.
The endpoint fails 503 if its prerequisite is missing; it does not use a legacy write fallback.
Prior read routes remain available. Rollback retains accepted RACI/audit/receipt history and
monotonic counters; never delete receipts or rewrite accepted assignments as an API rollback.
The production manifest, credentials, payment behavior and feature activation remain unchanged.
Verify least-privilege application DB execution grants during any separately approved rollout.

Example for an already authorized **local disposable/sandbox** fixture only; identifiers and
token below are placeholders, not a request executed against a live account:

```sh
curl --fail-with-body -X POST \
  -H 'Authorization: Bearer <test-session-token>' \
  -H 'Content-Type: application/json' \
  -H 'Idempotency-Key: 60000000-0000-4000-8000-000000000300' \
  --data '{"expectedRevision":"4","role":"responsible","fromPartyId":3,"toPartyId":2,"reason":"Reasignación de prueba","correlationId":"local-raci"}' \
  http://127.0.0.1:8080/event-operations/events/82/tasks/8200/raci/reassign
```

## Hosted status and remaining limitations

The inspected PR 383 head had successful formal and PostgreSQL gates, but failed
`persona-web-e2e`, `hardcoded-list-audit` and Cloudflare Pages; `backend-quality` was still
running at that snapshot. Failure causes were not established in this API increment, and
no waiver, rerun, merge or CI-green claim is made. Current branch hosted results are separate
from local evidence and must be reviewed after publication.

Next: scoped editor/recipient eligibility and conflict UX, explicit consent/notification,
timed RACI and collaborator-removal workflows, native mobile/offline support, admission/body/
rate limits and broader lifecycle effects. After an ambiguous network failure, preserve the
original command/key for verification; client rejection cannot undo a server commit.
