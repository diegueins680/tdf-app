# PR 25 — authorized, paginated RACI editor context

## Scope and dependency

Branch `feat/event-raci-editor-context`, based on DRAFT
[PR 384](https://github.com/diegueins680/tdf-app/pull/384), exact base
`bfd5f17b92d629f578bc02f4aee338ed15843806`. No merge, deployment, production feature activation
or real-money action. This is a prerequisite for the editor, not the editor itself.

Audit of the parent API found that its task DTO intentionally omits mutation authority,
assignment windows and eligible recipients. Inferring these from task visibility, RACI membership,
global people search or a cached role would be unsafe. The additive GET
`/event-operations/events/{eventId}/tasks/{activityId}/raci/context?afterPartyId=0` supplies
current manager authority, conservative unbounded source pairs and authorized recipient IDs.
It reuses canonical tasks, policy, RACI, grants, ownership and revision metadata; no second
event domain, identity directory, grant, task, receipt or audit record is created by this read.

## Requirements, formal checks and security

[EC01–06](raci-editor-context-contract.md) and the [traceability matrix](traceability-matrix.md)
define privacy, coherence, readiness, recipient scope, pagination and transport contracts.
Current task readers without management rights receive false readiness and empty options;
unreadable/foreign/absent targets remain opaque 404. Event.manage, coproduction or assignment
alone cannot enter the recipient list. No names, contacts, profiles, grants or invitation data
are returned. Optional cursor is an exclusive safe Party ID, default zero; pages contain at
most 100 unique ascending IDs and a continuation only if another eligible row exists.

The SQL function locks feature, event authorization and exact task metadata, then uses one fresh
clock and one statement snapshot. It never writes/repairs state or acquires the task-write fence.
Current session identity is revalidated inside the existing authenticated transaction. Strict
Haskell and web decoders reject foreign, malformed or contradictory projections; revision is
exact decimal text. Success and handler-generated errors carry `private, no-store`; framework
parsing/authentication errors retain their existing behavior. Public execution of the private
actor-selecting SQL function is revoked; rollout must separately provision the intended DB role.

Formal verification completed **before feature code**: **22 positive TLC configurations,
48 named negative controls, 13 PlusCal integrity tests, 2 SAT Alloy scenarios, 13 UNSAT
assertions**. New context model: **3,724 generated / 1,584 distinct states, depth 9**. The
contract records exact commands, finite bounds and correction of the initial latch-expression
model error. No fairness/liveness, universal proof or automatic SQL refinement claim is added.

## Executed local verification (2026-09-15)

- `sh scripts/test-event-raci-editor-context-migration.sh`: passed in an owned, disposable,
  network-isolated PostgreSQL 16 container. Double apply, double rollback, reapply, canonical
  data preservation, no read mutation, owner/manager/reader/outsider, scope/downgrade/revocation,
  future/expired grants, timed source exclusion, expired required RACI, unsupported lifecycle,
  disabled flag, exclusive pagination with 105 additional parties, and maximum BIGINT text.
  A second complete run also verifies that PUBLIC has no execution privilege on the function.
  Observed exact metadata blockers: READ COMMITTED sees the new revision and expired RACI;
  REPEATABLE READ and SERIALIZABLE reject the stale snapshot with SQLSTATE 40001. Separate
  blocked-read expiry cases deny an expired manager, omit an expired recipient and retain
  read-only access after management expires. These are six deterministic races, not a load test.
- `bash scripts/test-event-operations-schema-rehearsal.sh`: passed on the authoritative
  PostgreSQL 17 schema/production manifest baseline, **without** the diagnostic workaround.
  Complete ordered chain applied twice, asserted, rolled back and reapplied; legacy rows and
  immutable history preserved and final schema/ledger verifier passed. The new function stays
  outside the production migration manifest.
- `sh scripts/test-event-operations-http.sh`: **91 examples, zero failures**, including four
  inherited 100-case QuickCheck properties. Stack/GHC 9.10.3, real authenticated HTTP subrouter
  and disposable PostgreSQL 16. New cases cover manager/reader/outsider contexts, no-store,
  cursor/target errors, missing-function 503 with legacy task GET intact, and token revocation
  after production authentication. Strict database decoder checks include malformed/contradictory
  options and pagination. This is not a full application/backend build.
- `npm run generate:api:ui`: passed, openapi-typescript 7.10.1; additive route/schema/generated
  types. `npm test --workspace=tdf-hq-ui -- --runTestsByPath src/api/eventOperations.test.ts
  src/pages/EventTaskPage.test.tsx src/pages/SocialEventWorkspacePage.test.tsx
  src/pages/SocialEventDetailPage.test.tsx`: **185 tests in four suites**, including 40 new
  context cases. Explicit bearer/cancellation/cursor, exact BIGINT, reader/non-ready shapes,
  malformed/unknown/duplicate/unsafe/unsorted options, sentinel binding and no automatic paging,
  fallback, retry or write. Existing pages remain unchanged.
- `npm run typecheck:ui`: passed. From `tdf-hq-ui`,
  `../node_modules/.bin/eslint src/api/eventOperations.ts src/api/eventOperations.test.ts
  --max-warnings=0`: passed.
- `node --test scripts/__tests__/event-raci-editor-context-runner.test.mjs
  scripts/__tests__/event-operations-http-runner.test.mjs
  scripts/__tests__/event-operations-schema-rehearsal.test.mjs
  scripts/__tests__/event-raci-reassignment-runner.test.mjs
  scripts/__tests__/event-task-revisioned-read-runner.test.mjs`: **19 tests passed**. Guard
  checks enforce owned fixtures, formal negative controls, mandatory CI and migration order.
- `npm run quality:repo`: **146 tests passed**, no generated fixture drift. Separate heuristic
  audit: 9,731 findings, zero critical/errors, 355 warnings, 9,376 informational findings.
  Synthetic release/loop tests do not deploy or modify the real repository's main branch.

## Schema, compatibility and rollback

`2026-09-15_event_raci_editor_context.sql` adds one private SECURITY INVOKER function with
fixed search path. No tables, columns, indexes or existing task/command JSON change.
`2026-09-15_event_raci_editor_context_rollback.sql` drops only that function, without CASCADE;
retains all assignments, monotonic revisions, accepted receipts and immutable audit history.
Both migrations are transactional and time-bounded. Deployment compatibility remains additive.

Disable/remove a future UI entry point before removing the route or function. Missing SQL
returns sanitized unavailable (503); it never falls back to global discovery or assumes
management permission. Existing task reads and RACI commands remain separate. Do not roll
back unrelated parent migrations or delete accepted work as a context rollback.

Example for an already authorized **local disposable/sandbox** fixture only (not executed
against a live account; token and IDs are placeholders):

```sh
curl --fail-with-body \
  -H 'Authorization: Bearer <test-session-token>' \
  'http://127.0.0.1:8080/event-operations/events/82/tasks/8200/raci/context?afterPartyId=0'
```

## Limitations and next dependency

Options are advisory, not an authorization or consent token. Each page is freshly authorized;
cross-page roster consistency is not promised. Time expiry does not itself advance the stored
revision. `operationReady` means the conservative operation guards pass, not that a valid swap
necessarily exists. The editor must omit source/duplicate-role candidates and the existing
command must recheck current access, source/target, expected revision and all final constraints.

The recipient response is bounded, but the grant scan and source-pair array are not proven
constant-cost. No new index, latency benchmark, admission/rate limit or distributed cache is
claimed. No new UI controls, browser E2E, screenshots, accessibility audit, native mobile
generation/build, offline queue, notifications, consent or payment sandbox work was done here.
Next: scoped web selection, explicit confirmation and stale/conflict/ambiguous-result UX.
Naming and consent must not be inferred from recipient IDs or visibility.

Hosted checks are separate from local evidence. The new mandatory context PostgreSQL job
and formal controls are added without bypasses; hosted status must be inspected after
publication. No whole-project CI-green, final mission completion or production-readiness claim.

The inspected parent PR 384 head remains open/draft/unmerged at the exact dependency above.
Its formal, PostgreSQL, repository-quality and API-contract checks succeeded, but
`backend-quality`, `ui-quality`, `persona-web-e2e`, `hardcoded-list-audit`, aggregate `quality`,
Cloudflare Pages and Vercel failed. Causes were not established in this increment; no waiver,
rerun or corrective claim is made. Parent check status is not the new branch's check status.
