# PR draft: Existing event logistics transaction hardening

## Scope and dependency

This local PR-equivalent branch depends on
`b6ba963623861d1e2804b18adb46e6e4f5f5be80` (`feat/event-operations-api-foundation`). It fixes one
high-risk partial-write boundary in the already shipped social-event logistics implementation. It
does not add a competing task or logistics service, change routes, change DTOs, or expose the new
RACI sidecar.

## Requirement and invariant

EO-023–028 and EO-055 require deterministic concurrent task outcomes and reconcilable projections.
Previously, create/update committed `event_logistics_activity` in one `runSqlPool` transaction and
replaced `event_logistics_assignment`/`event_logistics_dependency` in a second transaction. A
dependency constraint or database cycle guard could therefore fail after the visible activity row
had committed.

The handler now performs the activity insert or optimistic compare-and-swap and the complete
assignment/dependency replacement in one `SqlPersistT` transaction. If a relation fails, PostgreSQL
restores the activity and its former relations. Route-provider verification remains after commit and
is explicitly outside this correction.

## Schema, API, security, and compatibility

- No schema or API contract changes.
- No migration or production-manifest change.
- Existing authorization and input validation are retained.
- Database cycle/event-local guards from the formal foundation remain the final concurrent safety
  boundary.
- Web and mobile payloads remain compatible.

## Verification executed locally

- `npm run test:event-operations-foundation-migration`: PASS on ephemeral PostgreSQL 16, now including:
  - activity update plus reverse dependency cycle fails with original status/version/dependencies;
  - activity create plus cross-event dependency fails with no inserted activity.
- Haskell `-fno-code` target typechecked 187/187 modules, including the changed handler and aggregate
  server. Cabal then started the same pass again because it expected artifacts intentionally omitted
  by `-fno-code`; the repetition was interrupted with exit 130. The wrapper is not claimed as PASS.
- Shell/YAML/JSON/whitespace static checks are recorded in the implementation report after final run.
- Remote CI was not run because GitHub DNS/auth remains unavailable.

## Rollback and remaining limits

Application rollback is the prior handler implementation; there is no database rollback. Rolling
back reintroduces the partial-write risk and should be limited to an emergency compatibility case.

This PR does not solve activity hard-delete history, RACI API writes/views, task acceptance evidence,
templates, recurrence, workstreams, checklists, run of show, procurement, inventory custody, or
external route-verification compensation. Those remain dependency-ordered phase-4 work.
