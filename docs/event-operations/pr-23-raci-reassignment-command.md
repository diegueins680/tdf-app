# PR 23 — private atomic RACI reassignment

## Scope and dependency

Branch: `feat/event-raci-reassignment-command`. Depends on DRAFT
[PR 381](https://github.com/diegueins680/tdf-app/pull/381), branch
`feat/event-task-revisioned-read`, inspected base commit
`503fd7b651843e9f7cadf4fd15931256bb847ece`. No merge or production activation.

Adds one private SQL command to replace a single current, unbounded `(party, RACI role)`
assignment during event draft/planning and task planned/confirmed. Reuses canonical tasks,
Party, RACI, task aggregate revisions, authorization/write fences, receipts and immutable audit.
Preserves all other assignments and the revoked source row. No second event/receipt system.
No HTTP route, generated client change, UI control, mobile change or notification is added.

## Requirements, formal gate and security

[RC01–06 and the exact command contract](raci-reassignment-contract.md) cover current scoped
authority, stale-write rejection, exact retry, task-isolated UUID keys, required roles and
atomic revision/audit/receipt effects. The specification and complete pinned formal suite
passed before feature SQL: **20 positive TLC configurations, 43 named negative controls,
13 PlusCal integrity tests, 2 SAT Alloy scenarios and 11 UNSAT assertions**. New model:
426,052 generated / 165,180 distinct states, depth 14. All six mutations returned exit 12
with their expected invariant. Finite bounds and abstractions are explicit; this is not a
universal proof or proof of SQL refinement, and no new liveness property is claimed.

The command checks current task read before receipt access, requires owner or exact applicable
`task.manage` for a new write, and rechecks time after blocking locks. Current read-only access
allows exact historical replay, not mutation. Task IDs namespace keys inside the existing
ledger. A server-computed SHA-256 binds the exact actor/request/revision/reason/correlation.
Unsupported lifecycle, timed source and ineligible recipient fail closed without exposing
party/grant details. No grants are created by reassignment. Recipient read eligibility is not
proof of consent, availability or booking. Passing an actor ID to private SQL is not authentication.

## Migration and rollback

Apply `2026-09-15_event_raci_reassignment.sql` after the existing foundation/API/task-commit/
task-read/revision/revisioned-read chain. Adds two SECURITY INVOKER functions, each revoked
from PUBLIC. No tables, indexes, canonical rows or generated ORM entities are rewritten.
The production manifest and provider flags remain unchanged. The existing event API flag
is checked; no new public entry point is enabled by that flag.

Rollback `2026-09-15_event_raci_reassignment_rollback.sql` removes only those functions.
Run it before rolling back preceding task revisions/read/commit functions. Accepted receipts,
audit, revoked assignments and monotonic counters survive down/up. Do not rewrite history to
undo an accepted reassignment; use a separately authorized compensating command after review.
Any future HTTP caller must be disabled first during rollback and fail unavailable, not silently
fall back. Roll-forward does not reactivate event operations.

Task writes serialize event-wide; large plans and long transactions require operational
timeouts/load testing. Stale RR/SERIALIZABLE transactions can return 40001; unexpected DB
faults abort the whole transaction. Callers must await COMMIT. Immediate caller constraints
may reject temporary Accountable removal; the command never weakens their mode. Trusted raw
legacy writers can introduce lock-order deadlocks; whole-transaction retry policy is future work.

## Executed local verification (2026-09-15)

- Pinned formal command: exit 0 before feature implementation; full command, bounds and
  toolchain are in the contract and formal README.
- `sh scripts/test-event-raci-reassignment-migration.sh`: initial run exit 0 on isolated
  PostgreSQL 16. Exact authorization/outcomes, private execution, scoped keys, stale versions,
  40 deterministic generated replacements over all four RACI roles, immutable history,
  audit/receipt failure injection, outer rollback, BIGINT overflow/maximum and immediate
  constraint failure, double apply/down/reapply. Nine concurrent retry/competing/legacy races
  over RC/RR/Serializable and three post-wait expiry cases passed with observed
  `pg_blocking_pids` barriers. Generated histories are not random property-based tests.
- Final rerun of the same PostgreSQL 16 command: **exit 0**, retaining all preceding cases
  and adding a fourth expiry wait on the recipient Party row, both explicit revocation
  orderings and aborted-first concurrent retry recovery. Correlation-bound key conflicts,
  oversized/null fields and unsafe identity input assertions also passed. Final local logs:
  `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-raci-command.MdbrCZ/`.
- `bash scripts/test-event-operations-schema-rehearsal.sh`: exit 0 on authoritative PostgreSQL
  17, without diagnostic workaround. Whole schema and migration ledger passed before/after;
  atomic Accountable replacement/replay, repeated migration, full-chain rollback/reapply,
  unchanged historical/legacy snapshots, retained revisions and disabled roll-forward passed.
- `node --test scripts/__tests__/event-raci-reassignment-runner.test.mjs
  scripts/__tests__/event-task-revisioned-read-runner.test.mjs
  scripts/__tests__/event-task-revision-runner.test.mjs
  scripts/__tests__/event-operations-schema-rehearsal.test.mjs`: **15 tests passed**.
- `npm run quality:repo`: **exit 0, 146 tests passed**, no generated fixture drift.
  Heuristic audit: 9,659 findings, zero critical/errors, 355 warnings and 9,304 informational.
  This heuristic is separate from TLC/Alloy; release/loop tests operate on synthetic local
  fixtures and do not constitute live merges or deployments.
- Shell syntax, whitespace and workflow YAML parsing passed; the RACI job is present.

All DB runners own disposable containers with no published ports, network or caller DSN.
The CI job requires both runner guards and PostgreSQL command tests; existing formal/full-schema
gates remain mandatory. Hosted results are separate from this local evidence.

Remaining: HTTP session-fence composition and strict typed DTO/OpenAPI/client, command admission
limits and sanitized rejection telemetry, editor conflicts, consent/notifications, richer timed
RACI, collaborator removal, later lifecycle effects, native mobile/offline integration and
production security/performance review. No current backend/mobile build, HTTP test, browser
E2E, screenshot, accessibility audit or payment sandbox result is implied by this SQL-only phase.
