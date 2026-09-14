# PR draft: Event operations formal specification and transactional foundation

## Scope

- Audit the existing event ecosystem and define the canonical integration boundary.
- Add bounded TLA+/PlusCal and Alloy models plus a reproducible, checksum-pinned gate.
- Add an additive PostgreSQL event-operations foundation around `social_event` and existing logistics
  activities/dependencies.
- Close authenticated event-invitation create/read/update authorization gaps.

## Dependency

Base: local `origin/main` at `17a33eca11d585d84435af85340beece9b51d14e`. This is the first PR in
the planned chain; later API/UI/resource/engagement/finance work depends on it. Do not merge until the
remote CI job and backend suite reproduce the local evidence.

## Requirements and invariants

- EO-003/007/008/009: explicit ownership/grants, visibility/lifecycle policy, revisions/audit.
- EO-023–028: event-local acyclic dependencies, exactly one Accountable, at least one Responsible,
  no silent orphan, dependency-gated completion, version-bound audited override.
- EO-033–038/041–044: reservation race, exact contract version, milestone/payout/idempotency models.
- EO-045/047/051: contextual authorization, invitation attenuation/replay, stale/offline terminal
  behavior.

Traceability: `docs/event-operations/traceability-matrix.md`.

## Schema and migration effects

- Adds event lifecycle policy/state sidecar, ownership/coproduction relations, scoped grants,
  migration issues, revisions, sessions, command receipts, audit events, transitions, invitation
  security metadata, task policy, RACI assignments, and task overrides.
- Backfills only deterministic legacy lifecycle/owner facts. Missing/unresolved ownership creates an
  issue and remains disabled for activation; it is never guessed.
- Adds advisory-lock/cycle and opted-in completion triggers to existing logistics tables.
- Does not alter or delete existing event, RSVP, ticket, logistics, directory, resource, booking,
  commerce, ledger, review, chat, or notification records.

## Security and privacy effects

- Event invitation creation becomes organizer/admin-only.
- Non-managers list only invitations addressed to their authenticated party.
- Recipients can accept/decline but cannot transfer invitations or edit organizer messages.
- New audit/revision/receipt/transition/override rows reject update/delete.
- Guest token material is designed for SHA-256 digests only; the complete external guest flow remains
  disabled/unimplemented.

## Tests and formal checks executed

- `npm run verify:event-operations:formal`: PASS with exact counts/bounds in the formal README.
- `npm run test:event-operations-foundation-migration`: PASS on ephemeral PostgreSQL 16.
- `npm run verify:formal`: PASS, 0 critical/errors; 351 pre-existing/advisory warnings.
- `npm run test:formal`: PASS, 4/4.
- Shell syntax, JSON parse, workflow YAML parse, and `git diff --check`: PASS.
- Linked Hspec binary, `social event handler helpers`: PASS, 31 examples/0 failures.
- Linked Hspec binary, atomic/idempotent moment-reaction test: PASS, 1 example/0 failures.
- The `stack test` wrapper compiled and linked all 203 modules, then failed during Cabal copy/register
  because it expected an unrelated `tdf-hq-exe` artifact that target did not build; it is not claimed
  as a passing wrapper command.
- `npm run quality:repo`: 41/42 loop tests passed; the remaining test could not load the declared but
  uninstalled `@testomatio/reporter/jest` dependency. Formal audit within the command passed with
  0 critical/errors. Re-run after a clean dependency install in CI.
- Remote CI was unavailable during the initial local implementation. GitHub access later
  recovered; publication/check status is tracked separately in the delivery report.

## Rollback

Deploy application rollback first. The SQL rollback removes the two triggers installed on legacy
logistics tables and retains all additive ownership/audit/invitation/RACI data for a safe roll-forward.
No destructive down migration is supplied for immutable operational history. The migration is not in
the production manifest in this PR.

## Remaining limitations/manual steps

- The original task completion/RACI guards are insufficient under relation replacement and
  concurrent responsibility removal. The dependent PR 04 supplies the modeled and DB-tested
  correction; do not activate the task sidecar from this foundation alone.
- No typed foundation API or generated client yet; no new workspace UI/mobile surface.
- No external invitation issue/accept/revoke worker, no resource binding/capacity path, and no
  event-engagement/contract/payment orchestration yet.
- Legal/accounting/security review and sandbox provider certification remain mandatory.
- All production providers, payouts, rollout flags, and production migration operations remain off.
