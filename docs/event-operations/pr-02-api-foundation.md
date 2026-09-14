# PR draft: Event operations typed lifecycle API foundation

## Scope and dependency

This local PR-equivalent branch depends on
`cde0e806b6a5365e1e9f5b4052b74e38ea1aa232` (`feat/event-operations-formal-foundation`). It adds the
first authenticated read/write boundary over that schema. It does not expose publishing, booking,
ticketing, contracting, notification, settlement, or payment effects.

## Requirements and invariants implemented

- EO-003/045: evaluate active event ownership and scoped, revocable, time-bounded grants in the same
  transaction as a lifecycle mutation.
- EO-008: preserve accepted, rejected, and conflicting command evidence in immutable receipts/audit.
- EO-009/010: validate canonical edge, authority, expected version, independent approval, command-key
  idempotency, rollback reason, and implementation-effect readiness under an event row lock.
- EO-055/056: expose strict typed Servant/OpenAPI contracts and reversible additive rollout behavior.

Only `draft -> planning`, `planning -> pending_approval`, `pending_approval -> planning`,
`pending_approval -> approved`, and `approved -> planning` are write-enabled. This is an explicit
implementation allowlist, not a modification of the canonical policy. Every other valid policy edge
returns `transition_effects_not_ready` until its guards and transactional/outbox effects exist.

## Schema and migration effects

- Adds persistent `event_operation_feature_flag`, seeded `event.operations.api = false`, plus immutable
  flag-change history. Enabling requires both an actor and a reason.
- Adds `event_operation_transition_capability` for per-edge fail-closed rollout.
- Adds the global `(event_id, operation_code, command_id)` receipt uniqueness index.
- Adds contextual capability/read/authority functions and the single lifecycle transition command
  function.
- The rollback disables the feature and removes the write function, but preserves flags, capabilities,
  immutable receipts, audit, and transition history for investigation and safe roll-forward.
- The migration is intentionally absent from the production manifest pending reviewed rollout.

Before applying the unique command index to any environment where the foundation was used outside
this branch chain, preflight duplicate command identifiers. The expected rollout starts with an empty
foundation table, but the migration does not silently delete or merge conflicting history.

## API and client effects

- `GET /event-operations/events/{eventId}` returns an authorized state/version projection, current
  capability codes, and only executable transitions.
- `POST /event-operations/events/{eventId}/transitions` requires a UUID `Idempotency-Key`, strict JSON,
  expected version, canonical target, and a non-empty correlation identifier.
- Feature-disabled, missing, and unreadable snapshot requests collapse to 404. Mutations return stable
  400/403/404/409/503 domain statuses; no success is fabricated.
- OpenAPI and generated web types are updated, with a typed web transport wrapper. There is no visible
  workspace UI control in this PR, preventing a dead or misleading feature surface.

## Security and privacy effects

- Authorization uses the authenticated `Party`, never a caller-supplied actor identifier.
- Grants are checked for resource scope, revocation, start, and expiration at mutation time.
- Owners do not implicitly receive event or finance approval authority; approval needs an explicit
  scope and a different actor from the review requester.
- Command identifiers are global within event/operation, so changing actors cannot turn a reused key
  into a second command; conflicting reuse is appended to audit without duplicating the receipt.
- API domain responses expose stable codes. Database exception logging still includes exception
  detail; sensitive-data redaction needs explicit security tests before activation.

## Verification executed locally

- `npm run test:event-operations-api-migration`: PASS on ephemeral PostgreSQL 16, including two
  simultaneous transitions from the same version (exactly one accepted, one conflict).
- Linked Hspec filter `event operations executable API contracts`: PASS, 5 examples/0 failures,
  including the stable non-success domain status map.
- Haskell target compiled and linked 208/208 test modules. Stack's later copy/register step again
  failed on the unrelated absent `tdf-hq-exe`, so the wrapper itself is not claimed as passing.
- OpenAPI generation: PASS with locally available `openapi-typescript` 7.10.1. The branch-local npm
  command lacked its binary because dependencies are incomplete.
- Focused strict TypeScript compilation of the new client and generated contract: PASS.
- Full UI TypeScript compilation remains non-green on two pre-existing missing onboarding exports;
  neither error is in this PR's files.
- Mobile client generation: skipped by the repository's own readiness guard because the submodule or
  install is incomplete.
- Remote CI was unavailable during the initial local implementation. GitHub access later
  recovered; publication/check status is tracked separately in the delivery report.

## Rollback and remaining limits

Roll back application consumers first, apply the non-destructive API rollback, and leave immutable
history available for reconciliation. Do not delete history or enable the flag during rollback.

Exact receipt replay currently precedes fresh authorization in the SQL transition function;
revoked-scope replay must be corrected and tested before activation. HTTP integration/authentication
tests, stale-session/offline replay, outbox effects, legacy lifecycle
cutover, mobile client generation, web workspace UI, and every later domain phase remain. No provider,
production database, credential, deployment, or real-money operation is touched.
