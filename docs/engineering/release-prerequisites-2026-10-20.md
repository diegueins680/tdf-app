# Production verification integration — 20 October 2026

PR #574's production migration, startup and runtime inspection work is reconciled
with main `6ceca759c`. Its other merge parent is
`ece1d7052bd8ddaaca757901fa51537e77abcdfd`. This continues the existing PR;
it does not replace the separate recovery executor in draft PR #495.

## Reconciliation

- Preserve both parents' CI jobs and steps, including the production migration
  batch, startup flags and poisoned Operations outbox checks.
- Preserve all formal model invocations from both parents.
- Retain all 124 requirement IDs and 71 distinct authority decision IDs. Resolve
  additive authority decisions by ID rather than choosing one entire file.
- Regenerate source fingerprints, traceability and catalog reports. Preserve
  historical catalog retirement records and reviewed active decisions.
- Keep main's Operations inbound-capture retention blocker in release preparation.
- Keep the complete 206-entry migration manifest, SQL, application sources,
  generated API clients and Mobile gitlink identical to main. The Mobile pin is
  `4c84c60a5836f7d39f18e10d34d136996bb0f63f`.

## Fresh local validation

Validation used an owned, disposable PostgreSQL 16.13 cluster on loopback port
55437, with pgvector 0.8.1 and the committed production-schema/catalog fixtures.
No production database or provider was used.

- All 206 migrations apply and replay; the ledger count matches the manifest.
- `migration-batch-postgres.mjs` passes, including crash after commit, transaction
  rollback, checksum drift, unknown ledger rows, holes and advisory-lock refusal.
- `startup-gates-postgres.mjs` passes all 42 per-flag states: 18 accepted and
  24 rejected. The test also checks the approved joint extremes and verifies
  that schema verification does not modify flag rows.
- The inherited `operations-worker-tick-postgres.mjs` passes against the same
  fully migrated schema.
- 86 focused release/inspection/preparation/entrypoint Node tests, 16 Python
  inspection tests and 28 CI-pipeline tests pass.
- Generated-client conformance passes for both web and the exact Mobile pin.
- The existing lightweight `npm run verify:formal` gate passes. This is not a
  fresh run of every bounded TLC/Alloy model or a whole-system proof.

Initial local attempts were incomplete: the PostgreSQL installation lacked
pgvector; a subsequent test database name did not meet the disposable fixture
guard; and inherited Node dependencies had an older YAML package. These attempts
are not counted as passes. Fresh full runs used the installed extension, a newly
created database ending in `_test`, and the lockfile's TypeScript 5.9.3/YAML 2.9.1.

## Remaining delivery requirements

The integrated head still needs hosted qualification, including PostgreSQL 17,
and independent current-head approval under the protected FIFO integration policy.
Earlier hosted results remain evidence for their original revision only.

The guarded recovery/deployment coordinator, writer fencing, independently
retrievable recovery keys, coordinated backup/restore, image identity and
restricted canary remain separate production prerequisites. Release preparation
continues to return `executionAllowed: false`. No deployment, production migration,
provider activation or financial transaction is established by this checkpoint.
