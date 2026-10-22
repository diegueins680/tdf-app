# Production verification integration — 21 October 2026

PR #574's production migration, startup and runtime inspection work is reconciled
with protected main `8de1bae4c30961eaa11637e5b4307271250ee176`. The other merge
parent is the previously approved candidate
`d53c9da6ad965bb49d14264e429738662a4aef4f`. This continues the existing PR;
the unfinished recovery coordinator remains separate in draft #495.

## Reconciliation

- Preserve both parents' CI steps and formal model invocations, including the
  migration batch, startup flags and poisoned Operations outbox checks.
- Preserve all 127 requirement IDs and 78 distinct authority decision IDs by
  checking each merged field against both parents and their common ancestor.
- Preserve main's complete 211-entry migration manifest, SQL, application
  sources, generated API clients and exact Mobile pin
  `3194982a01174d36017533e79400ce9209086162`.
- Preserve the Operations inbound-capture retention blocker in preparation.
- Preserve main's current catalog reviews and all existing successor/retirement
  records. Add only the original startup sweep domain review. The earlier refresh
  archives 24 differing shared reviews and six replaced source reviews through
  immutable parent-file and canonical entry hashes in
  `docs/catalog-persistence/runtime-main-refresh-20261021.json`.
  The earlier reconciliation record remains historical and unchanged.
- Regenerate source fingerprints, traceability and catalog reports. No
  requirement conformance classification is promoted by this reconciliation.

## Scoped local validation

The following runtime and model receipts were executed at candidate
`d53c9da6ad965bb49d14264e429738662a4aef4f`. This readiness integration preserves
their migration/startup implementations, test fixtures and expanded SQL bytes.
Source equivalence retains that bounded evidence; it is not a fresh execution of
every test at the new merge head. Current inventory, generated-client, source
admission and catalog checks are rerun. Full new-head hosted qualification is
required.

The database run used an owned, disposable PostgreSQL 16.13 cluster on loopback
port 55437, with pgvector 0.8.1 and the committed schema/catalog fixtures. No
production database or provider was used.

- All 211 migrations apply and replay; the ledger count matches the manifest.
- `migration-batch-postgres.mjs` passes, including crash after commit, transaction
  rollback, checksum drift, unknown ledger rows, holes and advisory-lock refusal.
- `startup-gates-postgres.mjs` passes all 42 per-flag states: 18 accepted and
  24 rejected. The test also checks the approved joint extremes and verifies
  that schema verification does not modify flag rows.
- The inherited `operations-worker-tick-postgres.mjs` passes against the same
  fully migrated schema.
- 117 focused release, entrypoint, preparation, catalog and CI-pipeline Node
  tests and 16 Python inspection tests pass.
- Generated-client conformance passes for both web and the exact Mobile pin.
- The existing lightweight `npm run verify:formal` gate passes. This is not a
  fresh run of every bounded TLC/Alloy model or a whole-system proof.
- The extra local CORS mutation runner could not execute because the local
  Stack environment lacks the Wai dependencies. This is an incomplete setup
  attempt, not a CORS test pass; compiled backend qualification remains hosted.

The previous 206- and 208-entry integrations were qualified separately.
Its initial local attempts were incomplete: the PostgreSQL installation lacked
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

## Polling-admission base refresh — 21 October 2026

The candidate now retains approved runtime head `1e1fbea150ce3497653eb189a6b7bcab64275d25` and verified protected main `5aea795e6a12d2fbe084a4d24b817ed4be83ab01` as normal merge parents. Main includes the completed public-route polling repair and its original checks; its application sources, exact 211-entry migration manifest and SQL tree, API clients and Mobile pin are retained. The 32 migration/startup/inspection qualification inputs remain byte-identical to the d53 execution recorded above. Both parent requirement fields, authority decisions, CI/model commands and existing catalog source reviews are preserved. Generated fingerprints and catalog reports are refreshed. New exact-head hosted CI and independent review remain required; source PRs remain open. Production recovery and retention gates remain unresolved.

## Synthetic-persona inspection control — 22 October 2026

The collector now records raw presence and explicit absence of `TDF_ENABLE_SYNTHETIC_PERSONAS`, whose Seed handler reads an environment association list rather than a suffix-named `lookupEnv` flag. Conservative discovery recognizes literal lookup membership in an all-boolean `Just` list and rejects non-boolean environment-mode lists. Regressions exercise a synthetic non-registry name, direct/normalized and wrapped lookups, actual collector absence/raw values and persona-password redaction. Both new tests fail against the prior collector/discovery source. This remains lexical discovery, not general Haskell dataflow or effective-default evaluation. No seed behavior, flag activation or production configuration changes. New inspection tests and exact-head hosted qualification supersede only the inspection portion of earlier source-equivalence receipts.
