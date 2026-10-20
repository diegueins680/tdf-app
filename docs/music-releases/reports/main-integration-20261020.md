# PR #548: semantic reconciliation with protected main

Music parent: `46f477373d0f2f3802e38fb2ac2c832c0a2e4be3`.
Main parent: `6ceca759cdb19a100afbce7063eef6a7fe09a117`.
Merge base: `bbf133ae6c74f08cd1879706792aef9ccce20fde`.
The integration retains both parents, without rebasing or rewriting source migrations.

## Shared paths

| Path | Resolution |
| --- | --- |
| `.github/workflows/ci.yml` | Keep the music worker prerequisites and database routing; add main's Operations poisoned-outbox PostgreSQL gate. No gate removed. |
| `docs/catalog-persistence/catalog-list-decisions.json` | Retain ten source-specific music reviews and their prior review references; regenerate 41 policy evidence records. Replace the two parent manifest fingerprints with an explicit combined 208-entry review. |
| `docs/catalog-persistence/event-catalog-current-successors.json` | Link the manifest successor to the combined fingerprint. Retain existing immutable historical references and add both complete parent entries with file and canonical entry hashes. |
| `docs/catalog-persistence/event-integration-retirements.json` | Keep every original retirement and original decision hash. Update the manifest replacement and current resolution; retain both parent resolutions through immutable locators. |
| `docs/catalog-persistence/reports/list-consumer-matrix.csv` | Render from the final audited inventory using the audit CLI's unchanged CSV renderer. |
| `docs/catalog-persistence/reports/static-list-inventory.json` | Rediscover candidates against the combined tracked tree and initialized pinned Mobile, with the strict decision audit enabled. |
| `formal/event-operations/README.md` | Keep Music publication, asset and DDEX model descriptions and Operations auxiliary-write, read-policy, worker-tick and replay documentation. |
| `formal/system/README.md` | Retain both domain indexes and their evidence boundaries. |
| `formal/system/authority-decisions.json` | Union the nonconflicting authority IDs: four music entries and five Operations entries unique to their parents. Shared entries agree. |
| `formal/system/inventory.json` | Regenerate from resolved sources. |
| `formal/system/requirements.json` | Keep both domain contracts, models, tests, recovery constraints and unresolved conformance obligations. No proof status upgraded by this merge. |
| `formal/system/traceability.json` | Regenerate fingerprints and reverse mappings from the combined inventory and requirements. |
| `scripts/__tests__/catalog-list-audit.test.mjs` | Retain main's equivalent retirement-preservation regression and add the 51-decision, ten-review, prefix and immutable SQL regression. |
| `scripts/build-catalog-decisions.mjs` | Keep main's equivalent preservation of reconciliation history once; recompute discovery counters without duplicating the spread. |
| `scripts/production-migrations.json` | Exact protected main prefix of 206 entries, followed by the original publication-isolation and upload-rebind entries. |
| `scripts/verify-event-operations-formal.sh` | Union Music and Operations positive models and named negative controls. Preserve pinned tool checksums and failure expectations. |
| `tdf-hq/tdf-hq.cabal` | Retain Music test registration and main's disposable PostgreSQL module and network-uri dependency. |

## Provenance and migration boundaries

`docs/catalog-persistence/music-main-reconciliation.json` records all 51 shared
decision differences and both immutable source locators for each. Forty-one
differences are generated evidence; ten distinguish source-specific reviews from
policy regeneration. The ten reviewed decisions are preserved byte-semantically,
including their prior source references. Both retired parent manifest reviews are
retained by file and canonical-entry hashes. Historical records never waive the
active candidate gate.

The manifest retains main's order and original introduction SHAs. The appended IDs
are `2026-10-11_music_publication_isolation` (introduced by
`598de30e3e1bf47b9f37cea86bd73f5b65eb5fac`) and
`2026-10-12_music_upload_rebind` (introduced by
`3f71add5783d9aefe818686abf4ee1c37ef6092b`). SQL checksums and the complete main
prefix hash are pinned in the reconciliation ledger. Neither SQL nor rollback SQL
is modified. The Mobile gitlink remains
`4c84c60a5836f7d39f18e10d34d136996bb0f63f`.

## Qualification

The PR conversation carries current candidate test results and failed attempts.
Historical successful workflow runs on either parent are not evidence for this
combined head. Complete PostgreSQL manifest apply/replay, rollback/reapply,
compiled backend tests, browser tests, pinned Mobile and all required hosted
checks must qualify the final published commit. Model checks remain bounded;
they do not prove universal implementation refinement.

A fresh independent approval after the final commit, resolved review discussions,
strict base freshness, and `main-integration-admission` remain required. No
protection is waived, source PR is closed, production deployment performed, or
music/payment operation activated by this reconciliation.
