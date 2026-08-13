# Instrument and input-list cutover

Status: candidate implementation and disposable PostgreSQL rehearsal complete; production has not
been modified.

## Classification and model decision

The production inventory found 21 `input_row` records and 14 distinct values in the legacy
`instrument` column. Inspection proved that the column did not represent an instrument catalog:
its values were microphone or DI model observations. The cutover therefore separates two domains:

- musical purpose is an `instrument_id` foreign key to the active, published `instrument` catalog;
- physical microphone or DI identity is the existing `mic_id` foreign key to `asset`;
- `live_session_intake.primary_genre_id` references `genre`;
- `live_session_musician.instrument_id` references `instrument`.

The retained text columns are rollback evidence only. New HTTP writes reject
`primaryGenre`, musician `instrument`, and musician `role`; the web sends UUIDs from one batched
`genres`/`instruments` catalog read. Input-list API and PDF presentation resolve the current
instrument and asset names by foreign key and do not fall back to copied text.

## Reviewed per-row mapping

The full deterministic map is embedded in both the dry-run and apply scripts. It is keyed by the
pair `(track_name, original microphone observation)` so a repeated microphone label cannot imply
the musical purpose by itself.

| Channels / tracks | Canonical instrument code | Canonical asset | Evidence |
| --- | --- | --- | --- |
| Kick In | `drums` | AKG D112 | Reviewed kick purpose and exact active asset name |
| Snare Up, Snare Down | `drums` | Shure SM57 | Reviewed snare purpose and exact active asset name |
| Hi-Hat | `drums` | Sennheiser MKE600 | Reviewed hi-hat purpose and exact active asset name |
| Tom 1, Tom Floor | `drums` | Sennheiser MD421 | Reviewed tom purpose and exact active asset name |
| OH L, OH R | `drums` | AKG C414 | `AKG C414 (HC)` was placement metadata, not another asset identity |
| Bass DI (post) | `bass-guitar` | Neve RNDI | Reviewed bass purpose and exact active DI asset |
| Bass Mic 1 (cab) | `bass-guitar` | AKG D112 | Reviewed bass-cab purpose and exact active asset |
| Bass Mic 2 (ataque) | `bass-guitar` | Neumann KM184 | Reviewed bass purpose and exact active asset |
| Gtr 1, Gtr 2 | `electric-guitar` | Sennheiser e906 | Reviewed electric-guitar purpose and exact active asset |
| Gtr 1 Ribbon, Gtr 2 Ribbon | `electric-guitar` | Royer R121 | Reviewed electric-guitar purpose and exact active asset |
| Vox 1 | `voice` | Electro-Voice RE20 | Reviewed voice purpose and exact active asset |
| Vox 2 | `voice` | Sennheiser e835 | Reviewed voice purpose and exact active asset |
| Vox 3, Vox 4 | `voice` | Shure SM58 | Reviewed voice purpose and exact active asset |
| KU-100 L, KU-100 R | `voice` | Neumann KU-100 | Left/right observations are channels of one binaural asset |

This yields 21 instrument references across four catalog identities (`drums` 8,
`bass-guitar` 3, `electric-guitar` 4, and `voice` 6) and 21 asset references across 13 physical
asset identities. Automatic mapping requires exactly one active published instrument and exactly
one active asset. Missing or duplicate candidates abort the transaction; the bootstrap seed now
uses the same fail-closed rule.

## Migration artifacts and safety

- `tdf-hq/sql/2026-08-11_instrument_input_cutover_dry_run.sql` is read-only and reports schema
  readiness, every proposed mapping, candidate counts, and legacy Live Session counts.
- `tdf-hq/sql/2026-08-11_instrument_input_cutover_apply.sql` performs nullable online expansion,
  takes a transaction-scoped advisory lock, enforces a configurable row threshold, records source
  and target values, validates foreign keys, installs active/publication triggers, and clears copied
  strings only after all safety gates pass.
- `tdf-hq/sql/2026-08-11_instrument_input_cutover_rollback.sql` restores a row only when its current
  UUIDs still match the mapped targets. It removes the new writer triggers but deliberately retains
  columns, foreign keys, mapping evidence, and audit/run records.

The specialized evidence tables are `catalog_input_reference_cutover_source` and
`catalog_live_session_reference_cutover_source`. Normal catalog mapping evidence records the
instrument relationship; asset provenance stays in the input-specific evidence table because a
physical asset is not an item in the instrument catalog.

## Disposable PostgreSQL 17 evidence

Final apply identifiers:

- run code: `instrument-input-final-20260811`;
- candidate revision: `local-candidate-v1`;
- run ID: `5e243db1-16ec-4b54-8889-f89c322cbcb3`;
- scanned/mapped/rejected/ambiguous: `21/21/0/0`;
- source evidence rows: 21;
- canonical row digest: `b193e331daa25871a337dd4e908bf64a`.

Apply and a no-op rerun retained one backfill run, 21 mappings, 21 source-evidence rows, and the
same digest. Rollback restored all 21 original rows and all 14 distinct source strings with zero
mismatches; a second rollback updated zero rows. Reapplying returned to the same canonical digest.
Three foreign keys validated successfully.

Transactional negative tests rejected a copied legacy string, an unknown instrument UUID, a
missing microphone/DI UUID, and an inactive instrument. Candidate startup before backfill exited
non-zero on the 21 invalid rows. After apply, the rebuilt candidate started twice, returned
`{"db":"ok","status":"ok"}`, and left the digest, evidence count, and run count unchanged.
The public batched catalog endpoint returned 21 published genres and 12 published instruments with
UUID identities and `ETag: "catalog-2"`; a matching conditional request returned 304.

Focused verification also passed 33 Live Session parsing/validation examples, the strict web
multipart and generated-client tests, web/mobile typechecks, directed web lint, the Haskell
executable build, and two input-list LaTeX examples. The OpenAPI contract now documents the
protected multipart endpoint and canonical Live Session musician/song schemas; both regenerated
clients have SHA-1 `09fddc9f14a66963ffd231c3553912e404ec552b`. The new PDF example proves
that `AKG C414` comes from the canonical asset instead of the old `AKG C414 (HC)` text.

## Production sequence and rollback boundary

Production rollout remains blocked by the repository-wide release gates. When authorized, this
slice must be deployed in the following order:

1. Back up the four source tables and catalog/mapping infrastructure and record the backup ID.
2. Run the read-only report and review every proposed row and candidate count.
3. Quiesce Live Session and input-list writers; old clients are not compatible with the new write
   contract.
4. Run apply with the exact candidate revision and a reviewed safety threshold.
5. Verify counts, foreign keys, evidence rows, digest, and representative API/PDF rendering.
6. Start the candidate backend, then release the matching web client; require a compatible client
   revision for any independently released writer.
7. Roll back the application first if needed, then run the guarded rollback while the writer is
   still quiesced. Do not delete evidence, UUID columns, catalog items, or audit history.

No production backup, migration, deployment, or external publication was performed during this
rehearsal.
