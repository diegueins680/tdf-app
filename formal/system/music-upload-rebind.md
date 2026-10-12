# Re-upload after a checksum mismatch (MEDIA-MUSIC-ASSET-001)

Slice of 2026-10-12 on main `86b6611fc`; follow-up of the defect recorded in
PR #549 (`formal/system/music-asset-processing.md`).

## What was wrong

`confirmMusicUpload` (`tdf-hq/src/TDF/Server/MusicReleaseUpload.hs`) inserts
the asset `ON CONFLICT (release_version_id, asset_role, sha256) DO NOTHING`
and, on conflict, binds the new upload session to the existing asset row and
enqueues a job keyed by the new session. The worker's claim reads bucket and
object key from the asset row, i.e. the first session's quarantine object.
Bytes Y uploaded under declared hash X fail the checksum, retry until
`max_attempts` and dead-letter; the user uploads the correct bytes X under
the same hash; the new job downloads the old object and dead-letters again.
No API path re-points the asset; the only exit was a new release version.
`version_completion_sql` also keeps the version `validation_failed` while a
dead-lettered job of the version exists.

Reproduced by the new scenario of `scripts/test-music-release-worker-runtime.mjs`
run without the migration (the asset keeps the old key, the job dead-letters).

## The fix (`tdf-hq/sql/2026-10-12_music_upload_rebind.sql`, manifest entry 203)

`trg_music_rebind_upload_asset`, BEFORE INSERT on `music_processing_job` for
`inspect_audio`, `inspect_artwork` and `validate_release`: when the job's key
is an upload session of the same version whose object differs from the
asset's, and the asset is not immutable and still `uploaded`/`inspecting`/
`failed`/`invalid` with the session's declared hash, the asset takes the
session's bucket, object key, provider, size, filename and media type, goes
back to `uploaded`, records `rebound_from_object_key` in its provenance, and
the asset's other `queued`/`retry`/`dead_letter` jobs become `cancelled`.
A first upload (same object) and a promoted asset are untouched. The handler
is unchanged; the rule lives where every enqueue passes.

A running attempt on the old object is not cancelled: if it verifies the old
bytes they carry the same hash, so its promotion is correct and the new job
then finds a promoted asset and repeats nothing.

Decision in AUTHORITY-068.

## Evidence

| Check | Where | Result (2026-10-12, local) |
| --- | --- | --- |
| Scenario: mismatch → dead letter → re-upload → inspected, promoted, old job cancelled | `scripts/test-music-release-worker-runtime.mjs` ("re-upload with the same declared hash…"), PostgreSQL 16 on 5432 with ffmpeg | PASS with the migration; the same scenario fails without it (asset keeps the old key) |
| Whole worker runtime suite | same script | 23 scenarios pass |
| Migration twice, rollback twice, forward again | schema copy | clean |
| Automatic-migration production-schema test | `scripts/test-automatic-migrations-production-schema.sh` on an empty database | pass, idempotent |

No new model: the rule is a single conditional rebinding; `MusicAssetProcessing.tla`
(PR #549) covers the lease and promotion it feeds into.

## Left open

- The runtime test is not a CI step (it needs PostgreSQL on 5432 and ffmpeg);
  the migration's trigger is exercised only locally and by the automatic
  migration test's apply/rollback.
- Gaps 1–3 of `music-asset-processing.md` (no purge of abandoned uploads,
  mismatch retried until `max_attempts`, dead-lettered asset left
  `inspecting`) remain.
- Two tracks of one version with byte-identical masters still bind the second
  upload to the first track's asset.
