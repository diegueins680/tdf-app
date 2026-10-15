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

`trg_music_rebind_upload_asset`, AFTER INSERT on `music_processing_job` for
`inspect_audio`, `inspect_artwork` and `validate_release` (AFTER, so an
insert that creates no row under `ON CONFLICT DO NOTHING` changes nothing):
when the job's key is the upload session completed with this asset, of the
same version and role, with the asset's declared hash but a different object,
and the asset is not immutable and still `uploaded`/`inspecting`/`failed`/
`invalid`, the asset takes the session's bucket, object key, provider, size,
filename and media type, drops its etag, goes back to `uploaded`, records the
previous key(s) in its provenance, and the asset's other `queued`/`retry`/
`running`/`dead_letter` jobs become `cancelled`. A running attempt is
cancelled too: the worker's lease guard refuses every later write of a
cancelled run (the runtime suite's "cancellation stops a blocked transfer"
scenario), so it cannot dead-letter the version after the re-upload; the
independent review showed that leaving a final attempt running re-created the
stuck `validation_failed` version. The trigger locks the version row
`FOR NO KEY UPDATE` first, the worker's lock order. A first upload (same
object) and a promoted asset are untouched. The handler is unchanged; the
rule lives where every enqueue passes.

Decision in AUTHORITY-068.

## Evidence

| Check | Where | Result (2026-10-12, local) |
| --- | --- | --- |
| Scenario: mismatch → dead letter → re-upload → inspected, promoted, old job cancelled, conflicting re-enqueue a no-op, old session untouched | `scripts/test-music-release-worker-runtime.mjs` ("re-upload with the same declared hash…"), PostgreSQL 16 on 5432 with ffmpeg | PASS with the migration; the same scenario fails without it (asset keeps the old key) |
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
- The old quarantine object is never deleted and the old session stays
  `completed` (it still counts against the 24-hour upload quota).
- `music-asset-processing.md` and `MusicAssetProcessing.tla` are in PR #549;
  until it merges those references dangle.
