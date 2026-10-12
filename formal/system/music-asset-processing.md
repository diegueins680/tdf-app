# Music asset processing under worker leases (MEDIA-MUSIC-ASSET-001)

Slice of 2026-10-12 on main `86b6611fc`. Scope: what keeps a processed music
asset truthful when worker runs overlap: the lease guard of
`scripts/run-music-release-worker-once.sh`, the claim query, and the
checksum-gated promotion of `process_audio` / `process_artwork`. Upload
signing, derivative rendering, public signing and the S3 adapters are not
re-verified here; their existing tests are listed on the requirement.

## What was read

- `confirmMusicUpload` (`tdf-hq/src/TDF/Server/MusicReleaseUpload.hs`) binds a
  completed multipart upload to one `music_asset` row in state `uploaded`
  with the client-declared size and SHA-256, then enqueues one
  `music_processing_job` per upload session (`ON CONFLICT (job_kind, job_key)
  DO NOTHING`).
- The worker claims one due job with `FOR UPDATE SKIP LOCKED`, incrementing
  `attempt_count` and taking `locked_at`/`locked_by`; a running job is
  claimable only after `locked_at` is older than the lease.
- Every later statement of that run is prefixed by `pg_temp.music_worker_guard`,
  which locks the version and the job and requires `status = 'running'`,
  `locked_by = <worker name>`, `attempt_count = <claimed attempt>` and a
  lease younger than the ttl; the attempt is the fencing token because worker
  names are reused across restarts.
- Promotion recomputes the SHA-256 of the downloaded object, compares it with
  the upload evidence, copies the bytes to the master bucket and only then
  updates the asset to `valid`, `immutable = TRUE`, with
  `WHERE sha256 = <expected> AND (NOT immutable OR same location)`.
  Derivatives are inserted `ON CONFLICT (release_version_id, asset_role,
  sha256) DO NOTHING`, so a reclaimed attempt that repeats finished work
  changes nothing. `trg_music_asset_immutable` rejects any later change of
  bytes or location.
- Public signing (`getPublicMusicAssetAccess`) goes through
  `music_public_asset_accessible`, which admits only `cover_display`,
  `thumbnail`, `stream_audio` and `preview_audio` of a public release; masters
  are reachable only through purchase entitlements or free grants
  (PAY-MUSIC-DOWNLOAD-001).

No defect was found in this scope. Three gaps are recorded below.

## Model

`formal/event-operations/MusicAssetProcessing.tla`: one job, two processes
with the same worker name, one source object whose bytes match the evidence or
not, `MaxAttempts = 3`. Actions: claim, lease expiry, fenced stop, promote,
fail. Invariants `PromotedOnlyVerified`, `CompletionIsCurrent` (a completion is
written by the attempt that owns the job), `LiveLeaseIsCurrent` (nobody
reclaims a live job), `OneLiveLease`. 300 states generated, 206 distinct.
Three controls remove one guard each: `Unfenced` (name only, no attempt token:
a stale process completes a reclaimed job — `CompletionIsCurrent`), `Preempt`
(a running job can be reclaimed before its lease expires —
`LiveLeaseIsCurrent`), `Unverified` (promotion without the checksum —
`PromotedOnlyVerified`).

The guard's SQL and the model agree on: name + attempt + lease age on the job
row, reclaim only after expiry, checksum before promotion. Not modelled: the
version-level `FOR NO KEY UPDATE` that serialises sibling jobs, S3 object
writes (idempotent by key and content), derivative rendering, and the
`heartbeat` that renews the lease; a renewal that fails marks the run
`lease-lost` and the run stops (the `Fenced` action).

## Evidence

| Check | Where | Result |
| --- | --- | --- |
| Lease guard, reclaim after expiry, fenced stale completion, checksum mismatch, duplicate derivatives | `scripts/test-music-release-worker-runtime.mjs` (`npm run test:music-worker-runtime`; needs a local PostgreSQL on 5432 and the pipeline tools; not a CI step) | Existing test, not re-run in this slice |
| TLC | the four configurations above | pass / expected failures (2026-10-12, local) |

The requirement previously listed four asset states (`quarantined`,
`processing`, `promoted`, `failed`); the column allows nine
(`quarantined`, `uploaded`, `inspecting`, `valid`, `invalid`, `processing`,
`ready`, `failed`, `deleted`) and the job has seven statuses. The requirement
now lists the real sets.

## Gaps (not fixed here)

1. **Abandoned uploads are never purged.** `music_upload_session` rows pass
   `expires_at` but no path sets `status = 'expired'`, nothing enqueues the
   `purge_abandoned_upload` job kind, and the quarantine object and the
   provider's multipart upload stay. Cost and a growing quarantine bucket, not
   an integrity problem.
2. **A checksum mismatch is retried as if transient.** `process_audio` and
   `process_artwork` return 1 on mismatch; `mark_failed` schedules
   `retry` with a growing delay until `max_attempts` (5) and only then
   `dead_letter`. Each retry downloads the object again. The outcome is right
   (never promoted, version `validation_failed`), the cost is not.
3. **A dead-lettered source asset keeps `processing_state = 'inspecting'`.**
   The worker never writes `failed`; the version state and the job carry the
   failure, the asset row does not.

The first two are candidates for a worker change with the runtime test
extended; the third is a reporting gap.
