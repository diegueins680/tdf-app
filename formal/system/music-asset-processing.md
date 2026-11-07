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

Independent review found the re-upload defect recorded below. The consolidated
AUTHORITY-068 migration resolves it; three further lifecycle gaps remain.

## Model

`formal/event-operations/MusicAssetProcessing.tla`: one job, two processes
with the same worker name, one source object whose bytes match the evidence or
not, `MaxAttempts = 3`, `MaxClaims = 4`. Actions: claim, lease expiry, fenced stop, promote,
fail (for any reason). Invariants `PromotedOnlyVerified`, `CompletionIsCurrent` (a completion is
written by the attempt that owns the job), `LiveLeaseIsCurrent` (nobody
reclaims a live job), `OneLiveLease`. 934 states generated, 554 distinct.
Three controls remove one guard each: `Unfenced` (name only, no attempt token:
a stale process completes a reclaimed job — `CompletionIsCurrent`), `Preempt`
(a running job can be reclaimed before its lease expires —
`LiveLeaseIsCurrent`), `Unverified` (promotion without the checksum —
`PromotedOnlyVerified`). A fourth configuration, `OverLimit`, is a
reachability witness, not a guard removal: it asserts `attempt <= MaxAttempts`
and TLC refutes it, showing that an expired last attempt is reclaimed and runs
past the bound, so the four invariants above are checked on that path too
(the independent review of the model asked for it).

The guard's SQL and the model agree on: name + attempt + lease age on the job
row, reclaim only after expiry, checksum before promotion. Abstractions: the
model's `Promote` is one step where the script runs two guarded transactions
(asset promotion, then `mark_succeeded`) with object writes and derivative
inserts between them, so "asset valid, job still running or retried" is a
real state the model does not have (safety still follows from the
per-statement guard); `Fail` fails an attempt for any reason; the SQL claim has no attempt
bound and neither has the model's `Claim` below `MaxClaims`, which exists only
for finiteness (an expired last attempt is reclaimed and runs once more, and
fails into `dead_letter`; a worker that dies on every run reclaims forever); the guard's second branch —
`mark_failed`'s follow-up transaction is fenced on `attempt_count`, final
status and `locked_by IS NULL` — is not modelled. Not modelled either: the
version-level `FOR NO KEY UPDATE` that serialises sibling jobs, S3 object
writes (idempotent by key and content), derivative rendering, and the
`heartbeat` that renews the lease; a renewal that fails marks the run
`lease-lost` and the run stops (the `Fenced` action). `OneLiveLease` follows
from `LiveLeaseIsCurrent`.

## Evidence

| Check | Where | Result |
| --- | --- | --- |
| Lease guard, reclaim after expiry, fenced stale completion, checksum mismatch, duplicate derivatives | `scripts/test-music-release-worker-runtime.mjs` (`npm run test:music-worker-runtime`; needs a local PostgreSQL on 5432 and the pipeline tools; not a CI step) | Existing test, not re-run in this slice |
| TLC | the four configurations above | pass / expected failures (2026-10-12, local) |

The requirement previously listed four asset states (`quarantined`,
`processing`, `promoted`, `failed`); the column allows nine
(`quarantined`, `uploaded`, `inspecting`, `valid`, `invalid`, `processing`,
`ready`, `failed`, `deleted`) and the job has seven statuses. The requirement
now lists the column sets; the values code actually writes are asset
`uploaded`, `inspecting`, `valid`, `ready` and job `queued`, `running`,
`retry`, `succeeded`, `dead_letter`.

## Historical defect (resolved by the consolidated re-upload migration)

The following finding records the original 2026-10-12 behavior. PR #550
adds the rebinding and cancellation trigger described in
[music-upload-rebind.md](music-upload-rebind.md), now included here. The
byte-identical multi-track limitation remains open.

**A corrected re-upload after a checksum-mismatch dead letter is never
inspected.** `confirmMusicUpload` inserts the asset `ON CONFLICT
(release_version_id, asset_role, sha256) DO NOTHING` and, on conflict, binds
the new session to the *existing* asset row and enqueues a job keyed by the
new session. The worker's claim reads bucket/object key from that row, i.e.
the first session's quarantine object. So: bytes Y uploaded under declared
hash X → mismatch → retry ×5 → dead letter, asset left at key 1; the user
uploads the correct bytes X under the same hash → conflict → bound to the old
row → the new job downloads key 1 again → dead letter again. No API writes
`deleted`/`invalid` or re-points the asset; the only exit is a new release
version. `version_completion_sql` also keeps the version `validation_failed`
while any dead-lettered job of the version exists. Proposed fix (follow-up
slice, with the worker runtime test extended): on conflict with a
non-immutable asset in `uploaded`/`inspecting`, re-point its bucket, object
key, etag, size and provenance to the new session before enqueueing, and
cancel that asset's dead-lettered jobs. Side effect of the same design: two
tracks of one version with byte-identical masters bind the second upload to
the first track's asset, so the second track never gets a master.

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
   (never promoted, version `validation_failed`), the cost is not. The
   consolidated re-upload migration now permits recovery under the same hash.
3. **A dead-lettered source asset keeps `processing_state = 'inspecting'`.**
   The worker never writes `failed`; the version state and the job carry the
   failure, the asset row does not, and the row stays in the in-flight
   partial index `idx_music_asset_processing` for good.

The first two are candidates for a worker change with the runtime test
extended; the third is a reporting gap.
