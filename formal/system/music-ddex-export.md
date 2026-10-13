# DDEX export lifecycle under worker leases (MEDIA-MUSIC-DDEX-001)

Slice of 2026-10-13 on main `75f1a7ce8`. Scope: what keeps a DDEX export row
truthful from the request to its package: the API transaction in
`tdf-hq/src/TDF/Server/MusicReleaseDDEX.hs`, the two gates and the failure
path of `process_ddex` / `mark_failed` in
`scripts/run-music-release-worker-once.sh`, and the lease guard they share
with every other job kind. ERN rendering, XSD validation, the package builder
and object storage are not re-verified here; their tests are listed on the
requirement.

## What was read

- `createDdexExport` takes `pg_advisory_xact_lock` on `music-ddex:<actor>:<key>`,
  locks the version `FOR UPDATE`, resolves the actor's key globally
  (version, operation, sender and recipient must all match, else 409),
  validates with `music_check_ddex_operation` and the credits parser, locks
  both registry rows `FOR SHARE`, inserts the export `queued` and its
  `generate_ddex` job in the same transaction; a controlled error after a
  write is an exception, so both rows roll back. A replay of a `queued`
  export recreates only a missing job (`ON CONFLICT (job_kind, job_key) DO
  NOTHING`) and never touches attempts.
- The worker claims one due job (`queued`/`retry`, or `running` with an
  expired lease) with `FOR UPDATE SKIP LOCKED`; every later statement runs
  under `pg_temp.music_worker_guard` (version `FOR NO KEY UPDATE`, job `FOR
  UPDATE`, status `running`, same worker name, claimed attempt, lease younger
  than the ttl).
- `process_ddex`: a `valid` export is closed as recovered without rendering;
  otherwise gate 1 re-runs `music_check_ddex_operation`, compares the
  snapshot hash and both registry rows (active, role, DPID) under `FOR SHARE`
  and, in the same statement, moves `queued | validation_failed | generating`
  to `generating`; rendering, resource download, package build and three
  uploads happen outside any transaction; gate 2 repeats the checks and moves
  `generating` to `valid` with the three asset ids; `mark_succeeded` closes
  the job.
- `mark_failed` (any failure, including the crash handler) moves the job to
  `retry` or, on the last attempt, `dead_letter`, and in the same write moves
  the export to `validation_failed` or `failed` unless it is `valid`. A gate
  refusal stores its field-level report.

## Model

`formal/event-operations/MusicDdexExport.tla`: one export, one job (or none,
the legacy stranded case), two processes sharing a worker name, a boolean
`rules` the environment flips at any time (registry revoked or reactivated,
version state changed), `MaxAttempts = 2`, `MaxClaims = 3`. Actions: replay,
claim, lease expiry, crash, fenced stop, recover, gate 1, upload, gate 2,
succeed, fail. Invariants: `ValidForever` (a valid export is never
regenerated or failed), `CurrentWriter` (every database write is by the
process holding the current attempt), `ValidOnlyUnderRules` (valid is
written only while the rules hold), `FailedIsFinal` (a failed export has its
dead-lettered job and gets no new one), `PendingHasLiveJob` (a pending export
always has a job the worker can still run; only the legacy queued export
lacks one until the replay repairs it, under the job statuses the code
writes: `cancelled` and `failed` are never written by any path, and a job
cancelled by hand leaves a non-queued export as stuck as a dead letter).
5,284 states generated, 1,272 distinct. `CurrentWriter` and
`ValidOnlyUnderRules` hold by construction of the positive model (the
guard is conjoined with every write), so the positive run is a consistency
check of the guards; the five controls are the evidence that each invariant
can fail.

Five controls remove one rule each and fail the named invariant:
`Unfenced` (name and lease only, no attempt token — `CurrentWriter`),
`Reopen` (the valid exclusion, which the code applies at two sites, gate 1
and `mark_failed`, is dropped at both; TLC's counterexample goes through
`mark_failed` — `ValidForever`), `NoCommitGate` (gate 2 does not re-check the rules —
`ValidOnlyUnderRules`), `ReplayFailed` (the replay re-enqueues a failed
export — `FailedIsFinal`), `SplitDeadLetter` (the dead letter and the
export's `failed` are separate writes — `PendingHasLiveJob`).

Abstractions: the model's `Gate2` and `Succeed` are two steps where the
script runs two guarded transactions, so "export valid, job still running
or retried" is a real state the model also has (it is why `Recover` exists);
`Fail` fails an attempt for any reason; the claim has no attempt bound and
neither has `Claim` below `MaxClaims` (an expired last attempt is reclaimed
once more and dead-letters). Not modelled: the API's advisory and version
locks (serialisation with the worker is by the version row; concurrent
requests are covered by `scripts/lib/music-ddex-enqueue-probe.mjs`), the
natural unique key, object writes (idempotent by key and content), the
heartbeat, and `mark_failed`'s second transaction (fenced on attempt, final
status and `locked_by IS NULL`).

## Defect fixed

**The export report copied the worker's stderr.** For every failure that was
not a gate refusal, `mark_failed` wrote `jsonb_build_object('valid', FALSE,
'code', <code>, 'message', <last twenty lines of the worker's stderr>)` into
`music_ddex_export.validation_report`, which `listDdexExports` serves to
every user with `release.downloads` on the release (the artist team), while
`docs/music-releases/ddex-queued-validation.md` promises that export messages
carry no object keys, SQL or secrets. That stderr holds libpq connection
errors (host, port, user), psql statement context, curl transport errors
and temporary paths. The export now receives a fixed message with its error
code; the stderr summary stays on `music_processing_job.error_summary`, the
operator read model, until the next claim clears it and permanently once
dead-lettered. In the same write, `status NOT IN ('valid','failed')` now
keeps a failed export failed when an operator re-queues its dead-lettered
job by hand (before, that path moved it to `validation_failed` and a later
attempt could produce a package, contradicting the declared relation).
Covered by the new scenario in
`scripts/test-music-release-worker-runtime.mjs` (a renderer double that
fails with a libpq-style message; the report must not contain it, the job
must, and a retry with the working renderer reaches `valid`), and by the
extended `queued DDEX` scenario (a re-queued dead letter dead-letters again
and leaves the failed export row unchanged).

## Evidence

| Check | Where | Result |
| --- | --- | --- |
| TLC positive model and five controls | `scripts/verify-event-operations-formal.sh` (CI formal runner) | pass / expected failures (TLC 1.7.2 on Temurin 21, 2026-10-13, local) |
| Commit gate, revocation during rendering, processing-failure report, retry to valid, valid recovery, queued revalidation, failed stays failed under a hand re-queue | `scripts/test-music-release-worker-runtime.mjs` (`npm run test:music-worker-runtime`; local PostgreSQL 16 on 5432 and the pipeline tools; not a CI step) | `complete audio release`, `valid DDEX retry`, `queued DDEX` scenarios pass on the patched worker (2026-10-13, local); the new scenario fails on the unpatched worker |
| Declared states equal the column CHECK | `scripts/check-state-machines.py` (CI, migrated PostgreSQL) | bound under AUTHORITY-082; transitions are enforced by the worker's conditional UPDATEs, not by a trigger, so only the state set is compared |

The requirement previously listed export states `queued`, `rendering`,
`validated`, `failed`, `retired` and a transition `suspended -> retired`;
nothing is ever retired and the column allows `queued`, `generating`,
`validation_failed`, `valid`, `failed`. The requirement now lists the
column's states, the job statuses and the worker's transition relation.

## Gaps (not fixed here)

1. **A dead-lettered export is unrecoverable.** `status = 'failed'` is
   terminal; the natural unique key `(release_version_id, recipient_dpid,
   operation, canonical_snapshot_sha256)` rejects a new request for the same
   package with 409 whatever its idempotency key, and the replay never
   re-enqueues a failed export (by design, ADR-018). Five attempts with the
   quadratic delay take about fifteen minutes, so an XSD-directory, renderer
   or object-storage outage of that length blocks that operation for that
   version and recipient until an operator edits the rows (export and job
   both: the worker no longer reopens a failed export). The same holds for
   any non-queued export whose job was cancelled by hand. An audited
   operator retry is an open product decision.
2. **The job's stderr summary is served to the uploader** through the upload
   status endpoint (`MusicReleaseUpload.hs`); that surface belongs to
   MEDIA-MUSIC-ASSET-001.
3. **`validation_failed` also names processing failures** (renderer,
   transfer, crash); the report's `code` distinguishes
   `ddex_preconditions_failed` from `generate_ddex_failed`.
