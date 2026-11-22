# Loudness check of delivered masters (MEDIA-MUSIC-LOUDNESS-001)

Status: specified and implemented behind a disabled flag. The owner answered
the four questions below on 2026-11-21 and then continued without choosing among
the implementer's remaining proposals, which are in force and marked as such.
Written on main `985117b3d`. Phase 1 of the audio-feature work; tempo, key and
duplicate fingerprinting are later phases and are not built here.

## What exists today

`inspect_audio` (`scripts/process-music-release-audio.sh`) already runs ffmpeg
`loudnorm` over the master to normalise the stream derivatives to -14 LUFS / -1 dBTP,
and stores the raw output in the master's `technical_metadata`. Nothing judges that
measurement, it has no lifecycle, ffmpeg has no time limit, and publication ignores it.

## Rule

For every `master_audio` asset that passes inspection, the worker measures the
unmodified master once and records the result against that exact file.

- **Measurement.** `scripts/measure-music-loudness.pl MASTER_FILE` runs
  `ffmpeg -nostdin -nostats -xerror -protocol_whitelist file -i file:MASTER -map 0:a:0
  -af ebur128=peak=true:framelog=verbose -f null -` and reads the summary block: integrated loudness `I` (LUFS, ITU-R BS.1770 gating) and
  the maximum true peak over all channels (dBTP). No transform is implemented here.
- **Units.** Both values are stored as integers in tenths (`-142` is -14.2 LUFS), the
  precision ffmpeg prints. No floating point is compared anywhere.
- **Decision** (pure, computed when read, never stored):

  | Flag | Raised when |
  |---|---|
  | `too_loud` | `I > target + tolerance` |
  | `too_quiet` | `I < target - tolerance` |
  | `true_peak_over` | `TP > ceiling` |

  `pass` when no flag is raised, otherwise `warn` with the flags. Because the decision
  is computed from stored numbers, changing a constant re-judges every track without
  re-measuring.
- **Enforcement.** Warn only. A warning, a failed measurement or a missing measurement
  never blocks validation, approval or publication, and never fails `inspect_audio`.

## Constants (owner decisions)

| Constant | Value | Source |
|---|---|---|
| Target integrated loudness | -14.0 LUFS | owner, 2026-11-21 |
| True-peak ceiling | -1.0 dBTP | owner, 2026-11-21 |
| Enforcement | warn only | owner, 2026-11-21 |
| Measurement time limit | 900 s wall clock, then TERM, KILL after 5 s | owner, 2026-11-21 |
| Accepted input | what `inspect_audio` accepts: PCM WAV/AIFF, FLAC, ALAC; 44.1–192 kHz; 1–8 channels; 1 s – 6 h; ≤ 8 GiB | owner, 2026-11-21 |
| Tolerance around the target | ± 2.0 LU (pass from -16.0 to -12.0, inclusive) | implementer proposal, in force |
| Accepted measured range | `I` in -69.9..+10.0, `TP` in -150.0..+20.0; outside is a failure | implementer proposal, in force |
| A silent file | `failed: silent`, not a measurement | implementer proposal, in force |
| The file a track's result describes | its newest master asset that was not deleted | implementer proposal, in force |

## Lifecycle

One row per attempt in a new table `music_audio_measurement`: `asset_id`,
`source_sha256`, `kind`, `state`, `tool`, `tool_version`,
`integrated_lufs_tenths`, `true_peak_dbtp_tenths`, `failure_reason`, `details`
(JSONB), `job_id`, `job_attempt`, timestamps. `kind` is `loudness` now; later phases
add kinds and keep their values in `details`. At most one row per `(asset_id, kind)`
is not `superseded` (partial unique index). The trigger
`music_audio_measurement_guard` refuses every other transition, any change of a
row's identity, and deletion.

| From | To | Cause |
|---|---|---|
| (none) | `pending` | the worker opens an attempt after the master passed the checksum and the pipeline (`music_begin_audio_measurement`) |
| `pending` | `measured` | the wrapper returned two in-range values for the file whose SHA-256 the job carries |
| `pending` | `failed` | any other wrapper outcome, with one reason |
| `pending`, `measured`, `failed` | `superseded` | a later attempt for the same asset starts, or an unpromoted asset is re-pointed at another object |

`superseded` is terminal; `measured` and `failed` never change except to `superseded`.

Failure reasons: `timeout`, `decode_error`, `no_summary`, `unparseable`,
`out_of_range`, `silent` (nothing above the gate), `tool_unavailable`, `interrupted`
(the wrapper printed nothing).

## Invariants

- **L1, file binding (critical).** A track is shown as checked only when a `measured`
  row exists for the track's newest undeleted master asset, with the same
  `source_sha256` as that asset, and the asset is valid and immutable. A version can
  hold several master files for one recording; a newer upload that is still being
  inspected, or that failed, therefore hides the older file's result. "Checked" is derived by that join on every read and is stored
  nowhere, so no write order can leave a stale mark.
- **L2, fenced writes.** Every measurement write goes through the existing lease guard
  (job id, attempt, owner) and is conditional on the asset still having the job's
  SHA-256. A reclaimed or cancelled run writes nothing.
- **L3, totality (critical).** For any input file, the wrapper exits 0 within the time
  limit plus the kill grace, printing exactly one JSON object: either two integers or
  one failure reason. It reaps its child before printing, does not wait for a
  descendant that keeps the output open, and keeps at most 64 KiB of tool output. The
  surrounding inspection continues in every case.
- **L4, monotonicity.** For fixed constants, each flag is monotone in its value:
  `too_loud` and `true_peak_over` never clear as the value rises, `too_quiet` never
  clears as it falls. The combined pass/warn is therefore monotone in `TP`, and in `I`
  on each side of the target, but not across the whole `I` axis: the band is two-sided.
- **L5, isolation.** With the flag off, the worker step is skipped, no row is written
  and the route answers as if absent; existing inspection output is byte-identical.

## Boundaries

- **Flag.** `music_releases.loudness_check` in `revenue_feature_flag`, seeded disabled
  for every environment. The worker does not read flags today; this adds one read.
- **Read access.** One new read-only route,
  `GET /music/releases/{releaseId}/versions/{versionId}/loudness`, under the same
  release permission as the other authoring reads. Nothing is added to public responses.
- **Personal data.** None. The result is a function of the file bytes and the constants
  only; there is no per-user behaviour, so there is nothing to personalise or opt out
  of. Measurements are visible only to parties who can already read the draft version.
- **Web only.** A warning line per track in the release editor. No mobile UI; the
  mobile client types are regenerated because the OpenAPI contract gains a route.
- **Migration.** One reviewed migration (table, index, flag rows, re-bind hook) with a
  rollback, listed in `scripts/production-migrations.json`.
- **Later phase.** Suspected duplicates go to a review queue and never block an upload.

## Evidence, by strength

Proofs, bounded models and tests are different things; each row says which it is.

| Property | Proved | Bounded model | Tested | Not shown |
|---|---|---|---|---|
| L1 file binding | Apalache inductive invariant on `formal/music/MusicLoudnessBinding.tla`, for every subset of up to 4 files and 3 workers and up to 6 attempts, any number of steps | TLC, 3 files and 2 workers; negative configurations `Unbound` and `StoredMark` violate `Safe` | `LoudnessSpec` (QuickCheck) on `trackCheck`; handler query on PostgreSQL 16 with two masters per track, another version, a deleted master | That the SQL and the Haskell equal the model: by the tests only |
| L2 fenced, never rewritten | Same proof (`NoRewrite`) | Negative configuration `Unfenced` | Trigger and functions on PostgreSQL 16: refused transitions, another attempt, wrong SHA-256, deletion | — |
| L3 totality | Apalache inductive invariant on `formal/music/MusicLoudnessSupervision.tla`, for every limit 1..900 s and grace 1..60 s: bounded wait, one honest result after reaping, no stall | TLC; negative configurations `NoKill`, `NoReap`, `Trusting` | Wrapper on empty, random, truncated, missing, silent input and on tools that hang, ignore TERM, leave a descendant or print megabytes; worker scenario | Termination of FFmpeg itself and Perl's signal semantics: the proof is about the supervision model |
| L4 monotonicity | Z3 on all integers, `formal/music/loudness_decision.py`, pinned to the source region of `loudnessFlags`; also a witness that pass/warn is not monotone across the whole loudness axis | — | `LoudnessSpec` (QuickCheck) | The Z3 encoding is a hand translation of the Haskell |
| L5 isolation | — | — | Worker scenario with the flag off and with only the other environment's flag on | The route's 404 when off: by review, no HTTP test |
| Accuracy | — | — | Wrapper against EBU Tech 3341 v4.0 Table 1 cases 1-6 (±0.1 LU) and 15-19 (+0.2/-0.4 dB), generated by formula, on FFmpeg 7.0.1 | Cases 7-8 and 20-23 (EBU downloads only); the worker image's FFmpeg build |

## Open for the owner

1. **Tolerance.** ± 2.0 LU is in force as a proposal. Commercial masters are often -9
   to -7 LUFS and will all warn `too_loud`; that may be the intended message or noise.
2. **Two-sided band.** Each flag is monotone, the combined pass/warn is not monotone
   across the whole loudness axis. Dropping `too_quiet` would make it so.
3. **Conformance signals.** Cases 7-8 and 20-23 of EBU Tech 3341 are not tested;
   committing EBU's files needs a licence check, or CI can fetch them.
4. **Which file is the track's file.** Nothing else in the platform names a current
   master when a version holds several for one recording. The newest undeleted one is
   used here; publication itself does not make that choice.
5. **Existing key mismatch.** The player and search read a numeric `loudness_lufs` that
   the worker never writes. Not changed here.
6. **Time limit against long masters.** A local run (2013 Xeon, ffmpeg 7.0.1, synthetic
   noise generated in the same process, per-frame logging on) measured 6 hours of
   48 kHz stereo in 647 s, and 10 minutes of 192 kHz 8-channel in 47 s. On that machine
   a 192 kHz 8-channel master longer than about 3 hours ends in `failed: timeout`.
   Not re-measured on the worker image.
7. **Stale pending.** A worker killed during a measurement leaves a `pending` row
   until another attempt for the asset runs.
8. **Production.** No music worker is deployed in production, so the check cannot run
   there whatever the flag says.
