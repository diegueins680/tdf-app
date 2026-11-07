# Scheduled music publication (MEDIA-MUSIC-RELEASE-001)

Slice of 2026-10-11 on main `1d8142a1e`. Scope: what `music_publish_due` and
`music_withdraw_due` do with the versions that are due, and how
`music_release.published_version_id` / `withdrawn_at` follow the versions.
Authoring, review, approval snapshots, corrections and the public read model
are unchanged and not re-verified here.

## What was wrong

Read from `tdf-hq/sql/2026-09-11_music_release_platform.sql` and reproduced on a
copy of the migrated schema (`scripts/__tests__/fixtures/production-schema-20260814.sql`
plus the 202-entry manifest) with releases built through the real submission
rules.

1. **One failing candidate stopped every publication.** `music_publish_due`
   ran all due candidates in one transaction. A candidate that failed at
   publication aborted the batch, and the next run hit the same row: nothing
   published for any release, and because `scripts/run-music-release-worker-once.sh`
   runs both functions in one `psql -c`, nothing was withdrawn either. The
   reproducible cause: two approved corrections of the same source (both carry
   `replaces_version_id = v1`); after the first publishes, the second only
   retires `v1` (already withdrawn), leaves the first published and violates
   `uq_music_release_one_published_version`. Any rule in `music_check_submission`
   that stops holding after scheduling has the same effect.
2. **A withdrawn release never published again.** `music_withdraw_due` set
   `withdrawn_at` when the live version left; nothing cleared it, and
   `music_publish_due` skipped releases with `withdrawn_at`. Staff could
   schedule a correction (the transition trigger does not look at the
   release), and it stayed `scheduled` forever with no error.
3. **The release pointer outlived its version.** `transitionReleaseVersion`
   moves a version with a plain `UPDATE`; `suspended -> withdrawn` or
   `suspended -> approved` left `published_version_id` on a version that is
   no longer live and `withdrawn_at` null. Not public (the view requires
   `state = 'published'`), but the release record lied and later publication
   logic read it.

## The fix (`tdf-hq/sql/2026-10-11_music_publication_isolation.sql`, manifest entry 203)

| Definition | Rule |
| --- | --- |
| `music_publish_due` | Each candidate runs in its own subtransaction. A transient failure (SQLSTATE class 40, `55P03`) leaves the candidate `scheduled` with a warning; any other failure makes it `suspended` with an audit event `publication_failed` carrying the SQLSTATE and message; the loop continues either way. Before publishing, `music_retire_live_versions` retires every other formerly published version of the release that is still `published`/`replacement_pending`/`takedown_scheduled` through the lifecycle chain (never-published and `suspended` versions stay). The `withdrawn_at` filter is gone. |
| `music_withdraw_due` | Same isolation. A failed withdrawal stays `takedown_scheduled` and updates its last-attempt priority (`updated_at`); eligible withdrawals are ordered by that priority before their scheduled time, so even a one-row batch advances past a poison row; unless transient it is recorded once (`withdrawal_failed`, not repeated while the latest audit row already says so). |
| `trg_music_release_live_version` | AFTER UPDATE OF state: `published` sets `published_version_id` and clears `withdrawn_at`; leaving the live states (`published`, `suspended`, `replacement_pending`, `takedown_scheduled`) through any path clears the pointer and sets `withdrawn_at`. The scheduler functions no longer write the release row themselves. |
| Repairs | Release pointing at a non-live version → pointer null, `withdrawn_at` set. Release with a `published` version it does not point at, or marked withdrawn while one is published → pointer and `withdrawn_at` corrected. |

Decisions recorded in AUTHORITY-066: a newer publication supersedes every
other formerly published version still live (not only the one it was created
from), including one whose staff-chosen takedown date lies in the future;
never-published and `suspended` versions are left alone; publication reopens a
withdrawn release; a version that fails at publication for a lasting reason is
suspended for staff rather than retried blindly, a transient failure is retried
next run; `suspended` keeps the pointer (the version is held, not gone); several
due versions of one release publish in release-time order, each superseding the
previous one (with the intermediate "published" notifications).

Locks: `music_publish_due` keeps `FOR UPDATE OF v, r SKIP LOCKED` on the
candidate and its release; retirement updates lock the sibling versions in the
same transaction. The API handler locks only the version row and then, through
the new trigger, the release row, so a handler move of a pointed sibling during
publication of the same release can deadlock; the scheduler is the victim, the
candidate stays `scheduled` and publishes on the next run (the independent
review reproduced this; it is why transient errors do not suspend).
`SET LOCAL lock_timeout = '10s'` on the migration.

## Model

`formal/event-operations/MusicPublication.tla`: one release, versions v1
(live), v2 and v3 (corrections of v1); staff actions without fairness; weak
fairness on the two schedulers. Invariants `OnePublished`, `PointerLive`,
`PublishedIsPointed`, `WithdrawnConsistent`, `OnlyInvalidFails`; property
`Progress` (a due scheduled version eventually leaves `scheduled`, stated
without the release's withdrawn mark so the `StayWithdrawn` control fails for
the documented reason) and `ValidProgress` (a valid due version leaves
`scheduled` even while a sibling fails; the `Batch` control fails on it).
`Blocked` is derived from the retired state, so `OnlyInvalidFails` is checked,
not assumed. 32,257
states generated, 7,168 distinct, depth 18. Four controls remove one guard
each: `Batch` violates `ValidProgress`, `StayWithdrawn` violates `Progress`;
`ReplacedOnly` violates `OnlyInvalidFails`; `PointerStale` violates `PointerLive`. The model found
defect 3's `suspended -> approved` case before the trigger covered it.

Not modelled: the chain `published -> replacement_pending -> takedown_scheduled
-> withdrawn` (one step), the content of `music_check_submission` (a boolean
that may flip), several releases, and lock interleaving with the API handler.

`MusicWithdrawalFairness.tla` separately models a finite queue of three due
versions (one permanent failure and two healthy withdrawals), a one-row batch
and a weakly fair recurring worker. Updating failed attempts moves the head to
the tail; `HealthyEventuallyWithdrawn` requires both healthy rows to complete.
The `MusicWithdrawalStarvation.cfg` control disables that rotation and must
violate the temporal property. This abstraction assumes successful persistence
of priority and increasing transaction timestamps. It does not prove progress
with clock regression, continual external rescheduling or unbounded arrivals.
The real PostgreSQL one-row regression test connects the ordering to SQL.

## Evidence

| Check | Where | Result (2026-10-11, local) |
| --- | --- | --- |
| Reproduction on the unmodified schema | SQL scenarios on a schema copy | batch aborts with `uq_music_release_one_published_version`, unrelated release stays `scheduled`; correction of withdrawn release stays `scheduled`; pointer stays on withdrawn version |
| PostgreSQL spec | `tdf-hq/test/TDF/MusicPublicationSpec.hs` via `scripts/test-music-publication.sh` (CI step "Verify contact HTTP concurrency…") | 8 cases: one-row withdrawal progress past a permanent failure, isolation with a synthetic lasting failure, a failing correction leaving the live version and the release untouched, a synthetic transient failure leaving the candidate scheduled and publishing next run, retire-all with two corrections, reopen after withdrawal, pointer cleared on the API path, withdrawal isolation recorded once. Against the unmigrated schema all cases that depend on the migration fail (control). |
| Migration twice, rollback twice, forward again | schema copy | clean |
| TLC | publication positive + four controls; withdrawal fairness + starvation control | pass / expected failures. Withdrawal fairness: 4 distinct states, depth 4 (2026-10-14); disabling rotation reproduces starvation. |

The synthetic failures are triggers raising `P0001` or `40P01` on one version;
the reproducible real cause (unique violation) cannot occur once retirement
covers every live version, so the exception path is exercised synthetically. A
real deadlock was reproduced by the independent review with two sessions. The
spec's rows stay in the shared CI database (the audit table is append-only);
no later CI step counts music rows.

## For the release

Manifest entry 203, additive: replaces two functions, adds one function, one
trigger function, one trigger, two repair updates. Old servers keep working
(they only call the two functions). Rollback restores the previous function
bodies and drops the trigger; repaired release rows stay repaired.

After applying, check that no version is left behind:

```sql
SELECT id, release_id FROM music_release_version
WHERE state = 'scheduled' AND release_at_utc <= NOW()
  AND (embargo_until_utc IS NULL OR embargo_until_utc <= NOW());
SELECT release_version_id, data FROM music_release_audit_event
WHERE event_type IN ('publication_failed', 'withdrawal_failed');
```

Production has not applied the music platform migrations (ledger 188 at
release 7); the feature flags are created disabled. This migration changes no
flag.

## Left open

- `transitionReleaseVersion` still offers `withdrawn` as a direct staff target;
  it now keeps the release consistent but bypasses the takedown schedule.
- The handler/scheduler lock order differs (version then release vs. release
  then siblings); the scheduler tolerates the deadlock but a lock-order fix in
  the handler would remove it.
- A suspended version has no in-app path to see `publication_failed`; staff
  read the audit trail.
- The requirement's state list previously named 7 states; the column allows 15
  (`music_release_version_state_check`). The requirement now lists the real
  set; the state-machine binding for this column remains unbound.
