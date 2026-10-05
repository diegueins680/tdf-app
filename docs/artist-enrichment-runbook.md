# Artist enrichment runbook

## Scope and safety policy

The artist inventory covers core profiles and roles, releases, artist-linked
service orders, storefront artist names, catalog artist/performer credits,
promotion slots, fan relationships, bands and members, live-session intake and
musicians, pipeline cards, social artist profiles and events, imported external
artist references, and social-sync posts. Ordinary booking customers are not
artists and are deliberately excluded. Original spellings remain attached to their source rows;
matching uses an accent- and punctuation-folded name only as a lookup key.

Name equality is never enough to publish an identity. Automatic creation or
field publication requires at least two independent cross-provider signals and
no detected exact-name homonym. Examples are a Spotify/MusicBrainz discography
overlap plus a MusicBrainz-linked official YouTube channel, or matching official
website and Instagram links across MusicBrainz and Discogs. Stable TDF party IDs
may link a profile only when corroborated by two distinct TDF relationship
families. All other cases remain pending for a strict administrator.

Production execution never deletes an artist or media object. Obsolete and
duplicate-looking references are review states. Field corrections always retain
the previous value, proposed value, evidence, confidence, timestamp, and actor.

## Data model

The additive migration `tdf-hq/sql/2026-08-05_artist_enrichment.sql` creates:

- `artist_profile_enrichment`: new public profile fields and verification state.
- `artist_inventory_reference`: normalized discovery rows and aliases.
- `artist_research_source`: queryable field-level provenance.
- `artist_enrichment_suggestion`: idempotent per-field proposals and decisions.
- `artist_field_change`: immutable change history.
- `artist_enrichment_run`: resumable run/checkpoint/error state.
- `artist_identity_candidate`: ambiguous or externally corroborated identities.
- `artist_media_asset`: authorized Drive objects, hashes, dimensions, and variants.

The legacy `artist_profile` contract remains valid. New public fields are nullable
and are assembled from the related enrichment row. The rollback migration drops
only these new objects and the case-insensitive slug index; it does not reverse
approved values copied into legacy artist-profile columns.

## Required environment

Secrets must be supplied only by the process environment or authenticated
integration. Never place their values in command arguments, logs, reports, Git,
or pull-request text.

Production database logging suppresses Persistent's debug-level SQL bind values
so bearer tokens and provider credentials cannot appear in application logs;
database warnings and errors remain enabled.

- `ADMIN_TOKEN` (or `API_TOKEN`): active bearer token for a strict Admin with the
  Admin module.
- `TDF_API_BASE`: defaults to `https://api.tdfrecords.net`.
- `TDF_API_TIMEOUT_MS`: protected TDF API request timeout; defaults to 180000 ms
  and is 300000 ms in the daily workflow so full discovery can complete. This
  does not relax the shorter timeouts used for external providers.
- Spotify: `SPOTIFY_CLIENT_ID`, `SPOTIFY_CLIENT_SECRET`.
- YouTube: `YOUTUBE_API_KEY`.
- Optional Discogs: `DISCOGS_TOKEN`.
- MusicBrainz: descriptive `MUSICBRAINZ_USER_AGENT`.
- Google Drive OAuth: `DRIVE_CLIENT_ID`, `DRIVE_CLIENT_SECRET`,
  `DRIVE_REFRESH_TOKEN`, `DRIVE_UPLOAD_FOLDER_ID`; or the existing service-account
  variables `GDRIVE_CLIENT_EMAIL`, `GDRIVE_PRIVATE_KEY`, `GDRIVE_PARENT_ID`.

The command fails closed when the admin token is absent. When the runner does
not have complete Drive credentials, image ingestion uses the authenticated TDF
`/drive/upload` proxy so the current backend Drive integration remains the sole
secret holder. It fails closed if neither direct Drive authentication nor that
backend proxy is available.
Direct uploads also require Google Drive to confirm the public-reader permission;
if Workspace policy rejects sharing, the run fails before storing or publishing
the generated Drive URL.

## Operator commands

Install the repository-supported Node22+ and FFmpeg/FFprobe. Run from the repository root.
The manual CLI and scheduled workflow default to `https://api.tdfrecords.net`.
For an isolated local/staging target, set `TDF_API_BASE` explicitly; `API_BASE`
is the legacy fallback override. Do not target the retired Fly database.

Audit only, with no external research or profile publication:

```bash
npm run artists:enrich -- \
  --mode dry-run --scope audit --batch-size 500 --concurrency 3 \
  --checkpoint artifacts/artist-enrichment/audit-checkpoint.json \
  --report artifacts/artist-enrichment/audit-report.json
```

Complete dry-run, including unprofiled inventory and external candidates:

```bash
npm run artists:enrich -- \
  --mode dry-run --scope full --batch-size 500 --concurrency 3 \
  --checkpoint artifacts/artist-enrichment/dry-run-checkpoint.json \
  --report artifacts/artist-enrichment/dry-run-report.json
```

An external image is never ingested merely because Spotify or another provider
returned one. After verifying reuse rights, an operator can ingest it explicitly:

```bash
npm run artists:enrich -- \
  --mode production --scope media --artist ARTIST_ID --batch-size 1 \
  --image-source-url 'https://official.example/press/artist.jpg' \
  --image-rights licensed --image-attribution 'Official press kit license' \
  --focal-point center --checkpoint artifacts/artist-enrichment/media.json \
  --report artifacts/artist-enrichment/media-report.json
```

The source URL, attribution, rights status, retrieval time, hash, dimensions,
Drive ID, and every derivative are retained. Candidates without explicit
`authorized` or `licensed` status remain in the report and are not downloaded.

Bounded production execution without automatic publication:

```bash
npm run artists:enrich -- \
  --mode production --scope full --batch-size 25 --concurrency 3 \
  --checkpoint artifacts/artist-enrichment/production-checkpoint.json \
  --report artifacts/artist-enrichment/production-report.json
```

After reviewing the dry-run, add `--auto-publish` to publish only corroborated,
non-homonymous candidates. Use `--artist ID` for one existing artist and
`--no-resume` to intentionally ignore a local checkpoint. A successful rerun is
idempotent by inventory source/record/name, candidate identity, field/proposed
value, source/fields, content hash, and media variant.

The safety circuit stops production research after three errors, or above a 10%
error rate after at least five attempts. The backend run retains the checkpoint
and redacted error summary. Correct the cause and rerun with the same checkpoint.
Historical errors remain in the report as `previousAttemptErrors`, but only
errors from the current attempt count toward the circuit breaker.

## Review interface

Strict administrators can open `/admin/artists/enrichment` (the localized alias
is `/admin/artistas-enriquecimiento`). The screen supports
status/confidence/missing-field filters, current-versus-proposed comparison,
source and evidence inspection, image previews, individual or set decisions,
ambiguous identity review, historical decisions, and artist-scoped reruns.
Approval and rejection are safe to repeat. Approving an externally corroborated
candidate without an existing party creates the missing Party, Artist role,
profile, and enrichment row atomically; a matching existing profile is reused.
Approval also supersedes every pending candidate for the same normalized-name
group, preventing a later decision from silently reassigning its references.

## Daily maintenance

Two complementary layers run daily in `America/Guayaquil`:

- Backend discovery on the current Hetzner deployment at configurable local hour 04:00. It discovers TDF
  references, checks slugs/staleness, and queues review records without calling
  external providers.
- GitHub Actions external research at 10:00 UTC (05:00 Ecuador), with concurrency
  group `production-artist-enrichment`, bounded provider concurrency, and a
  120-minute timeout. It validates links, researches identities, queues
  corroborated changes, and stores a redacted artifact for 30 days. The
  external-research window rotates deterministically by UTC date, so platforms
  larger than the 500-record safety batch eventually cover every profile rather
  than repeatedly selecting the first 500. The scheduled job does not
  auto-publish or ingest an image without explicit rights; a strict administrator
  can approve queued fields or launch an authorized image run.

Backend variables (inspect effective production values; do not enable by merging code):

- `ARTIST_ENRICHMENT_ENABLED` (`false` disables backend discovery immediately)
- `ARTIST_ENRICHMENT_AUTO_PUBLISH`
- `ARTIST_ENRICHMENT_HOUR_LOCAL`
- `ARTIST_ENRICHMENT_BATCH_SIZE`
- `ARTIST_ENRICHMENT_STALE_DAYS`

Disable the external job by disabling the `Daily artist enrichment` workflow.
Rerun it with `workflow_dispatch`; default manual mode is dry-run. GitHub schedule
activation requires this workflow to exist on the default branch.

## Current deployment and recovery authority

Cloudflare Pages must target `https://api.tdfrecords.net`. The UI deliberately
ignores the retired `https://the-dream-factory.koyeb.app` value when that stale
value is injected into a `*.tdf-app.pages.dev` build, while retaining other
explicit API overrides for local or alternate deployments. Remove the retired
dashboard variable when Cloudflare account access is available; the repository
fallback prevents it from breaking previews in the meantime.

Production is the canonical Hetzner system described in
[ops/hetzner/README.md](../ops/hetzner/README.md), with API
`https://api.tdfrecords.net`. The previous Fly deployment, volume-snapshot and
schema-rollback instructions in this runbook are retired. They must not redirect
production, edit already-applied SQL or drop audit tables.

Use the canonical read-only inspection and isolated restoration/migration
rehearsal first. The guarded production executor and coordinated database/media
recovery obligations remain open in that contract; a successful worker dry-run
or a green unit test does not supply deployment eligibility. Provider secret
values must never appear in captured commands or reports.

Preserve accepted enrichment changes, provider/source evidence and checkpoints
through recovery. Disabling discovery, external scheduling or publication is an
operator action under the current release procedure. Retrying may resume completed
items and replace the current attempt's error summary while retaining previous
attempt errors; it does not authorize deleting failed-run evidence. No real
provider requests or workflow dispatch are required for local conformance tests.

A failed item causes backend status `failed`, a `run_failed` log, a retained
report and a nonzero CLI exit, even when the early-stop threshold was not reached.
The threshold controls when to stop processing additional items, not whether the
run succeeded. A successful return requires the final backend update to have
completed. Process termination may leave a running lease; stale-lease recovery
and external-provider exactly-once effects are separate obligations.

1. Record the exact current release, immutable image and Hetzner deployment.
2. Follow the backup, restore, migration-ancestry and recovery gates in
   [the current portable deployment runbook](../ops/hetzner/README.md).
   Preserve a verified off-host logical backup and the current production data.
3. Restore the custom-format PostgreSQL dump into an isolated instance, apply
   the forward migration twice, execute a full dry-run against the clone, apply
   the rollback, and verify legacy profile counts and representative rows.
4. Run all tests and inspect dry-run counts, automatic candidates, corrections,
   ambiguity queue, and expected zero deletions.
5. Obtain the required independent review and checks for the immutable release.
6. An authorized operator uses the current guarded deployment/recovery procedure;
   do not run the retired Fly release lane or restore service on its stale data.
7. Execute production enrichment in batches of 25 first. Check errors, duplicate
   slugs, candidates, public API responses, and frontend pages before raising the
   batch size.

Useful read-only checks:

```bash
curl -fsS https://api.tdfrecords.net/health
curl -fsS https://api.tdfrecords.net/version
```

Do not paste a bearer token into shell history. Export it through the secure
operator environment, then omit it from captured command output.

## Rollback

Application rollback is preferred when the additive schema is healthy:

1. Stop production enrichment by disabling the workflow and setting
   `ARTIST_ENRICHMENT_ENABLED=false` in the current backend configuration, using
   an authorized maintenance operation.
2. Use the reviewed compatible recovery image against the current Hetzner
   database, following the portable recovery runbook; verify `/health` and
   exact `/version`. Never route traffic back to the stale Fly copy.
3. Leave the additive tables intact so old application revisions continue to
   ignore them and audit evidence remains available.

Schema rollback is for a confirmed schema defect and requires a maintenance
window:

1. Export all eight enrichment tables and verify that export.
2. Apply `tdf-hq/sql/2026-08-05_artist_enrichment_rollback.sql` with
   `ON_ERROR_STOP=1`.
3. Verify legacy `artist_profile`, `party`, releases, bookings, and public artist
   endpoints.
4. If legacy data was affected or schema rollback fails, restore the verified
   current PostgreSQL backup through the guarded recovery procedure. Preserve
   post-cutover writes; a provider reversal needs a new freeze and reverse
   migration, not a restore of the retired Fly snapshot.

Never restore a database or volume over production without first confirming the
exact app, volume, snapshot/dump identifier, recovery point, and maintenance
window.

## Tests

```bash
npm run test:artist-enrichment
npm run test:production-release
npm run typecheck:ui
npm run lint:ui
npm run test:ui
npm run build:ui
cd tdf-hq && stack test
```

Migration CI applies every registered migration, applies the enrichment migration
twice, checks all required tables/indexes/foreign keys, executes the rollback in
an isolated database, and confirms the pre-existing artist row survives.
