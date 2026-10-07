# Canonical preview images and artist-profile derived listings

Status: implemented 2026-10-07 (`2026-10-07_directory_artist_derived_listings`,
`2026-10-07_directory_artist_listing_backfill_apply`).

## Preview images

`directory_profile_preview_image_url(profile_id)` is the only resolver for a
directory profile's preview image. Every projection (search documents, public
profile `previewImageUrl`, derived listing `imageUrl`, managed profiles) calls it;
web and mobile only make the URL absolute and show the per-kind placeholder when
it returns `NULL` or the image fails to load.

Priority:

1. `directory_profile.cover_image_url` (designated cover), then a portfolio image
   explicitly marked `featured`/`primary`/`cover`.
2. Primary linked profile media through `directory_legacy_link`: artist
   `hero_image_url`, band `photo_url`, venue `contact.imageUrl`.
3. Avatar or logo: linked social artist `avatar_url`, active merch store `logo_image_url`.
4. First valid portfolio image.
5. `NULL` → client placeholder.

`directory_safe_image_url` accepts HTTPS/HTTP hosts and same-origin paths only,
rejects credentials, backslashes and whitespace, and rewrites Google Drive viewer
links to the public content endpoint. Event and venue search documents now carry
their own images (event metadata `imageUrl`, venue `contact.imageUrl`).

Root cause of the TDF Records / Domo del Pululahua placeholders: both profiles were
created by hand on 2026-08-18 with an empty portfolio, and search documents only
read the first portfolio image. Their media existed only as public web-app assets,
so the backfill designates those assets as covers, audited and only when the
resolver finds no other media.

## Artist classification

`directory_profile_kind_is_artist(kind)` is true for `artist` and `band`, the
artist kinds of the directory taxonomy. Persons, projects, venues, studios, labels,
agencies and organizations are not advertised. Legacy `artist_profile` rows enter
the directory through the existing claim/backfill paths; publication still
requires the owner's consent and age assurance, so no public profile is created
automatically on their behalf.

## Derived listing

- One `classified` row per profile in the `artist-profile` category
  (`requirements.derivation = 'artist-profile'`), with
  `source_profile_id = author_profile_id`.
- `classified_source_profile_uidx` (partial unique index) allows at most one
  derived row per profile, ever; lifecycle changes reuse it.
- Content (title, description, modality, radius, genres, instruments,
  professions, approximate location) is derived by
  `directory_sync_profile_listing(profile_id)` and is read-only elsewhere: the
  `directory_classified_derivation_guard_trigger` rejects manual inserts and
  content edits (only the transaction-local sync flag may write), and the API
  answers 400 for the derived category and 409 for status changes.
- Expiry: `expires_at = 'infinity'`; APIs expose `null`. The listing follows the
  profile instead of expiring.

### Synchronization and lifecycle

The sync runs inside the same transaction as every profile change:

- `directory_profile_artist_listing_sync_trigger` (AFTER INSERT/UPDATE on the
  profile row) covers lifecycle, moderation, merge, kind and content changes,
  including administrative paths;
- `directory_refresh_profile_search` ends with the sync, covering taxonomy and
  location children written by the handlers.

| Profile state | Listing |
| --- | --- |
| published, public, allowed, not merged, artist kind | `published` (created on first publication) |
| draft / pending / private / unlisted, never public | no listing is created |
| paused, archived, suspended, merged, unlisted, private, blocked, or kind no longer artist | `paused` (hidden from discovery) |
| public again | same row back to `published` |
| moderator set listing `moderated`/`withdrawn` | final; never revived by the profile |

Archive no longer withdraws the derived row (manual classifieds are still
withdrawn), so restoring a profile restores its listing without duplicates.

### Privacy

Only `directory_profile_location` (public) is read: sector and commercial-exact
precision are reduced to the city, country/region precision stays at that level,
and `directory_private_location` is never read. Search documents use the city
centroid.

### Discovery

Mixed `/buscar` results exclude derived listings, because their profile already
appears; the Classifieds tab and `entityType=classified` include them, with
`sourceProfile` linking to the canonical profile ("Ver perfil"). Saved-search
alerts and favorites see the same search documents.

## Backfill, audit and recovery

- `2026-10-07_directory_artist_listing_backfill_dry_run.sql`: read-only preview.
- `..._backfill_apply.sql`: assigns the two audited brand covers, runs
  `directory_reconcile_artist_listings()` (idempotent), stores counts in
  `directory_artist_listing_backfill_run` and findings in
  `directory_artist_listing_audit_finding`, and fails if any hard invariant is
  violated.
- `directory_artist_listing_audit()` reports missing/duplicate listings, public
  listings of non-public profiles, orphaned non-artist listings, image or content
  divergence, stale search rows and ambiguous manual lookalikes. Manual listings
  are never merged or re-categorized: they keep their own category semantics and
  are reported for human review.
- Recovery after any incident: `SELECT * FROM directory_reconcile_artist_listings();`
  then re-run the audit.

## Rollout and rollback

Additive, forward-only: nullable columns, a small partial unique index, a seeded
category and function/trigger replacements; the public search view is unchanged so
older registered migrations remain re-applicable. Old and new application versions
are compatible (old code ignores new columns; the guard only constrains derived
rows). `_rollback.sql` drops the triggers, restores the previous projection
functions and pauses derived rows without deleting them.

Verification: `scripts/test-directory-artist-listings.sh` (production-shaped
schema, behavior, two-session concurrency, backfill twice, rollback/re-apply and,
with a backend binary, the HTTP flow) plus the production schema verification
gate.
