-- Runs after directory_artist_derived_listings_postgres.sql and two
-- consecutive applications of 2026-10-07_directory_artist_listing_backfill_apply.
\set ON_ERROR_STOP on

CREATE OR REPLACE FUNCTION pg_temp.expect(condition BOOLEAN, message TEXT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  IF condition IS DISTINCT FROM TRUE THEN
    RAISE EXCEPTION 'assertion failed: %', message;
  END IF;
END
$$;

DO $backfill$
DECLARE
  first_run directory_artist_listing_backfill_run;
  second_run directory_artist_listing_backfill_run;
BEGIN
  SELECT * INTO first_run FROM directory_artist_listing_backfill_run ORDER BY started_at DESC, id DESC OFFSET 1 LIMIT 1;
  SELECT * INTO second_run FROM directory_artist_listing_backfill_run ORDER BY started_at DESC, id DESC LIMIT 1;

  PERFORM pg_temp.expect(first_run.listings_created = 2,
    format('first run creates the two missing legacy listings (created %s)', first_run.listings_created));
  PERFORM pg_temp.expect(first_run.listings_reused > 0, 'existing derived relationships are reused, not recreated');
  PERFORM pg_temp.expect(second_run.id <> first_run.id AND second_run.listings_created = 0,
    'rerunning the backfill creates nothing');

  PERFORM pg_temp.expect((SELECT count(*) FROM classified WHERE source_profile_id = 'a0000000-0000-4000-8000-000000000001') = 1
    AND (SELECT count(*) FROM classified WHERE source_profile_id = 'a0000000-0000-4000-8000-000000000002') = 1,
    'each legacy artist profile has exactly one derived listing');
  PERFORM pg_temp.expect((SELECT count(*) FROM classified WHERE source_profile_id = 'a0000000-0000-4000-8000-000000000005') = 0,
    'unpublished legacy artist is not advertised');
  PERFORM pg_temp.expect(NOT EXISTS (
      SELECT source_profile_id FROM classified WHERE source_profile_id IS NOT NULL
      GROUP BY source_profile_id HAVING count(*) > 1),
    'no profile owns more than one derived listing');

  -- Ambiguous manual listing is reported and left untouched.
  PERFORM pg_temp.expect(EXISTS (
      SELECT 1 FROM directory_artist_listing_audit_finding finding
      JOIN classified item ON item.id = finding.classified_id
      WHERE finding.finding_kind = 'ambiguous_manual_artist_listing' AND item.slug = 'legacy-artist-manual'),
    'lookalike manual listing is recorded for human review');
  PERFORM pg_temp.expect((SELECT status = 'published' AND source_profile_id IS NULL
      FROM classified WHERE slug = 'legacy-artist-manual'),
    'manual listing keeps its category, status and independence');

  -- Brand profile regressions: real media instead of the placeholder.
  PERFORM pg_temp.expect((SELECT image_url FROM directory_public_search_document
      WHERE entity_kind = 'profile' AND slug = 'tdf-records-estudio') = 'https://www.tdfrecords.net/tdf-app-icon-1024.png',
    'TDF Records resolves its real image');
  PERFORM pg_temp.expect((SELECT image_url FROM directory_public_search_document
      WHERE entity_kind = 'profile' AND slug = 'domo-del-pululahua') = 'https://www.tdfrecords.net/assets/tdf-ui/domo-pululahua-hero-cozy.jpg',
    'Domo del Pululahua resolves its real image');
  PERFORM pg_temp.expect(NOT EXISTS (SELECT 1 FROM classified WHERE source_profile_id IN (
      'a0000000-0000-4000-8000-000000000003','a0000000-0000-4000-8000-000000000004')),
    'studio and venue profiles never get artist listings');
  PERFORM pg_temp.expect((SELECT count(*) FROM directory_audit_event WHERE action = 'profile.cover_assigned') = 2,
    'brand covers are assigned once and audited');

  PERFORM pg_temp.expect(NOT EXISTS (
      SELECT 1 FROM directory_artist_listing_audit()
      WHERE finding_kind IN ('missing_listing','duplicate_derived_listing','public_listing_non_public_profile',
                             'stale_listing_search_document','inconsistent_listing_image')),
    'audit is clean after the backfill');
END
$backfill$;

-- An existing venue image wins over the designated brand asset.
DO $venue_media$
DECLARE
  venue_id BIGINT;
  profile_id_value UUID := gen_random_uuid();
BEGIN
  INSERT INTO venue (name, contact, created_at, updated_at)
  VALUES ('Venue With Photo', '{"imageUrl":"https://cdn.example.test/venue.jpg","phone":"+593"}', now(), now())
  RETURNING id INTO venue_id;
  INSERT INTO directory_profile (id, subject_party_id, profile_kind, public_name, slug, bio, profile_status,
    visibility, moderation_status, completeness_score, published_at)
  VALUES (profile_id_value, 910003, 'venue', 'Venue With Photo', 'venue-with-photo',
    'Venue con fotografía propia en su registro.', 'published', 'public', 'allowed', .8, now());
  INSERT INTO directory_legacy_link (profile_id, legacy_kind, legacy_id, source_table)
  VALUES (profile_id_value, 'venue', venue_id::text, 'venue');
  PERFORM directory_refresh_profile_search(profile_id_value);
  PERFORM pg_temp.expect((SELECT image_url FROM directory_public_search_document
      WHERE entity_kind = 'profile' AND entity_id = profile_id_value::text) = 'https://cdn.example.test/venue.jpg',
    'canonical venue media is projected to the preview card');
END
$venue_media$;
