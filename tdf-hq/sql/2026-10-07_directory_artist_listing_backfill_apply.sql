-- Retroactive derived listings for existing artist profiles, plus a persisted
-- consistency audit. Idempotent: rerunning creates nothing new because each
-- profile owns at most one derived listing (classified_source_profile_uidx)
-- and directory_sync_profile_listing() reuses it.
--
-- Manual classifieds are never re-categorized or merged: they keep their own
-- category semantics. Lookalikes are recorded as ambiguous findings for human
-- review in directory_artist_listing_audit_finding.
\set ON_ERROR_STOP on
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '30min';

CREATE TABLE IF NOT EXISTS directory_artist_listing_backfill_run (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  finished_at TIMESTAMPTZ,
  profiles_processed INTEGER NOT NULL DEFAULT 0,
  listings_created INTEGER NOT NULL DEFAULT 0,
  listings_reused INTEGER NOT NULL DEFAULT 0,
  listings_published INTEGER NOT NULL DEFAULT 0,
  covers_assigned INTEGER NOT NULL DEFAULT 0,
  findings_recorded INTEGER NOT NULL DEFAULT 0,
  CHECK (profiles_processed >= 0 AND listings_created >= 0 AND listings_reused >= 0
    AND listings_published >= 0 AND covers_assigned >= 0 AND findings_recorded >= 0)
);

CREATE TABLE IF NOT EXISTS directory_artist_listing_audit_finding (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  run_id UUID NOT NULL REFERENCES directory_artist_listing_backfill_run(id),
  finding_kind TEXT NOT NULL,
  profile_id UUID REFERENCES directory_profile(id),
  classified_id UUID REFERENCES classified(id),
  detail JSONB NOT NULL DEFAULT '{}'::jsonb,
  review_status TEXT NOT NULL DEFAULT 'open',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (finding_kind ~ '^[a-z][a-z_]{2,79}$'),
  CHECK (review_status IN ('open','resolved','dismissed')),
  CHECK (jsonb_typeof(detail) = 'object')
);
CREATE INDEX IF NOT EXISTS directory_artist_listing_audit_finding_open_idx
  ON directory_artist_listing_audit_finding (finding_kind, created_at DESC)
  WHERE review_status = 'open';

-- Listings created for artists that were already public are not new results:
-- saved-search alerts stay off for this transaction.
-- Sync lock first, the order every concurrent sync uses, so a write that
-- commits while this runs cannot invert the lock order.
SELECT directory_search_sync_lock();
ALTER TABLE directory_search_document DISABLE TRIGGER directory_search_alert_trigger;

DO $backfill$
DECLARE
  run_id_value UUID := gen_random_uuid();
  derived_before INTEGER;
  derived_after INTEGER;
  processed INTEGER;
  published_count INTEGER;
  cover_count INTEGER := 0;
  finding_count INTEGER;
  brand RECORD;
BEGIN
  INSERT INTO directory_artist_listing_backfill_run (id) VALUES (run_id_value);

  -- Brand profiles created by hand on 2026-08-18 with an empty portfolio.
  -- Their media already ships with the public web app; it is designated as the
  -- profile cover only when no profile media resolves, so real uploads win.
  FOR brand IN
    SELECT profile.id, seed.cover_url
    FROM (VALUES
      ('domo-del-pululahua', 'https://www.tdfrecords.net/assets/tdf-ui/domo-pululahua-hero-cozy.jpg'),
      ('tdf-records-estudio', 'https://www.tdfrecords.net/tdf-app-icon-1024.png')
    ) AS seed(slug, cover_url)
    JOIN directory_profile profile ON profile.slug = seed.slug
    WHERE profile.cover_image_url IS NULL
      AND directory_profile_preview_image_url(profile.id) IS NULL
  LOOP
    UPDATE directory_profile SET cover_image_url = brand.cover_url WHERE id = brand.id;
    PERFORM directory_refresh_profile_search(brand.id);
    INSERT INTO directory_audit_event(action, entity_kind, entity_id, correlation_id, metadata)
    VALUES ('profile.cover_assigned', 'profile', brand.id::text,
            'artist-listing-backfill-' || run_id_value::text,
            jsonb_build_object('coverImageUrl', brand.cover_url, 'source', 'web-app-public-asset'));
    cover_count := cover_count + 1;
  END LOOP;

  SELECT count(*) INTO derived_before FROM classified WHERE source_profile_id IS NOT NULL;

  SELECT count(*), count(*) FILTER (WHERE reconciled.listing_status = 'published')
    INTO processed, published_count
  FROM directory_reconcile_artist_listings() reconciled;

  SELECT count(*) INTO derived_after FROM classified WHERE source_profile_id IS NOT NULL;

  INSERT INTO directory_artist_listing_audit_finding (run_id, finding_kind, profile_id, classified_id, detail)
  SELECT run_id_value, finding.finding_kind, finding.profile_id, finding.classified_id, finding.detail
  FROM directory_artist_listing_audit() finding;
  GET DIAGNOSTICS finding_count = ROW_COUNT;

  UPDATE directory_artist_listing_backfill_run SET
    finished_at = now(),
    profiles_processed = processed,
    listings_created = derived_after - derived_before,
    listings_reused = derived_before,
    listings_published = published_count,
    covers_assigned = cover_count,
    findings_recorded = finding_count
  WHERE id = run_id_value;

  -- Hard invariants: fail the migration instead of leaving inconsistent state.
  IF EXISTS (
    SELECT 1 FROM directory_artist_listing_audit() finding
    WHERE finding.finding_kind IN (
      'missing_listing','duplicate_derived_listing','public_listing_non_public_profile',
      'unpublished_listing_public_profile','stale_listing_search_document')
      AND NOT (finding.finding_kind = 'unpublished_listing_public_profile'
               AND EXISTS (SELECT 1 FROM classified item
                           WHERE item.id = finding.classified_id
                             AND (item.moderation_status <> 'allowed'
                                  OR item.status IN ('moderated','withdrawn','rejected','filled'))))
  ) THEN
    RAISE EXCEPTION 'artist listing backfill left inconsistent derived listings';
  END IF;

  RAISE NOTICE 'artist listing backfill %: processed=% created=% reused=% published=% covers=% findings=%',
    run_id_value, processed, derived_after - derived_before, derived_before, published_count, cover_count, finding_count;
END
$backfill$;

ALTER TABLE directory_search_document ENABLE TRIGGER directory_search_alert_trigger;

COMMIT;
