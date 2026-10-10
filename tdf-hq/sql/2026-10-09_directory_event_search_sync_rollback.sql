-- Rollback for 2026-10-09_directory_event_search_sync.sql. Removes the sync
-- triggers and helpers and restores the original full refresh. Search rows and
-- resolved venue city ids stay: they are derived/correct data, and every query
-- still re-checks directory_public_event at read time.
BEGIN;
CREATE OR REPLACE VIEW directory_public_search_document AS
SELECT
  document.entity_kind,
  document.entity_id,
  document.slug,
  document.title,
  document.subtitle,
  document.summary,
  document.image_url,
  document.city_id,
  document.city_name,
  document.country_code,
  document.public_latitude,
  document.public_longitude,
  document.location_precision,
  document.profession_ids,
  document.service_ids,
  document.instrument_ids,
  document.genre_ids,
  document.search_text,
  document.search_vector,
  document.profile_completeness,
  document.reputation_score,
  document.availability_score,
  document.effective_at,
  document.expires_at,
  document.source_updated_at,
  document.source_version,
  document.sponsored,
  document.sponsor_disclosure,
  document.onsite,
  document.remote,
  document.available_to_travel
FROM directory_search_document document
WHERE document.source_status = 'published'
  AND document.visibility = 'public'
  AND document.moderation_status = 'allowed'
  AND (document.effective_at IS NULL OR document.effective_at <= now())
  AND (document.expires_at IS NULL OR document.expires_at > now())
  AND CASE document.entity_kind
    WHEN 'event' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_event event
        WHERE event.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    WHEN 'venue' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_venue venue
        WHERE venue.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    ELSE TRUE
  END;


DROP TRIGGER IF EXISTS trg_directory_sync_external_event_search ON external_event_ref;
DROP TRIGGER IF EXISTS trg_directory_sync_venue_search ON venue;
DROP TRIGGER IF EXISTS trg_directory_sync_social_event_search ON social_event;
DROP TRIGGER IF EXISTS trg_directory_fill_venue_city_reference ON venue;
DROP FUNCTION IF EXISTS directory_sync_external_event_search_trigger();
DROP FUNCTION IF EXISTS directory_sync_venue_search_trigger();
DROP FUNCTION IF EXISTS directory_sync_social_event_search_trigger();
DROP FUNCTION IF EXISTS directory_fill_venue_city_reference();

CREATE OR REPLACE FUNCTION directory_refresh_legacy_event_search()
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM directory_search_document WHERE entity_kind IN ('event','venue');
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,effective_at,expires_at,
    source_updated_at,source_version,sponsored
  )
  SELECT 'event',event.id::text,'evento-'||event.id::text,event.title,event.venue_name,
    event.description,event.city_id,event.city_name,event.country_code,event.public_latitude,
    event.public_longitude,'city',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name))),
    'published','public','allowed',event.start_time,event.end_time,event.updated_at,1,FALSE
  FROM directory_public_event event;
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,source_updated_at,source_version,sponsored
  )
  SELECT 'venue',venue.id::text,'venue-'||venue.id::text,venue.name,venue.city_name,
    venue.city_id,venue.city_name,venue.country_code,venue.public_latitude,venue.public_longitude,
    'city',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name))),
    'published','public','allowed',venue.updated_at,1,FALSE
  FROM directory_public_venue venue;
END;
$$;

DROP FUNCTION IF EXISTS directory_sync_event_search(BIGINT);
DROP FUNCTION IF EXISTS directory_sync_venue_search(BIGINT);
DROP FUNCTION IF EXISTS directory_social_event_metadata_image(TEXT);
DROP FUNCTION IF EXISTS directory_resolve_city_reference(TEXT);
DROP FUNCTION IF EXISTS directory_search_sync_lock();
COMMIT;
