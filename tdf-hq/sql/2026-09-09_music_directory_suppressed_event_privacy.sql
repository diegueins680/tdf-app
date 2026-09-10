-- Forward-only privacy repair for imported events carrying a suppression tombstone.
-- The released music-directory core migration is immutable because deployed databases
-- can already have its checksum recorded in the production migration ledger.
\set ON_ERROR_STOP on
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Public event and venue detail must fail closed when any provider identity for
-- an event has been suppressed. This also prevents a later active provider
-- identity from making a tombstoned canonical event visible again.
CREATE OR REPLACE VIEW directory_public_event AS
SELECT
  event.id,
  event.title,
  event.description,
  event.start_time,
  event.end_time,
  event.timezone,
  event.price_cents,
  event.currency_id,
  event.capacity,
  event.venue_id,
  venue.name AS venue_name,
  city.id AS city_id,
  coalesce(city.name_es, venue.city) AS city_name,
  country.alpha2 AS country_code,
  city.latitude AS public_latitude,
  city.longitude AS public_longitude,
  event.updated_at
FROM social_event event
JOIN workflow_state state ON state.id = event.workflow_state_id
JOIN workflow_state_capability capability
  ON capability.state_id = state.id
 AND capability.capability_code = 'public-listable'
 AND capability.enabled
LEFT JOIN venue ON venue.id = event.venue_id
LEFT JOIN city_reference city ON city.id = venue.city_id
LEFT JOIN country_reference country
  ON country.id = coalesce(venue.country_id, city.country_id)
WHERE state.active
  AND NOT EXISTS (
    SELECT 1
    FROM external_event_ref reference
    WHERE reference.event_id = event.id
      AND lower(btrim(reference.source_status)) = 'suppressed'
  );

-- Search rows are materialized. Recheck live event/venue eligibility in the
-- anonymous view so a stale row cannot leak a tombstoned event between refreshes.
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

DO $privacy_gate$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM directory_public_event event
    JOIN external_event_ref reference ON reference.event_id = event.id
    WHERE lower(btrim(reference.source_status)) = 'suppressed'
  ) THEN
    RAISE EXCEPTION
      'directory privacy repair failed: suppressed events remain public';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM directory_public_search_document document
    WHERE document.entity_kind = 'event'
      AND NOT EXISTS (
        SELECT 1
        FROM directory_public_event event
        WHERE event.id::text = document.entity_id
      )
  ) THEN
    RAISE EXCEPTION
      'directory privacy repair failed: ineligible event search rows remain public';
  END IF;

  IF EXISTS (
    SELECT 1
    FROM directory_public_search_document document
    WHERE document.entity_kind = 'venue'
      AND NOT EXISTS (
        SELECT 1
        FROM directory_public_venue venue
        WHERE venue.id::text = document.entity_id
      )
  ) THEN
    RAISE EXCEPTION
      'directory privacy repair failed: ineligible venue search rows remain public';
  END IF;
END
$privacy_gate$;

COMMIT;
