-- Compose both released privacy boundaries after either historical migration order.
-- Earlier migrations are immutable and may already be recorded independently.
-- Forward-only recovery: never restore a view that drops either predicate.
\set ON_ERROR_STOP on
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

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
  AND directory_social_event_metadata_is_public(event.metadata)
  AND NOT EXISTS (
    SELECT 1
    FROM external_event_ref reference
    WHERE reference.event_id = event.id
      AND lower(btrim(reference.source_status)) = 'suppressed'
  );

COMMIT;
