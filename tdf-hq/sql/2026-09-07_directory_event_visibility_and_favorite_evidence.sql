-- Close the anonymous directory-event visibility gap without mutating source
-- events. The predicate intentionally mirrors the stricter public upcoming
-- event decoder: missing metadata is public, while malformed, duplicated,
-- unsupported, incorrectly typed, and explicitly private metadata fails
-- closed.
CREATE OR REPLACE FUNCTION directory_social_event_metadata_is_public(event_metadata TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN event_metadata IS NULL OR btrim(event_metadata) = '' THEN TRUE
    WHEN NOT pg_input_is_valid(event_metadata, 'jsonb') THEN FALSE
    WHEN jsonb_typeof(event_metadata::jsonb) <> 'object' THEN FALSE
    ELSE
      (SELECT count(*) FROM json_each(event_metadata::json)) =
        (SELECT count(DISTINCT metadata_field.key)
         FROM json_each(event_metadata::json) AS metadata_field)
      AND NOT EXISTS (
        SELECT 1
        FROM jsonb_object_keys(event_metadata::jsonb) AS metadata_key
        WHERE metadata_key NOT IN ('ticketUrl', 'imageUrl', 'isPublic', 'currency', 'budgetCents')
      )
      AND (
        jsonb_typeof(event_metadata::jsonb -> 'ticketUrl') IS NULL
        OR jsonb_typeof(event_metadata::jsonb -> 'ticketUrl') IN ('null', 'string')
      )
      AND (
        jsonb_typeof(event_metadata::jsonb -> 'imageUrl') IS NULL
        OR jsonb_typeof(event_metadata::jsonb -> 'imageUrl') IN ('null', 'string')
      )
      AND (
        jsonb_typeof(event_metadata::jsonb -> 'currency') IS NULL
        OR jsonb_typeof(event_metadata::jsonb -> 'currency') IN ('null', 'string')
      )
      AND CASE
        WHEN jsonb_typeof(event_metadata::jsonb -> 'budgetCents') IS NULL
          OR jsonb_typeof(event_metadata::jsonb -> 'budgetCents') = 'null' THEN TRUE
        WHEN jsonb_typeof(event_metadata::jsonb -> 'budgetCents') <> 'number' THEN FALSE
        WHEN NOT pg_input_is_valid(event_metadata::jsonb ->> 'budgetCents', 'numeric') THEN FALSE
        ELSE
          (event_metadata::jsonb ->> 'budgetCents')::numeric =
            trunc((event_metadata::jsonb ->> 'budgetCents')::numeric)
          AND (event_metadata::jsonb ->> 'budgetCents')::numeric
            BETWEEN -9223372036854775808 AND 9223372036854775807
      END
      AND (
        jsonb_typeof(event_metadata::jsonb -> 'isPublic') IS NULL
        OR jsonb_typeof(event_metadata::jsonb -> 'isPublic') = 'null'
        OR event_metadata::jsonb -> 'isPublic' = 'true'::jsonb
      )
  END
$$;

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
LEFT JOIN country_reference country ON country.id = coalesce(venue.country_id, city.country_id)
WHERE state.active
  AND directory_social_event_metadata_is_public(event.metadata);

-- Delete only derived public projections that no longer satisfy the repaired
-- boundary. Source events, venues, purchases, and user-owned favorites remain
-- untouched. The existing refresh function will use the corrected views on
-- subsequent refreshes without replaying alerts during this migration.
DELETE FROM directory_search_document document
WHERE document.entity_kind = 'event'
  AND NOT EXISTS (
    SELECT 1
    FROM directory_public_event event
    WHERE event.id::text = document.entity_id
  );

DELETE FROM directory_search_document document
WHERE document.entity_kind = 'venue'
  AND NOT EXISTS (
    SELECT 1
    FROM directory_public_venue venue
    WHERE venue.id::text = document.entity_id
  );

-- Supports Party- and time-bounded first-value evidence without changing the
-- append-only audit record used by directory operations.
CREATE INDEX IF NOT EXISTS directory_audit_actor_action_created_idx
  ON directory_audit_event (actor_party_id, action, created_at DESC);

-- Deliberately no confidentiality-weakening rollback: reverting this boundary
-- could re-expose private events. Recovery should restore this migration or a
-- stricter replacement, never the prior view definition.
