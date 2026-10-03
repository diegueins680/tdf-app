-- Accept the private ingestion snapshot while preserving every public metadata
-- type, duplicate-key and privacy check. Existing migration bytes are immutable.
-- No source data or public views are changed; suppression remains enforced by
-- the composed directory_public_event view.
\set ON_ERROR_STOP on
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';
DO $$ BEGIN
  IF to_regprocedure('directory_social_event_metadata_is_public(text)') IS NULL THEN
    RAISE EXCEPTION 'Directory metadata visibility prerequisite is missing';
  END IF;
END $$;

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
        WHERE metadata_key NOT IN ('ticketUrl', 'imageUrl', 'isPublic', 'currency', 'budgetCents', '_discoveryOwned')
      )
      AND (
        jsonb_typeof(event_metadata::jsonb -> '_discoveryOwned') IS NULL
        OR jsonb_typeof(event_metadata::jsonb -> '_discoveryOwned') = 'object'
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

COMMIT;
