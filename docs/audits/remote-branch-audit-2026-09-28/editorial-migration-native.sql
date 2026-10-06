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

DO $$ BEGIN ASSERT NOT directory_social_event_metadata_is_public('{"isPublic":true,"_discoveryOwned":{}}'), 'Expected historical defect'; END $$;
\i /private/tmp/tdf-branch-audit-20260928/editorial-repair-20261003/tdf-hq/sql/2026-10-03_discovery_ownership_metadata_boundary.sql
DO $$ BEGIN ASSERT NOT EXISTS(SELECT 1 FROM (VALUES ('{"isPublic":true,"_discoveryOwned":{}}', true), ('{"isPublic":true,"_discoveryOwned":{"isPublic":false}}', true), ('{"isPublic":false,"_discoveryOwned":{"isPublic":true}}', false), ('{"isPublic":true,"_discoveryOwned":null}', false), ('{"isPublic":true,"_discoveryOwned":false}', false), ('{"isPublic":true,"_discoveryOwned":[]}', false), ('{"isPublic":true,"_discoveryOwned":{},"unexpected":1}', false), ('{"isPublic":true,"_discoveryOwned":{},"_discoveryOwned":{}}', false), ('{"isPublic":true,"budgetCents":1.5,"_discoveryOwned":{}}', false), ('{"isPublic":true,"budgetCents":9223372036854775807.0,"_discoveryOwned":{}}', true), ('{"isPublic":true,"budgetCents":9223372036854775808,"_discoveryOwned":{}}', false), ('{"isPublic":true,"currency":false,"_discoveryOwned":{}}', false), ('{"isPublic":true,"_discoveryOwned":{},"isPublic":false}', false), ('{', false), ('[]', false), ('{"isPublic":true}', true)) AS fixture(metadata,expected) WHERE directory_social_event_metadata_is_public(metadata) IS DISTINCT FROM expected), 'Metadata boundary regression'; END $$;
\i /private/tmp/tdf-branch-audit-20260928/editorial-repair-20261003/tdf-hq/sql/2026-10-03_discovery_ownership_metadata_boundary.sql
DO $$ BEGIN ASSERT NOT EXISTS(SELECT 1 FROM (VALUES ('{"isPublic":true,"_discoveryOwned":{}}', true), ('{"isPublic":true,"_discoveryOwned":{"isPublic":false}}', true), ('{"isPublic":false,"_discoveryOwned":{"isPublic":true}}', false), ('{"isPublic":true,"_discoveryOwned":null}', false), ('{"isPublic":true,"_discoveryOwned":false}', false), ('{"isPublic":true,"_discoveryOwned":[]}', false), ('{"isPublic":true,"_discoveryOwned":{},"unexpected":1}', false), ('{"isPublic":true,"_discoveryOwned":{},"_discoveryOwned":{}}', false), ('{"isPublic":true,"budgetCents":1.5,"_discoveryOwned":{}}', false), ('{"isPublic":true,"budgetCents":9223372036854775807.0,"_discoveryOwned":{}}', true), ('{"isPublic":true,"budgetCents":9223372036854775808,"_discoveryOwned":{}}', false), ('{"isPublic":true,"currency":false,"_discoveryOwned":{}}', false), ('{"isPublic":true,"_discoveryOwned":{},"isPublic":false}', false), ('{', false), ('[]', false), ('{"isPublic":true}', true)) AS fixture(metadata,expected) WHERE directory_social_event_metadata_is_public(metadata) IS DISTINCT FROM expected), 'Metadata boundary regression'; END $$;
SELECT 'PASS: reproduced old directory defect; 16 cases pass after apply and reapply';
