-- Shared, non-mutating extraction for the label-projects CMS cutover.
-- Identity is preserved from a valid source UUID. Legacy non-UUID item IDs
-- receive a deterministic UUID derived from the immutable CMS row + ordinal.

CREATE TEMP TABLE label_project_note_latest ON COMMIT DROP AS
SELECT DISTINCT ON (locale)
  id AS cms_content_id,
  locale,
  payload::jsonb AS payload,
  created_by,
  created_at
FROM cms_content
WHERE slug = 'label-projects'
  AND status = 'published'
ORDER BY locale, version DESC, published_at DESC NULLS LAST, id DESC;

CREATE TEMP TABLE label_project_note_candidates ON COMMIT DROP AS
WITH expanded AS (
  SELECT
    latest.cms_content_id,
    latest.locale,
    latest.created_by,
    latest.created_at AS cms_created_at,
    item.ordinality::integer AS source_order,
    item.value AS source_item
  FROM label_project_note_latest latest
  CROSS JOIN LATERAL jsonb_array_elements(
    CASE
      WHEN jsonb_typeof(latest.payload->'items') = 'array' THEN latest.payload->'items'
      ELSE '[]'::jsonb
    END
  ) WITH ORDINALITY AS item(value, ordinality)
), normalized AS (
  SELECT
    expanded.*,
    NULLIF(btrim(source_item->>'id'), '') AS supplied_item_id,
    NULLIF(btrim(source_item->>'text'), '') AS note_text,
    CASE
      WHEN jsonb_typeof(source_item->'done') = 'boolean' THEN (source_item->>'done')::boolean
      ELSE false
    END AS completed,
    md5('label-project-note|' || cms_content_id::text || '|' || source_order::text) AS identity_hash
  FROM expanded
), identified AS (
  SELECT
    normalized.*,
    COALESCE(supplied_item_id, 'ordinal:' || source_order::text) AS source_item_id,
    CASE
      WHEN supplied_item_id ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
        THEN lower(supplied_item_id)::uuid
      ELSE (
        substr(identity_hash, 1, 8) || '-' ||
        substr(identity_hash, 9, 4) || '-4' ||
        substr(identity_hash, 14, 3) || '-8' ||
        substr(identity_hash, 18, 3) || '-' ||
        substr(identity_hash, 21, 12)
      )::uuid
    END AS entity_id
  FROM normalized
), counted AS (
  SELECT identified.*,
    count(*) OVER (PARTITION BY cms_content_id, source_item_id) AS source_identity_count,
    count(*) OVER (PARTITION BY entity_id) AS entity_identity_count
  FROM identified
)
SELECT
  counted.*,
  CASE
    WHEN jsonb_typeof(source_item) <> 'object' THEN 'rejected'
    WHEN note_text IS NULL THEN 'rejected'
    WHEN length(note_text) > 1000 THEN 'rejected'
    WHEN source_identity_count > 1 OR entity_identity_count > 1 THEN 'ambiguous'
    ELSE 'mapped'
  END AS mapping_status,
  CASE
    WHEN jsonb_typeof(source_item) <> 'object' THEN 'item is not a JSON object'
    WHEN note_text IS NULL THEN 'text is missing or blank'
    WHEN length(note_text) > 1000 THEN 'text exceeds 1000 characters'
    WHEN source_identity_count > 1 THEN 'duplicate source item id in the selected CMS version'
    WHEN entity_identity_count > 1 THEN 'multiple source items resolve to the same UUID'
    WHEN supplied_item_id IS NULL THEN 'deterministic UUID from CMS row and item ordinal'
    WHEN supplied_item_id::text !~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
      THEN 'deterministic UUID from CMS row and item ordinal; original id preserved as provenance'
    ELSE 'source UUID preserved'
  END AS mapping_evidence
FROM counted;
