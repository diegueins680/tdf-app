-- Preserve the immutable catalog-integrity migration while retaining its later
-- scoped revalidation. Valid aliases are left untouched; any invalid alias is
-- routed through the existing integrity trigger and fails the transaction.
BEGIN;

UPDATE catalog_slug_alias alias
SET entity_id = alias.entity_id
WHERE NOT (
  (
    alias.entity_kind = 'authored_content'
    AND EXISTS (
      SELECT 1
      FROM authored_content target
      JOIN catalog_definition catalog
        ON catalog.id = alias.catalog_id
       AND catalog.code = 'authored-content'
      WHERE target.id = alias.entity_id
    )
  )
  OR (
    alias.entity_kind = 'record-release'
    AND EXISTS (
      SELECT 1 FROM record_release target
      WHERE target.id = alias.entity_id
        AND target.catalog_id = alias.catalog_id
    )
  )
  OR (
    alias.entity_kind = 'recording'
    AND EXISTS (
      SELECT 1 FROM recording target
      WHERE target.id = alias.entity_id
        AND target.catalog_id = alias.catalog_id
    )
  )
  OR (
    alias.entity_kind = 'recording-session'
    AND EXISTS (
      SELECT 1 FROM recording_session target
      WHERE target.id = alias.entity_id
        AND target.catalog_id = alias.catalog_id
    )
  )
);

COMMIT;
