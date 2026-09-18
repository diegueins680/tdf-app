\set ON_ERROR_STOP on
BEGIN;
-- Run after rolling application code back. Preserve newly emitted UUID metadata:
-- the nullable additive column is intentionally retained for forward recovery.
UPDATE notification n SET target_type=b.previous_type,target_id=b.previous_id,target_key=b.previous_key
FROM notification_navigation_backfill b WHERE n.id=b.notification_id
 AND n.target_type IS NOT DISTINCT FROM b.resolved_type
 AND n.target_id IS NOT DISTINCT FROM b.resolved_id
 AND n.target_key IS NOT DISTINCT FROM b.resolved_key;
CREATE OR REPLACE FUNCTION directory_enqueue_saved_search_alerts()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.sponsored OR NEW.source_status<>'published' OR NEW.visibility<>'public'
     OR NEW.moderation_status<>'allowed' OR (NEW.expires_at IS NOT NULL AND NEW.expires_at<=now()) THEN
    RETURN NEW;
  END IF;
  WITH matches AS (
    SELECT saved.id,saved.account_party_id
    FROM directory_saved_search saved
    WHERE saved.alerts_enabled AND saved.alert_frequency<>'off'
      AND (saved.canonical_query->>'q' IS NULL OR saved.canonical_query->>'q'='' OR
        NEW.search_vector @@ plainto_tsquery('simple',directory_normalize_text(saved.canonical_query->>'q')) OR
        directory_text_similarity(NEW.search_text,saved.canonical_query->>'q')>=.2)
      AND (saved.canonical_query->>'entityType' IS NULL OR saved.canonical_query->>'entityType'=NEW.entity_kind)
      AND (saved.canonical_query->>'cityId' IS NULL OR saved.canonical_query->>'cityId'=NEW.city_id::text)
      AND CASE WHEN saved.canonical_query->>'professionId' IS NULL THEN TRUE WHEN saved.canonical_query->>'professionId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'professionId')::uuid=ANY(NEW.profession_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'instrumentId' IS NULL THEN TRUE WHEN saved.canonical_query->>'instrumentId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'instrumentId')::uuid=ANY(NEW.instrument_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'genreId' IS NULL THEN TRUE WHEN saved.canonical_query->>'genreId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'genreId')::uuid=ANY(NEW.genre_ids) ELSE FALSE END
  ), inserted AS (
    INSERT INTO directory_alert_delivery(saved_search_id,result_kind,result_id,result_version,email_status,push_status)
    SELECT matches.id,NEW.entity_kind,NEW.entity_id,NEW.source_version,'disabled','disabled'
    FROM matches
    ON CONFLICT(saved_search_id,result_kind,result_id,result_version) DO NOTHING
    RETURNING saved_search_id
  )
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,is_read,created_at)
  SELECT saved.account_party_id,'directory.saved-search-match','Nueva coincidencia en tu alerta',
    'Hay un nuevo resultado para "'||saved.name||'".','directory_alert',FALSE,now()
  FROM inserted JOIN directory_saved_search saved ON saved.id=inserted.saved_search_id;
  UPDATE directory_saved_search saved SET last_evaluated_at=now()
  WHERE EXISTS (SELECT 1 FROM directory_alert_delivery delivery WHERE delivery.saved_search_id=saved.id AND delivery.result_kind=NEW.entity_kind AND delivery.result_id=NEW.entity_id AND delivery.result_version=NEW.source_version);
  RETURN NEW;
END;
$$;

-- Restore a preexisting type constraint only when it can retain every current row.
DO $$ DECLARE previous TEXT; compatible BOOLEAN; BEGIN
 SELECT original_expression INTO previous FROM notification_navigation_constraint_history WHERE id=1;
 IF previous IS NOT NULL THEN
   EXECUTE format('SELECT NOT EXISTS(SELECT 1 FROM notification WHERE NOT (%s))',previous) INTO compatible;
   IF compatible THEN
     ALTER TABLE notification DROP CONSTRAINT IF EXISTS notification_notif_type_check;
     EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (%s)',previous);
     DELETE FROM notification_navigation_constraint_history WHERE id=1;
   END IF;
 END IF;
END $$;

COMMIT;
