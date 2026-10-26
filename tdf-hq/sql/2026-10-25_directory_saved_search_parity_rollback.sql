BEGIN;

-- Restore the exact previous trigger body without deleting saved searches,
-- delivery receipts, notifications or hashes.
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
      -- A derived listing duplicates its profile in mixed results, so it only
      -- matches searches that ask for classifieds explicitly.
      AND (NEW.source_profile_id IS NULL OR saved.canonical_query->>'entityType'='classified')
      AND (saved.canonical_query->>'cityId' IS NULL OR saved.canonical_query->>'cityId'=NEW.city_id::text)
      -- People-only filters describe profiles and listings; an event or a
      -- venue neither carries nor is excluded by them, as in /directory/search.
      AND (NEW.entity_kind IN ('event','venue') OR (
        CASE WHEN saved.canonical_query->>'professionId' IS NULL THEN TRUE WHEN saved.canonical_query->>'professionId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'professionId')::uuid=ANY(NEW.profession_ids) ELSE FALSE END
        AND CASE WHEN saved.canonical_query->>'instrumentId' IS NULL THEN TRUE WHEN saved.canonical_query->>'instrumentId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'instrumentId')::uuid=ANY(NEW.instrument_ids) ELSE FALSE END))
      AND CASE WHEN saved.canonical_query->>'genreId' IS NULL THEN TRUE WHEN saved.canonical_query->>'genreId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'genreId')::uuid=ANY(NEW.genre_ids) ELSE FALSE END
  ), inserted AS (
    INSERT INTO directory_alert_delivery(saved_search_id,result_kind,result_id,result_version,email_status,push_status)
    SELECT matches.id,NEW.entity_kind,NEW.entity_id,NEW.source_version,'disabled','disabled'
    FROM matches
    ON CONFLICT(saved_search_id,result_kind,result_id,result_version) DO NOTHING
    RETURNING id,saved_search_id
  )
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at)
  SELECT saved.account_party_id,'directory.saved-search-match','Nueva coincidencia en tu alerta',
    'Hay un nuevo resultado para "'||saved.name||'".','directory_alert',inserted.id::text,FALSE,now()
  FROM inserted JOIN directory_saved_search saved ON saved.id=inserted.saved_search_id;
  UPDATE directory_alert_delivery delivery SET internal_notification_id=notification.id
  FROM notification WHERE delivery.id::text=notification.target_key
    AND notification.target_type='directory_alert' AND delivery.internal_notification_id IS NULL;
  UPDATE directory_saved_search saved SET last_evaluated_at=now()
  WHERE EXISTS (SELECT 1 FROM directory_alert_delivery delivery WHERE delivery.saved_search_id=saved.id AND delivery.result_kind=NEW.entity_kind AND delivery.result_id=NEW.entity_id AND delivery.result_version=NEW.source_version);
  RETURN NEW;
END;
$$;

DROP FUNCTION IF EXISTS directory_parse_saved_search(JSONB);
DROP FUNCTION IF EXISTS directory_matches_entity_and_derivation(TEXT,TEXT,UUID,TEXT);
DROP FUNCTION IF EXISTS directory_matches_non_geo_filters(TEXT,TEXT,UUID,UUID[],UUID[],UUID[],UUID[],TEXT,TSVECTOR,BOOLEAN,NUMERIC,TIMESTAMPTZ,TIMESTAMPTZ,TEXT,UUID[],TEXT,UUID,UUID,UUID,UUID,UUID,BOOLEAN,BOOLEAN,TIMESTAMPTZ,TIMESTAMPTZ);
DROP FUNCTION IF EXISTS directory_query_alias_entity_ids(TEXT);
COMMIT;
