\set ON_ERROR_STOP on
BEGIN;
-- Add UUID/context identities without changing notification history or read state.
ALTER TABLE notification ADD COLUMN IF NOT EXISTS target_key TEXT;
CREATE INDEX IF NOT EXISTS notification_target_key_idx ON notification(target_key) WHERE target_key IS NOT NULL;
-- Preserve any existing expression while admitting all already implemented producers.
-- Unconstrained installations stay unconstrained; unknown historical types survive.
CREATE TABLE IF NOT EXISTS notification_navigation_constraint_history (
  id INTEGER PRIMARY KEY CHECK(id=1), original_expression TEXT NOT NULL
);
DO $$ DECLARE previous TEXT; BEGIN
 SELECT pg_get_expr(conbin,conrelid) INTO previous FROM pg_constraint
 WHERE conrelid='notification'::regclass AND conname='notification_notif_type_check' AND contype='c';
 IF previous IS NOT NULL AND NOT EXISTS(SELECT 1 FROM notification_navigation_constraint_history) THEN
   INSERT INTO notification_navigation_constraint_history VALUES(1,previous);
   ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
   EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''event_logistics_route'',''internship_audit_assigned'',''internship_midpoint_reached'',''internship_assignment_blocked'',''internship_final_ready'',''internal_feedback_received'',''internal_feedback_needs_information'',''internal_feedback_ready_for_retest'',''internal_feedback_closed'',''internal_feedback_reopened'',''internal_feedback_state_changed'',''internal_feedback_information_response'',''internal_feedback_retest_recorded'',''directory.application'',''directory.invitation'',''directory.review-created'',''directory.saved-search-match''))',previous);
 END IF;
END $$;
CREATE TABLE IF NOT EXISTS notification_navigation_backfill (
  notification_id BIGINT PRIMARY KEY REFERENCES notification(id) ON DELETE CASCADE,
  previous_type TEXT, previous_id BIGINT, previous_key TEXT,
  resolved_type TEXT NOT NULL, resolved_id BIGINT, resolved_key TEXT
);
-- The engagement event is written in the same follow transaction with the exact
-- same timestamp. Only a unique actor is evidence; names/current follows are not.
INSERT INTO notification_navigation_backfill
SELECT n.id,n.target_type,n.target_id,n.target_key,'party_profile',min(e.actor_party_id),NULL
FROM notification n JOIN engagement_event e
  ON e.target_artist_id=n.recipient_party_id AND e.created_at=n.created_at
 AND e.event_type='follow' AND e.entity_type='artist' AND e.entity_id=n.target_id
WHERE n.notif_type='artist_liked' AND n.target_type='artist'
  AND n.target_id=n.recipient_party_id AND n.target_key IS NULL
GROUP BY n.id HAVING count(*)=1 AND count(e.actor_party_id)=1
ON CONFLICT DO NOTHING;
-- Audit delivery rows retain plan/report identity, template, recipient and the
-- transaction timestamp. Only one matching delivery is safe to recover.
INSERT INTO notification_navigation_backfill
SELECT n.id,n.target_type,n.target_id,n.target_key,
 CASE WHEN min(o.plan_id::text) IS NOT NULL THEN 'intern_audit_plan' ELSE 'internal_feedback_report' END,
 NULL,coalesce(min(o.plan_id::text),min(o.report_id::text))
FROM notification n JOIN intern_audit_notification_outbox o
 ON o.recipient_party_id=n.recipient_party_id AND o.template_key=n.notif_type
 AND o.created_at=n.created_at AND o.delivery_mode='immediate'
WHERE n.target_key IS NULL AND n.target_id IS NULL
 AND n.target_type IN ('internship_task','intern_audit_plan','internal_feedback_report')
 AND ((o.plan_id IS NOT NULL AND n.target_type IN ('internship_task','intern_audit_plan'))
   OR (o.report_id IS NOT NULL AND n.target_type='internal_feedback_report'))
GROUP BY n.id HAVING count(*)=1
ON CONFLICT DO NOTHING;
INSERT INTO notification_navigation_backfill
SELECT n.id,n.target_type,n.target_id,n.target_key,'directory_alert',NULL,min(d.id::text)
FROM notification n JOIN directory_alert_delivery d ON d.internal_notification_id=n.id
WHERE n.target_type='directory_alert' AND n.target_key IS NULL
GROUP BY n.id HAVING count(*)=1
ON CONFLICT DO NOTHING;
UPDATE notification n SET target_type=b.resolved_type,target_id=b.resolved_id,target_key=b.resolved_key
FROM notification_navigation_backfill b WHERE n.id=b.notification_id
 AND n.target_type IS NOT DISTINCT FROM b.previous_type
 AND n.target_id IS NOT DISTINCT FROM b.previous_id
 AND n.target_key IS NOT DISTINCT FROM b.previous_key;

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

COMMIT;
