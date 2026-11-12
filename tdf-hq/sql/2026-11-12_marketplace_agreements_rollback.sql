BEGIN;

-- Rollback de 2026-11-12_marketplace_agreements (ADR 0135).
-- Elimina los acuerdos y sus tarifas. Las reseñas ligadas a interacciones de
-- tipo 'marketplace_agreement' se eliminan con ellas: sin acuerdo no hay
-- interacción verificada que las respalde. Las ofertas aceptadas se conservan.

DELETE FROM directory_review WHERE interaction_id IN (
  SELECT id FROM directory_interaction WHERE interaction_kind = 'marketplace_agreement');
DROP TRIGGER IF EXISTS marketplace_agreement_transition_trigger ON marketplace_agreement;
DROP TABLE IF EXISTS marketplace_agreement;
DROP FUNCTION IF EXISTS marketplace_guard_agreement_transition();
DROP FUNCTION IF EXISTS marketplace_agreement_transition_allowed(TEXT, TEXT);
DROP TABLE IF EXISTS marketplace_agreement_pricing;
DELETE FROM directory_interaction WHERE interaction_kind = 'marketplace_agreement';

DELETE FROM notification WHERE notif_type LIKE 'marketplace.agreement.%';
DELETE FROM directory_analytics_event WHERE event_name IN ('agreement_created','agreement_completed','agreement_cancelled');

DO $$ DECLARE saved RECORD; constraint_name TEXT; BEGIN
  SELECT * INTO saved FROM marketplace_agreements_migration_state WHERE id = 1;
  IF NOT FOUND THEN RETURN; END IF;

  SELECT conname INTO constraint_name FROM pg_constraint
  WHERE conrelid = 'directory_interaction'::regclass AND contype = 'c'
    AND pg_get_expr(conbin, conrelid) LIKE '%marketplace_agreement%' ORDER BY conname LIMIT 1;
  IF constraint_name IS NOT NULL AND saved.interaction_expression IS NOT NULL THEN
    EXECUTE format('ALTER TABLE directory_interaction DROP CONSTRAINT %I', constraint_name);
    EXECUTE format('ALTER TABLE directory_interaction ADD CONSTRAINT %I CHECK (%s)', constraint_name, saved.interaction_expression);
  END IF;

  IF saved.notification_expression IS NOT NULL AND EXISTS (
    SELECT 1 FROM pg_constraint WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check'
      AND pg_get_expr(conbin, conrelid) LIKE '%marketplace.agreement.created%') THEN
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (%s)', saved.notification_expression);
  END IF;

  SELECT conname INTO constraint_name FROM pg_constraint
  WHERE conrelid = 'directory_analytics_event'::regclass AND contype = 'c'
    AND pg_get_expr(conbin, conrelid) LIKE '%agreement_created%' ORDER BY conname LIMIT 1;
  IF constraint_name IS NOT NULL AND saved.analytics_expression IS NOT NULL THEN
    EXECUTE format('ALTER TABLE directory_analytics_event DROP CONSTRAINT %I', constraint_name);
    EXECUTE format('ALTER TABLE directory_analytics_event ADD CONSTRAINT %I CHECK (%s)', constraint_name, saved.analytics_expression);
  END IF;
END $$;
DROP TABLE IF EXISTS marketplace_agreements_migration_state;

DELETE FROM revenue_feature_flag WHERE flag_key = 'marketplace.agreements';

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.rollback', 'migration', '2026-11-12_marketplace_agreements', 'applied', 'rolled_back',
        'migration-2026-11-12-marketplace-agreements-rollback', '{"adr":"0135"}'::jsonb);

COMMIT;
