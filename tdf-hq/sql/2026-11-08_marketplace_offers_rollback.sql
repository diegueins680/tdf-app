BEGIN;

-- Rollback de 2026-11-08_marketplace_offers (ADR 0132).
-- Conserva ofertas y anuncios: los estados nuevos se mapean a los antiguos
-- antes de restaurar la restricción original. Las rondas de negociación se
-- pierden (su monto vigente queda reflejado en proposed_amount_minor).

-- Mantener el último monto negociado como monto propuesto visible.
UPDATE classified_application
SET proposed_amount_minor = coalesce(accepted_amount_minor, current_amount_minor, proposed_amount_minor)
WHERE current_amount_minor IS NOT NULL OR accepted_amount_minor IS NOT NULL;

UPDATE classified_application SET status = 'viewed'    WHERE status = 'countered_by_author';
UPDATE classified_application SET status = 'submitted' WHERE status = 'countered_by_applicant';
UPDATE classified_application SET status = 'withdrawn', withdrawn_at = coalesce(withdrawn_at, now())
WHERE status = 'expired';

DROP TABLE IF EXISTS classified_offer_round;

-- Restaurar la expresión de tipos de notificación anterior a la migración.
-- Las notificaciones del marketplace dejan de ser válidas y se eliminan.
-- Si una migración posterior amplió la expresión, revertir esa migración
-- primero (orden inverso del ledger).
DELETE FROM notification WHERE notif_type LIKE 'marketplace.%';
DO $$ DECLARE saved TEXT; BEGIN
  SELECT notification_expression INTO saved FROM marketplace_offers_migration_state WHERE id = 1;
  IF FOUND THEN
    IF EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check') THEN
      ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    END IF;
    IF saved IS NOT NULL THEN
      EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (%s)', saved);
    END IF;
  END IF;
END $$;
-- Restaurar la expresión de eventos analíticos previa (offer_countered y
-- offer_accepted dejan de ser válidos y se eliminan).
DELETE FROM directory_analytics_event WHERE event_name IN ('offer_countered','offer_accepted');
DO $$ DECLARE saved TEXT; constraint_name TEXT; BEGIN
  SELECT analytics_expression INTO saved FROM marketplace_offers_migration_state WHERE id = 1;
  SELECT conname INTO constraint_name FROM pg_constraint
  WHERE conrelid = 'directory_analytics_event'::regclass AND contype = 'c'
    AND pg_get_expr(conbin, conrelid) LIKE '%offer_accepted%'
  ORDER BY conname LIMIT 1;
  IF constraint_name IS NOT NULL AND saved IS NOT NULL THEN
    EXECUTE format('ALTER TABLE directory_analytics_event DROP CONSTRAINT %I', constraint_name);
    EXECUTE format('ALTER TABLE directory_analytics_event ADD CONSTRAINT %I CHECK (%s)', constraint_name, saved);
  END IF;
END $$;
DROP TABLE IF EXISTS marketplace_offers_migration_state;

-- Restaurar la relación de transiciones del anuncio de 2026-08-14 (sin
-- filled -> published). Anuncios reabiertos permanecen publicados.
CREATE OR REPLACE FUNCTION directory_classified_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('draft','pending_moderation'), ('draft','published'), ('draft','withdrawn'),
    ('pending_moderation','published'), ('pending_moderation','rejected'), ('pending_moderation','withdrawn'),
    ('published','paused'), ('published','filled'), ('published','expired'), ('published','withdrawn'), ('published','moderated'),
    ('paused','published'), ('paused','filled'), ('paused','expired'), ('paused','withdrawn'), ('paused','moderated'),
    ('expired','published'), ('expired','withdrawn'),
    ('rejected','draft'), ('rejected','withdrawn'),
    ('moderated','draft'), ('moderated','withdrawn')
  );
$$;

DELETE FROM revenue_feature_flag WHERE flag_key = 'marketplace.offers';

ALTER TABLE classified_application
  DROP CONSTRAINT IF EXISTS classified_application_rejection_check,
  DROP CONSTRAINT IF EXISTS classified_application_accepted_check,
  DROP CONSTRAINT IF EXISTS classified_application_current_amount_check,
  DROP CONSTRAINT IF EXISTS classified_application_terms_check,
  DROP CONSTRAINT IF EXISTS classified_application_actor_check,
  DROP CONSTRAINT IF EXISTS classified_application_round_check,
  DROP CONSTRAINT IF EXISTS classified_application_status_check;

ALTER TABLE classified_application
  ADD CONSTRAINT classified_application_status_check
  CHECK (status IN ('submitted','viewed','shortlisted','accepted','rejected','withdrawn','conversation_open','converted'));

DROP INDEX IF EXISTS classified_application_open_idx;
DROP INDEX IF EXISTS classified_application_expiry_idx;

ALTER TABLE classified_application
  DROP COLUMN IF EXISTS rejection_reason,
  DROP COLUMN IF EXISTS accepted_at,
  DROP COLUMN IF EXISTS accepted_amount_minor,
  DROP COLUMN IF EXISTS offer_expires_at,
  DROP COLUMN IF EXISTS terms,
  DROP COLUMN IF EXISTS last_actor_role,
  DROP COLUMN IF EXISTS current_round,
  DROP COLUMN IF EXISTS current_amount_minor;

ALTER TABLE classified
  DROP CONSTRAINT IF EXISTS classified_terms_check,
  DROP CONSTRAINT IF EXISTS classified_offer_deadline_check,
  DROP CONSTRAINT IF EXISTS classified_price_mode_fixed_check,
  DROP CONSTRAINT IF EXISTS classified_price_mode_budget_check,
  DROP CONSTRAINT IF EXISTS classified_price_mode_check,
  DROP CONSTRAINT IF EXISTS classified_slots_remaining_check,
  DROP CONSTRAINT IF EXISTS classified_slots_check;

ALTER TABLE classified
  DROP COLUMN IF EXISTS terms,
  DROP COLUMN IF EXISTS offer_deadline_at,
  DROP COLUMN IF EXISTS price_mode,
  DROP COLUMN IF EXISTS slots_remaining,
  DROP COLUMN IF EXISTS slots;

ALTER TABLE classified_category
  DROP CONSTRAINT IF EXISTS classified_category_intent_check;
ALTER TABLE classified_category
  DROP COLUMN IF EXISTS intent;

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.rollback', 'migration', '2026-11-08_marketplace_offers', 'applied', 'rolled_back',
        'migration-2026-11-08-marketplace-offers', '{"adr":"0132"}'::jsonb);

COMMIT;
