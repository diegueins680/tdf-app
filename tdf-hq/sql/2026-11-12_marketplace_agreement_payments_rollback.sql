BEGIN;

-- Rollback de 2026-11-12_marketplace_agreement_payments (ADR 0136).
-- Se niega a ejecutarse si existe algún cobro pagado: ese dinero ya se movió y
-- su evidencia (checkout, intento, recibo, libro contable) no se reescribe.
-- Los acuerdos que esperaban un cobro en la plataforma vuelven a coordinarse
-- fuera de ella; los checkouts canónicos se conservan como historial.

DO $$ DECLARE paid_exists BOOLEAN := FALSE; BEGIN
  IF to_regclass('marketplace_agreement_payment') IS NOT NULL THEN
    EXECUTE 'SELECT EXISTS (SELECT 1 FROM marketplace_agreement_payment WHERE status = ''paid'')' INTO paid_exists;
  END IF;
  IF paid_exists THEN
    RAISE EXCEPTION 'marketplace agreement payments rollback refused: paid agreement payments exist';
  END IF;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_agreement_sync_verified_checkout ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_marketplace_agreement_require_verified_payment ON commerce_checkout_session;
DROP FUNCTION IF EXISTS marketplace_agreement_sync_verified_checkout();
DROP FUNCTION IF EXISTS marketplace_agreement_checkout_require_verified_payment();
DROP TRIGGER IF EXISTS marketplace_agreement_paid_payment_trigger ON marketplace_agreement;
DROP FUNCTION IF EXISTS marketplace_agreement_require_paid_payment();

-- Sin cobros en plataforma, un acuerdo que esperaba depósito queda confirmado
-- fuera de plataforma (igual que en la parte A). El trigger de transición del
-- acuerdo permite pending_deposit -> confirmed.
UPDATE marketplace_agreement
   SET status = 'confirmed', settlement_mode = 'off_platform', deposit_checkout_id = NULL,
       deposit_due_at = NULL, updated_at = now(), version = version + 1
 WHERE status = 'pending_deposit';
UPDATE marketplace_agreement
   SET settlement_mode = 'off_platform', deposit_checkout_id = NULL, balance_checkout_id = NULL,
       updated_at = now(), version = version + 1
 WHERE settlement_mode = 'platform' AND status NOT IN ('pending_deposit','balance_due');
-- Un acuerdo que esperaba el saldo no puede cobrarlo ya: pasa a disputa para
-- que un administrador lo resuelva con las partes.
UPDATE marketplace_agreement
   SET status = 'disputed', disputed_at = coalesce(disputed_at, now()), balance_checkout_id = NULL,
       updated_at = now(), version = version + 1
 WHERE status = 'balance_due';
UPDATE marketplace_agreement
   SET settlement_mode = 'off_platform', deposit_checkout_id = NULL
 WHERE settlement_mode = 'platform';

-- DROP TABLE elimina también sus triggers.
DROP TABLE IF EXISTS marketplace_agreement_payment;
DROP FUNCTION IF EXISTS marketplace_guard_agreement_payment_insert();
DROP FUNCTION IF EXISTS marketplace_guard_agreement_payment();
DROP FUNCTION IF EXISTS marketplace_agreement_payment_transition_allowed(TEXT, TEXT);
DROP TABLE IF EXISTS marketplace_payout_account;
DROP TABLE IF EXISTS marketplace_payment_fee_rule;

-- Relación de transiciones del acuerdo tal como la dejó ADR 0135.
CREATE OR REPLACE FUNCTION marketplace_agreement_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('pending_deposit','confirmed'), ('pending_deposit','cancelled'), ('pending_deposit','expired'),
    ('confirmed','in_progress'), ('confirmed','completed'), ('confirmed','cancelled'), ('confirmed','disputed'),
    ('in_progress','balance_due'), ('in_progress','completed'), ('in_progress','cancelled'), ('in_progress','disputed'),
    ('balance_due','completed'), ('balance_due','disputed'),
    ('disputed','completed'), ('disputed','cancelled')
  );
$$;

DELETE FROM notification
 WHERE notif_type IN ('marketplace.agreement.deposit_paid','marketplace.agreement.balance_due','marketplace.agreement.expired');

DO $$ DECLARE saved RECORD; BEGIN
  IF to_regclass('marketplace_agreement_payments_migration_state') IS NULL THEN RETURN; END IF;
  SELECT * INTO saved FROM marketplace_agreement_payments_migration_state WHERE id = 1;
  IF NOT FOUND THEN RETURN; END IF;
  IF saved.notification_expression IS NOT NULL AND EXISTS (
    SELECT 1 FROM pg_constraint WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check'
      AND pg_get_expr(conbin, conrelid) LIKE '%marketplace.agreement.deposit_paid%') THEN
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK (%s)', saved.notification_expression);
  END IF;
END $$;
DROP TABLE IF EXISTS marketplace_agreement_payments_migration_state;

DELETE FROM revenue_feature_flag WHERE flag_key = 'marketplace.agreement_payments';

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.rollback', 'migration', '2026-11-12_marketplace_agreement_payments', 'applied', 'rolled_back',
        'migration-2026-11-12-marketplace-agreement-payments-rollback', '{"adr":"0136"}'::jsonb);

COMMIT;
