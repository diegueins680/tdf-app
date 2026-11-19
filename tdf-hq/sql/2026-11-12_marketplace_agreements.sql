BEGIN;

-- ADR 0135: acuerdos del marketplace (Fase 2, parte A).
-- Al aceptarse una oferta con monto, y con el gate marketplace.agreements
-- activo, se crea un acuerdo que congela partes, monto, términos, comisión de
-- TDF y reparto depósito/saldo. Esta migración no cobra nada: el cobro del
-- depósito (checkout), la liquidación y los reembolsos llegan en la parte B.
-- Aditiva e idempotente; el rollback está en
-- 2026-11-12_marketplace_agreements_rollback.sql.

-- ---------------------------------------------------------------------------
-- 1. Tarifas: comisión y depósito en puntos básicos, por categoría (NULL =
--    regla por defecto). Se editan con UPDATE, sin desplegar; cada acuerdo
--    congela los valores vigentes al crearse.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_agreement_pricing (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  category_id UUID REFERENCES classified_category(id),
  commission_bps INTEGER NOT NULL CHECK (commission_bps BETWEEN 0 AND 10000),
  deposit_bps INTEGER NOT NULL CHECK (deposit_bps BETWEEN 0 AND 10000),
  deposit_due_hours INTEGER NOT NULL DEFAULT 72 CHECK (deposit_due_hours BETWEEN 1 AND 720),
  cancellation_policy JSONB NOT NULL DEFAULT '{"version":1,"freeCancellationHours":72,"lateCancellationDepositRetainedBps":10000}'::jsonb,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (jsonb_typeof(cancellation_policy) = 'object')
);
CREATE UNIQUE INDEX IF NOT EXISTS marketplace_agreement_pricing_category_uidx
  ON marketplace_agreement_pricing (category_id) WHERE category_id IS NOT NULL AND active;
CREATE UNIQUE INDEX IF NOT EXISTS marketplace_agreement_pricing_default_uidx
  ON marketplace_agreement_pricing ((TRUE)) WHERE category_id IS NULL AND active;

-- Regla por defecto: 10 % de comisión, 30 % de depósito.
INSERT INTO marketplace_agreement_pricing (category_id, commission_bps, deposit_bps)
SELECT NULL, 1000, 3000
WHERE NOT EXISTS (SELECT 1 FROM marketplace_agreement_pricing WHERE category_id IS NULL AND active);

-- ---------------------------------------------------------------------------
-- 2. Acuerdo: uno por oferta aceptada. buyer = quien busca y paga; provider =
--    quien presta. Invariantes contables en CHECK: depósito + saldo = total;
--    0 <= comisión <= total.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_agreement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  application_id UUID NOT NULL UNIQUE REFERENCES classified_application(id),
  classified_id UUID NOT NULL REFERENCES classified(id),
  buyer_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  provider_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency_id UUID NOT NULL REFERENCES currency_reference(id),
  commission_bps INTEGER NOT NULL CHECK (commission_bps BETWEEN 0 AND 10000),
  commission_minor BIGINT NOT NULL,
  deposit_bps INTEGER NOT NULL CHECK (deposit_bps BETWEEN 0 AND 10000),
  deposit_minor BIGINT NOT NULL,
  balance_minor BIGINT NOT NULL,
  terms JSONB NOT NULL DEFAULT '{}'::jsonb,
  terms_version INTEGER NOT NULL DEFAULT 1,
  cancellation_policy JSONB NOT NULL DEFAULT '{}'::jsonb,
  -- 'off_platform': el pago se coordina fuera de TDF (parte A).
  -- 'platform': depósito y saldo se cobran con checkout (parte B).
  settlement_mode TEXT NOT NULL DEFAULT 'off_platform',
  status TEXT NOT NULL DEFAULT 'confirmed',
  deposit_checkout_id UUID,
  balance_checkout_id UUID,
  deposit_due_at TIMESTAMPTZ,
  starts_at TIMESTAMPTZ,
  ends_at TIMESTAMPTZ,
  started_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  cancelled_by_profile_id UUID REFERENCES directory_profile(id),
  cancellation_reason TEXT,
  disputed_at TIMESTAMPTZ,
  interaction_id UUID REFERENCES directory_interaction(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  CONSTRAINT marketplace_agreement_status_check
    CHECK (status IN ('pending_deposit','confirmed','in_progress','balance_due','completed','cancelled','disputed','expired')),
  CONSTRAINT marketplace_agreement_settlement_check
    CHECK (settlement_mode IN ('off_platform','platform')),
  CONSTRAINT marketplace_agreement_parties_check
    CHECK (buyer_profile_id <> provider_profile_id),
  CONSTRAINT marketplace_agreement_commission_check
    CHECK (commission_minor BETWEEN 0 AND amount_minor),
  CONSTRAINT marketplace_agreement_deposit_check
    CHECK (deposit_minor BETWEEN 0 AND amount_minor),
  CONSTRAINT marketplace_agreement_balance_check
    CHECK (balance_minor = amount_minor - deposit_minor),
  CONSTRAINT marketplace_agreement_terms_check
    CHECK (jsonb_typeof(terms) = 'object' AND jsonb_typeof(cancellation_policy) = 'object'),
  -- Un acuerdo off_platform nunca espera un depósito en la plataforma.
  CONSTRAINT marketplace_agreement_pending_deposit_check
    CHECK (status NOT IN ('pending_deposit','balance_due') OR settlement_mode = 'platform'),
  CONSTRAINT marketplace_agreement_completed_check
    CHECK (status <> 'completed' OR completed_at IS NOT NULL),
  CONSTRAINT marketplace_agreement_cancelled_check
    CHECK (status <> 'cancelled' OR cancelled_at IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS marketplace_agreement_buyer_idx
  ON marketplace_agreement (buyer_profile_id, status, updated_at DESC);
CREATE INDEX IF NOT EXISTS marketplace_agreement_provider_idx
  ON marketplace_agreement (provider_profile_id, status, updated_at DESC);
CREATE INDEX IF NOT EXISTS marketplace_agreement_classified_idx
  ON marketplace_agreement (classified_id);

-- ---------------------------------------------------------------------------
-- 3. Relación de transiciones del acuerdo (misma técnica que el anuncio): el
--    trigger rechaza cualquier cambio de estado no declarado. Debe coincidir
--    con TDF.Directory.Policy.allowedAgreementTransition y con
--    docs/music-directory/formal-model.yaml#agreement.
-- ---------------------------------------------------------------------------
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

CREATE OR REPLACE FUNCTION marketplace_guard_agreement_transition()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NOT marketplace_agreement_transition_allowed(OLD.status, NEW.status) THEN
    RAISE EXCEPTION 'undeclared agreement transition: % -> %', OLD.status, NEW.status
      USING ERRCODE = '23514';
  END IF;
  -- Los valores congelados al crear el acuerdo no cambian después.
  IF NEW.amount_minor <> OLD.amount_minor OR NEW.currency_id <> OLD.currency_id
     OR NEW.commission_bps <> OLD.commission_bps OR NEW.commission_minor <> OLD.commission_minor
     OR NEW.deposit_bps <> OLD.deposit_bps OR NEW.deposit_minor <> OLD.deposit_minor
     OR NEW.buyer_profile_id <> OLD.buyer_profile_id OR NEW.provider_profile_id <> OLD.provider_profile_id
     OR NEW.application_id <> OLD.application_id OR NEW.terms <> OLD.terms THEN
    RAISE EXCEPTION 'agreement amounts, parties and terms are frozen at creation'
      USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS marketplace_agreement_transition_trigger ON marketplace_agreement;
CREATE TRIGGER marketplace_agreement_transition_trigger
BEFORE UPDATE ON marketplace_agreement
FOR EACH ROW EXECUTE FUNCTION marketplace_guard_agreement_transition();

-- ---------------------------------------------------------------------------
-- 4. Allowlists existentes ampliadas con la técnica de expresión guardada
--    (igual que 2026-11-08): interacción verificada 'marketplace_agreement',
--    tipos de notificación y eventos analíticos del acuerdo. Las expresiones
--    previas se guardan para el rollback.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_agreements_migration_state (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  interaction_expression TEXT,
  notification_expression TEXT,
  analytics_expression TEXT
);
INSERT INTO marketplace_agreements_migration_state(id) VALUES (1) ON CONFLICT (id) DO NOTHING;

DO $$ DECLARE previous TEXT; constraint_name TEXT; BEGIN
  SELECT conname, pg_get_expr(conbin, conrelid) INTO constraint_name, previous FROM pg_constraint
  WHERE conrelid = 'directory_interaction'::regclass AND contype = 'c'
    AND pg_get_expr(conbin, conrelid) LIKE '%confirmed_collaboration%'
  ORDER BY conname LIMIT 1;
  UPDATE marketplace_agreements_migration_state SET interaction_expression = coalesce(interaction_expression, previous) WHERE id = 1;
  IF constraint_name IS NOT NULL AND position('marketplace_agreement' in previous) = 0 THEN
    EXECUTE format('ALTER TABLE directory_interaction DROP CONSTRAINT %I', constraint_name);
    EXECUTE format(
      'ALTER TABLE directory_interaction ADD CONSTRAINT %I CHECK ((%s) OR interaction_kind = ''marketplace_agreement'')',
      constraint_name, previous);
  END IF;
END $$;

DO $$ DECLARE previous TEXT; BEGIN
  SELECT pg_get_expr(conbin, conrelid) INTO previous FROM pg_constraint
  WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check' AND contype = 'c';
  UPDATE marketplace_agreements_migration_state SET notification_expression = coalesce(notification_expression, previous) WHERE id = 1;
  IF previous IS NOT NULL AND position('marketplace.agreement.created' in previous) = 0 THEN
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format(
      'ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''marketplace.agreement.created'',''marketplace.agreement.started'',''marketplace.agreement.completed'',''marketplace.agreement.cancelled'',''marketplace.agreement.disputed''))',
      previous);
  END IF;
END $$;

DO $$ DECLARE previous TEXT; constraint_name TEXT; BEGIN
  SELECT conname, pg_get_expr(conbin, conrelid) INTO constraint_name, previous FROM pg_constraint
  WHERE conrelid = 'directory_analytics_event'::regclass AND contype = 'c'
    AND pg_get_expr(conbin, conrelid) LIKE '%search_submitted%'
  ORDER BY conname LIMIT 1;
  UPDATE marketplace_agreements_migration_state SET analytics_expression = coalesce(analytics_expression, previous) WHERE id = 1;
  IF constraint_name IS NOT NULL AND position('agreement_created' in previous) = 0 THEN
    EXECUTE format('ALTER TABLE directory_analytics_event DROP CONSTRAINT %I', constraint_name);
    EXECUTE format(
      'ALTER TABLE directory_analytics_event ADD CONSTRAINT %I CHECK ((%s) OR event_name IN (''agreement_created'',''agreement_completed'',''agreement_cancelled''))',
      constraint_name, previous);
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 5. Gate de runtime: apagado en producción hasta habilitarlo explícitamente.
-- ---------------------------------------------------------------------------
INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('marketplace.agreements', FALSE, 'production', 'ADR 0135: enable after Phase 2 manual gates'),
  ('marketplace.agreements', TRUE, 'staging', 'ADR 0135: Phase 2 rehearsal'),
  ('marketplace.agreements', TRUE, 'sandbox', 'ADR 0135: local development')
ON CONFLICT (flag_key, environment) DO NOTHING;

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.apply', 'migration', '2026-11-12_marketplace_agreements', NULL, 'applied',
        'migration-2026-11-12-marketplace-agreements', '{"adr":"0135"}'::jsonb);

COMMIT;
