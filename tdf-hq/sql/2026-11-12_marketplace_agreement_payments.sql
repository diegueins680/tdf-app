BEGIN;

-- ADR 0136: pagos de acuerdos del marketplace (Fase 2, parte B1).
-- Cada cobro de un acuerdo (depósito o saldo) es una fila con su reparto
-- congelado y un checkout canónico de dominio 'marketplace_agreement'. Solo la
-- reconciliación con evidencia verificada del proveedor puede marcarlo pagado.
-- Aditiva e idempotente; el rollback está en
-- 2026-11-12_marketplace_agreement_payments_rollback.sql.

-- ---------------------------------------------------------------------------
-- 1. Comisión del procesador por proveedor de pagos. Se edita con UPDATE, sin
--    desplegar; cada cobro congela el valor vigente al crearse. 'payer' dice
--    quién la asume: el proveedor del servicio o la plataforma (de su comisión).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_payment_fee_rule (
  provider TEXT PRIMARY KEY,
  fee_bps INTEGER NOT NULL CHECK (fee_bps BETWEEN 0 AND 10000),
  fee_fixed_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_fixed_minor >= 0),
  payer TEXT NOT NULL DEFAULT 'provider',
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT marketplace_payment_fee_rule_provider_check
    CHECK (provider IN ('payphone','stripe','datafast')),
  CONSTRAINT marketplace_payment_fee_rule_payer_check
    CHECK (payer IN ('provider','platform'))
);

-- PayPhone descuenta 5,75 % en pagos con tarjeta antes de repartir.
INSERT INTO marketplace_payment_fee_rule (provider, fee_bps, fee_fixed_minor, payer)
VALUES ('payphone', 575, 0, 'provider')
ON CONFLICT (provider) DO NOTHING;

-- ---------------------------------------------------------------------------
-- 2. Cuenta de cobro del proveedor del servicio: una por perfil y proveedor de
--    pagos. PayPhone identifica al receptor por identificación (Type 3) o
--    teléfono (Type 4); Stripe por cuenta conectada (B2).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_payout_account (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  provider TEXT NOT NULL,
  identifier_type TEXT NOT NULL,
  identifier TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'ready',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  CONSTRAINT marketplace_payout_account_profile_provider_key UNIQUE (profile_id, provider),
  CONSTRAINT marketplace_payout_account_provider_check
    CHECK (provider IN ('payphone','stripe')),
  CONSTRAINT marketplace_payout_account_identifier_type_check
    CHECK ((provider = 'payphone' AND identifier_type IN ('identification','phone'))
        OR (provider = 'stripe' AND identifier_type = 'account')),
  CONSTRAINT marketplace_payout_account_identifier_check
    CHECK ((identifier_type = 'identification' AND identifier ~ '^[0-9]{10}([0-9]{3})?$')
        OR (identifier_type = 'phone' AND identifier ~ '^\+[1-9][0-9]{7,14}$')
        OR (identifier_type = 'account' AND identifier ~ '^acct_[A-Za-z0-9]{8,64}$')),
  CONSTRAINT marketplace_payout_account_status_check
    CHECK (status IN ('pending','ready','disabled'))
);

-- ---------------------------------------------------------------------------
-- 3. Cobro del acuerdo. amount = platform_fee + processing_fee + provider_amount.
--    El receptor y el reparto se congelan al crear el cobro.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_agreement_payment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  agreement_id UUID NOT NULL REFERENCES marketplace_agreement(id),
  kind TEXT NOT NULL,
  provider TEXT NOT NULL,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  platform_fee_minor BIGINT NOT NULL CHECK (platform_fee_minor >= 0),
  processing_fee_minor BIGINT NOT NULL CHECK (processing_fee_minor >= 0),
  processing_fee_payer TEXT NOT NULL,
  provider_amount_minor BIGINT NOT NULL CHECK (provider_amount_minor >= 0),
  payout_account_id UUID NOT NULL REFERENCES marketplace_payout_account(id),
  payout_identifier_type TEXT NOT NULL,
  payout_identifier TEXT NOT NULL,
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id),
  status TEXT NOT NULL DEFAULT 'pending',
  due_at TIMESTAMPTZ NOT NULL,
  paid_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  CONSTRAINT marketplace_agreement_payment_kind_check
    CHECK (kind IN ('deposit','balance')),
  CONSTRAINT marketplace_agreement_payment_provider_check
    CHECK (provider IN ('payphone','stripe','datafast')),
  CONSTRAINT marketplace_agreement_payment_payer_check
    CHECK (processing_fee_payer IN ('provider','platform')),
  CONSTRAINT marketplace_agreement_payment_split_check
    CHECK (amount_minor = platform_fee_minor + processing_fee_minor + provider_amount_minor),
  CONSTRAINT marketplace_agreement_payment_status_check
    CHECK (status IN ('pending','paid','expired','cancelled')),
  CONSTRAINT marketplace_agreement_payment_paid_check
    CHECK ((status = 'paid') = (paid_at IS NOT NULL)),
  CONSTRAINT marketplace_agreement_payment_checkout_key UNIQUE (checkout_id)
);
-- A lo sumo un cobro vivo (pendiente o pagado) por acuerdo y tipo.
CREATE UNIQUE INDEX IF NOT EXISTS marketplace_agreement_payment_live_uidx
  ON marketplace_agreement_payment (agreement_id, kind) WHERE status IN ('pending','paid');
CREATE INDEX IF NOT EXISTS marketplace_agreement_payment_due_idx
  ON marketplace_agreement_payment (due_at) WHERE status = 'pending';

-- ---------------------------------------------------------------------------
-- 4. Relación de transiciones del cobro y evidencia obligatoria. Debe coincidir
--    con TDF.Directory.Policy.allowedAgreementPaymentTransition y con
--    docs/music-directory/formal-model.yaml#agreement_payment.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION marketplace_agreement_payment_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('pending','paid'), ('pending','expired'), ('pending','cancelled')
  );
$$;

CREATE OR REPLACE FUNCTION marketplace_guard_agreement_payment()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NOT marketplace_agreement_payment_transition_allowed(OLD.status, NEW.status) THEN
    RAISE EXCEPTION 'undeclared agreement payment transition: % -> %', OLD.status, NEW.status
      USING ERRCODE = '23514';
  END IF;
  IF NEW.agreement_id <> OLD.agreement_id OR NEW.kind <> OLD.kind OR NEW.provider <> OLD.provider
     OR NEW.amount_minor <> OLD.amount_minor OR NEW.currency <> OLD.currency
     OR NEW.platform_fee_minor <> OLD.platform_fee_minor
     OR NEW.processing_fee_minor <> OLD.processing_fee_minor
     OR NEW.processing_fee_payer <> OLD.processing_fee_payer
     OR NEW.provider_amount_minor <> OLD.provider_amount_minor
     OR NEW.payout_account_id <> OLD.payout_account_id
     OR NEW.payout_identifier_type <> OLD.payout_identifier_type
     OR NEW.payout_identifier <> OLD.payout_identifier
     OR NEW.checkout_id <> OLD.checkout_id THEN
    RAISE EXCEPTION 'agreement payment amounts, split and recipient are frozen at creation'
      USING ERRCODE = '23514';
  END IF;
  -- Un cobro solo pasa a pagado con evidencia verificada del proveedor ligada
  -- a su propio checkout canónico, por el monto y la moneda exactos.
  IF NEW.status = 'paid' AND OLD.status <> 'paid' AND NOT EXISTS (
       SELECT 1 FROM commerce_checkout_session checkout
       WHERE checkout.id = NEW.checkout_id
         AND checkout.domain_type = 'marketplace_agreement'
         AND checkout.domain_order_id = NEW.id::text
         AND checkout.total_minor = NEW.amount_minor
         AND checkout.currency = NEW.currency
         AND commerce_checkout_has_verified_payment(checkout)
     ) THEN
    RAISE EXCEPTION 'Agreement payment cannot become paid without bound verified payment evidence'
      USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS marketplace_agreement_payment_guard_trigger ON marketplace_agreement_payment;
CREATE TRIGGER marketplace_agreement_payment_guard_trigger
BEFORE UPDATE ON marketplace_agreement_payment
FOR EACH ROW EXECUTE FUNCTION marketplace_guard_agreement_payment();

-- Un cobro nace pendiente: nadie inserta uno ya pagado.
CREATE OR REPLACE FUNCTION marketplace_guard_agreement_payment_insert()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.status <> 'pending' THEN
    RAISE EXCEPTION 'agreement payments are created pending' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS marketplace_agreement_payment_insert_trigger ON marketplace_agreement_payment;
CREATE TRIGGER marketplace_agreement_payment_insert_trigger
BEFORE INSERT ON marketplace_agreement_payment
FOR EACH ROW EXECUTE FUNCTION marketplace_guard_agreement_payment_insert();

-- ---------------------------------------------------------------------------
-- 5. El acuerdo en plataforma avanza solo con su cobro pagado: el depósito
--    para confirmarse y el saldo (si existe) para completarse desde balance_due.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION marketplace_agreement_require_paid_payment()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.settlement_mode = 'platform' AND OLD.status = 'pending_deposit' AND NEW.status = 'confirmed'
     AND NOT EXISTS (SELECT 1 FROM marketplace_agreement_payment payment
                     WHERE payment.agreement_id = NEW.id AND payment.kind = 'deposit' AND payment.status = 'paid') THEN
    RAISE EXCEPTION 'agreement cannot be confirmed without a paid deposit' USING ERRCODE = '23514';
  END IF;
  IF NEW.settlement_mode = 'platform' AND OLD.status = 'balance_due' AND NEW.status = 'completed'
     AND NOT EXISTS (SELECT 1 FROM marketplace_agreement_payment payment
                     WHERE payment.agreement_id = NEW.id AND payment.kind = 'balance' AND payment.status = 'paid') THEN
    RAISE EXCEPTION 'agreement cannot be completed without a paid balance' USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS marketplace_agreement_paid_payment_trigger ON marketplace_agreement;
CREATE TRIGGER marketplace_agreement_paid_payment_trigger
BEFORE UPDATE ON marketplace_agreement
FOR EACH ROW EXECUTE FUNCTION marketplace_agreement_require_paid_payment();

-- ---------------------------------------------------------------------------
-- 5a. El comprador puede confirmar la entrega sin que el proveedor haya
--     marcado el inicio; con saldo pendiente en plataforma eso lleva a
--     balance_due, así que la relación declara confirmed -> balance_due.
--     Debe coincidir con TDF.Directory.Policy.allowedAgreementTransition y con
--     docs/music-directory/formal-model.yaml#agreement.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION marketplace_agreement_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('pending_deposit','confirmed'), ('pending_deposit','cancelled'), ('pending_deposit','expired'),
    ('confirmed','in_progress'), ('confirmed','balance_due'), ('confirmed','completed'), ('confirmed','cancelled'), ('confirmed','disputed'),
    ('in_progress','balance_due'), ('in_progress','completed'), ('in_progress','cancelled'), ('in_progress','disputed'),
    ('balance_due','completed'), ('balance_due','disputed'),
    ('disputed','completed'), ('disputed','cancelled')
  );
$$;

-- ---------------------------------------------------------------------------
-- 5b. Sincronización con el checkout canónico (mismo patrón que reservas,
--     cursos y Domo). El checkout de un cobro de acuerdo solo pasa a pagado con
--     evidencia verificada; al pasar, el cobro queda pagado y el acuerdo
--     avanza. Orden de bloqueo: (checkout) -> acuerdo -> cobro; ningún camino
--     del marketplace bloquea un checkout mientras sostiene el acuerdo.
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION marketplace_agreement_checkout_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'marketplace_agreement'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT commerce_checkout_has_verified_payment(NEW) THEN
    RAISE EXCEPTION 'Marketplace agreement checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_agreement_require_verified_payment ON commerce_checkout_session;
CREATE TRIGGER trg_marketplace_agreement_require_verified_payment
BEFORE UPDATE OF status ON commerce_checkout_session
FOR EACH ROW EXECUTE FUNCTION marketplace_agreement_checkout_require_verified_payment();

CREATE OR REPLACE FUNCTION marketplace_agreement_sync_verified_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_agreement UUID;
  payment_kind TEXT;
  paid_payment UUID;
  previous_status TEXT;
  next_status TEXT;
BEGIN
  IF NEW.domain_type <> 'marketplace_agreement' OR OLD.status = NEW.status OR NEW.status <> 'paid' THEN
    RETURN NEW;
  END IF;
  SELECT payment.agreement_id, payment.kind INTO target_agreement, payment_kind
    FROM marketplace_agreement_payment payment WHERE payment.checkout_id = NEW.id;
  IF target_agreement IS NULL THEN RETURN NEW; END IF;

  SELECT agreement.status INTO previous_status
    FROM marketplace_agreement agreement WHERE agreement.id = target_agreement FOR UPDATE;

  UPDATE marketplace_agreement_payment
     SET status = 'paid', paid_at = COALESCE(NEW.paid_at, now()), updated_at = now(), version = version + 1
   WHERE checkout_id = NEW.id AND status = 'pending'
   RETURNING id INTO paid_payment;

  IF paid_payment IS NULL THEN
    -- El cobro ya no estaba pendiente (venció o se canceló) y aun así llegó
    -- dinero verificado. No se reescribe el estado: queda constancia para que
    -- operaciones lo devuelva.
    INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
    VALUES (NULL, 'agreement.payment.unapplied', 'agreement', target_agreement::text, previous_status, previous_status,
            'agreement-payment-unapplied-' || NEW.id::text,
            jsonb_build_object('checkoutId', NEW.id, 'kind', payment_kind, 'amountMinor', NEW.total_minor, 'currency', NEW.currency));
    RETURN NEW;
  END IF;

  next_status := CASE
    WHEN payment_kind = 'deposit' AND previous_status = 'pending_deposit' THEN 'confirmed'
    WHEN payment_kind = 'balance' AND previous_status = 'balance_due' THEN 'completed'
    ELSE NULL END;

  IF next_status IS NULL THEN
    INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
    VALUES (NULL, 'agreement.payment.unexpected_state', 'agreement', target_agreement::text, previous_status, previous_status,
            'agreement-payment-unexpected-' || NEW.id::text,
            jsonb_build_object('checkoutId', NEW.id, 'kind', payment_kind, 'paymentId', paid_payment));
    RETURN NEW;
  END IF;

  UPDATE marketplace_agreement
     SET status = next_status,
         completed_at = CASE WHEN next_status = 'completed' THEN now() ELSE completed_at END,
         updated_at = now(), version = version + 1
   WHERE id = target_agreement;

  IF next_status = 'completed' THEN
    -- La interacción verificada habilita las reseñas (I12), igual que al
    -- completar un acuerdo fuera de plataforma.
    INSERT INTO directory_interaction(interaction_kind, external_id, profile_a_id, profile_b_id, status, verified_at)
    SELECT 'marketplace_agreement', agreement.id::text, agreement.buyer_profile_id, agreement.provider_profile_id, 'completed', now()
      FROM marketplace_agreement agreement WHERE agreement.id = target_agreement
    ON CONFLICT (interaction_kind, external_id)
      DO UPDATE SET status = 'completed', verified_at = COALESCE(directory_interaction.verified_at, now());
    UPDATE marketplace_agreement
       SET interaction_id = (SELECT interaction.id FROM directory_interaction interaction
                              WHERE interaction.interaction_kind = 'marketplace_agreement'
                                AND interaction.external_id = target_agreement::text)
     WHERE id = target_agreement;
  END IF;

  INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
  VALUES (NULL, 'agreement.payment.paid', 'agreement', target_agreement::text, previous_status, next_status,
          'agreement-payment-paid-' || paid_payment::text,
          jsonb_build_object('checkoutId', NEW.id, 'kind', payment_kind, 'paymentId', paid_payment, 'amountMinor', NEW.total_minor));

  INSERT INTO notification(recipient_party_id, notif_type, title, body, target_type, target_key, is_read, created_at)
  SELECT DISTINCT manager.account_party_id,
         CASE WHEN next_status = 'completed' THEN 'marketplace.agreement.completed' ELSE 'marketplace.agreement.deposit_paid' END,
         CASE WHEN next_status = 'completed' THEN 'Acuerdo completado' ELSE 'Depósito recibido' END,
         CASE WHEN next_status = 'completed' THEN 'El saldo se pagó y el acuerdo quedó completado. Ya pueden dejarse una reseña.'
              ELSE 'El depósito del acuerdo se pagó y el acuerdo quedó confirmado.' END,
         'directory_application', agreement.application_id::text, FALSE, now()
    FROM marketplace_agreement agreement
    JOIN directory_profile_manager manager
      ON manager.profile_id IN (agreement.buyer_profile_id, agreement.provider_profile_id)
     AND manager.active AND manager.can_contact
   WHERE agreement.id = target_agreement;

  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_agreement_sync_verified_checkout ON commerce_checkout_session;
CREATE TRIGGER trg_marketplace_agreement_sync_verified_checkout
AFTER UPDATE OF status ON commerce_checkout_session
FOR EACH ROW EXECUTE FUNCTION marketplace_agreement_sync_verified_checkout();

-- ---------------------------------------------------------------------------
-- 6. Tipos de notificación del cobro (técnica de expresión guardada).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS marketplace_agreement_payments_migration_state (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  notification_expression TEXT
);
INSERT INTO marketplace_agreement_payments_migration_state(id) VALUES (1) ON CONFLICT (id) DO NOTHING;

DO $$ DECLARE previous TEXT; BEGIN
  SELECT pg_get_expr(conbin, conrelid) INTO previous FROM pg_constraint
  WHERE conrelid = 'notification'::regclass AND conname = 'notification_notif_type_check' AND contype = 'c';
  UPDATE marketplace_agreement_payments_migration_state
     SET notification_expression = coalesce(notification_expression, previous) WHERE id = 1;
  IF previous IS NOT NULL AND position('marketplace.agreement.deposit_paid' in previous) = 0 THEN
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format(
      'ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''marketplace.agreement.deposit_paid'',''marketplace.agreement.balance_due'',''marketplace.agreement.expired''))',
      previous);
  END IF;
END $$;

-- ---------------------------------------------------------------------------
-- 7. Gate de runtime: apagado en producción hasta habilitarlo explícitamente.
-- ---------------------------------------------------------------------------
INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('marketplace.agreement_payments', FALSE, 'production', 'ADR 0136: enable after a split-capable provider is contracted and verified'),
  ('marketplace.agreement_payments', FALSE, 'staging', 'ADR 0136: enable when a split-capable provider flow is verified'),
  ('marketplace.agreement_payments', FALSE, 'sandbox', 'ADR 0136: enable when a split-capable provider flow is verified')
ON CONFLICT (flag_key, environment) DO NOTHING;

INSERT INTO directory_audit_event(actor_party_id, action, entity_kind, entity_id, previous_state, new_state, correlation_id, metadata)
VALUES (NULL, 'migration.apply', 'migration', '2026-11-12_marketplace_agreement_payments', NULL, 'applied',
        'migration-2026-11-12-marketplace-agreement-payments', '{"adr":"0136"}'::jsonb);

COMMIT;
