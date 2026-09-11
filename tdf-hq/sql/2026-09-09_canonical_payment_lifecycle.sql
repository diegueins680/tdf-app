-- Provider-neutral payment lifecycle and marketplace accounting foundation.
--
-- This migration is additive. It does not backfill, reinterpret or delete any
-- historical checkout/payment row, and every provider starts disabled.
\set ON_ERROR_STOP on
BEGIN;

ALTER TABLE commerce_payment_attempt
  DROP CONSTRAINT IF EXISTS commerce_payment_attempt_provider_check,
  DROP CONSTRAINT IF EXISTS ck_commerce_payment_attempt_provider;
ALTER TABLE commerce_payment_attempt
  ADD CONSTRAINT ck_commerce_payment_attempt_provider CHECK (
    provider IN (
      'datafast','paypal','placetopay','payphone','stripe',
      'bank_transfer','cash','pos','cardano'
    )
  );

ALTER TABLE commerce_refund
  DROP CONSTRAINT IF EXISTS ck_commerce_refund_provider;
ALTER TABLE commerce_refund
  ADD CONSTRAINT ck_commerce_refund_provider CHECK (
    provider IS NULL OR provider IN (
      'datafast','paypal','placetopay','payphone','stripe',
      'bank_transfer','cash','pos'
    )
  );

CREATE TABLE IF NOT EXISTS commerce_provider_account (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  provider TEXT NOT NULL CHECK (provider IN ('datafast','paypal','placetopay','payphone','stripe','bank_transfer')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  merchant_account_ref TEXT,
  status TEXT NOT NULL DEFAULT 'disabled' CHECK (status IN ('disabled','testing','ready','suspended','blocked')),
  contract_status TEXT NOT NULL DEFAULT 'unverified' CHECK (contract_status IN ('unverified','pending','approved','blocked')),
  credential_status TEXT NOT NULL DEFAULT 'absent' CHECK (credential_status IN ('absent','configured','validated','invalid')),
  settlement_currency TEXT NOT NULL DEFAULT 'USD' CHECK (settlement_currency ~ '^[A-Z]{3}$'),
  feature_flag_key TEXT NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  verified_at TIMESTAMPTZ,
  verified_by BIGINT,
  disabled_reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment),
  CHECK (enabled = FALSE OR (
    status = 'ready'
    AND contract_status = 'approved'
    AND credential_status = 'validated'
    AND merchant_account_ref IS NOT NULL
    AND verified_at IS NOT NULL
    AND verified_by IS NOT NULL
  )),
  CHECK (environment <> 'production' OR enabled = FALSE OR settlement_currency = 'USD')
);

CREATE TABLE IF NOT EXISTS commerce_provider_capability (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  provider_account_id UUID NOT NULL REFERENCES commerce_provider_account(id) ON DELETE RESTRICT,
  payment_method TEXT NOT NULL CHECK (payment_method IN (
    'card','paypal_wallet','bank_redirect','deuna_qr','payphone_wallet',
    'payment_link','manual_bank_transfer'
  )),
  capability TEXT NOT NULL CHECK (capability IN (
    'one_time','recurring','tokenization','three_ds','installments','authorize',
    'capture','void','full_refund','partial_refund','disputes','chargebacks',
    'payment_link','signed_webhook','server_verification','connected_accounts',
    'split_settlement','seller_payouts'
  )),
  verification_status TEXT NOT NULL DEFAULT 'documented' CHECK (verification_status IN (
    'documented','contract_required','sandbox_verified','production_verified','disabled'
  )),
  source_reference TEXT NOT NULL,
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider_account_id, payment_method, capability),
  CHECK (verification_status NOT IN ('sandbox_verified','production_verified') OR verified_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS commerce_payment_intent (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  status TEXT NOT NULL CHECK (status IN (
    'requires_payment_method','requires_customer_action','processing','authorized',
    'partially_captured','captured','voided','failed','cancelled',
    'partially_refunded','refunded','disputed','chargeback'
  )),
  capture_method TEXT NOT NULL CHECK (capture_method IN ('automatic','manual')),
  provider TEXT CHECK (provider IS NULL OR provider IN ('datafast','paypal','placetopay','payphone','stripe','bank_transfer')),
  payment_method TEXT CHECK (payment_method IS NULL OR payment_method IN (
    'card','paypal_wallet','bank_redirect','deuna_qr','payphone_wallet',
    'payment_link','manual_bank_transfer'
  )),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  authorized_minor BIGINT NOT NULL DEFAULT 0 CHECK (authorized_minor >= 0),
  captured_minor BIGINT NOT NULL DEFAULT 0 CHECK (captured_minor >= 0),
  refunded_minor BIGINT NOT NULL DEFAULT 0 CHECK (refunded_minor >= 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  idempotency_key TEXT NOT NULL,
  ambiguous_since TIMESTAMPTZ,
  reconciled_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (checkout_id, idempotency_key),
  CHECK (authorized_minor <= amount_minor),
  CHECK (captured_minor <= amount_minor),
  CHECK (refunded_minor <= captured_minor),
  CHECK (reconciled_at IS NULL OR ambiguous_since IS NOT NULL),
  CHECK (status <> 'authorized' OR authorized_minor > captured_minor),
  CHECK (status NOT IN ('captured','partially_refunded','refunded','disputed','chargeback') OR captured_minor > 0),
  CHECK (status <> 'refunded' OR refunded_minor = captured_minor)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_payment_intent_active_checkout
  ON commerce_payment_intent(checkout_id)
  WHERE status NOT IN ('voided','failed','cancelled','refunded','chargeback');

CREATE TABLE IF NOT EXISTS commerce_payment_authorization (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider_authorization_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('pending','authorized','partially_captured','captured','voided','expired','failed')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  captured_minor BIGINT NOT NULL DEFAULT 0 CHECK (captured_minor >= 0),
  voided_minor BIGINT NOT NULL DEFAULT 0 CHECK (voided_minor >= 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  authorized_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payment_intent_id, payment_attempt_id),
  UNIQUE (payment_attempt_id, provider_authorization_id),
  CHECK (captured_minor + voided_minor <= amount_minor),
  CHECK (expires_at IS NULL OR authorized_at IS NULL OR expires_at > authorized_at)
);

CREATE TABLE IF NOT EXISTS commerce_payment_capture (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  authorization_id UUID REFERENCES commerce_payment_authorization(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider_capture_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('pending','succeeded','failed','reversed')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  captured_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payment_attempt_id, provider_capture_id)
);

CREATE TABLE IF NOT EXISTS commerce_payment_void (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  authorization_id UUID NOT NULL REFERENCES commerce_payment_authorization(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider_void_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('pending','succeeded','failed')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  voided_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payment_attempt_id, provider_void_id)
);

CREATE TABLE IF NOT EXISTS commerce_payment_state_history (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  event_type TEXT NOT NULL,
  provider_event_id UUID REFERENCES commerce_provider_event_inbox(id) ON DELETE RESTRICT,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','customer','provider','staff')),
  actor_party_id BIGINT,
  correlation_id TEXT NOT NULL,
  reason TEXT,
  occurred_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (actor_type <> 'staff' OR actor_party_id IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_commerce_payment_state_history_intent
  ON commerce_payment_state_history(payment_intent_id, occurred_at, id);

CREATE TABLE IF NOT EXISTS commerce_payment_amount_component (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  component_type TEXT NOT NULL CHECK (component_type IN (
    'subtotal','discount','tax','customer_fee','provider_fee','platform_commission',
    'seller_payable','withholding','refund','chargeback','fx_adjustment'
  )),
  amount_minor BIGINT NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  source TEXT NOT NULL CHECK (source IN ('quote','provider_estimate','provider_actual','tax_document','manual_adjustment')),
  external_reference TEXT,
  occurred_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (amount_minor <> 0)
);

CREATE TABLE IF NOT EXISTS commerce_connected_account (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  seller_party_id BIGINT NOT NULL,
  provider TEXT NOT NULL CHECK (provider IN ('paypal','placetopay','nuvei','dlocal')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  provider_account_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('created','onboarding','restricted','ready','suspended','closed')),
  charges_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  payouts_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  requirements_due BOOLEAN NOT NULL DEFAULT TRUE,
  provider_managed_funds BOOLEAN NOT NULL DEFAULT TRUE,
  onboarded_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment, provider_account_id),
  UNIQUE (seller_party_id, provider, environment),
  CHECK (provider_managed_funds),
  CHECK (status <> 'ready' OR (charges_enabled AND requirements_due = FALSE))
);

CREATE TABLE IF NOT EXISTS commerce_commission (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  connected_account_id UUID NOT NULL REFERENCES commerce_connected_account(id) ON DELETE RESTRICT,
  basis_amount_minor BIGINT NOT NULL CHECK (basis_amount_minor > 0),
  commission_minor BIGINT NOT NULL CHECK (commission_minor >= 0),
  provider_fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (provider_fee_minor >= 0),
  tax_minor BIGINT NOT NULL DEFAULT 0 CHECK (tax_minor >= 0),
  seller_net_minor BIGINT NOT NULL CHECK (seller_net_minor >= 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  terms_version TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payment_intent_id, connected_account_id),
  CHECK (seller_net_minor = basis_amount_minor - commission_minor - provider_fee_minor - tax_minor)
);

CREATE TABLE IF NOT EXISTS commerce_settlement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  provider TEXT NOT NULL CHECK (provider IN ('datafast','paypal','placetopay','payphone','stripe','bank_transfer')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  provider_settlement_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('reported','in_transit','settled','failed','reversed','requires_reconciliation')),
  gross_minor BIGINT NOT NULL CHECK (gross_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  withholding_minor BIGINT NOT NULL DEFAULT 0 CHECK (withholding_minor >= 0),
  refund_minor BIGINT NOT NULL DEFAULT 0 CHECK (refund_minor >= 0),
  chargeback_minor BIGINT NOT NULL DEFAULT 0 CHECK (chargeback_minor >= 0),
  net_minor BIGINT NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  expected_at TIMESTAMPTZ,
  settled_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment, provider_settlement_id),
  CHECK (net_minor = gross_minor - fee_minor - withholding_minor - refund_minor - chargeback_minor),
  CHECK (status <> 'settled' OR settled_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS commerce_settlement_allocation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  settlement_id UUID NOT NULL REFERENCES commerce_settlement(id) ON DELETE RESTRICT,
  payment_intent_id UUID NOT NULL REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  capture_id UUID REFERENCES commerce_payment_capture(id) ON DELETE RESTRICT,
  gross_minor BIGINT NOT NULL CHECK (gross_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  withholding_minor BIGINT NOT NULL DEFAULT 0 CHECK (withholding_minor >= 0),
  refund_minor BIGINT NOT NULL DEFAULT 0 CHECK (refund_minor >= 0),
  chargeback_minor BIGINT NOT NULL DEFAULT 0 CHECK (chargeback_minor >= 0),
  net_minor BIGINT NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (settlement_id, payment_intent_id, capture_id),
  CHECK (net_minor = gross_minor - fee_minor - withholding_minor - refund_minor - chargeback_minor)
);

CREATE TABLE IF NOT EXISTS commerce_seller_balance_entry (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  connected_account_id UUID NOT NULL REFERENCES commerce_connected_account(id) ON DELETE RESTRICT,
  payment_intent_id UUID REFERENCES commerce_payment_intent(id) ON DELETE RESTRICT,
  payout_id UUID,
  entry_type TEXT NOT NULL CHECK (entry_type IN (
    'sale','commission','provider_fee','tax','withholding','refund','dispute',
    'chargeback','payout','adjustment','reversal'
  )),
  amount_minor BIGINT NOT NULL CHECK (amount_minor <> 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  available_at TIMESTAMPTZ,
  correlation_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS commerce_payout (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  connected_account_id UUID NOT NULL REFERENCES commerce_connected_account(id) ON DELETE RESTRICT,
  provider_payout_id TEXT,
  status TEXT NOT NULL CHECK (status IN ('draft','pending_review','approved','submitted','paid','failed','cancelled','reversed')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  idempotency_key TEXT NOT NULL,
  requested_by BIGINT NOT NULL,
  approved_by BIGINT,
  requested_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  approved_at TIMESTAMPTZ,
  submitted_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  failure_summary TEXT,
  UNIQUE (connected_account_id, idempotency_key),
  UNIQUE (provider_payout_id),
  CHECK (approved_by IS NULL OR approved_by <> requested_by),
  CHECK (status NOT IN ('approved','submitted','paid') OR (approved_by IS NOT NULL AND approved_at IS NOT NULL)),
  CHECK (status <> 'paid' OR (provider_payout_id IS NOT NULL AND completed_at IS NOT NULL))
);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'fk_commerce_seller_balance_payout'
      AND conrelid = 'commerce_seller_balance_entry'::regclass
  ) THEN
    ALTER TABLE commerce_seller_balance_entry
      ADD CONSTRAINT fk_commerce_seller_balance_payout
      FOREIGN KEY (payout_id) REFERENCES commerce_payout(id) ON DELETE RESTRICT;
  END IF;
END $$;

CREATE TABLE IF NOT EXISTS commerce_payout_allocation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payout_id UUID NOT NULL REFERENCES commerce_payout(id) ON DELETE RESTRICT,
  seller_balance_entry_id UUID NOT NULL REFERENCES commerce_seller_balance_entry(id) ON DELETE RESTRICT,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payout_id, seller_balance_entry_id)
);

CREATE TABLE IF NOT EXISTS commerce_payment_mandate (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_party_id BIGINT,
  customer_email TEXT NOT NULL,
  provider TEXT NOT NULL CHECK (provider IN ('datafast','paypal','placetopay','payphone','stripe')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  provider_vault_reference TEXT NOT NULL,
  mandate_type TEXT NOT NULL CHECK (mandate_type IN ('saved_method','recurring')),
  status TEXT NOT NULL CHECK (status IN ('pending','active','suspended','cancelled','expired','revoked')),
  consent_version TEXT NOT NULL,
  consented_at TIMESTAMPTZ NOT NULL,
  cancelled_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment, provider_vault_reference),
  CHECK (status NOT IN ('cancelled','revoked') OR cancelled_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS commerce_payment_link (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  provider TEXT NOT NULL CHECK (provider IN ('datafast','paypal','placetopay','payphone')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  provider_link_id TEXT NOT NULL,
  public_token_hash TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('active','completed','expired','cancelled')),
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment, provider_link_id),
  UNIQUE (public_token_hash),
  CHECK (expires_at > created_at)
);

CREATE OR REPLACE FUNCTION commerce_protect_payment_external_reference()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION '% rows are financial evidence and cannot be deleted', TG_TABLE_NAME;
  END IF;
  IF to_jsonb(OLD) - ARRAY['status','updated_at','failure_summary','completed_at','settled_at','approved_by','approved_at','submitted_at','captured_minor','voided_minor','authorized_minor','refunded_minor','ambiguous_since','reconciled_at','charges_enabled','payouts_enabled','requirements_due','onboarded_at','verified_at','verified_by','disabled_reason','enabled','contract_status','credential_status','merchant_account_ref','provider_payout_id']
     <> to_jsonb(NEW) - ARRAY['status','updated_at','failure_summary','completed_at','settled_at','approved_by','approved_at','submitted_at','captured_minor','voided_minor','authorized_minor','refunded_minor','ambiguous_since','reconciled_at','charges_enabled','payouts_enabled','requirements_due','onboarded_at','verified_at','verified_by','disabled_reason','enabled','contract_status','credential_status','merchant_account_ref','provider_payout_id'] THEN
    RAISE EXCEPTION '% immutable identity or monetary fields cannot be changed', TG_TABLE_NAME;
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION commerce_protect_bound_payout_reference()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.provider_payout_id IS NOT NULL
     AND OLD.provider_payout_id IS DISTINCT FROM NEW.provider_payout_id THEN
    RAISE EXCEPTION 'Bound provider payout reference cannot be changed';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_payment_authorization_external_ref ON commerce_payment_authorization;
DROP TRIGGER IF EXISTS trg_commerce_payment_capture_external_ref ON commerce_payment_capture;
DROP TRIGGER IF EXISTS trg_commerce_payment_void_external_ref ON commerce_payment_void;
DROP TRIGGER IF EXISTS trg_commerce_connected_account_external_ref ON commerce_connected_account;
DROP TRIGGER IF EXISTS trg_commerce_settlement_external_ref ON commerce_settlement;
DROP TRIGGER IF EXISTS trg_commerce_payout_external_ref ON commerce_payout;
DROP TRIGGER IF EXISTS trg_commerce_payout_bound_ref ON commerce_payout;
DROP TRIGGER IF EXISTS trg_commerce_payment_mandate_external_ref ON commerce_payment_mandate;
DROP TRIGGER IF EXISTS trg_commerce_payment_link_external_ref ON commerce_payment_link;
DROP TRIGGER IF EXISTS trg_commerce_payment_state_history_immutable ON commerce_payment_state_history;
DROP TRIGGER IF EXISTS trg_commerce_payment_amount_component_immutable ON commerce_payment_amount_component;
DROP TRIGGER IF EXISTS trg_commerce_commission_immutable ON commerce_commission;
DROP TRIGGER IF EXISTS trg_commerce_settlement_allocation_immutable ON commerce_settlement_allocation;
DROP TRIGGER IF EXISTS trg_commerce_seller_balance_entry_immutable ON commerce_seller_balance_entry;
DROP TRIGGER IF EXISTS trg_commerce_payout_allocation_immutable ON commerce_payout_allocation;

CREATE TRIGGER trg_commerce_payment_authorization_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payment_authorization
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_payment_capture_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payment_capture
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_payment_void_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payment_void
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_connected_account_external_ref
  BEFORE UPDATE OR DELETE ON commerce_connected_account
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_settlement_external_ref
  BEFORE UPDATE OR DELETE ON commerce_settlement
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_payout_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payout
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_payout_bound_ref
  BEFORE UPDATE ON commerce_payout
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_bound_payout_reference();
CREATE TRIGGER trg_commerce_payment_mandate_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payment_mandate
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();
CREATE TRIGGER trg_commerce_payment_link_external_ref
  BEFORE UPDATE OR DELETE ON commerce_payment_link
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_payment_external_reference();

CREATE TRIGGER trg_commerce_payment_state_history_immutable
  BEFORE UPDATE OR DELETE ON commerce_payment_state_history
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_payment_amount_component_immutable
  BEFORE UPDATE OR DELETE ON commerce_payment_amount_component
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_commission_immutable
  BEFORE UPDATE OR DELETE ON commerce_commission
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_settlement_allocation_immutable
  BEFORE UPDATE OR DELETE ON commerce_settlement_allocation
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_seller_balance_entry_immutable
  BEFORE UPDATE OR DELETE ON commerce_seller_balance_entry
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_payout_allocation_immutable
  BEFORE UPDATE OR DELETE ON commerce_payout_allocation
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();

INSERT INTO commerce_provider_account (
  provider, environment, status, contract_status, credential_status,
  settlement_currency, feature_flag_key, enabled, disabled_reason
)
SELECT provider, environment, 'disabled', 'unverified', 'absent', 'USD',
       'checkout.' || provider, FALSE,
       'Activation requires contract, credential, sandbox and operational verification'
FROM unnest(ARRAY['datafast','paypal','placetopay','payphone']) AS provider
CROSS JOIN unnest(ARRAY['sandbox','production']) AS environment
ON CONFLICT (provider, environment) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, environment, enabled, reason)
VALUES
  ('checkout.placetopay', 'production', FALSE, 'PlaceToPay production payment rail'),
  ('checkout.payphone', 'production', FALSE, 'PayPhone production payment rail'),
  ('commerce.marketplace_connected_accounts', 'production', FALSE, 'Provider-managed marketplace connected accounts and payouts'),
  ('commerce.recurring_payments', 'production', FALSE, 'Cancelable recurring mandates'),
  ('commerce.payment_links', 'production', FALSE, 'Shareable provider-hosted payment links')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
