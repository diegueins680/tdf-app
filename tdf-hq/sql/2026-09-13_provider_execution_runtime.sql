-- Durable, provider-neutral execution records for remote payment operations.
-- No request bodies, credentials, PAN/CVV, or raw provider responses are
-- persisted. Redirect URLs are encrypted because hosted-checkout URLs can act
-- as bearer capabilities.
\set ON_ERROR_STOP on
BEGIN;

-- PayPhone does not document a cryptographic signature for its external
-- notification contract. Such callbacks may be durably accepted in
-- production only as untrusted query triggers; they can never be used as
-- payment evidence. PlaceToPay and PayPal remain signature-verified.
ALTER TABLE commerce_provider_event_inbox
  ADD COLUMN IF NOT EXISTS evidence_type TEXT NOT NULL DEFAULT 'signature_verified';

DO $$
DECLARE
  constraint_name TEXT;
BEGIN
  FOR constraint_name IN
    SELECT conname
      FROM pg_constraint
     WHERE conrelid = 'commerce_provider_event_inbox'::regclass
       AND contype = 'c'
       AND pg_get_constraintdef(oid) LIKE '%environment%production%signature_verified%'
  LOOP
    EXECUTE format(
      'ALTER TABLE commerce_provider_event_inbox DROP CONSTRAINT %I',
      constraint_name
    );
  END LOOP;
END $$;

ALTER TABLE commerce_provider_event_inbox
  DROP CONSTRAINT IF EXISTS ck_commerce_provider_event_evidence;
ALTER TABLE commerce_provider_event_inbox
  ADD CONSTRAINT ck_commerce_provider_event_evidence CHECK (
    (evidence_type = 'signature_verified' AND signature_verified = TRUE)
    OR (evidence_type = 'untrusted_callback' AND signature_verified = FALSE)
  );

CREATE OR REPLACE FUNCTION commerce_protect_provider_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  IF (OLD.provider, OLD.environment, OLD.merchant_account_ref, OLD.provider_event_id,
      OLD.event_type, OLD.signature_verified, OLD.evidence_type,
      OLD.provider_created_at, OLD.payload_ciphertext, OLD.payload_sha256)
     IS DISTINCT FROM
     (NEW.provider, NEW.environment, NEW.merchant_account_ref, NEW.provider_event_id,
      NEW.event_type, NEW.signature_verified, NEW.evidence_type,
      NEW.provider_created_at, NEW.payload_ciphertext, NEW.payload_sha256) THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  RETURN NEW;
END $$;

CREATE TABLE IF NOT EXISTS commerce_provider_operation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider TEXT NOT NULL,
  environment TEXT NOT NULL,
  merchant_account_ref TEXT NOT NULL,
  provider_reference TEXT NOT NULL CHECK (
    length(provider_reference) BETWEEN 1 AND 128
    AND provider_reference ~ '^[A-Za-z0-9._-]+$'
  ),
  operation TEXT NOT NULL CHECK (operation IN ('create','query','cancel','same_day_reverse')),
  idempotency_key TEXT NOT NULL,
  request_sha256 TEXT NOT NULL CHECK (request_sha256 ~ '^[0-9a-f]{64}$'),
  status TEXT NOT NULL DEFAULT 'prepared' CHECK (status IN (
    'prepared','in_flight','requires_customer_action','processing','succeeded',
    'confirmed_no_charge','ambiguous','failed'
  )),
  outcome_certainty TEXT NOT NULL DEFAULT 'not_contacted' CHECK (outcome_certainty IN (
    'not_contacted','rejected_before_creation','confirmed_no_charge','ambiguous','succeeded'
  )),
  provider_resource_id TEXT,
  redirect_url_ciphertext BYTEA,
  last_error_code TEXT,
  started_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (payment_attempt_id, operation, idempotency_key),
  CHECK (status <> 'in_flight' OR started_at IS NOT NULL),
  CHECK (status NOT IN ('requires_customer_action','processing','succeeded','confirmed_no_charge')
    OR provider_resource_id IS NOT NULL),
  CHECK (status <> 'ambiguous' OR outcome_certainty = 'ambiguous'),
  CHECK (status <> 'succeeded' OR outcome_certainty = 'succeeded')
);

-- Provider/environment authority belongs to the canonical account registry.
-- The explicit drops also repair databases that exercised an earlier draft of
-- this not-yet-released migration before the registry foreign key was added.
ALTER TABLE commerce_provider_operation
  DROP CONSTRAINT IF EXISTS commerce_provider_operation_provider_check,
  DROP CONSTRAINT IF EXISTS commerce_provider_operation_environment_check;

DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conrelid = 'commerce_provider_operation'::regclass
      AND conname = 'fk_commerce_provider_operation_account'
  ) THEN
    ALTER TABLE commerce_provider_operation
      ADD CONSTRAINT fk_commerce_provider_operation_account
      FOREIGN KEY (provider, environment)
      REFERENCES commerce_provider_account(provider, environment)
      ON DELETE RESTRICT;
  END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_provider_operation_create
  ON commerce_provider_operation(payment_attempt_id)
  WHERE operation = 'create';

CREATE INDEX IF NOT EXISTS idx_commerce_provider_operation_reconciliation
  ON commerce_provider_operation(status, updated_at)
  WHERE status IN ('in_flight','ambiguous','processing');

CREATE INDEX IF NOT EXISTS idx_commerce_provider_event_untrusted_work
  ON commerce_provider_event_inbox(processing_status, next_attempt_at, received_at)
  WHERE evidence_type = 'untrusted_callback'
    AND processing_status IN ('pending','retry','processing');

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.placetopay.webhooks', TRUE, 'sandbox',
    'Signed PlaceToPay notifications trigger an authoritative session query'),
  ('checkout.placetopay.webhooks', FALSE, 'production',
    'Requires production merchant approval, registered HTTPS URL, and activation evidence'),
  ('checkout.payphone.notifications', TRUE, 'sandbox',
    'Unsigned PayPhone notifications are query triggers only'),
  ('checkout.payphone.notifications', FALSE, 'production',
    'Requires PayPhone approval, registered HTTPS URL, and production authorization')
ON CONFLICT (flag_key, environment) DO NOTHING;

CREATE OR REPLACE FUNCTION commerce_guard_provider_operation_immutable()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.payment_attempt_id <> NEW.payment_attempt_id
     OR OLD.provider <> NEW.provider
     OR OLD.environment <> NEW.environment
     OR OLD.merchant_account_ref <> NEW.merchant_account_ref
     OR OLD.provider_reference <> NEW.provider_reference
     OR OLD.operation <> NEW.operation
     OR OLD.idempotency_key <> NEW.idempotency_key
     OR OLD.request_sha256 <> NEW.request_sha256
     OR OLD.created_at <> NEW.created_at
     OR (OLD.provider_resource_id IS NOT NULL
         AND OLD.provider_resource_id IS DISTINCT FROM NEW.provider_resource_id) THEN
    RAISE EXCEPTION 'provider operation immutable fields cannot change';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_commerce_provider_operation_immutable
  ON commerce_provider_operation;
CREATE TRIGGER trg_commerce_provider_operation_immutable
  BEFORE UPDATE ON commerce_provider_operation
  FOR EACH ROW EXECUTE FUNCTION commerce_guard_provider_operation_immutable();

COMMIT;
