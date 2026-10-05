\set ON_ERROR_STOP on
BEGIN;

DO $$
BEGIN
  IF to_regclass('commerce_provider_operation') IS NOT NULL
     AND EXISTS (SELECT 1 FROM commerce_provider_operation LIMIT 1) THEN
    RAISE EXCEPTION 'refusing to remove non-empty commerce_provider_operation';
  END IF;
  IF EXISTS (
    SELECT 1 FROM commerce_provider_event_inbox
    WHERE evidence_type = 'untrusted_callback'
    LIMIT 1
  ) THEN
    RAISE EXCEPTION 'refusing to remove untrusted provider callback evidence';
  END IF;
END;
$$;

DROP TABLE IF EXISTS commerce_provider_operation;
DROP FUNCTION IF EXISTS commerce_guard_provider_operation_immutable();

DROP INDEX IF EXISTS idx_commerce_provider_event_untrusted_work;
DELETE FROM revenue_feature_flag
WHERE (flag_key, environment, enabled, reason) IN (
  ('checkout.placetopay.webhooks', 'sandbox', TRUE,
    'Signed PlaceToPay notifications trigger an authoritative session query'),
  ('checkout.placetopay.webhooks', 'production', FALSE,
    'Requires production merchant approval, registered HTTPS URL, and activation evidence'),
  ('checkout.payphone.notifications', 'sandbox', TRUE,
    'Unsigned PayPhone notifications are query triggers only'),
  ('checkout.payphone.notifications', 'production', FALSE,
    'Requires PayPhone approval, registered HTTPS URL, and production authorization')
);
ALTER TABLE commerce_provider_event_inbox
  DROP CONSTRAINT IF EXISTS ck_commerce_provider_event_evidence;
CREATE OR REPLACE FUNCTION commerce_protect_provider_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  IF (OLD.provider, OLD.environment, OLD.merchant_account_ref, OLD.provider_event_id,
      OLD.event_type, OLD.signature_verified, OLD.provider_created_at,
      OLD.payload_ciphertext, OLD.payload_sha256)
     IS DISTINCT FROM
     (NEW.provider, NEW.environment, NEW.merchant_account_ref, NEW.provider_event_id,
      NEW.event_type, NEW.signature_verified, NEW.provider_created_at,
      NEW.payload_ciphertext, NEW.payload_sha256) THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  RETURN NEW;
END $$;
ALTER TABLE commerce_provider_event_inbox
  ADD CONSTRAINT ck_commerce_provider_event_production_signature CHECK (
    environment <> 'production' OR signature_verified
  );
ALTER TABLE commerce_provider_event_inbox
  DROP COLUMN IF EXISTS evidence_type;

COMMIT;
