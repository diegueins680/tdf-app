-- Safe rollback for the canonical payment lifecycle foundation.
-- Refuses to erase evidence or remove provider values that are already in use.
\set ON_ERROR_STOP on
BEGIN;

DO $$
DECLARE
  evidence_count BIGINT;
BEGIN
  SELECT
      (SELECT count(*) FROM commerce_provider_capability)
    + (SELECT count(*) FROM commerce_payment_intent)
    + (SELECT count(*) FROM commerce_payment_authorization)
    + (SELECT count(*) FROM commerce_payment_capture)
    + (SELECT count(*) FROM commerce_payment_void)
    + (SELECT count(*) FROM commerce_payment_state_history)
    + (SELECT count(*) FROM commerce_payment_amount_component)
    + (SELECT count(*) FROM commerce_connected_account)
    + (SELECT count(*) FROM commerce_commission)
    + (SELECT count(*) FROM commerce_settlement)
    + (SELECT count(*) FROM commerce_settlement_allocation)
    + (SELECT count(*) FROM commerce_seller_balance_entry)
    + (SELECT count(*) FROM commerce_payout)
    + (SELECT count(*) FROM commerce_payout_allocation)
    + (SELECT count(*) FROM commerce_payment_mandate)
    + (SELECT count(*) FROM commerce_payment_link)
    + (SELECT count(*) FROM commerce_payment_attempt WHERE provider IN ('placetopay','payphone'))
    + (SELECT count(*) FROM commerce_refund WHERE provider IN ('placetopay','payphone'))
  INTO evidence_count;

  IF evidence_count > 0 THEN
    RAISE EXCEPTION 'Canonical payment lifecycle rollback refused: % evidence rows exist', evidence_count;
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE flag_key IN (
  'checkout.placetopay',
  'checkout.payphone',
  'commerce.marketplace_connected_accounts',
  'commerce.recurring_payments',
  'commerce.payment_links'
)
AND environment = 'production'
AND enabled = FALSE;

DELETE FROM commerce_provider_account;

DROP TABLE commerce_payment_link;
DROP TABLE commerce_payment_mandate;
DROP TABLE commerce_payout_allocation;
DROP TABLE commerce_seller_balance_entry;
DROP TABLE commerce_payout;
DROP TABLE commerce_settlement_allocation;
DROP TABLE commerce_settlement;
DROP TABLE commerce_commission;
DROP TABLE commerce_connected_account;
DROP TABLE commerce_payment_amount_component;
DROP TABLE commerce_payment_state_history;
DROP TABLE commerce_payment_void;
DROP TABLE commerce_payment_capture;
DROP TABLE commerce_payment_authorization;
DROP TABLE commerce_payment_intent;
DROP TABLE commerce_provider_capability;
DROP TABLE commerce_provider_account;
DROP FUNCTION commerce_protect_bound_payout_reference();
DROP FUNCTION commerce_protect_payment_external_reference();

ALTER TABLE commerce_payment_attempt
  DROP CONSTRAINT ck_commerce_payment_attempt_provider;
ALTER TABLE commerce_payment_attempt
  ADD CONSTRAINT commerce_payment_attempt_provider_check CHECK (
    provider IN ('datafast','paypal','stripe','bank_transfer','cash','pos','cardano')
  );

ALTER TABLE commerce_refund
  DROP CONSTRAINT ck_commerce_refund_provider;
ALTER TABLE commerce_refund
  ADD CONSTRAINT ck_commerce_refund_provider CHECK (
    provider IS NULL OR provider IN ('datafast','paypal','stripe','bank_transfer','cash','pos')
  );

COMMIT;
