-- Emergency rollback for the provider-neutral checkout foundation.
-- Refuses destructive teardown once financial records exist.
BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM commerce_payment_attempt LIMIT 1)
     OR EXISTS (SELECT 1 FROM commerce_ledger_entry LIMIT 1) THEN
    RAISE EXCEPTION 'Refusing checkout-core rollback: financial records exist';
  END IF;
END $$;

DROP TABLE IF EXISTS revenue_feature_flag;
DROP TABLE IF EXISTS commerce_reconciliation_exception;
DROP TABLE IF EXISTS commerce_idempotency_record;
DROP TABLE IF EXISTS commerce_checkout_audit_event;
DROP TABLE IF EXISTS commerce_reservation_hold;
DROP TABLE IF EXISTS commerce_ledger_entry;
DROP TABLE IF EXISTS commerce_ledger_transaction;
DROP TABLE IF EXISTS commerce_receipt;
DROP TABLE IF EXISTS commerce_manual_payment_evidence;
DROP TABLE IF EXISTS commerce_dispute;
DROP TABLE IF EXISTS commerce_refund_allocation;
DROP TABLE IF EXISTS commerce_refund;
DROP TABLE IF EXISTS commerce_provider_event_inbox;
DROP TABLE IF EXISTS commerce_provider_binding;
DROP TABLE IF EXISTS commerce_payment_attempt;
DROP TABLE IF EXISTS commerce_checkout_line_item;
DROP TABLE IF EXISTS commerce_checkout_session;
DROP TABLE IF EXISTS commerce_quote_line;
DROP TABLE IF EXISTS commerce_quote;

DROP FUNCTION IF EXISTS commerce_protect_posted_ledger();
DROP FUNCTION IF EXISTS commerce_validate_ledger_posting();
DROP FUNCTION IF EXISTS commerce_protect_provider_event();
DROP FUNCTION IF EXISTS commerce_reject_immutable_mutation();
DROP FUNCTION IF EXISTS commerce_validate_provider_binding();
DROP FUNCTION IF EXISTS commerce_validate_payment_attempt();

COMMIT;
