-- Stop refund writers before rollback. Preserve reconciled totals and audit
-- history; older application code does not maintain these canonical totals.
\set ON_ERROR_STOP on
BEGIN;
DROP TRIGGER IF EXISTS trg_commerce_sync_payment_intent_from_refund ON commerce_refund;
DROP FUNCTION IF EXISTS commerce_sync_payment_intent_from_refund();
DROP FUNCTION IF EXISTS commerce_reconcile_intent_refunds(UUID, TEXT);
COMMIT;
