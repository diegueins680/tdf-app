-- Rollback removes only runtime synchronization. Canonical intent/history
-- evidence is retained and must never be erased by a deployment rollback.
\set ON_ERROR_STOP on
BEGIN;

DROP TRIGGER IF EXISTS trg_commerce_sync_payment_intent_from_attempt
  ON commerce_payment_attempt;
DROP FUNCTION IF EXISTS commerce_sync_payment_intent_from_attempt();

COMMIT;
