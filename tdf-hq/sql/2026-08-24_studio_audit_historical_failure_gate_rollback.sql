-- Remove the forward-only historical execution gate. The original completion
-- trigger remains installed and continues to validate each latest execution.
BEGIN;

DROP TRIGGER IF EXISTS trg_enforce_intern_audit_historical_failures ON intern_task;
DROP FUNCTION IF EXISTS enforce_intern_audit_historical_failures();

COMMIT;
