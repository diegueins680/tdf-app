BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Application rollback is intentionally non-destructive. Older application
-- versions do not read this additive relation, so retaining it preserves
-- account-bound onboarding history for a later roll-forward.
SELECT 1;

COMMIT;
