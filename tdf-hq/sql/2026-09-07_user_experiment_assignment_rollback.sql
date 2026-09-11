BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Application rollback is intentionally non-destructive. Paused older
-- versions ignore this additive table, while retaining assignments prevents
-- later roll-forward from rebucketing or duplicating exposures.
SELECT 1;

COMMIT;
