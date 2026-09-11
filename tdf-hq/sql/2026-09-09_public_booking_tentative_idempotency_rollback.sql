BEGIN;

-- Application rollback is intentionally non-destructive. Older application
-- versions ignore this additive table, while dropping it would make a replay
-- capable of creating a second booking after an ambiguous response.
SELECT 1;

COMMIT;
