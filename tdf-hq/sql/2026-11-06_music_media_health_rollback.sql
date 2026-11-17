-- Application rollback preserves observations and source history. Removing the
-- gate would republish confirmed-invalid media, so it deliberately stays active.
BEGIN;
SELECT 1;
COMMIT;
