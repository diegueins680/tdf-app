\set ON_ERROR_STOP on
BEGIN;

DROP TRIGGER IF EXISTS trg_reputation_category_validate_safety ON reputation_category;
DROP FUNCTION IF EXISTS reputation_category_validate_safety();

COMMIT;
