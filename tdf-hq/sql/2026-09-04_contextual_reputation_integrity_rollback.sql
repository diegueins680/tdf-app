\set ON_ERROR_STOP on
BEGIN;
DROP TRIGGER IF EXISTS trg_reputation_evaluation_category_validate_active ON reputation_evaluation_category;
DROP FUNCTION IF EXISTS reputation_evaluation_category_validate_active();
DROP TRIGGER IF EXISTS trg_reputation_evaluation_validate_participants ON reputation_evaluation;
DROP FUNCTION IF EXISTS reputation_evaluation_validate_participants();
COMMIT;
