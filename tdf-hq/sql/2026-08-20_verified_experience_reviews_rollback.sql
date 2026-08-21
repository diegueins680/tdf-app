\set ON_ERROR_STOP on

BEGIN;

DROP TRIGGER IF EXISTS trg_experience_review_validate_write ON experience_review;
DROP FUNCTION IF EXISTS experience_review_validate_write();
DROP FUNCTION IF EXISTS experience_review_source_is_eligible(TEXT, TEXT, TEXT, TEXT, BIGINT);
DROP TABLE IF EXISTS experience_review;

-- `experience-review` is intentionally retained in the shared scope enum.
-- Existing abuse-control rows remain valid and auditable after application
-- rollback, matching the directory review rollback policy.

COMMIT;
