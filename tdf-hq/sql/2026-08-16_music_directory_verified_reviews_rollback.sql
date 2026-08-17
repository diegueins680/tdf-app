\set ON_ERROR_STOP on

-- Operational rollback for the derived reputation machinery. Roll application
-- code back first and freeze directory-review writes. The additive `review`
-- enum values are deliberately retained so existing reports/rate rows remain
-- valid and auditable; no review or interaction row is deleted.
BEGIN;

DROP TRIGGER IF EXISTS directory_profile_status_reputation_trigger ON directory_profile;
DROP TRIGGER IF EXISTS directory_interaction_reputation_trigger ON directory_interaction;
DROP TRIGGER IF EXISTS directory_review_reputation_trigger ON directory_review;

DROP FUNCTION IF EXISTS directory_refresh_profile_status_reputation();
DROP FUNCTION IF EXISTS directory_refresh_interaction_reputation();
DROP FUNCTION IF EXISTS directory_refresh_review_reputation();
DROP FUNCTION IF EXISTS directory_refresh_profile_reputation(UUID);

DROP INDEX IF EXISTS directory_interaction_profile_b_status_idx;
DROP INDEX IF EXISTS directory_interaction_profile_a_status_idx;
DROP INDEX IF EXISTS directory_review_author_subject_idx;
DROP INDEX IF EXISTS directory_review_subject_public_idx;

COMMIT;
