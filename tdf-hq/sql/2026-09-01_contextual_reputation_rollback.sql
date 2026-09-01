\set ON_ERROR_STOP on
BEGIN;
-- Roll back readers/writers before applying. Historical verified reviews are
-- intentionally untouched; this only removes the additive v1 projection.
DROP TABLE IF EXISTS reputation_audit_log;
DROP TABLE IF EXISTS reputation_public_aggregate;
DROP TABLE IF EXISTS reputation_private_ranking;
DROP TABLE IF EXISTS reputation_evaluation_rank;
DROP TABLE IF EXISTS reputation_evaluation_category;
DROP TABLE IF EXISTS reputation_evaluation;
DROP TABLE IF EXISTS reputation_interaction;
DROP TABLE IF EXISTS reputation_category;
DROP TABLE IF EXISTS reputation_formula_version;
COMMIT;
