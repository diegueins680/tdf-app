BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP INDEX IF EXISTS party_selector_active_username_trgm_idx;
DROP INDEX IF EXISTS party_selector_legal_name_trgm_idx;
DROP INDEX IF EXISTS party_selector_display_name_trgm_idx;

COMMIT;
