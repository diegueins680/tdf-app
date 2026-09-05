BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP INDEX IF EXISTS party_selector_active_username_compact_trgm_idx;
DROP INDEX IF EXISTS party_selector_legal_name_compact_trgm_idx;
DROP INDEX IF EXISTS party_selector_display_name_compact_trgm_idx;
DROP INDEX IF EXISTS party_selector_active_username_compact_idx;
DROP INDEX IF EXISTS party_selector_legal_name_compact_idx;
DROP INDEX IF EXISTS party_selector_display_name_compact_idx;

COMMIT;
