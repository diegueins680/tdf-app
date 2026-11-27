-- Rollback for 2026-11-21_venue_search_sql.sql (EVT-VENUE-SEARCH-001).
-- Removing the flag rows first returns every binary to the in-memory search;
-- a binary that finds no fold function also keeps that path.

BEGIN;

SET LOCAL lock_timeout = '10s';

DELETE FROM revenue_feature_flag WHERE flag_key = 'social_venues.sql_search';
DROP INDEX IF EXISTS venue_search_name_trgm_idx;
DROP INDEX IF EXISTS venue_search_city_trgm_idx;
DROP INDEX IF EXISTS venue_search_address_trgm_idx;
DROP INDEX IF EXISTS venue_search_out_of_range_idx;
DROP INDEX IF EXISTS venue_search_latitude_longitude_idx;
DROP FUNCTION IF EXISTS venue_search_fold(text);

COMMIT;
