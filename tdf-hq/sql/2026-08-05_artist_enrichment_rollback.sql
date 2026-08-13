-- Emergency rollback for 2026-08-05_artist_enrichment.sql.
-- Export the tables before executing. Existing artist_profile/party rows and
-- media in Google Drive are intentionally preserved.

BEGIN;

DROP TABLE IF EXISTS artist_media_asset;
DROP TABLE IF EXISTS artist_identity_candidate;
DROP TABLE IF EXISTS artist_field_change;
DROP TABLE IF EXISTS artist_enrichment_suggestion;
DROP TABLE IF EXISTS artist_research_source;
DROP TABLE IF EXISTS artist_inventory_reference;
DROP TABLE IF EXISTS artist_enrichment_run;
DROP TABLE IF EXISTS artist_profile_enrichment;
DROP INDEX IF EXISTS uq_artist_profile_slug_ci;

COMMIT;
