-- Recovery strategy for a pre-write environment only.
-- Once directory writes exist, disable feature flags and preserve these tables.
BEGIN;

DROP FUNCTION IF EXISTS directory_execute_profile_merge(UUID,UUID,UUID,BIGINT,TEXT);
DROP VIEW IF EXISTS directory_public_profile_resolution;
DROP VIEW IF EXISTS directory_public_profile;
DROP VIEW IF EXISTS directory_public_venue;
DROP VIEW IF EXISTS directory_public_event;
DROP VIEW IF EXISTS directory_public_search_document;
DROP TABLE IF EXISTS directory_search_document;
DROP TABLE IF EXISTS directory_analytics_event;
DROP TABLE IF EXISTS directory_audit_event;
DROP TABLE IF EXISTS directory_idempotency;
DROP TABLE IF EXISTS directory_rate_limit;
DROP TABLE IF EXISTS directory_moderation_decision;
DROP TABLE IF EXISTS directory_moderation_case;
DROP TABLE IF EXISTS directory_moderation_report;
DROP TABLE IF EXISTS directory_review;
DROP TABLE IF EXISTS directory_interaction;
DROP TABLE IF EXISTS directory_verification;
ALTER TABLE IF EXISTS directory_profile_manager DROP CONSTRAINT IF EXISTS directory_profile_manager_source_claim_fk;
DROP TABLE IF EXISTS directory_claim;
DROP TABLE IF EXISTS directory_alert_delivery;
DROP TABLE IF EXISTS directory_saved_search;
DROP TABLE IF EXISTS directory_favorite;
DROP TABLE IF EXISTS directory_conversation_context;
DROP TABLE IF EXISTS directory_invitation;
DROP TABLE IF EXISTS classified_application;
DROP TABLE IF EXISTS classified_attachment;
DROP TABLE IF EXISTS classified_location;
DROP TABLE IF EXISTS classified_genre;
DROP TABLE IF EXISTS classified_instrument;
DROP TABLE IF EXISTS classified_profession;
DROP TABLE IF EXISTS classified;
DROP TABLE IF EXISTS directory_profile_block;
DROP TABLE IF EXISTS directory_contact_preference;
DROP TABLE IF EXISTS directory_age_assurance;
DROP TABLE IF EXISTS directory_profile_credit;
DROP TABLE IF EXISTS directory_private_location;
DROP TABLE IF EXISTS directory_profile_location;
DROP TABLE IF EXISTS directory_profile_language;
DROP TABLE IF EXISTS directory_profile_service;
DROP TABLE IF EXISTS directory_profile_genre;
DROP TABLE IF EXISTS directory_profile_instrument;
DROP TABLE IF EXISTS directory_profile_profession;
DROP TABLE IF EXISTS directory_merge_operation;
DROP TABLE IF EXISTS directory_backfill_mapping;
DROP TABLE IF EXISTS directory_legacy_link;
DROP TABLE IF EXISTS directory_profile_membership;
DROP TABLE IF EXISTS directory_profile_manager;
DROP TABLE IF EXISTS directory_backfill_run;
DROP TABLE IF EXISTS directory_profile;
DROP TABLE IF EXISTS metropolitan_area_city;
DROP TABLE IF EXISTS metropolitan_area;
DROP TABLE IF EXISTS compensation_type;
DROP TABLE IF EXISTS classified_category;
DROP TABLE IF EXISTS profession;
DROP TABLE IF EXISTS catalog_item_translation;

DELETE FROM catalog_definition
WHERE code IN ('professions','classified-categories','compensation-types','metropolitan-areas')
  AND NOT EXISTS (SELECT 1 FROM catalog_revision WHERE catalog_revision.catalog_id=catalog_definition.id)
  AND NOT EXISTS (SELECT 1 FROM catalog_audit_event WHERE catalog_audit_event.catalog_id=catalog_definition.id);

DROP FUNCTION IF EXISTS directory_withdraw_profile_surfaces();
DROP FUNCTION IF EXISTS directory_guard_review_interaction();
DROP FUNCTION IF EXISTS directory_guard_profile_manager_claim();
DROP FUNCTION IF EXISTS directory_guard_classified_transition();
DROP FUNCTION IF EXISTS directory_classified_transition_allowed(TEXT,TEXT);
DROP FUNCTION IF EXISTS directory_refresh_legacy_event_search();
DROP FUNCTION IF EXISTS directory_enqueue_saved_search_alerts();
DROP FUNCTION IF EXISTS directory_refresh_classified_search(UUID);
DROP FUNCTION IF EXISTS directory_refresh_profile_search(UUID);
DROP FUNCTION IF EXISTS directory_distance_km(DOUBLE PRECISION,DOUBLE PRECISION,DOUBLE PRECISION,DOUBLE PRECISION);
DROP FUNCTION IF EXISTS directory_stable_uuid(TEXT,TEXT);
DROP FUNCTION IF EXISTS directory_text_similarity(TEXT,TEXT);
DROP FUNCTION IF EXISTS directory_normalize_text(TEXT);

COMMIT;
