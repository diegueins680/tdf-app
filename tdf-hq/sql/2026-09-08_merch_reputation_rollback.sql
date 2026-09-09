-- Destructive rollback is intentionally available only for a pristine, unused install.
-- For any environment with merch or reputation evidence, disable flags and roll forward.
\set ON_ERROR_STOP on

BEGIN;

DO $$
DECLARE has_durable_data BOOLEAN;
BEGIN
  SELECT
    EXISTS (SELECT 1 FROM merch_review_revision)
    OR EXISTS (SELECT 1 FROM merch_reputation_priority_revision)
    OR EXISTS (SELECT 1 FROM merch_reputation_operational_signal)
    OR EXISTS (SELECT 1 FROM merch_reputation_evidence)
    OR EXISTS (SELECT 1 FROM merch_reputation_report)
    OR EXISTS (SELECT 1 FROM merch_reputation_appeal)
    OR EXISTS (SELECT 1 FROM merch_reputation_audit_event)
    OR EXISTS (SELECT 1 FROM merch_reputation_risk_case)
    OR EXISTS (SELECT 1 FROM merch_reputation_notification_outbox)
    OR EXISTS (SELECT 1 FROM merch_order_line)
    OR EXISTS (SELECT 1 FROM merch_order)
    OR EXISTS (SELECT 1 FROM merch_product)
    OR EXISTS (SELECT 1 FROM merch_store)
  INTO has_durable_data;
  IF has_durable_data THEN
    RAISE EXCEPTION USING
      MESSAGE='Unsafe merch reputation rollback refused: durable commerce or reputation evidence exists',
      HINT='Disable all merch reputation flags, retain evidence, and use an audited forward migration.';
  END IF;
END $$;

DROP VIEW IF EXISTS merch_reputation_projection_alerts;
DROP VIEW IF EXISTS merch_reputation_metrics;

DROP FUNCTION IF EXISTS merch_reputation_search_contribution(UUID,NUMERIC,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_decide_category_suggestion(BIGINT,UUID,TEXT,INTEGER,JSONB,JSONB,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_submit_category_suggestion(BIGINT,TEXT,TEXT,TEXT,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_set_priorities(BIGINT,TEXT,JSONB,INTEGER,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_enqueue_review_invitation();
DROP FUNCTION IF EXISTS merch_reputation_process_events(INTEGER,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_recalculate_badges(UUID,TIMESTAMPTZ,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_appeal_decision(BIGINT,UUID,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_resolve_appeal(BIGINT,UUID,TEXT,TEXT,JSONB,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_decide_moderation(BIGINT,UUID,TEXT,TEXT,TEXT,JSONB,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_transition_moderation_case(BIGINT,UUID,TEXT,TEXT,JSONB,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_report_content(BIGINT,TEXT,UUID,TEXT,TEXT,JSONB,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_respond(BIGINT,UUID,TEXT,INTEGER,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_submit_review(BIGINT,TEXT,UUID,UUID,SMALLINT,BOOLEAN,TEXT,JSONB,JSONB,INTEGER,TEXT,TEXT);
DROP FUNCTION IF EXISTS merch_reputation_record_operational_signal(TEXT,UUID,UUID,TEXT,NUMERIC,NUMERIC,TEXT,TEXT,TEXT,TEXT,TEXT,JSONB,TIMESTAMPTZ);
DROP FUNCTION IF EXISTS merch_reputation_rebuild_aggregate(TEXT,UUID,TIMESTAMPTZ,UUID);

DROP TABLE IF EXISTS merch_reputation_exposure_daily;
DROP TABLE IF EXISTS merch_reputation_notification_outbox;
DROP TABLE IF EXISTS merch_reputation_notification_preference;
DROP TABLE IF EXISTS merch_reputation_risk_measure;
DROP TABLE IF EXISTS merch_reputation_risk_case;
DROP TABLE IF EXISTS merch_reputation_risk_policy_version;
DROP TABLE IF EXISTS merch_reputation_audit_event;
DROP TABLE IF EXISTS merch_reputation_appeal;
DROP TABLE IF EXISTS merch_reputation_moderation_decision;
DROP TABLE IF EXISTS merch_reputation_moderation_case;
DROP TABLE IF EXISTS merch_reputation_report;
DROP TABLE IF EXISTS merch_reputation_badge_award;
DROP TABLE IF EXISTS merch_reputation_badge_definition;
DROP TABLE IF EXISTS merch_reputation_idempotency;
DROP TABLE IF EXISTS merch_reputation_projection_checkpoint;
DROP TABLE IF EXISTS merch_reputation_event;
DROP TABLE IF EXISTS merch_reputation_dimension_aggregate;
DROP TABLE IF EXISTS merch_reputation_aggregate;
DROP TABLE IF EXISTS merch_reputation_operational_signal;
DROP TABLE IF EXISTS merch_reputation_evidence;
DROP TABLE IF EXISTS merch_seller_response_revision;
DROP TABLE IF EXISTS merch_seller_response;
DROP TABLE IF EXISTS merch_review_image;
DROP TABLE IF EXISTS merch_review_dimension_rating;
DROP TABLE IF EXISTS merch_review_revision;
DROP TABLE IF EXISTS merch_review;
DROP TABLE IF EXISTS merch_review_media_asset;
DROP TABLE IF EXISTS merch_review_privacy_preference;
DROP TABLE IF EXISTS merch_reputation_priority_item;
DROP TABLE IF EXISTS merch_reputation_priority_revision;
DROP TABLE IF EXISTS merch_reputation_priority_profile;
DROP TABLE IF EXISTS merch_reputation_category_suggestion;
DROP TABLE IF EXISTS merch_reputation_dimension;
DROP TABLE IF EXISTS merch_reputation_formula_version;
DROP TABLE IF EXISTS merch_reputation_feature_flag;
DROP TABLE IF EXISTS merch_order_line;
DROP TABLE IF EXISTS merch_order;
DROP TABLE IF EXISTS merch_product;
DROP TABLE IF EXISTS merch_store_member;
DROP TABLE IF EXISTS merch_store;

DROP FUNCTION IF EXISTS merch_review_validate_revision_dimensions();
DROP FUNCTION IF EXISTS merch_reputation_immutable();
DROP FUNCTION IF EXISTS merch_review_validate_identity();
DROP FUNCTION IF EXISTS merch_review_evidence_is_eligible(TEXT,UUID,BIGINT,TIMESTAMPTZ);
DROP FUNCTION IF EXISTS merch_reputation_accounts_related(UUID,BIGINT);
DROP FUNCTION IF EXISTS merch_reputation_formula_immutable();
DROP FUNCTION IF EXISTS merch_store_sync_owner_membership();

COMMIT;
