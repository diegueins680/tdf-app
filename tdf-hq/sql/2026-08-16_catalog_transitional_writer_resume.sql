\set ON_ERROR_STOP on

BEGIN;

-- The interrupted release committed strict canonical triggers before the
-- legacy rows they protect were backfilled. Restore legacy-writer semantics
-- until each later cutover atomically installs its final canonical trigger.
DROP TRIGGER IF EXISTS catalog_pipeline_card_integrity ON pipeline_card;
DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;
DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;

COMMIT;
