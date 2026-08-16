\set ON_ERROR_STOP on

BEGIN;

-- The canonical cutover clears the copied locale/currency text only after it
-- has resolved both UUID references. Older installations declared those
-- evidence columns NOT NULL, so make the expand/contract boundary explicit
-- before the ledgered backfill runs. This migration changes no row values.
DO $resume$
BEGIN
  IF to_regclass('public.user_locale_preferences') IS NULL THEN
    RAISE EXCEPTION 'user_locale_preferences is required for catalog cutover resume';
  END IF;

  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'user_locale_preferences'
      AND column_name = 'locale_id'
      AND data_type = 'uuid'
  ) OR NOT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'user_locale_preferences'
      AND column_name = 'currency_id'
      AND data_type = 'uuid'
  ) THEN
    RAISE EXCEPTION 'canonical locale preference UUID columns are missing';
  END IF;

  ALTER TABLE user_locale_preferences
    ALTER COLUMN locale DROP NOT NULL,
    ALTER COLUMN currency DROP NOT NULL;
END
$resume$;

-- The interrupted release committed strict canonical triggers before the
-- legacy rows they protect were backfilled. Restore legacy-writer semantics
-- until each later cutover atomically installs its final canonical trigger.
DROP TRIGGER IF EXISTS catalog_pipeline_card_integrity ON pipeline_card;
DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;
DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;

COMMIT;
