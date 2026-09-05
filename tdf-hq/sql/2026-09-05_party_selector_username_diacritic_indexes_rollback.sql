-- Restore the punctuation-only username indexes used before the diacritic
-- fold. Run only while rolling the backend back to the matching query shape.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP INDEX IF EXISTS party_selector_active_username_compact_trgm_idx;
DROP INDEX IF EXISTS party_selector_active_username_compact_idx;

CREATE INDEX party_selector_active_username_compact_idx
  ON user_credential (
    regexp_replace(lower(username), '[@''’‘_.[:space:]-]+', '', 'g')
  )
  WHERE active = TRUE;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
    EXECUTE $index$
      CREATE INDEX party_selector_active_username_compact_trgm_idx
      ON user_credential USING GIN (
        regexp_replace(lower(username), '[@''’‘_.[:space:]-]+', '', 'g') gin_trgm_ops
      )
      WHERE active = TRUE
    $index$;
  ELSE
    RAISE NOTICE 'pg_trgm unavailable; Party selector keeps bounded-query fallback';
  END IF;
END
$$;

COMMIT;
