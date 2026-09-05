-- Additive acceleration for the Party selector's bounded infix discovery.
-- pg_trgm is already an optional platform capability: restricted PostgreSQL
-- installations retain the deterministic bounded-query fallback and the
-- expression indexes from 2026-09-02_party_selector_search_indexes.sql.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_display_name_trgm_idx
      ON party USING GIN (
        translate(lower(display_name),
          'áàäâéèëêíìïîóòöôúùüûñ',
          'aaaaeeeeiiiioooouuuun') gin_trgm_ops
      )
    $index$;
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_legal_name_trgm_idx
      ON party USING GIN (
        translate(lower(COALESCE(legal_name, '')),
          'áàäâéèëêíìïîóòöôúùüûñ',
          'aaaaeeeeiiiioooouuuun') gin_trgm_ops
      )
    $index$;
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_active_username_trgm_idx
      ON user_credential USING GIN (
        lower(replace(username, '@', '')) gin_trgm_ops
      )
      WHERE active = TRUE
    $index$;
  ELSE
    RAISE NOTICE 'pg_trgm unavailable; Party selector keeps bounded-query fallback';
  END IF;
END
$$;

COMMIT;
