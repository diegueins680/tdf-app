-- Keep active-username discovery aligned with the selector matcher. The prior
-- compact indexes removed punctuation but did not fold Spanish diacritics, so
-- a query such as "elise" could not discover the stored username "élise".

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DROP INDEX IF EXISTS party_selector_active_username_compact_trgm_idx;
DROP INDEX IF EXISTS party_selector_active_username_compact_idx;

CREATE INDEX party_selector_active_username_compact_idx
  ON user_credential (
    regexp_replace(
      translate(lower(username),
        'áàäâéèëêíìïîóòöôúùüûñ',
        'aaaaeeeeiiiioooouuuun'),
      '[@''’‘_.[:space:]-]+', '', 'g'
    )
  )
  WHERE active = TRUE;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
    EXECUTE $index$
      CREATE INDEX party_selector_active_username_compact_trgm_idx
      ON user_credential USING GIN (
        regexp_replace(
          translate(lower(username),
            'áàäâéèëêíìïîóòöôúùüûñ',
            'aaaaeeeeiiiioooouuuun'),
          '[@''’‘_.[:space:]-]+', '', 'g'
        ) gin_trgm_ops
      )
      WHERE active = TRUE
    $index$;
  ELSE
    RAISE NOTICE 'pg_trgm unavailable; Party selector keeps bounded-query fallback';
  END IF;
END
$$;

COMMIT;
