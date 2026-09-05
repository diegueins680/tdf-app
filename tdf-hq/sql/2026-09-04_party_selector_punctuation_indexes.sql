-- Keep SQL candidate discovery aligned with the selector's Unicode-aware
-- comparison. Apostrophes, hyphens, username separators and whitespace are
-- presentation details, so searches such as "oneil" still discover O'Neil.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE INDEX IF NOT EXISTS party_selector_display_name_compact_idx
  ON party (
    regexp_replace(
      translate(lower(display_name),
        'áàäâéèëêíìïîóòöôúùüûñ',
        'aaaaeeeeiiiioooouuuun'),
      '[@''’‘_.[:space:]-]+', '', 'g'
    )
  );

CREATE INDEX IF NOT EXISTS party_selector_legal_name_compact_idx
  ON party (
    regexp_replace(
      translate(lower(COALESCE(legal_name, '')),
        'áàäâéèëêíìïîóòöôúùüûñ',
        'aaaaeeeeiiiioooouuuun'),
      '[@''’‘_.[:space:]-]+', '', 'g'
    )
  );

CREATE INDEX IF NOT EXISTS party_selector_active_username_compact_idx
  ON user_credential (
    regexp_replace(lower(username), '[@''’‘_.[:space:]-]+', '', 'g')
  )
  WHERE active = TRUE;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_display_name_compact_trgm_idx
      ON party USING GIN (
        regexp_replace(
          translate(lower(display_name),
            'áàäâéèëêíìïîóòöôúùüûñ',
            'aaaaeeeeiiiioooouuuun'),
          '[@''’‘_.[:space:]-]+', '', 'g'
        ) gin_trgm_ops
      )
    $index$;
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_legal_name_compact_trgm_idx
      ON party USING GIN (
        regexp_replace(
          translate(lower(COALESCE(legal_name, '')),
            'áàäâéèëêíìïîóòöôúùüûñ',
            'aaaaeeeeiiiioooouuuun'),
          '[@''’‘_.[:space:]-]+', '', 'g'
        ) gin_trgm_ops
      )
    $index$;
    EXECUTE $index$
      CREATE INDEX IF NOT EXISTS party_selector_active_username_compact_trgm_idx
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
