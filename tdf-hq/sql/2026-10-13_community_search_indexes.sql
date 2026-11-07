-- Reuse the directory's normalizer and pg_trgm extension; no new search service.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname='pg_trgm') THEN
    CREATE INDEX IF NOT EXISTS party_discovery_name_trgm_idx
      ON party USING gin (directory_normalize_text(display_name) gin_trgm_ops);
    CREATE INDEX IF NOT EXISTS artist_discovery_text_trgm_idx
      ON artist_profile USING gin (directory_normalize_text(
        coalesce(slug,'') || ' ' || coalesce(bio,'') || ' ' || coalesce(city,'') || ' ' ||
        coalesce(country_code,'') || ' ' || coalesce(genres,'') || ' ' || coalesce(highlights,'')
      ) gin_trgm_ops);
  END IF;
END $$;
COMMIT;
