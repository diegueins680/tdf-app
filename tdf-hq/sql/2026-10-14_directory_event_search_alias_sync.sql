-- A catalog search alias is one of the names a legacy lineup genre word can
-- map through (directory_social_event_genre_ids). Adding, changing or removing
-- a genre alias therefore resyncs the events it reaches, as a genre rename
-- does (2026-10-09_directory_event_search_sync):
--   * events whose lineup has a legacy social_artist_profile.genres word that
--     normalises to the alias's old or new term (the id appears or goes);
--   * events whose document already carries the alias's genre id (an alias
--     moved to another genre, or removed).
-- Aliases of other catalog entities (professions, ...) are ignored.
-- Rollback: 2026-10-14_directory_event_search_alias_sync_rollback.sql, which
-- must run before rolling back the 2026-10-09 event search sync.
BEGIN;

CREATE OR REPLACE FUNCTION directory_sync_alias_events_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  alias_event_id BIGINT;
  old_genre UUID;
  new_genre UUID;
  old_term TEXT;
  new_term TEXT;
BEGIN
  IF TG_OP IN ('UPDATE', 'DELETE') THEN
    SELECT catalog_genre.id INTO old_genre FROM genre catalog_genre WHERE catalog_genre.id = OLD.entity_id;
    old_term := CASE WHEN old_genre IS NOT NULL THEN nullif(OLD.normalized_term, '') END;
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') THEN
    SELECT catalog_genre.id INTO new_genre FROM genre catalog_genre WHERE catalog_genre.id = NEW.entity_id;
    new_term := CASE WHEN new_genre IS NOT NULL THEN nullif(NEW.normalized_term, '') END;
  END IF;
  IF old_genre IS NULL AND new_genre IS NULL THEN
    RETURN NULL;
  END IF;
  FOR alias_event_id IN
    SELECT lineup.event_id
    FROM event_artist lineup
    JOIN social_artist_profile artist ON artist.id = lineup.artist_id
    WHERE EXISTS (
      SELECT 1 FROM unnest(coalesce(artist.genres, '{}'::text[])) AS raw(term)
      WHERE directory_normalize_text(raw.term) IN (old_term, new_term)
    )
    UNION
    SELECT event.id
    FROM directory_search_document document
    JOIN social_event event ON event.id::text = document.entity_id
    WHERE document.entity_kind = 'event'
      AND document.genre_ids && array_remove(ARRAY[old_genre, new_genre], NULL)
    ORDER BY 1
  LOOP
    PERFORM directory_sync_event_search(alias_event_id);
  END LOOP;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_alias_events_search ON catalog_search_alias;
CREATE CONSTRAINT TRIGGER trg_directory_sync_alias_events_search
  AFTER INSERT OR UPDATE OR DELETE ON catalog_search_alias
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_alias_events_search_trigger();

COMMIT;
