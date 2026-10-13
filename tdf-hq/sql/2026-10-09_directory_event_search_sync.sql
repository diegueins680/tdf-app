-- Keep directory search documents for events and venues in sync with their
-- sources. Until now they were only written by the one-off backfill
-- (directory_refresh_legacy_event_search), so events created afterwards never
-- appeared in /buscar. Documents are derived from the privacy-reviewed
-- directory_public_event / directory_public_venue views; search queries still
-- re-check those views at read time, so a stale row can never expose an event.
--
-- Also: venues whose city is only free text ("Quito") get the matching
-- city_reference id when the match is unambiguous, so city filters find them;
-- and event/venue documents carry their kind as words ("evento", "venue") so
-- a search for "eventos" returns events.
BEGIN;

CREATE OR REPLACE FUNCTION directory_resolve_city_reference(city_text TEXT)
RETURNS UUID
LANGUAGE sql
STABLE
AS $$
  SELECT CASE WHEN count(*) = 1 THEN (array_agg(city.id))[1] END
  FROM city_reference city
  WHERE nullif(btrim(city_text), '') IS NOT NULL
    -- Only cities users can select: a retired namesake must neither be
    -- assigned nor make the active city ambiguous.
    AND city.active
    AND directory_normalize_text(city.name_es) = directory_normalize_text(city_text);
$$;

CREATE OR REPLACE FUNCTION directory_fill_venue_city_reference()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.city_id IS NULL THEN
    NEW.city_id := directory_resolve_city_reference(NEW.city);
  ELSIF TG_OP = 'UPDATE'
    AND NEW.city IS DISTINCT FROM OLD.city
    AND NEW.city_id IS NOT DISTINCT FROM OLD.city_id
    AND (
      (OLD.city_id IS NULL AND directory_resolve_city_reference(OLD.city) IS NULL)
      OR EXISTS (
        SELECT 1 FROM city_reference city
        WHERE city.id = OLD.city_id
          AND directory_normalize_text(city.name_es) = directory_normalize_text(OLD.city)
      )
    ) THEN
    -- The id agreed with the previous city text and the caller changed only
    -- the text (the venue API never writes city_id), so the id follows the
    -- text. An id that was chosen independently of the text is kept. The old
    -- city is matched by name whether or not it is still active, so a venue
    -- does not stay pinned to a city that was retired after it was assigned.
    NEW.city_id := directory_resolve_city_reference(NEW.city);
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_fill_venue_city_reference ON venue;
CREATE TRIGGER trg_directory_fill_venue_city_reference
  BEFORE INSERT OR UPDATE OF city, city_id ON venue
  FOR EACH ROW EXECUTE FUNCTION directory_fill_venue_city_reference();

-- The result image follows the rule of the public event detail
-- (directory_public_rsvp_event, 2026-09-08): an HTTPS URL whose host carries
-- no credentials, or a same-origin path. A flyer the detail page shows is
-- therefore never a placeholder on the search card, and vice versa.
CREATE OR REPLACE FUNCTION directory_social_event_metadata_image(event_metadata TEXT)
RETURNS TEXT
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN event_metadata IS NULL OR NOT pg_input_is_valid(event_metadata, 'jsonb') THEN NULL
    WHEN jsonb_typeof(event_metadata::jsonb) <> 'object' THEN NULL
    WHEN jsonb_typeof(event_metadata::jsonb -> 'imageUrl') <> 'string' THEN NULL
    WHEN strpos(event_metadata::jsonb ->> 'imageUrl', chr(92)) > 0 THEN NULL
    WHEN (event_metadata::jsonb ->> 'imageUrl') ~* '^https://[^[:space:][:cntrl:]]+$'
      AND split_part(event_metadata::jsonb ->> 'imageUrl', '/', 3) NOT LIKE '%@%'
      THEN event_metadata::jsonb ->> 'imageUrl'
    WHEN (event_metadata::jsonb ->> 'imageUrl') ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'
      THEN event_metadata::jsonb ->> 'imageUrl'
  END;
$$;

-- An event carries no genre of its own; its lineup does. The free-text genres
-- of the artists on the bill ("techno", "Rock") map to catalog genre ids by
-- code, slug, name or search alias, so the genre filter and the taxonomy
-- ranking apply to events the same way they apply to profiles. Terms that do
-- not name a catalog genre are kept only as search text.
CREATE OR REPLACE FUNCTION directory_social_event_genre_ids(event_genres TEXT[])
RETURNS UUID[]
LANGUAGE sql
STABLE
AS $$
  SELECT coalesce(array_agg(DISTINCT catalog_genre.id ORDER BY catalog_genre.id), '{}'::uuid[])
  FROM unnest(coalesce(event_genres, '{}'::text[])) AS raw(term)
  CROSS JOIN LATERAL (SELECT nullif(directory_normalize_text(raw.term), '') AS normalized) AS needle
  JOIN genre catalog_genre
    ON catalog_genre.active
   AND (
     directory_normalize_text(catalog_genre.code) = needle.normalized
     OR directory_normalize_text(coalesce(catalog_genre.current_slug, '')) = needle.normalized
     OR directory_normalize_text(catalog_genre.name_es) = needle.normalized
     OR directory_normalize_text(catalog_genre.name_en) = needle.normalized
     OR EXISTS (
       SELECT 1 FROM catalog_search_alias alias
       WHERE alias.entity_id = catalog_genre.id AND alias.normalized_term = needle.normalized
     )
   )
  WHERE needle.normalized IS NOT NULL;
$$;

-- The lineup's genres and artist names, projected into the event document.
-- Genres come from the canonical membership table (artist_genre_membership,
-- written by the current artist API) as catalog ids, and from the legacy
-- social_artist_profile.genres text array as words; both feed genre_ids
-- (the words through the catalog mapping) and the search text.
-- Dropped first: OUT parameters are part of the return type, which
-- CREATE OR REPLACE cannot change; callers are PL/pgSQL bodies resolved at
-- run time.
DROP FUNCTION IF EXISTS directory_social_event_lineup_terms(BIGINT);
CREATE FUNCTION directory_social_event_lineup_terms(
  target_event_id BIGINT,
  OUT genre_ids UUID[],
  OUT genre_terms TEXT[],
  OUT artist_names TEXT[]
)
LANGUAGE sql
STABLE
AS $$
  SELECT
    coalesce((
      SELECT array_agg(DISTINCT membership.genre_id ORDER BY membership.genre_id)
      FROM event_artist lineup
      JOIN artist_genre_membership membership ON membership.artist_id = lineup.artist_id
      JOIN genre catalog_genre ON catalog_genre.id = membership.genre_id AND catalog_genre.active
      WHERE lineup.event_id = target_event_id
    ), '{}'::uuid[]),
    coalesce((
      SELECT array_agg(terms.term ORDER BY terms.term)
      FROM (
        SELECT DISTINCT genre_term AS term
        FROM event_artist lineup
        JOIN social_artist_profile artist ON artist.id = lineup.artist_id
        CROSS JOIN LATERAL unnest(coalesce(artist.genres, '{}'::text[])) AS genre_term
        WHERE lineup.event_id = target_event_id
          AND nullif(btrim(genre_term), '') IS NOT NULL
        UNION
        SELECT DISTINCT catalog_genre.name_es
        FROM event_artist lineup
        JOIN artist_genre_membership membership ON membership.artist_id = lineup.artist_id
        JOIN genre catalog_genre ON catalog_genre.id = membership.genre_id AND catalog_genre.active
        WHERE lineup.event_id = target_event_id
      ) terms
    ), '{}'::text[]),
    coalesce((
      SELECT array_agg(artist.name ORDER BY artist.name)
      FROM event_artist lineup
      JOIN social_artist_profile artist ON artist.id = lineup.artist_id
      WHERE lineup.event_id = target_event_id
    ), '{}'::text[]);
$$;

-- Event, venue and suppression writes all rewrite the same search documents
-- from each other's rows. One transaction-scoped advisory lock serializes the
-- sync, so a projection computed before a concurrent edit can never overwrite
-- a newer one (READ COMMITTED reads a fresh snapshot once the lock is granted)
-- and a deleted source's document cannot be reinserted.
--
-- The sync triggers below are deferred constraint triggers: they run at
-- commit, after the transaction has taken every source row lock it will ever
-- take. The lock is therefore never held while its holder still has to lock
-- another event or venue row, which is what would let a multi-event
-- transaction deadlock with a concurrent single-event writer.
CREATE OR REPLACE FUNCTION directory_search_sync_lock()
RETURNS VOID
LANGUAGE sql
AS $$
  SELECT pg_advisory_xact_lock(hashtextextended('directory_event_venue_search_sync', 0));
$$;

CREATE OR REPLACE FUNCTION directory_sync_venue_search(target_venue_id BIGINT)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  IF target_venue_id IS NULL THEN
    RETURN;
  END IF;
  -- The statement below reads its sources after any concurrent sync committed.
  PERFORM directory_search_sync_lock();
  IF NOT EXISTS (SELECT 1 FROM directory_public_venue WHERE id = target_venue_id) THEN
    -- No public event left: the cached document is kept but marked, so the
    -- venue becoming listable again is a visible change (see the event sync).
    UPDATE directory_search_document SET source_status = 'paused'
    WHERE entity_kind = 'venue' AND entity_id = target_venue_id::text AND source_status = 'published';
    RETURN;
  END IF;
  INSERT INTO directory_search_document (
    entity_kind, entity_id, slug, title, subtitle, city_id, city_name, country_code,
    public_latitude, public_longitude, location_precision, search_text, search_vector,
    source_status, visibility, moderation_status, source_updated_at, source_version, sponsored
  )
  SELECT DISTINCT ON (venue.id)
    'venue', venue.id::text, 'venue-' || venue.id::text, venue.name, venue.city_name,
    venue.city_id, venue.city_name, venue.country_code, venue.public_latitude, venue.public_longitude,
    'city',
    directory_normalize_text(concat_ws(' ', venue.name, venue.city_name, 'venue venues local locales')),
    to_tsvector('simple', directory_normalize_text(concat_ws(' ', venue.name, venue.city_name, 'venue venues local locales'))),
    'published', 'public', 'allowed', venue.updated_at, 1, FALSE
  FROM directory_public_venue venue
  WHERE venue.id = target_venue_id
  ON CONFLICT (entity_kind, entity_id) DO UPDATE SET
    slug = EXCLUDED.slug, title = EXCLUDED.title, subtitle = EXCLUDED.subtitle,
    city_id = EXCLUDED.city_id, city_name = EXCLUDED.city_name, country_code = EXCLUDED.country_code,
    public_latitude = EXCLUDED.public_latitude, public_longitude = EXCLUDED.public_longitude,
    location_precision = EXCLUDED.location_precision, search_text = EXCLUDED.search_text,
    search_vector = EXCLUDED.search_vector, source_status = EXCLUDED.source_status,
    visibility = EXCLUDED.visibility, moderation_status = EXCLUDED.moderation_status,
    source_updated_at = EXCLUDED.source_updated_at,
    source_version = directory_search_document.source_version + 1
  -- Saved-search alerts fire on any write of source_version, so this update
  -- runs only when a projected field changes: a rebuild, or an edit to
  -- something search does not show (capacity, price), does not notify.
  WHERE (directory_search_document.slug, directory_search_document.title,
         directory_search_document.subtitle, directory_search_document.city_id,
         directory_search_document.city_name, directory_search_document.country_code,
         directory_search_document.public_latitude, directory_search_document.public_longitude,
         directory_search_document.location_precision, directory_search_document.search_text,
         directory_search_document.source_status, directory_search_document.visibility,
         directory_search_document.moderation_status)
    IS DISTINCT FROM
        (EXCLUDED.slug, EXCLUDED.title, EXCLUDED.subtitle, EXCLUDED.city_id,
         EXCLUDED.city_name, EXCLUDED.country_code, EXCLUDED.public_latitude,
         EXCLUDED.public_longitude, EXCLUDED.location_precision, EXCLUDED.search_text,
         EXCLUDED.source_status, EXCLUDED.visibility, EXCLUDED.moderation_status);
  -- The source timestamp is recorded separately, without touching the
  -- columns the alert trigger watches.
  UPDATE directory_search_document document
     SET source_updated_at = source.updated_at
    FROM (SELECT max(updated_at) AS updated_at FROM directory_public_venue WHERE id = target_venue_id) source
   WHERE document.entity_kind = 'venue' AND document.entity_id = target_venue_id::text
     AND document.source_updated_at IS DISTINCT FROM source.updated_at;
END;
$$;

CREATE OR REPLACE FUNCTION directory_sync_event_search(target_event_id BIGINT)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  IF target_event_id IS NULL THEN
    RETURN;
  END IF;
  -- The statement below reads its sources after any concurrent sync committed.
  PERFORM directory_search_sync_lock();
  -- A hidden event keeps its cached document (the privacy projection never
  -- destroys cached data, and directory_public_search_document re-checks
  -- directory_public_event when reading). It is marked as not published, so
  -- that publishing it again is a change to an alert-watched column and
  -- saved searches created in the meantime are notified. Only source
  -- deletion removes the document.
  IF NOT EXISTS (SELECT 1 FROM directory_public_event WHERE id = target_event_id) THEN
    UPDATE directory_search_document SET source_status = 'paused'
    WHERE entity_kind = 'event' AND entity_id = target_event_id::text AND source_status = 'published';
    RETURN;
  END IF;
  INSERT INTO directory_search_document (
    entity_kind, entity_id, slug, title, subtitle, summary, image_url, city_id, city_name,
    country_code, public_latitude, public_longitude, location_precision, genre_ids, search_text,
    search_vector, source_status, visibility, moderation_status, effective_at, expires_at,
    source_updated_at, source_version, sponsored
  )
  SELECT
    'event', event.id::text, 'evento-' || event.id::text, event.title, event.venue_name,
    event.description, directory_social_event_metadata_image(source.metadata),
    event.city_id, event.city_name, event.country_code, event.public_latitude,
    event.public_longitude, 'city',
    (SELECT coalesce(array_agg(DISTINCT lineup_genre ORDER BY lineup_genre), '{}'::uuid[])
       FROM unnest(lineup.genre_ids || directory_social_event_genre_ids(lineup.genre_terms)) AS lineup_genre),
    directory_normalize_text(concat_ws(' ', event.title, event.description, event.venue_name, event.city_name, array_to_string(lineup.artist_names, ' '), array_to_string(lineup.genre_terms, ' '), 'evento eventos')),
    to_tsvector('simple', directory_normalize_text(concat_ws(' ', event.title, event.description, event.venue_name, event.city_name, array_to_string(lineup.artist_names, ' '), array_to_string(lineup.genre_terms, ' '), 'evento eventos'))),
    'published', 'public', 'allowed', event.start_time,
    -- An event without an end time is over a day after it starts; without
    -- this it would stay listed forever.
    coalesce(event.end_time, event.start_time + interval '1 day'),
    event.updated_at, 1, FALSE
  FROM directory_public_event event
  JOIN social_event source ON source.id = event.id
  CROSS JOIN LATERAL directory_social_event_lineup_terms(event.id) lineup
  WHERE event.id = target_event_id
  ON CONFLICT (entity_kind, entity_id) DO UPDATE SET
    slug = EXCLUDED.slug, title = EXCLUDED.title, subtitle = EXCLUDED.subtitle,
    summary = EXCLUDED.summary, image_url = EXCLUDED.image_url, city_id = EXCLUDED.city_id,
    city_name = EXCLUDED.city_name, country_code = EXCLUDED.country_code,
    public_latitude = EXCLUDED.public_latitude, public_longitude = EXCLUDED.public_longitude,
    location_precision = EXCLUDED.location_precision, genre_ids = EXCLUDED.genre_ids,
    search_text = EXCLUDED.search_text,
    search_vector = EXCLUDED.search_vector, source_status = EXCLUDED.source_status,
    visibility = EXCLUDED.visibility, moderation_status = EXCLUDED.moderation_status,
    effective_at = EXCLUDED.effective_at, expires_at = EXCLUDED.expires_at,
    source_updated_at = EXCLUDED.source_updated_at,
    source_version = directory_search_document.source_version + 1
  -- Saved-search alerts fire on any write of source_version, so this update
  -- runs only when a projected field changes: a rebuild, or an edit to
  -- something search does not show (capacity, price), does not notify.
  WHERE (directory_search_document.slug, directory_search_document.title,
         directory_search_document.subtitle, directory_search_document.summary,
         directory_search_document.image_url, directory_search_document.city_id,
         directory_search_document.city_name, directory_search_document.country_code,
         directory_search_document.public_latitude, directory_search_document.public_longitude,
         directory_search_document.location_precision, directory_search_document.genre_ids,
         directory_search_document.search_text,
         directory_search_document.source_status, directory_search_document.visibility,
         directory_search_document.moderation_status, directory_search_document.effective_at,
         directory_search_document.expires_at)
    IS DISTINCT FROM
        (EXCLUDED.slug, EXCLUDED.title, EXCLUDED.subtitle, EXCLUDED.summary,
         EXCLUDED.image_url, EXCLUDED.city_id, EXCLUDED.city_name, EXCLUDED.country_code,
         EXCLUDED.public_latitude, EXCLUDED.public_longitude, EXCLUDED.location_precision,
         EXCLUDED.genre_ids, EXCLUDED.search_text, EXCLUDED.source_status, EXCLUDED.visibility,
         EXCLUDED.moderation_status, EXCLUDED.effective_at, EXCLUDED.expires_at);
  -- The source timestamp is recorded separately, without touching the
  -- columns the alert trigger watches.
  UPDATE directory_search_document document
     SET source_updated_at = source.updated_at
    FROM (SELECT max(updated_at) AS updated_at FROM directory_public_event WHERE id = target_event_id) source
   WHERE document.entity_kind = 'event' AND document.entity_id = target_event_id::text
     AND document.source_updated_at IS DISTINCT FROM source.updated_at;
END;
$$;

CREATE OR REPLACE FUNCTION directory_sync_social_event_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    -- Serialized with the sync functions, so a concurrent sync that already
    -- read this source cannot reinsert its document after the delete.
    PERFORM directory_search_sync_lock();
    DELETE FROM directory_search_document
    WHERE entity_kind = 'event' AND entity_id = OLD.id::text;
    -- Its venue may have lost its last public event.
    PERFORM directory_sync_venue_search(OLD.venue_id);
    RETURN NULL;
  END IF;
  IF TG_OP = 'UPDATE' AND NEW.venue_id IS DISTINCT FROM OLD.venue_id THEN
    PERFORM directory_sync_venue_search(OLD.venue_id);
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') THEN
    PERFORM directory_sync_event_search(NEW.id);
    -- Any update can make the event public (publishing changes only
    -- workflow_state_id), which is what makes its venue listable.
    PERFORM directory_sync_venue_search(NEW.venue_id);
  END IF;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_social_event_search ON social_event;
CREATE CONSTRAINT TRIGGER trg_directory_sync_social_event_search
  AFTER INSERT OR UPDATE OR DELETE ON social_event
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_social_event_search_trigger();

CREATE OR REPLACE FUNCTION directory_sync_venue_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  venue_event_id BIGINT;
BEGIN
  IF TG_OP = 'DELETE' THEN
    -- Serialized with the sync functions, so a concurrent sync that already
    -- read this source cannot reinsert its document after the delete.
    PERFORM directory_search_sync_lock();
    DELETE FROM directory_search_document
    WHERE entity_kind = 'venue' AND entity_id = OLD.id::text;
    RETURN NULL;
  END IF;
  -- Locked before listing the venue's events, so an event that a concurrent
  -- transaction just added to this venue is not missed.
  PERFORM directory_search_sync_lock();
  FOR venue_event_id IN SELECT id FROM social_event WHERE venue_id = NEW.id ORDER BY id LOOP
    PERFORM directory_sync_event_search(venue_event_id);
  END LOOP;
  PERFORM directory_sync_venue_search(NEW.id);
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_venue_search ON venue;
CREATE CONSTRAINT TRIGGER trg_directory_sync_venue_search
  AFTER UPDATE OR DELETE ON venue
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_venue_search_trigger();

-- Provider suppression hides an imported event; resync keeps it hidden and
-- reindexes it if the suppression is lifted.
CREATE OR REPLACE FUNCTION directory_sync_external_event_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  -- Suppression changes whether the event is public, and with it whether its
  -- venue is listable, so both documents are synced.
  IF TG_OP IN ('UPDATE', 'DELETE') THEN
    PERFORM directory_sync_event_search(OLD.event_id);
    PERFORM directory_sync_venue_search((SELECT venue_id FROM social_event WHERE id = OLD.event_id));
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') AND NEW.event_id IS DISTINCT FROM OLD.event_id THEN
    PERFORM directory_sync_event_search(NEW.event_id);
    PERFORM directory_sync_venue_search((SELECT venue_id FROM social_event WHERE id = NEW.event_id));
  END IF;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_external_event_search ON external_event_ref;
CREATE CONSTRAINT TRIGGER trg_directory_sync_external_event_search
  AFTER INSERT OR UPDATE OR DELETE ON external_event_ref
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_external_event_search_trigger();

-- The lineup feeds the event's genre ids and search text, so a change to the
-- bill or to an artist's genres or name resyncs the artist's events.
CREATE OR REPLACE FUNCTION directory_sync_event_lineup_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP IN ('UPDATE', 'DELETE') THEN
    PERFORM directory_sync_event_search(OLD.event_id);
  END IF;
  IF TG_OP IN ('INSERT', 'UPDATE') AND (TG_OP = 'INSERT' OR NEW.event_id IS DISTINCT FROM OLD.event_id) THEN
    PERFORM directory_sync_event_search(NEW.event_id);
  END IF;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_event_lineup_search ON event_artist;
CREATE CONSTRAINT TRIGGER trg_directory_sync_event_lineup_search
  AFTER INSERT OR UPDATE OR DELETE ON event_artist
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_event_lineup_search_trigger();

CREATE OR REPLACE FUNCTION directory_sync_artist_events_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  artist_event_id BIGINT;
BEGIN
  FOR artist_event_id IN
    SELECT lineup.event_id FROM event_artist lineup WHERE lineup.artist_id = NEW.id ORDER BY lineup.event_id
  LOOP
    PERFORM directory_sync_event_search(artist_event_id);
  END LOOP;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_artist_events_search ON social_artist_profile;
CREATE CONSTRAINT TRIGGER trg_directory_sync_artist_events_search
  AFTER UPDATE OF genres, name ON social_artist_profile
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_artist_events_search_trigger();

-- The canonical genre memberships (written by the artist API) feed the same
-- projection, so a membership change resyncs the artist's events.
CREATE OR REPLACE FUNCTION directory_sync_artist_genre_events_search_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  artist_event_id BIGINT;
BEGIN
  FOR artist_event_id IN
    SELECT DISTINCT lineup.event_id FROM event_artist lineup
    WHERE lineup.artist_id IN (
      CASE WHEN TG_OP IN ('UPDATE', 'DELETE') THEN OLD.artist_id END,
      CASE WHEN TG_OP IN ('INSERT', 'UPDATE') THEN NEW.artist_id END
    )
    ORDER BY lineup.event_id
  LOOP
    PERFORM directory_sync_event_search(artist_event_id);
  END LOOP;
  RETURN NULL;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_sync_artist_genre_events_search ON artist_genre_membership;
CREATE CONSTRAINT TRIGGER trg_directory_sync_artist_genre_events_search
  AFTER INSERT OR UPDATE OR DELETE ON artist_genre_membership
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION directory_sync_artist_genre_events_search_trigger();

-- The full refresh now reuses the same projection, so a manual rebuild and the
-- incremental triggers can never disagree.
CREATE OR REPLACE FUNCTION directory_refresh_legacy_event_search()
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  target BIGINT;
BEGIN
  -- Taken before the first document write, the order every sync uses, so a
  -- standalone recovery refresh cannot deadlock with a concurrent sync.
  PERFORM directory_search_sync_lock();
  -- Documents of events that left the public projection stay cached and are
  -- hidden at read time; documents whose source row is gone are removed.
  DELETE FROM directory_search_document document
  WHERE document.entity_kind = 'event'
    AND NOT EXISTS (SELECT 1 FROM social_event event WHERE event.id::text = document.entity_id);
  DELETE FROM directory_search_document document
  WHERE document.entity_kind = 'venue'
    AND NOT EXISTS (SELECT 1 FROM venue WHERE venue.id::text = document.entity_id);
  -- Cached documents of sources that are not public are marked, as the
  -- incremental sync does.
  UPDATE directory_search_document document SET source_status = 'paused'
  WHERE document.source_status = 'published'
    AND ((document.entity_kind = 'event'
          AND NOT EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id::text = document.entity_id))
      OR (document.entity_kind = 'venue'
          AND NOT EXISTS (SELECT 1 FROM directory_public_venue venue WHERE venue.id::text = document.entity_id)));
  FOR target IN SELECT id FROM directory_public_event LOOP
    PERFORM directory_sync_event_search(target);
  END LOOP;
  FOR target IN SELECT DISTINCT id FROM directory_public_venue LOOP
    PERFORM directory_sync_venue_search(target);
  END LOOP;
END;
$$;

-- Operator entry point for a historical backfill or a recovery reindex. It
-- runs the same full refresh and reports what changed, so a rerun can be
-- audited: unchanged documents keep their version (no saved-search alert),
-- an event never gets a second document (the primary key is the canonical
-- id), and events whose venue city could not be resolved are counted rather
-- than guessed. Run it as
--   SELECT * FROM directory_event_search_reindex();
-- and, outside a migration, disable directory_search_alert_trigger first if
-- the indexed events must not notify saved searches.
CREATE OR REPLACE FUNCTION directory_event_search_reindex()
RETURNS TABLE (
  events_examined BIGINT,
  events_eligible BIGINT,
  events_skipped_not_public BIGINT,
  venues_eligible BIGINT,
  documents_inserted BIGINT,
  documents_updated BIGINT,
  documents_unchanged BIGINT,
  documents_paused BIGINT,
  documents_deleted BIGINT,
  events_unresolved_city BIGINT
)
LANGUAGE plpgsql
AS $$
BEGIN
  PERFORM directory_search_sync_lock();
  CREATE TEMP TABLE directory_event_search_reindex_before ON COMMIT DROP AS
    SELECT document.entity_kind, document.entity_id, document.source_version, document.source_status
    FROM directory_search_document document
    WHERE document.entity_kind IN ('event', 'venue');
  PERFORM directory_refresh_legacy_event_search();
  RETURN QUERY
  SELECT
    (SELECT count(*) FROM social_event),
    (SELECT count(*) FROM directory_public_event),
    (SELECT count(*) FROM social_event) - (SELECT count(*) FROM directory_public_event),
    (SELECT count(*) FROM directory_public_venue),
    (SELECT count(*) FROM directory_search_document document
      WHERE document.entity_kind IN ('event', 'venue')
        AND NOT EXISTS (SELECT 1 FROM directory_event_search_reindex_before before
                        WHERE before.entity_kind = document.entity_kind AND before.entity_id = document.entity_id)),
    (SELECT count(*) FROM directory_search_document document
      JOIN directory_event_search_reindex_before before
        ON before.entity_kind = document.entity_kind AND before.entity_id = document.entity_id
      WHERE document.source_version > before.source_version),
    (SELECT count(*) FROM directory_search_document document
      JOIN directory_event_search_reindex_before before
        ON before.entity_kind = document.entity_kind AND before.entity_id = document.entity_id
      WHERE document.source_version = before.source_version
        AND document.source_status = before.source_status),
    (SELECT count(*) FROM directory_search_document document
      JOIN directory_event_search_reindex_before before
        ON before.entity_kind = document.entity_kind AND before.entity_id = document.entity_id
      WHERE document.source_status = 'paused' AND before.source_status = 'published'),
    (SELECT count(*) FROM directory_event_search_reindex_before before
      WHERE NOT EXISTS (SELECT 1 FROM directory_search_document document
                        WHERE document.entity_kind = before.entity_kind AND document.entity_id = before.entity_id)),
    (SELECT count(*) FROM directory_public_event event WHERE event.city_id IS NULL);
  DROP TABLE directory_event_search_reindex_before;
END;
$$;

-- Upcoming events were hidden by the effective_at check (it holds their start
-- time). Same view as 2026-09-09 except for that one condition.
CREATE OR REPLACE VIEW directory_public_search_document AS
SELECT
  document.entity_kind,
  document.entity_id,
  document.slug,
  document.title,
  document.subtitle,
  document.summary,
  document.image_url,
  document.city_id,
  document.city_name,
  document.country_code,
  document.public_latitude,
  document.public_longitude,
  document.location_precision,
  document.profession_ids,
  document.service_ids,
  document.instrument_ids,
  document.genre_ids,
  document.search_text,
  document.search_vector,
  document.profile_completeness,
  document.reputation_score,
  document.availability_score,
  document.effective_at,
  document.expires_at,
  document.source_updated_at,
  document.source_version,
  document.sponsored,
  document.sponsor_disclosure,
  document.onsite,
  document.remote,
  document.available_to_travel
FROM directory_search_document document
WHERE document.source_status = 'published'
  AND document.visibility = 'public'
  AND document.moderation_status = 'allowed'
  -- Events use effective_at as their start time (date filters); upcoming
  -- events must stay listed, and their visibility is governed by
  -- directory_public_event below.
  AND (document.entity_kind = 'event' OR document.effective_at IS NULL OR document.effective_at <= now())
  AND (document.expires_at IS NULL OR document.expires_at > now())
  AND CASE document.entity_kind
    WHEN 'event' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_event event
        WHERE event.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    WHEN 'venue' THEN
      EXISTS (
        SELECT 1
        FROM directory_public_venue venue
        WHERE venue.id = CASE
          WHEN document.entity_id ~ '^[0-9]+$' THEN document.entity_id::bigint
          ELSE NULL
        END
      )
    ELSE TRUE
  END;


-- Backfill: resolve unambiguous free-text cities, then index current events.
-- Events that already existed are not new results, so saved-search alerts
-- stay off while their documents are written (until this transaction ends).
-- The sync lock comes first, the order every concurrent sync uses (advisory
-- lock, then the documents), so this cannot deadlock with a write that commits
-- while the migration runs.
SELECT directory_search_sync_lock();
ALTER TABLE directory_search_document DISABLE TRIGGER directory_search_alert_trigger;

UPDATE venue
SET city_id = directory_resolve_city_reference(city)
WHERE city_id IS NULL AND directory_resolve_city_reference(city) IS NOT NULL;

-- The reported counts are the backfill's audit record in the migration log.
SELECT * FROM directory_event_search_reindex();

ALTER TABLE directory_search_document ENABLE TRIGGER directory_search_alert_trigger;

COMMIT;
