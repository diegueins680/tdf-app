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
    AND OLD.city_id IS NOT DISTINCT FROM directory_resolve_city_reference(OLD.city) THEN
    -- The id agreed with the previous city text and the caller changed only
    -- the text (the venue API never writes city_id), so the id follows the
    -- text. An id that was chosen independently of the text is kept.
    NEW.city_id := directory_resolve_city_reference(NEW.city);
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_directory_fill_venue_city_reference ON venue;
CREATE TRIGGER trg_directory_fill_venue_city_reference
  BEFORE INSERT OR UPDATE OF city, city_id ON venue
  FOR EACH ROW EXECUTE FUNCTION directory_fill_venue_city_reference();

-- Only HTTPS image URLs from valid event metadata are projected.
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
    WHEN (event_metadata::jsonb ->> 'imageUrl') ~ '^https://[^[:space:]@]+$' THEN event_metadata::jsonb ->> 'imageUrl'
  END;
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
    country_code, public_latitude, public_longitude, location_precision, search_text,
    search_vector, source_status, visibility, moderation_status, effective_at, expires_at,
    source_updated_at, source_version, sponsored
  )
  SELECT
    'event', event.id::text, 'evento-' || event.id::text, event.title, event.venue_name,
    event.description, directory_social_event_metadata_image(source.metadata),
    event.city_id, event.city_name, event.country_code, event.public_latitude,
    event.public_longitude, 'city',
    directory_normalize_text(concat_ws(' ', event.title, event.description, event.venue_name, event.city_name, 'evento eventos')),
    to_tsvector('simple', directory_normalize_text(concat_ws(' ', event.title, event.description, event.venue_name, event.city_name, 'evento eventos'))),
    'published', 'public', 'allowed', event.start_time, event.end_time, event.updated_at, 1, FALSE
  FROM directory_public_event event
  JOIN social_event source ON source.id = event.id
  WHERE event.id = target_event_id
  ON CONFLICT (entity_kind, entity_id) DO UPDATE SET
    slug = EXCLUDED.slug, title = EXCLUDED.title, subtitle = EXCLUDED.subtitle,
    summary = EXCLUDED.summary, image_url = EXCLUDED.image_url, city_id = EXCLUDED.city_id,
    city_name = EXCLUDED.city_name, country_code = EXCLUDED.country_code,
    public_latitude = EXCLUDED.public_latitude, public_longitude = EXCLUDED.public_longitude,
    location_precision = EXCLUDED.location_precision, search_text = EXCLUDED.search_text,
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
         directory_search_document.location_precision, directory_search_document.search_text,
         directory_search_document.source_status, directory_search_document.visibility,
         directory_search_document.moderation_status, directory_search_document.effective_at,
         directory_search_document.expires_at)
    IS DISTINCT FROM
        (EXCLUDED.slug, EXCLUDED.title, EXCLUDED.subtitle, EXCLUDED.summary,
         EXCLUDED.image_url, EXCLUDED.city_id, EXCLUDED.city_name, EXCLUDED.country_code,
         EXCLUDED.public_latitude, EXCLUDED.public_longitude, EXCLUDED.location_precision,
         EXCLUDED.search_text, EXCLUDED.source_status, EXCLUDED.visibility,
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
  FOR venue_event_id IN SELECT id FROM social_event WHERE venue_id = NEW.id LOOP
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

-- The full refresh now reuses the same projection, so a manual rebuild and the
-- incremental triggers can never disagree.
CREATE OR REPLACE FUNCTION directory_refresh_legacy_event_search()
RETURNS VOID
LANGUAGE plpgsql
AS $$
DECLARE
  target BIGINT;
BEGIN
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
ALTER TABLE directory_search_document DISABLE TRIGGER directory_search_alert_trigger;

UPDATE venue
SET city_id = directory_resolve_city_reference(city)
WHERE city_id IS NULL AND directory_resolve_city_reference(city) IS NOT NULL;

SELECT directory_refresh_legacy_event_search();

ALTER TABLE directory_search_document ENABLE TRIGGER directory_search_alert_trigger;

COMMIT;
