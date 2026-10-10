-- Directory search stays in sync with events and venues
-- (2026-10-09_directory_event_search_sync). Synthetic data only; rolls back.
\set ON_ERROR_STOP on
BEGIN;
-- The sync runs at commit; make it per-statement so each step can be asserted.
SET CONSTRAINTS ALL IMMEDIATE;
DO $$
DECLARE
  quito UUID;
  public_state UUID;
  event_type UUID;
  free_text_venue BIGINT;
  synth_event_id BIGINT;
  doc directory_search_document%ROWTYPE;
  other_city UUID;
  other_city_name TEXT;
  explicit_venue BIGINT;
  late_venue BIGINT;
  late_event_id BIGINT;
  versions_before BIGINT;
  subscriber BIGINT;
  late_search UUID;
BEGIN
  SELECT id INTO quito FROM city_reference
  WHERE directory_normalize_text(name_es) = directory_normalize_text('Quito')
  LIMIT 1;
  IF quito IS NULL THEN
    RAISE EXCEPTION 'Quito city_reference seed is missing';
  END IF;
  SELECT state.id INTO public_state
  FROM workflow_state state
  JOIN workflow_state_capability capability
    ON capability.state_id = state.id AND capability.capability_code = 'public-listable' AND capability.enabled
  WHERE state.active LIMIT 1;
  SELECT id INTO event_type FROM event_type ORDER BY sort_order, id LIMIT 1;

  -- 1. A venue created with free-text city gets the unambiguous city id.
  INSERT INTO venue (name, city, created_at, updated_at)
  VALUES ('Synthetic sync venue', 'quito', now(), now())
  RETURNING id INTO free_text_venue;
  IF (SELECT city_id FROM venue WHERE id = free_text_venue) IS DISTINCT FROM quito THEN
    RAISE EXCEPTION 'free-text city was not resolved to city_reference';
  END IF;

  -- 2. A newly created public event is searchable at once, with city, image and kind words.
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'Synthetic Arabian sync night', 'Synthetic lineup', free_text_venue, event_type,
          public_state, 'America/Guayaquil', now() + interval '1 day', now() + interval '2 days',
          '{"isPublic": true, "imageUrl": "https://api.example.test/flyer.jpg"}', now(), now())
  RETURNING id INTO synth_event_id;
  SELECT * INTO doc FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text;
  IF doc.entity_id IS NULL THEN RAISE EXCEPTION 'new public event was not indexed'; END IF;
  IF doc.city_id IS DISTINCT FROM quito THEN RAISE EXCEPTION 'event document lacks the city id'; END IF;
  IF doc.image_url IS DISTINCT FROM 'https://api.example.test/flyer.jpg' THEN RAISE EXCEPTION 'event image not projected'; END IF;
  IF NOT doc.search_vector @@ plainto_tsquery('simple', directory_normalize_text('Eventos')) THEN
    RAISE EXCEPTION 'searching "Eventos" does not match the event';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document
                 WHERE entity_kind = 'event' AND entity_id = synth_event_id::text AND city_id = quito) THEN
    RAISE EXCEPTION 'event not visible through the public search view';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_search_document
                 WHERE entity_kind = 'venue' AND entity_id = free_text_venue::text AND city_id = quito) THEN
    RAISE EXCEPTION 'venue of the public event was not indexed';
  END IF;

  -- 3. Edits propagate.
  UPDATE social_event SET title = 'Synthetic renamed sync night', updated_at = now() WHERE id = synth_event_id;
  IF (SELECT title FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text)
     IS DISTINCT FROM 'Synthetic renamed sync night' THEN
    RAISE EXCEPTION 'event edit did not propagate';
  END IF;

  -- 3a. An edit to a field search does not show keeps the version (no new
  -- saved-search alert) while the document still records the newer source.
  SELECT * INTO doc FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text;
  -- A saved search created after the event was indexed has no delivery yet;
  -- an invisible edit must not produce one.
  INSERT INTO party (display_name, is_org, created_at) VALUES ('Synthetic sync subscriber', FALSE, now())
  RETURNING id INTO subscriber;
  INSERT INTO directory_saved_search (account_party_id, name, canonical_query, query_hash, alerts_enabled, alert_frequency)
  VALUES (subscriber, 'Synthetic sync alert', '{"entityType":"event"}'::jsonb, repeat('d', 64), TRUE, 'instant')
  RETURNING id INTO late_search;
  UPDATE social_event SET capacity = 321, updated_at = now() + interval '1 minute' WHERE id = synth_event_id;
  IF (SELECT source_version FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text)
     IS DISTINCT FROM doc.source_version THEN
    RAISE EXCEPTION 'an edit that search does not show advanced the alert version';
  END IF;
  IF (SELECT source_updated_at FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text)
     IS NOT DISTINCT FROM doc.source_updated_at THEN
    RAISE EXCEPTION 'the document did not record the newer source timestamp';
  END IF;
  IF EXISTS (SELECT 1 FROM directory_alert_delivery WHERE saved_search_id = late_search) THEN
    RAISE EXCEPTION 'an edit that search does not show notified a saved search';
  END IF;
  UPDATE social_event SET title = 'Synthetic renamed sync night II', updated_at = now() + interval '2 minutes' WHERE id = synth_event_id;
  IF (SELECT source_version FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text)
     IS DISTINCT FROM doc.source_version + 1 THEN
    RAISE EXCEPTION 'a visible edit did not advance the version exactly once';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_alert_delivery WHERE saved_search_id = late_search
                 AND result_kind = 'event' AND result_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'a visible edit did not notify the matching saved search';
  END IF;

  -- 4. Non-HTTPS images are never projected.
  UPDATE social_event SET metadata = '{"isPublic": true, "imageUrl": "javascript:alert(1)"}' WHERE id = synth_event_id;
  IF (SELECT image_url FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) IS NOT NULL THEN
    RAISE EXCEPTION 'unsafe image URL was projected';
  END IF;

  -- 5. Making the event private hides it and its now-empty venue (cache kept).
  UPDATE social_event SET metadata = '{"isPublic": false}' WHERE id = synth_event_id;
  IF EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'private event is publicly searchable';
  END IF;
  IF EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind = 'venue' AND entity_id = free_text_venue::text) THEN
    RAISE EXCEPTION 'venue without public events is publicly searchable';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'hiding an event destroyed its cached document';
  END IF;

  -- 6. Republishing reindexes it.
  UPDATE social_event SET metadata = '{"isPublic": true}' WHERE id = synth_event_id;
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'republished event is not publicly searchable';
  END IF;

  -- 7. Provider suppression hides the document; lifting it shows it again.
  INSERT INTO external_event_ref (provider, external_id, event_id, city, country_code, source_url,
                                  last_seen_at, missing_runs, source_status)
  VALUES ('synthetic-sync-provider', 'synthetic-sync-event', synth_event_id, 'Quito', 'EC',
          'https://example.test/synthetic-sync-event', now(), 0, 'active');
  UPDATE external_event_ref SET source_status = 'suppressed'
  WHERE provider = 'synthetic-sync-provider' AND external_id = 'synthetic-sync-event';
  IF EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'suppressed event is publicly searchable';
  END IF;
  DELETE FROM external_event_ref WHERE provider = 'synthetic-sync-provider';
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'unsuppressed event is not publicly searchable';
  END IF;

  -- 7a. Moving a venue to another city moves an inferred city id with it,
  -- so the venue and its events leave the old city's results.
  other_city_name := 'Ciudad Sintetica De Sincronizacion';
  INSERT INTO city_reference (country_id, code, name_es, name_en, source_name)
  SELECT country_id, 'SYNTHETIC-SYNC-CITY', other_city_name, other_city_name, 'synthetic-sync-test'
  FROM city_reference WHERE id = quito
  RETURNING id INTO other_city;
  UPDATE venue SET city = other_city_name WHERE id = free_text_venue;
  IF (SELECT city_id FROM venue WHERE id = free_text_venue) IS DISTINCT FROM other_city THEN
    RAISE EXCEPTION 'inferred city id did not follow the venue city';
  END IF;
  IF EXISTS (SELECT 1 FROM directory_public_search_document
             WHERE entity_kind = 'event' AND entity_id = synth_event_id::text AND city_id = quito) THEN
    RAISE EXCEPTION 'event stayed searchable under the venue''s previous city';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document
                 WHERE entity_kind = 'event' AND entity_id = synth_event_id::text AND city_id = other_city) THEN
    RAISE EXCEPTION 'event is not searchable under the venue''s new city';
  END IF;
  UPDATE venue SET city = 'Quito' WHERE id = free_text_venue;
  IF (SELECT city_id FROM venue WHERE id = free_text_venue) IS DISTINCT FROM quito THEN
    RAISE EXCEPTION 'inferred city id did not follow the venue back';
  END IF;
  -- A city id chosen independently of the text is never overwritten.
  INSERT INTO venue (name, city, city_id, created_at, updated_at)
  VALUES ('Synthetic explicit-city venue', 'Synthetic unmatched sector', quito, now(), now())
  RETURNING id INTO explicit_venue;
  UPDATE venue SET city = other_city_name WHERE id = explicit_venue;
  IF (SELECT city_id FROM venue WHERE id = explicit_venue) IS DISTINCT FROM quito THEN
    RAISE EXCEPTION 'explicitly selected city id was overwritten';
  END IF;

  -- 7b. An event created hidden and published later indexes its venue too.
  INSERT INTO venue (name, city, created_at, updated_at)
  VALUES ('Synthetic late venue', 'Quito', now(), now())
  RETURNING id INTO late_venue;
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'Synthetic late publication', 'Synthetic lineup', late_venue, event_type,
          public_state, 'America/Guayaquil', now() + interval '1 day', now() + interval '2 days',
          '{"isPublic": false}', now(), now())
  RETURNING id INTO late_event_id;
  IF EXISTS (SELECT 1 FROM directory_search_document WHERE entity_kind = 'venue' AND entity_id = late_venue::text) THEN
    RAISE EXCEPTION 'venue without public events was indexed';
  END IF;
  UPDATE social_event SET metadata = '{"isPublic": true}' WHERE id = late_event_id;
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document
                 WHERE entity_kind = 'venue' AND entity_id = late_venue::text AND city_id = quito) THEN
    RAISE EXCEPTION 'publishing an event did not index its venue';
  END IF;
  DELETE FROM social_event WHERE id = late_event_id;

  -- 8. Venue rename propagates to its events; event deletion removes the document.
  UPDATE venue SET name = 'Synthetic renamed venue' WHERE id = free_text_venue;
  IF (SELECT subtitle FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text)
     IS DISTINCT FROM 'Synthetic renamed venue' THEN
    RAISE EXCEPTION 'venue rename did not propagate to its events';
  END IF;
  DELETE FROM event_artist WHERE event_artist.event_id = synth_event_id;
  DELETE FROM social_event WHERE id = synth_event_id;
  IF EXISTS (SELECT 1 FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = synth_event_id::text) THEN
    RAISE EXCEPTION 'deleted event stayed indexed';
  END IF;

  -- 9. The full rebuild agrees with the incremental projection.
  PERFORM directory_refresh_legacy_event_search();
  IF EXISTS (
    SELECT 1 FROM directory_public_event event
    WHERE NOT EXISTS (SELECT 1 FROM directory_search_document document
                      WHERE document.entity_kind = 'event' AND document.entity_id = event.id::text)) THEN
    RAISE EXCEPTION 'full refresh left a public event unindexed';
  END IF;

  -- 10. A rebuild of unchanged sources keeps every version, so saved-search
  -- alerts (fired on source_version) are not sent again.
  SELECT coalesce(sum(source_version), 0) INTO versions_before
  FROM directory_search_document WHERE entity_kind IN ('event', 'venue');
  PERFORM directory_refresh_legacy_event_search();
  IF (SELECT coalesce(sum(source_version), 0) FROM directory_search_document
      WHERE entity_kind IN ('event', 'venue')) IS DISTINCT FROM versions_before THEN
    RAISE EXCEPTION 'rebuilding unchanged events or venues bumped their versions';
  END IF;
END
$$;
ROLLBACK;
\echo 'directory event search sync checks passed'
