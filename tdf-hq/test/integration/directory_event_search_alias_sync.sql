-- A genre search alias change resyncs the events it reaches
-- (2026-10-14_directory_event_search_alias_sync). Synthetic data only; rolls back.
\set ON_ERROR_STOP on
BEGIN;
-- The sync runs at commit; make it per-statement so each step can be asserted.
SET CONSTRAINTS ALL IMMEDIATE;
DO $$
DECLARE
  public_state UUID;
  event_type UUID;
  genres_catalog UUID;
  professions_catalog UUID;
  spanish UUID;
  first_genre UUID;
  second_genre UUID;
  alias_id UUID;
  alias_event_id BIGINT;
  other_event_id BIGINT;
  lineup_artist_id BIGINT;
  other_version BIGINT;
BEGIN
  SELECT state.id INTO public_state
  FROM workflow_state state
  JOIN workflow_state_capability capability
    ON capability.state_id = state.id AND capability.capability_code = 'public-listable' AND capability.enabled
  WHERE state.active LIMIT 1;
  SELECT id INTO event_type FROM event_type ORDER BY sort_order, id LIMIT 1;
  SELECT id INTO genres_catalog FROM catalog_definition WHERE code = 'genres';
  SELECT id INTO professions_catalog FROM catalog_definition WHERE code = 'professions';
  SELECT id INTO spanish FROM locale_reference WHERE code = 'es';
  SELECT id INTO first_genre FROM genre WHERE active ORDER BY sort_order, id LIMIT 1;
  SELECT id INTO second_genre FROM genre WHERE active AND id <> first_genre ORDER BY sort_order, id LIMIT 1;
  IF genres_catalog IS NULL OR spanish IS NULL OR second_genre IS NULL THEN
    RAISE EXCEPTION 'genre catalog or locale seed is missing';
  END IF;

  -- A lineup artist whose only genre is a legacy word no catalog name matches.
  INSERT INTO social_artist_profile (name, genres, created_at, updated_at)
  VALUES ('Synthetic Alias Headliner', ARRAY['Synthetic Alias Word'], now(), now())
  RETURNING id INTO lineup_artist_id;
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'Synthetic alias night', 'Synthetic lineup', NULL, event_type,
          public_state, 'America/Guayaquil', now() + interval '2 days', now() + interval '2 days 4 hours',
          '{"isPublic": true}', now(), now())
  RETURNING id INTO alias_event_id;
  INSERT INTO event_artist (event_id, artist_id, role) VALUES (alias_event_id, lineup_artist_id, 'headliner');
  -- An unrelated event, which no alias edit below may rewrite.
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'Synthetic unrelated night', 'Synthetic lineup', NULL, event_type,
          public_state, 'America/Guayaquil', now() + interval '3 days', now() + interval '3 days 4 hours',
          '{"isPublic": true}', now(), now())
  RETURNING id INTO other_event_id;
  SELECT source_version INTO other_version FROM directory_search_document
  WHERE entity_kind = 'event' AND entity_id = other_event_id::text;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) <> '{}'::uuid[] THEN
    RAISE EXCEPTION 'an unmapped legacy word carried a genre id';
  END IF;

  -- 1. Adding an alias that names the word maps the event to the genre.
  INSERT INTO catalog_search_alias (catalog_id, entity_kind, entity_id, locale_id, term, normalized_term, source)
  VALUES (genres_catalog, 'genre', first_genre, spanish, 'Synthetic Alias Word',
          directory_normalize_text('Synthetic Alias Word'), 'synthetic-test')
  RETURNING id INTO alias_id;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) IS DISTINCT FROM ARRAY[first_genre] THEN
    RAISE EXCEPTION 'adding a genre alias did not resync the legacy-genre event';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM directory_public_search_document
                 WHERE entity_kind = 'event' AND entity_id = alias_event_id::text AND first_genre = ANY(genre_ids)) THEN
    RAISE EXCEPTION 'genre filter does not find the event through the alias';
  END IF;

  -- 2. Moving the alias to another genre moves the event with it.
  UPDATE catalog_search_alias SET entity_id = second_genre WHERE id = alias_id;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) IS DISTINCT FROM ARRAY[second_genre] THEN
    RAISE EXCEPTION 'moving the alias to another genre did not resync the event';
  END IF;

  -- 3. Changing the alias away from the word drops the id.
  UPDATE catalog_search_alias SET term = 'Synthetic Other Word', normalized_term = directory_normalize_text('Synthetic Other Word')
  WHERE id = alias_id;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) <> '{}'::uuid[] THEN
    RAISE EXCEPTION 'changing the alias away from the word left its genre id on the event';
  END IF;

  -- 4. Changing it back towards the word restores the id.
  UPDATE catalog_search_alias SET term = 'Synthetic Alias Word', normalized_term = directory_normalize_text('Synthetic Alias Word')
  WHERE id = alias_id;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) IS DISTINCT FROM ARRAY[second_genre] THEN
    RAISE EXCEPTION 'changing the alias towards the word did not resync the event';
  END IF;

  -- 5. An alias of another catalog entity is ignored.
  IF professions_catalog IS NOT NULL THEN
    INSERT INTO catalog_search_alias (catalog_id, entity_kind, entity_id, locale_id, term, normalized_term, source)
    SELECT professions_catalog, 'profession', profession.id, spanish, 'Synthetic Alias Word',
           directory_normalize_text('Synthetic Alias Word') || ' profesion', 'synthetic-test'
    FROM profession WHERE active ORDER BY sort_order, id LIMIT 1;
  END IF;
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) IS DISTINCT FROM ARRAY[second_genre] THEN
    RAISE EXCEPTION 'a profession alias changed the event genres';
  END IF;

  -- 6. Unrelated events keep their version (no alert is sent for them).
  IF (SELECT source_version FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = other_event_id::text) IS DISTINCT FROM other_version THEN
    RAISE EXCEPTION 'an alias edit rewrote an unrelated event';
  END IF;

  -- 7. The full rebuild agrees with the incremental result.
  PERFORM directory_refresh_legacy_event_search();
  IF (SELECT genre_ids FROM directory_search_document WHERE entity_kind = 'event' AND entity_id = alias_event_id::text) IS DISTINCT FROM ARRAY[second_genre] THEN
    RAISE EXCEPTION 'full refresh disagrees with the alias resync';
  END IF;
END
$$;
ROLLBACK;
\echo 'directory event search alias sync checks passed'
