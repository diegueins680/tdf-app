-- Synthetic events for the /directory/search HTTP checks
-- (scripts/__tests__/directory-event-search-http.mjs): one public event in
-- Quito with a lineup whose artist plays the first catalog genre, one public
-- event in a venue whose city is not resolvable. Titles are unique so later
-- assertions in the same database are unaffected.
\set ON_ERROR_STOP on
DO $$
DECLARE
  quito UUID;
  public_state UUID;
  event_type UUID;
  quito_venue BIGINT;
  other_venue BIGINT;
  quito_event BIGINT;
  headliner BIGINT;
  genre_name TEXT;
BEGIN
  SELECT id INTO quito FROM city_reference WHERE code = 'quito-ec-p';
  IF quito IS NULL THEN RAISE EXCEPTION 'Quito city_reference seed is missing'; END IF;
  SELECT state.id INTO public_state
  FROM workflow_state state
  JOIN workflow_state_capability capability
    ON capability.state_id = state.id AND capability.capability_code = 'public-listable' AND capability.enabled
  WHERE state.active LIMIT 1;
  SELECT id INTO event_type FROM event_type ORDER BY sort_order, id LIMIT 1;
  SELECT name_es INTO genre_name FROM genre WHERE active ORDER BY sort_order, id LIMIT 1;

  INSERT INTO venue (name, city, created_at, updated_at)
  VALUES ('HTTP search Quito venue', 'Quito', now(), now())
  RETURNING id INTO quito_venue;
  INSERT INTO venue (name, city, created_at, updated_at)
  VALUES ('HTTP search unresolved venue', 'Synthetic unresolved town', now(), now())
  RETURNING id INTO other_venue;
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'HTTP search Quito synth night', 'Synthetic lineup', quito_venue, event_type,
          public_state, 'America/Guayaquil', now() + interval '2 days', now() + interval '2 days 4 hours',
          '{"isPublic": true, "imageUrl": "https://cdn.example.test/http-search-quito.jpg"}', now(), now())
  RETURNING id INTO quito_event;
  INSERT INTO social_event (organizer_party_id, title, description, venue_id, event_type_id,
                            workflow_state_id, timezone, start_time, end_time, metadata,
                            created_at, updated_at)
  VALUES (NULL, 'HTTP search unresolved city night', 'Synthetic lineup', other_venue, event_type,
          public_state, 'America/Guayaquil', now() + interval '2 days', now() + interval '2 days 4 hours',
          '{"isPublic": true}', now(), now());
  INSERT INTO social_artist_profile (name, genres, created_at, updated_at)
  VALUES ('HTTP search headliner', ARRAY[genre_name], now(), now())
  RETURNING id INTO headliner;
  INSERT INTO event_artist (event_id, artist_id, role) VALUES (quito_event, headliner, 'headliner');
END
$$;
