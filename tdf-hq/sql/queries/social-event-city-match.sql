EXISTS (
  SELECT 1 FROM (SELECT directory_normalize_text(normalize(?, NFC)) AS needle) event_search_input
  WHERE event_search_input.needle <> '' AND (
    EXISTS (
      SELECT 1 FROM venue event_search_venue
      LEFT JOIN city_reference event_search_city
        ON event_search_city.id = event_search_venue.city_id
      WHERE event_search_venue.id = social_event.venue_id AND (
        (event_search_city.id IS NOT NULL AND (
          position(event_search_input.needle in directory_normalize_text(normalize(event_search_city.name_es, NFC))) > 0
          OR position(event_search_input.needle in directory_normalize_text(normalize(event_search_city.name_en, NFC))) > 0
          OR EXISTS (
            SELECT 1 FROM catalog_search_alias event_search_alias
            JOIN catalog_definition event_search_catalog
              ON event_search_catalog.id = event_search_alias.catalog_id
            WHERE event_search_catalog.code = 'cities'
              AND event_search_alias.entity_id = event_search_city.id
              AND directory_normalize_text(normalize(event_search_alias.term, NFC)) = event_search_input.needle
          )
        ))
        OR (event_search_city.id IS NULL
          AND position(event_search_input.needle in directory_normalize_text(normalize(event_search_venue.city, NFC))) > 0)
      )
    )
    OR (
      NOT EXISTS (
        SELECT 1 FROM venue event_search_venue
        WHERE event_search_venue.id = social_event.venue_id
          AND (event_search_venue.city_id IS NOT NULL
            OR NULLIF(trim(event_search_venue.city), '') IS NOT NULL)
      )
      AND EXISTS (
        SELECT 1 FROM external_event_ref event_search_ref
        WHERE event_search_ref.event_id = social_event.id
          AND lower(trim(event_search_ref.source_status)) <> 'suppressed'
          AND position(event_search_input.needle in directory_normalize_text(normalize(event_search_ref.city, NFC))) > 0
      )
    )
  )
)
