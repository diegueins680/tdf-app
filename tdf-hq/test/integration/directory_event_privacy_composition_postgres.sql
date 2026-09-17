-- Synthetic rollback-only checks against the migrated production schema.
BEGIN;
DO $$
DECLARE
  public_state_id UUID;
  public_event_type_id UUID;
  fixture_venue_id BIGINT;
  fixture_event_id BIGINT;
BEGIN
  SELECT state.id INTO STRICT public_state_id
  FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  JOIN workflow_state_capability capability ON capability.state_id=state.id
  WHERE workflow.code='social-event-lifecycle'
    AND workflow.active
    AND state.active
    AND capability.capability_code='public-listable'
    AND capability.enabled
  ORDER BY state.sort_order,state.id
  LIMIT 1;

  SELECT item.id INTO STRICT public_event_type_id
  FROM event_type item
  JOIN catalog_definition catalog
    ON catalog.id=item.catalog_id
   AND catalog.code='event-types'
   AND catalog.active
  JOIN workflow_state state
    ON state.id=item.workflow_state_id
   AND state.workflow_id=catalog.workflow_id
   AND state.code='published'
   AND state.active
  WHERE item.active
    AND item.deprecated_at IS NULL
    AND (item.effective_from IS NULL OR item.effective_from<=CURRENT_DATE)
    AND (item.effective_until IS NULL OR item.effective_until>=CURRENT_DATE)
  ORDER BY item.sort_order,item.id
  LIMIT 1;

  INSERT INTO venue(name,city,timezone,created_at,updated_at)
  VALUES ('Synthetic suppressed directory venue','Quito','America/Guayaquil',now(),now())
  RETURNING id INTO fixture_venue_id;

  INSERT INTO social_event(
    organizer_party_id,title,description,venue_id,event_type_id,workflow_state_id,timezone,
    start_time,end_time,metadata,created_at,updated_at
  )
  VALUES (
    NULL,'Synthetic suppressed directory event',
    'Synthetic public event used only to verify tombstone privacy.',
    fixture_venue_id,public_event_type_id,public_state_id,'America/Guayaquil',
    now()-interval '1 hour',now()+interval '7 days',
    '{"isPublic":true}',now(),now()
  )
  RETURNING id INTO fixture_event_id;

  INSERT INTO external_event_ref(
    provider,external_id,event_id,city,country_code,source_url,last_seen_at,
    missing_runs,source_status
  )
  VALUES (
    'synthetic-directory-provider','synthetic-directory-event',fixture_event_id,
    'Quito','EC','https://example.test/synthetic-directory-event',now(),0,'active'
  );

  PERFORM directory_refresh_legacy_event_search();
  IF NOT EXISTS (SELECT 1 FROM directory_public_event WHERE id=fixture_event_id) THEN
    RAISE EXCEPTION 'Public active event is missing';
  END IF;
  UPDATE social_event SET metadata='{"isPublic":false}' WHERE id=fixture_event_id;
  IF EXISTS (SELECT 1 FROM directory_public_event WHERE id=fixture_event_id)
    OR EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind='event' AND entity_id=fixture_event_id::text)
    OR EXISTS (SELECT 1 FROM directory_public_venue WHERE id=fixture_venue_id) THEN
    RAISE EXCEPTION 'Private metadata leaked through a public projection';
  END IF;
  UPDATE social_event SET metadata='{"isPublic":true}' WHERE id=fixture_event_id;
  UPDATE external_event_ref SET source_status=' Suppressed ' WHERE event_id=fixture_event_id;
  IF EXISTS (SELECT 1 FROM directory_public_event WHERE id=fixture_event_id)
    OR EXISTS (SELECT 1 FROM directory_public_search_document WHERE entity_kind='event' AND entity_id=fixture_event_id::text)
    OR EXISTS (SELECT 1 FROM directory_public_venue WHERE id=fixture_venue_id) THEN
    RAISE EXCEPTION 'Suppressed event leaked through a public projection';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM social_event WHERE id=fixture_event_id)
    OR NOT EXISTS (SELECT 1 FROM directory_search_document WHERE entity_kind='event' AND entity_id=fixture_event_id::text) THEN
    RAISE EXCEPTION 'Privacy projection destroyed source or cached data';
  END IF;
END
$$;
ROLLBACK;
