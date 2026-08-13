\set ON_ERROR_STOP on

-- Run after the normal backend migrations and catalog seed. All fixtures and
-- mutations remain inside this transaction and are rolled back.
BEGIN;

DO $social_event_workflow_test$
DECLARE
  workflow_id_value uuid;
  publication_workflow_id uuid;
  planning_state uuid;
  announced_state uuid;
  on_sale_state uuid;
  live_state uuid;
  completed_state uuid;
  published_state uuid;
  event_type_value uuid;
  fixture_event bigint;
  revision_before bigint;
  revision_after bigint;
  protected_failures integer := 0;
BEGIN
  SELECT id, cache_revision INTO STRICT workflow_id_value, revision_before
  FROM workflow_definition
  WHERE code='social-event-lifecycle' AND active AND public_read AND NOT sensitive;

  SELECT id INTO STRICT publication_workflow_id
  FROM workflow_definition WHERE code='catalog-publication' AND active;

  SELECT state.id INTO STRICT planning_state FROM workflow_state state
  WHERE state.workflow_id=workflow_id_value AND state.code='planning' AND state.active;
  SELECT state.id INTO STRICT announced_state FROM workflow_state state
  WHERE state.workflow_id=workflow_id_value AND state.code='announced' AND state.active;
  SELECT state.id INTO STRICT on_sale_state FROM workflow_state state
  WHERE state.workflow_id=workflow_id_value AND state.code='on_sale' AND state.active;
  SELECT state.id INTO STRICT live_state FROM workflow_state state
  WHERE state.workflow_id=workflow_id_value AND state.code='live' AND state.active;
  SELECT state.id INTO STRICT completed_state FROM workflow_state state
  WHERE state.workflow_id=workflow_id_value AND state.code='completed' AND state.active;
  SELECT state.id INTO STRICT published_state FROM workflow_state state
  WHERE state.workflow_id=publication_workflow_id AND state.code='published' AND state.active;

  SELECT item.id INTO STRICT event_type_value FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='event-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
  ORDER BY item.sort_order, item.id LIMIT 1;

  IF (SELECT count(*) FROM workflow_state WHERE workflow_id=workflow_id_value AND active)<>9 THEN
    RAISE EXCEPTION 'social event lifecycle must expose exactly nine active persisted states';
  END IF;
  IF (SELECT count(*) FROM workflow_default_state WHERE workflow_id=workflow_id_value AND context='initial' AND active)<>1 THEN
    RAISE EXCEPTION 'social event lifecycle must have exactly one active persisted initial state';
  END IF;

  INSERT INTO social_event (
    title, event_type_id, workflow_state_id, start_time, end_time,
    metadata, created_at, updated_at
  ) VALUES (
    'Social event workflow PostgreSQL fixture', event_type_value, planning_state,
    now()+interval '2 days', now()+interval '2 days 2 hours',
    '{"fixture":true}', now(), now()
  ) RETURNING id INTO fixture_event;

  UPDATE social_event SET workflow_state_id=announced_state WHERE id=fixture_event;

  BEGIN
    UPDATE social_event SET workflow_state_id=planning_state WHERE id=fixture_event;
    RAISE EXCEPTION 'an undeclared announced-to-planning direct transition was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%not allowed for direct execution%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  UPDATE social_event SET workflow_state_id=on_sale_state WHERE id=fixture_event;
  UPDATE social_event SET workflow_state_id=live_state WHERE id=fixture_event;
  UPDATE social_event SET workflow_state_id=completed_state WHERE id=fixture_event;

  BEGIN
    UPDATE social_event SET workflow_state_id=announced_state WHERE id=fixture_event;
    RAISE EXCEPTION 'a terminal completed state was reopened without a persisted transition';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%not allowed for direct execution%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  BEGIN
    UPDATE social_event SET metadata='{"eventStatus":"announced"}' WHERE id=fixture_event;
    RAISE EXCEPTION 'legacy metadata eventStatus was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%migration evidence only%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  BEGIN
    UPDATE social_event SET workflow_state_id=published_state WHERE id=fixture_event;
    RAISE EXCEPTION 'a state from another workflow was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%social-event-lifecycle%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  BEGIN
    UPDATE workflow_state SET code='completed-renamed' WHERE id=completed_state;
    RAISE EXCEPTION 'workflow-state code mutation was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%immutable%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  BEGIN
    UPDATE workflow_state SET active=FALSE WHERE id=completed_state;
    RAISE EXCEPTION 'referenced workflow-state deactivation was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%cannot be deactivated%' THEN RAISE; END IF;
      protected_failures := protected_failures+1;
  END;

  BEGIN
    UPDATE workflow_definition SET sensitive=TRUE WHERE id=workflow_id_value;
    RAISE EXCEPTION 'a public sensitive workflow was accepted';
  EXCEPTION
    WHEN check_violation THEN
      protected_failures := protected_failures+1;
  END;

  UPDATE workflow_state SET name_en='Completed integration label', version=version+1
  WHERE id=completed_state;
  SELECT cache_revision INTO STRICT revision_after FROM workflow_definition WHERE id=workflow_id_value;
  IF revision_after<=revision_before THEN
    RAISE EXCEPTION 'workflow behavior/label change did not invalidate cache: before=%, after=%',
      revision_before, revision_after;
  END IF;

  IF protected_failures<>7 THEN
    RAISE EXCEPTION 'expected seven protected negative checks, observed %', protected_failures;
  END IF;

  RAISE NOTICE 'social-event workflow PostgreSQL integration checks passed; cache revision % -> %',
    revision_before, revision_after;
END;
$social_event_workflow_test$;

ROLLBACK;
