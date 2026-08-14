\set ON_ERROR_STOP on
SELECT 'social-event-workflow-postgres-integration-' || gen_random_uuid() AS run_code,
  'integration-workflow-v1-20260812' AS candidate_revision
\gset
SELECT set_config('tdf.test_workflow_run_code', :'run_code', FALSE);
\set safety_threshold 20
\set batch_size 2

CREATE TEMP TABLE social_event_workflow_fixture (
  fixture_key text PRIMARY KEY,
  social_event_id bigint NOT NULL,
  original_metadata text,
  original_workflow_state_id uuid
) ON COMMIT PRESERVE ROWS;

ALTER TABLE social_event DISABLE TRIGGER social_event_workflow_state_integrity;

WITH event_type_value AS (
  SELECT item.id FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='event-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
  ORDER BY item.sort_order, item.id LIMIT 1
), fixtures(fixture_key, metadata) AS (
  VALUES
    ('missing', '{"fixture":"missing"}'::text),
    ('canceled-alias', '{"eventStatus":" canceled ","fixture":"canceled-alias"}'),
    ('punctuation-alias', '{"eventStatus":" On-Sale ","fixture":"punctuation-alias"}'),
    ('import-status', '{"eventStatus":"unavailable","fixture":"import-status"}')
), inserted AS (
  INSERT INTO social_event (
    title, event_type_id, workflow_state_id, start_time, end_time,
    metadata, created_at, updated_at
  )
  SELECT 'Workflow cutover fixture ' || fixture.fixture_key,
    event_type_value.id, NULL, now()+interval '3 days', now()+interval '3 days 2 hours',
    fixture.metadata, now(), now()
  FROM fixtures fixture CROSS JOIN event_type_value
  RETURNING id, title, metadata, workflow_state_id
)
INSERT INTO social_event_workflow_fixture (
  fixture_key, social_event_id, original_metadata, original_workflow_state_id
)
SELECT replace(title, 'Workflow cutover fixture ', ''), id, metadata, workflow_state_id
FROM inserted;

ALTER TABLE social_event ENABLE TRIGGER social_event_workflow_state_integrity;

\ir ../../sql/2026-08-11_social_event_workflow_cutover_dry_run.sql
\ir ../../sql/2026-08-11_social_event_workflow_cutover_apply.sql

DO $assert_first_apply$
DECLARE
  run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_workflow_run_code')
    AND candidate_revision='integration-workflow-v1-20260812' AND NOT dry_run;

  IF (SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value AND status='mapped')<>4 THEN
    RAISE EXCEPTION 'first apply did not preserve exactly four source/mapping evidence rows';
  END IF;
  BEGIN
    DELETE FROM catalog_social_event_workflow_cutover_source
    WHERE run_id=run_id_value AND social_event_id=(
      SELECT min(social_event_id) FROM catalog_social_event_workflow_cutover_source WHERE run_id=run_id_value
    );
    RAISE EXCEPTION 'immutable cutover source evidence was hard-deleted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN NULL;
  END;
  IF EXISTS (
    SELECT 1 FROM social_event_workflow_fixture fixture
    JOIN social_event event ON event.id=fixture.social_event_id
    JOIN workflow_state state ON state.id=event.workflow_state_id
    WHERE event.metadata::jsonb ? 'eventStatus'
      OR state.code IS DISTINCT FROM CASE fixture.fixture_key
        WHEN 'missing' THEN 'planning'
        WHEN 'canceled-alias' THEN 'cancelled'
        WHEN 'punctuation-alias' THEN 'on_sale'
        WHEN 'import-status' THEN 'unavailable'
      END
  ) THEN RAISE EXCEPTION 'first apply produced an incorrect state or retained legacy metadata'; END IF;
END
$assert_first_apply$;

CREATE TEMP TABLE social_event_workflow_apply_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(string_agg(
  concat_ws('|', event.id::text, event.workflow_state_id::text, event.metadata),
  ';' ORDER BY event.id
)) AS digest
FROM social_event_workflow_fixture fixture
JOIN social_event event ON event.id=fixture.social_event_id;

\ir ../../sql/2026-08-11_social_event_workflow_cutover_apply.sql

DO $assert_rerun$
DECLARE current_digest text; expected_digest text; run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_workflow_run_code')
    AND candidate_revision='integration-workflow-v1-20260812' AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM social_event_workflow_apply_digest;
  SELECT md5(string_agg(
    concat_ws('|', event.id::text, event.workflow_state_id::text, event.metadata),
    ';' ORDER BY event.id
  )) INTO current_digest
  FROM social_event_workflow_fixture fixture
  JOIN social_event event ON event.id=fixture.social_event_id;
  IF current_digest IS DISTINCT FROM expected_digest THEN
    RAISE EXCEPTION 'idempotent rerun changed canonical event rows';
  END IF;
  IF (SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value)<>4 THEN
    RAISE EXCEPTION 'idempotent rerun duplicated source or mapping evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-11_social_event_workflow_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM social_event_workflow_fixture fixture
    JOIN social_event event ON event.id=fixture.social_event_id
    WHERE event.workflow_state_id IS DISTINCT FROM fixture.original_workflow_state_id
      OR event.metadata IS DISTINCT FROM fixture.original_metadata
  ) THEN RAISE EXCEPTION 'rollback did not restore the exact original workflow UUID and metadata'; END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-11_social_event_workflow_cutover_apply.sql

DO $assert_reapply$
BEGIN
  IF EXISTS (
    SELECT 1 FROM social_event_workflow_fixture fixture
    JOIN social_event event ON event.id=fixture.social_event_id
    LEFT JOIN workflow_state state ON state.id=event.workflow_state_id
    WHERE state.id IS NULL OR event.metadata::jsonb ? 'eventStatus'
  ) THEN RAISE EXCEPTION 'reapply did not restore canonical workflow references'; END IF;
  RAISE NOTICE 'social-event workflow cutover dry-run/apply/rerun/rollback/reapply checks passed';
END
$assert_reapply$;

DELETE FROM social_event event USING social_event_workflow_fixture fixture
WHERE event.id=fixture.social_event_id;
