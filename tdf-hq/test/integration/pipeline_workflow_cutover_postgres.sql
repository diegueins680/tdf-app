\set ON_ERROR_STOP on
SELECT 'pipeline-workflow-postgres-integration-' || gen_random_uuid() AS run_code,
  'integration-pipeline-v1-20260812' AS candidate_revision
\gset
SELECT set_config('tdf.test_pipeline_run_code', :'run_code', FALSE);
\set safety_threshold 20
\set batch_size 2

CREATE TEMP TABLE pipeline_workflow_fixture (
  fixture_key text PRIMARY KEY,
  pipeline_card_id uuid NOT NULL,
  original_service_kind text NOT NULL,
  original_stage text NOT NULL,
  original_service_offering_id uuid NOT NULL,
  original_workflow_state_id uuid
) ON COMMIT PRESERVE ROWS;

ALTER TABLE pipeline_card DISABLE TRIGGER catalog_pipeline_card_integrity;

WITH fixtures(fixture_key, service_kind, service_code, stage) AS (
  VALUES
    ('mixing-brief', 'Mixing', 'mixing', 'Brief'),
    ('mixing-prep', 'Mixing', 'mixing', 'Prep'),
    ('mastering-v1', 'Mastering', 'mastering', 'v1'),
    ('mastering-approved', 'Mastering', 'mastering', 'Approved')
), inserted AS (
  INSERT INTO pipeline_card (
    service_kind, service_offering_id, title, artist, stage,
    workflow_state_id, sort_order, notes, created_at, updated_at
  )
  SELECT fixture.service_kind, service.id,
    'Pipeline cutover fixture ' || fixture.fixture_key, 'TDF test',
    fixture.stage, NULL, 10, 'integration fixture', now(), now()
  FROM fixtures fixture
  JOIN service_offering service ON service.code=fixture.service_code AND service.active
  RETURNING id, title, service_kind, stage, service_offering_id, workflow_state_id
)
INSERT INTO pipeline_workflow_fixture (
  fixture_key, pipeline_card_id, original_service_kind, original_stage,
  original_service_offering_id, original_workflow_state_id
)
SELECT replace(title, 'Pipeline cutover fixture ', ''), id,
  service_kind, stage, service_offering_id, workflow_state_id
FROM inserted;

ALTER TABLE pipeline_card ENABLE TRIGGER catalog_pipeline_card_integrity;

\ir ../../sql/2026-08-11_pipeline_workflow_cutover_dry_run.sql
\ir ../../sql/2026-08-11_pipeline_workflow_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_pipeline_run_code')
    AND candidate_revision='integration-pipeline-v1-20260812' AND NOT dry_run;
  IF (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value AND status='mapped')<>4 THEN
    RAISE EXCEPTION 'first apply did not preserve exactly four source/mapping evidence rows';
  END IF;
  BEGIN
    DELETE FROM catalog_pipeline_workflow_cutover_source
    WHERE run_id=run_id_value AND pipeline_card_id=(
      SELECT pipeline_card_id FROM catalog_pipeline_workflow_cutover_source
      WHERE run_id=run_id_value ORDER BY pipeline_card_id LIMIT 1
    );
    RAISE EXCEPTION 'immutable pipeline cutover source evidence was hard-deleted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN NULL;
  END;
  IF EXISTS (
    SELECT 1 FROM pipeline_workflow_fixture fixture
    JOIN pipeline_card card ON card.id=fixture.pipeline_card_id
    JOIN pipeline_workflow_binding binding ON binding.service_offering_id=card.service_offering_id AND binding.active
    JOIN workflow_state state ON state.id=card.workflow_state_id AND state.workflow_id=binding.workflow_id
    WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL
      OR state.code IS DISTINCT FROM CASE fixture.fixture_key
        WHEN 'mixing-brief' THEN 'brief'
        WHEN 'mixing-prep' THEN 'prep'
        WHEN 'mastering-v1' THEN 'v1'
        WHEN 'mastering-approved' THEN 'approved'
      END
  ) THEN RAISE EXCEPTION 'first apply produced an incorrect canonical pipeline relation'; END IF;
END
$assert_first_apply$;

CREATE TEMP TABLE pipeline_workflow_apply_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(string_agg(
  concat_ws('|', card.id::text, card.service_offering_id::text,
    card.workflow_state_id::text, card.service_kind, card.stage),
  ';' ORDER BY card.id
)) AS digest
FROM pipeline_workflow_fixture fixture
JOIN pipeline_card card ON card.id=fixture.pipeline_card_id;

\ir ../../sql/2026-08-11_pipeline_workflow_cutover_apply.sql

DO $assert_rerun$
DECLARE current_digest text; expected_digest text; run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_pipeline_run_code')
    AND candidate_revision='integration-pipeline-v1-20260812' AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM pipeline_workflow_apply_digest;
  SELECT md5(string_agg(
    concat_ws('|', card.id::text, card.service_offering_id::text,
      card.workflow_state_id::text, card.service_kind, card.stage),
    ';' ORDER BY card.id
  )) INTO current_digest
  FROM pipeline_workflow_fixture fixture
  JOIN pipeline_card card ON card.id=fixture.pipeline_card_id;
  IF current_digest IS DISTINCT FROM expected_digest THEN
    RAISE EXCEPTION 'idempotent rerun changed canonical pipeline rows';
  END IF;
  IF (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value)<>4 THEN
    RAISE EXCEPTION 'idempotent rerun duplicated pipeline source or mapping evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-11_pipeline_workflow_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM pipeline_workflow_fixture fixture
    JOIN pipeline_card card ON card.id=fixture.pipeline_card_id
    WHERE card.service_kind IS DISTINCT FROM fixture.original_service_kind
      OR card.stage IS DISTINCT FROM fixture.original_stage
      OR card.service_offering_id IS DISTINCT FROM fixture.original_service_offering_id
      OR card.workflow_state_id IS DISTINCT FROM fixture.original_workflow_state_id
  ) THEN RAISE EXCEPTION 'rollback did not restore exact original pipeline relations'; END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-11_pipeline_workflow_cutover_apply.sql

DO $assert_reapply$
DECLARE revision_before bigint; revision_after bigint;
BEGIN
  IF EXISTS (
    SELECT 1 FROM pipeline_workflow_fixture fixture
    JOIN pipeline_card card ON card.id=fixture.pipeline_card_id
    LEFT JOIN pipeline_workflow_binding binding
      ON binding.service_offering_id=card.service_offering_id AND binding.active
    LEFT JOIN workflow_state state
      ON state.id=card.workflow_state_id AND state.workflow_id=binding.workflow_id AND state.active
    WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL
      OR binding.id IS NULL OR state.id IS NULL
  ) THEN RAISE EXCEPTION 'reapply did not restore canonical pipeline references'; END IF;

  SELECT workflow.cache_revision INTO STRICT revision_before
  FROM workflow_definition workflow WHERE workflow.code='pipeline-mixing';
  UPDATE pipeline_card SET notes='revision invalidation check'
  WHERE id=(SELECT pipeline_card_id FROM pipeline_workflow_fixture WHERE fixture_key='mixing-brief');
  SELECT workflow.cache_revision INTO STRICT revision_after
  FROM workflow_definition workflow WHERE workflow.code='pipeline-mixing';
  IF revision_after<>revision_before+1 THEN
    RAISE EXCEPTION 'pipeline card mutation did not increment the affected snapshot revision';
  END IF;
  RAISE NOTICE 'pipeline workflow cutover dry-run/apply/rerun/rollback/reapply checks passed';
END
$assert_reapply$;

DELETE FROM pipeline_card card USING pipeline_workflow_fixture fixture
WHERE card.id=fixture.pipeline_card_id;
