\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'pipeline-workflow-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 10000
\endif
\if :{?batch_size}
\else
  \set batch_size 500
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-pipeline-workflow-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);
SELECT set_config('tdf.catalog_batch_size', :'batch_size', TRUE);

INSERT INTO catalog_backfill_run (
  id, run_code, candidate_revision, dry_run, status, safety_threshold,
  started_at, correlation_id
) VALUES (
  gen_random_uuid(), :'run_code', :'candidate_revision', FALSE, 'mapping',
  :safety_threshold, now(), :'run_code' || ':' || :'candidate_revision'
)
ON CONFLICT (run_code, candidate_revision, dry_run)
DO UPDATE SET status='mapping', safety_threshold=EXCLUDED.safety_threshold, completed_at=NULL;

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id', :'backfill_run_id', TRUE);

CREATE TABLE IF NOT EXISTS catalog_pipeline_workflow_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  pipeline_card_id uuid NOT NULL,
  original_service_kind text,
  original_stage text,
  normalized_stage text NOT NULL,
  original_service_offering_id uuid,
  original_workflow_state_id uuid,
  target_service_offering_id uuid NOT NULL,
  target_workflow_state_id uuid NOT NULL,
  workflow_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, pipeline_card_id)
);

DROP TRIGGER IF EXISTS catalog_no_hard_delete ON catalog_pipeline_workflow_cutover_source;
CREATE TRIGGER catalog_no_hard_delete
  BEFORE DELETE ON catalog_pipeline_workflow_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

CREATE TEMP TABLE resolved_pipeline_workflow ON COMMIT DROP AS
WITH source AS (
  SELECT card.id, card.service_kind::text AS original_service_kind,
    card.stage AS original_stage, card.service_offering_id AS original_service_offering_id,
    card.workflow_state_id AS original_workflow_state_id,
    COALESCE(card.service_offering_id, service.id) AS target_service_offering_id,
    CASE
      WHEN card.stage IS NULL THEN state.code
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='make-up-needed' THEN 'makeup-needed'
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='pre-prod' THEN 'pre-production'
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='post-prod' THEN 'post-production'
      ELSE lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))
    END AS normalized_stage
  FROM pipeline_card card
  LEFT JOIN service_offering service ON service.code=CASE card.service_kind::text
    WHEN 'Recording' THEN 'recording' WHEN 'Mixing' THEN 'mixing'
    WHEN 'Mastering' THEN 'mastering' WHEN 'Rehearsal' THEN 'rehearsal'
    WHEN 'Classes' THEN 'classes' WHEN 'EventProduction' THEN 'event-production' END
    AND service.active
  LEFT JOIN workflow_state state ON state.id=card.workflow_state_id
  WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL
    OR card.service_offering_id IS NULL OR card.workflow_state_id IS NULL
)
SELECT source.*, match.workflow_id, match.target_state_id, match.candidate_count
FROM source
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(binding.workflow_id ORDER BY binding.workflow_id))[1] AS workflow_id,
    (array_agg(state.id ORDER BY state.id))[1] AS target_state_id
  FROM pipeline_workflow_binding binding
  JOIN workflow_definition workflow ON workflow.id=binding.workflow_id
    AND workflow.active AND workflow.code LIKE 'pipeline-%'
  JOIN workflow_state state ON state.workflow_id=workflow.id
    AND state.active AND state.code=source.normalized_stage
  WHERE binding.service_offering_id=source.target_service_offering_id AND binding.active
) match ON TRUE;

DO $gate$
DECLARE source_rows bigint; invalid_rows bigint; workflow_count bigint;
  state_count bigint; binding_count bigint; initial_count bigint;
BEGIN
  SELECT count(*) INTO workflow_count FROM workflow_definition
    WHERE code LIKE 'pipeline-%' AND active AND NOT public_read AND NOT sensitive;
  SELECT count(*) INTO state_count FROM workflow_state state JOIN workflow_definition workflow
    ON workflow.id=state.workflow_id WHERE workflow.code LIKE 'pipeline-%'
    AND workflow.active AND state.active;
  SELECT count(*) INTO binding_count FROM pipeline_workflow_binding WHERE active;
  SELECT count(*) INTO initial_count FROM workflow_default_state default_state
    JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id
    JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id
    WHERE workflow.code LIKE 'pipeline-%' AND workflow.active
      AND default_state.context='initial' AND default_state.active AND state.active;
  SELECT count(*), count(*) FILTER (WHERE candidate_count<>1
    OR target_service_offering_id IS NULL OR target_state_id IS NULL
    OR (original_workflow_state_id IS NOT NULL AND original_workflow_state_id<>target_state_id))
  INTO source_rows, invalid_rows FROM resolved_pipeline_workflow;
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000
    OR source_rows > current_setting('tdf.catalog_safety_threshold')::bigint
    OR workflow_count<>6 OR state_count<>35 OR binding_count<>11
    OR initial_count<>6 OR invalid_rows<>0 THEN
    RAISE EXCEPTION 'pipeline workflow safety gate failed: rows=%, invalidOrAmbiguous=%, workflows=%, states=%, bindings=%, initialStates=%',
      source_rows, invalid_rows, workflow_count, state_count, binding_count, initial_count
      USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_pipeline_workflow_cutover_source (
  run_id, pipeline_card_id, original_service_kind, original_stage, normalized_stage,
  original_service_offering_id, original_workflow_state_id,
  target_service_offering_id, target_workflow_state_id, workflow_id, evidence
)
SELECT :'backfill_run_id'::uuid, id, original_service_kind, original_stage, normalized_stage,
  original_service_offering_id, original_workflow_state_id,
  target_service_offering_id, target_state_id, workflow_id,
  'explicit service-offering binding plus one normalized state code in its active persisted workflow'
FROM resolved_pipeline_workflow
ON CONFLICT (run_id, pipeline_card_id) DO NOTHING;

INSERT INTO workflow_migration_mapping (
  id, run_id, workflow_id, source_table, source_column, source_record_id,
  original_value, normalized_value, state_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, source.workflow_id, 'pipeline_card',
  'stage', source.pipeline_card_id::text, COALESCE(source.original_stage, '<missing>'),
  source.normalized_stage, source.target_workflow_state_id, 'mapped', source.evidence, 1, now()
FROM catalog_pipeline_workflow_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET state_id=EXCLUDED.state_id, normalized_value=EXCLUDED.normalized_value,
  status='mapped', evidence=EXCLUDED.evidence;

DROP TRIGGER IF EXISTS catalog_pipeline_card_integrity ON pipeline_card;
ALTER TABLE pipeline_card ALTER COLUMN service_kind DROP NOT NULL;
ALTER TABLE pipeline_card ALTER COLUMN stage DROP NOT NULL;

DO $batches$
DECLARE changed_rows integer;
BEGIN
  LOOP
    WITH batch AS (
      SELECT target.id FROM pipeline_card target
      JOIN catalog_pipeline_workflow_cutover_source source
        ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
          AND source.pipeline_card_id=target.id
      WHERE target.service_kind IS NOT DISTINCT FROM source.original_service_kind
        AND target.stage IS NOT DISTINCT FROM source.original_stage
        AND target.service_offering_id IS NOT DISTINCT FROM source.original_service_offering_id
        AND target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
      ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer
      FOR UPDATE OF target SKIP LOCKED
    )
    UPDATE pipeline_card target SET service_kind=NULL, stage=NULL,
      service_offering_id=source.target_service_offering_id,
      workflow_state_id=source.target_workflow_state_id, updated_at=now()
    FROM catalog_pipeline_workflow_cutover_source source, batch
    WHERE target.id=batch.id AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.pipeline_card_id=target.id;
    GET DIAGNOSTICS changed_rows = ROW_COUNT;
    EXIT WHEN changed_rows=0;
  END LOOP;
END
$batches$;

CREATE OR REPLACE FUNCTION catalog_validate_pipeline_card() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE workflow_id_value uuid; current_workflow_id uuid; BEGIN
  IF NEW.service_kind IS NOT NULL OR NEW.stage IS NOT NULL THEN
    RAISE EXCEPTION 'pipeline cards require canonical service_offering_id and workflow_state_id; legacy strings are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NEW.service_offering_id IS NULL OR NEW.workflow_state_id IS NULL THEN
    RAISE EXCEPTION 'pipeline cards require service_offering_id and workflow_state_id' USING ERRCODE='23514';
  END IF;
  SELECT binding.workflow_id INTO workflow_id_value FROM pipeline_workflow_binding binding
  JOIN service_offering service ON service.id=binding.service_offering_id AND service.active
  JOIN workflow_definition workflow ON workflow.id=binding.workflow_id AND workflow.active
  WHERE binding.service_offering_id=NEW.service_offering_id AND binding.active;
  IF workflow_id_value IS NULL OR NOT EXISTS (SELECT 1 FROM workflow_state state
    WHERE state.id=NEW.workflow_state_id AND state.workflow_id=workflow_id_value AND state.active) THEN
    RAISE EXCEPTION 'pipeline card state must belong to the active workflow bound to its service offering' USING ERRCODE='23514';
  END IF;
  IF TG_OP='UPDATE' AND OLD.workflow_state_id IS NOT NULL
    AND NEW.workflow_state_id IS DISTINCT FROM OLD.workflow_state_id THEN
    SELECT state.workflow_id INTO current_workflow_id FROM workflow_state state WHERE state.id=OLD.workflow_state_id;
    IF current_workflow_id IS DISTINCT FROM workflow_id_value OR NOT EXISTS (
      SELECT 1 FROM workflow_transition transition WHERE transition.workflow_id=workflow_id_value
        AND transition.from_state_id=OLD.workflow_state_id AND transition.to_state_id=NEW.workflow_state_id
        AND transition.active AND transition.required_permission_id IS NULL
        AND NOT transition.requires_review AND NOT transition.requires_distinct_approver
        AND (transition.effective_from IS NULL OR transition.effective_from<=CURRENT_TIMESTAMP)
        AND (transition.effective_until IS NULL OR transition.effective_until>CURRENT_TIMESTAMP)
    ) THEN RAISE EXCEPTION 'pipeline workflow transition is not allowed for direct execution' USING ERRCODE='23514';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER catalog_pipeline_card_integrity
  BEFORE INSERT OR UPDATE OF service_kind, service_offering_id, stage, workflow_state_id ON pipeline_card
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_pipeline_card();

DO $gate$
DECLARE invalid_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_rows FROM pipeline_card card
  LEFT JOIN pipeline_workflow_binding binding ON binding.service_offering_id=card.service_offering_id AND binding.active
  LEFT JOIN service_offering service ON service.id=card.service_offering_id AND service.active
  LEFT JOIN workflow_state state ON state.id=card.workflow_state_id AND state.active
  LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id AND workflow.active
  WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL
    OR service.id IS NULL OR state.id IS NULL OR binding.workflow_id IS DISTINCT FROM workflow.id;
  IF invalid_rows<>0 THEN
    RAISE EXCEPTION 'canonical pipeline workflow gate failed: invalidRows=%', invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_backfill_run SET status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'pipelineCards', (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'serviceOfferingReferencesMapped', (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid AND original_service_offering_id IS NULL),
    'workflowStateReferencesMapped', (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid AND original_workflow_state_id IS NULL),
    'legacyColumnsCleared', (SELECT count(*) FROM catalog_pipeline_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'batchSize', current_setting('tdf.catalog_batch_size')::integer,
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
