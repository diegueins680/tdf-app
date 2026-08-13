\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'social-event-workflow-cutover-2026-08-11'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-social-event-workflow-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);
SELECT set_config('tdf.catalog_batch_size', :'batch_size', TRUE);

ALTER TABLE social_event ADD COLUMN IF NOT EXISTS workflow_state_id uuid;

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

CREATE TABLE IF NOT EXISTS catalog_social_event_workflow_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  social_event_id bigint NOT NULL,
  original_event_status text,
  normalized_status text NOT NULL,
  original_workflow_state_id uuid,
  original_metadata text,
  target_workflow_state_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, social_event_id)
);

DROP TRIGGER IF EXISTS catalog_no_hard_delete ON catalog_social_event_workflow_cutover_source;
CREATE TRIGGER catalog_no_hard_delete
  BEFORE DELETE ON catalog_social_event_workflow_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

CREATE TEMP TABLE resolved_social_event_workflow ON COMMIT DROP AS
WITH source AS (
  SELECT event.id,
    event.workflow_state_id AS original_workflow_state_id,
    event.metadata AS original_metadata,
    CASE WHEN event.metadata IS NOT NULL THEN event.metadata::jsonb ->> 'eventStatus' END AS original_event_status
  FROM social_event event
  WHERE event.workflow_state_id IS NULL
     OR event.metadata IS NOT NULL
        AND event.metadata::jsonb ? 'eventStatus'
), normalized AS (
  SELECT source.*,
    CASE
      WHEN NULLIF(btrim(original_event_status), '') IS NULL THEN 'planning'
      WHEN lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))='canceled' THEN 'cancelled'
      WHEN lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))='onsale' THEN 'on_sale'
      ELSE lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))
    END AS normalized_status
  FROM source
)
SELECT normalized.*,
  match.candidate_count,
  match.target_id
FROM normalized
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(state.id ORDER BY state.id))[1] AS target_id
  FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    AND workflow.code='social-event-lifecycle' AND workflow.active
  WHERE state.active AND state.code=normalized.normalized_status
) match ON TRUE;

DO $gate$
DECLARE
  source_rows bigint;
  invalid_rows bigint;
  workflow_count bigint;
  state_count bigint;
  initial_count bigint;
BEGIN
  SELECT count(*) INTO workflow_count FROM workflow_definition
  WHERE code='social-event-lifecycle' AND active AND public_read AND NOT sensitive;
  SELECT count(*) INTO state_count FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='social-event-lifecycle' AND workflow.active AND state.active;
  SELECT count(*) INTO initial_count FROM workflow_default_state default_state
  JOIN workflow_definition workflow ON workflow.id=default_state.workflow_id
  JOIN workflow_state state ON state.id=default_state.state_id AND state.workflow_id=workflow.id
  WHERE workflow.code='social-event-lifecycle' AND workflow.active
    AND default_state.context='initial' AND default_state.active AND state.active;
  SELECT count(*), count(*) FILTER (
    WHERE candidate_count<>1 OR target_id IS NULL
      OR (original_workflow_state_id IS NOT NULL AND original_workflow_state_id<>target_id)
  ) INTO source_rows, invalid_rows FROM resolved_social_event_workflow;
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000
    OR source_rows > current_setting('tdf.catalog_safety_threshold')::bigint
    OR workflow_count<>1 OR state_count<>9
    OR initial_count<>1 OR invalid_rows<>0 THEN
    RAISE EXCEPTION 'social-event workflow safety gate failed: rows=%, invalidOrAmbiguous=%, workflows=%, states=%, initialStates=%',
      source_rows, invalid_rows, workflow_count, state_count, initial_count
      USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_social_event_workflow_cutover_source (
  run_id, social_event_id, original_event_status, normalized_status,
  original_workflow_state_id, original_metadata, target_workflow_state_id, evidence
)
SELECT :'backfill_run_id'::uuid, id, original_event_status, normalized_status,
  original_workflow_state_id, original_metadata, target_id,
  CASE
    WHEN original_workflow_state_id=target_id THEN 'existing canonical UUID confirmed in social-event-lifecycle'
    WHEN original_event_status IS NULL OR btrim(original_event_status)='' THEN 'legacy API read/default semantics deterministically resolve an absent status to planning'
    ELSE 'reviewed normalized status/alias resolves to one active state in social-event-lifecycle'
  END
FROM resolved_social_event_workflow
ON CONFLICT (run_id, social_event_id) DO NOTHING;

INSERT INTO workflow_migration_mapping (
  id, run_id, workflow_id, source_table, source_column, source_record_id,
  original_value, normalized_value, state_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, workflow.id, 'social_event',
  CASE WHEN source.original_event_status IS NULL THEN 'workflow_state_id/default' ELSE 'metadata.eventStatus' END,
  source.social_event_id::text,
  COALESCE(source.original_event_status, source.original_workflow_state_id::text, '<missing>'),
  source.normalized_status, source.target_workflow_state_id, 'mapped', source.evidence, 1, now()
FROM catalog_social_event_workflow_cutover_source source
JOIN workflow_definition workflow ON workflow.code='social-event-lifecycle'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET state_id=EXCLUDED.state_id, normalized_value=EXCLUDED.normalized_value,
  status='mapped', evidence=EXCLUDED.evidence;

DO $batches$
DECLARE changed_rows integer;
BEGIN
  LOOP
    WITH batch AS (
      SELECT target.id
      FROM social_event target
      JOIN catalog_social_event_workflow_cutover_source source
        ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
          AND source.social_event_id=target.id
      WHERE target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
        AND target.metadata IS NOT DISTINCT FROM source.original_metadata
      ORDER BY target.id
      LIMIT current_setting('tdf.catalog_batch_size')::integer
      FOR UPDATE OF target SKIP LOCKED
    )
    UPDATE social_event target SET
      workflow_state_id=source.target_workflow_state_id,
      metadata=NULLIF((target.metadata::jsonb - 'eventStatus')::text, '{}')
    FROM catalog_social_event_workflow_cutover_source source, batch
    WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.social_event_id=target.id;
    GET DIAGNOSTICS changed_rows = ROW_COUNT;
    EXIT WHEN changed_rows=0;
  END LOOP;
END
$batches$;

CREATE INDEX IF NOT EXISTS ix_social_event_workflow_state
  ON social_event (workflow_state_id, start_time DESC, id);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_social_event_workflow_state') THEN
    ALTER TABLE social_event ADD CONSTRAINT fk_social_event_workflow_state
      FOREIGN KEY (workflow_state_id) REFERENCES workflow_state(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE social_event VALIDATE CONSTRAINT fk_social_event_workflow_state;

CREATE OR REPLACE FUNCTION catalog_validate_social_event_workflow_state() RETURNS trigger
LANGUAGE plpgsql AS $$ DECLARE workflow_id_value uuid; BEGIN
  IF NEW.workflow_state_id IS NULL THEN
    RAISE EXCEPTION 'social events require workflow_state_id' USING ERRCODE='23514';
  END IF;
  IF NEW.metadata IS NOT NULL AND NEW.metadata::jsonb ? 'eventStatus' THEN
    RAISE EXCEPTION 'social events require workflow_state_id; metadata eventStatus strings are migration evidence only' USING ERRCODE='23514';
  END IF;
  SELECT state.workflow_id INTO workflow_id_value FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE state.id=NEW.workflow_state_id AND state.active AND workflow.active
    AND workflow.code='social-event-lifecycle';
  IF workflow_id_value IS NULL THEN
    RAISE EXCEPTION 'social event requires an active state in social-event-lifecycle' USING ERRCODE='23514';
  END IF;
  IF TG_OP='UPDATE' AND OLD.workflow_state_id IS NOT NULL
    AND NEW.workflow_state_id IS DISTINCT FROM OLD.workflow_state_id
    AND NOT EXISTS (
      SELECT 1 FROM workflow_transition transition
      WHERE transition.workflow_id=workflow_id_value
        AND transition.from_state_id=OLD.workflow_state_id
        AND transition.to_state_id=NEW.workflow_state_id AND transition.active
        AND transition.required_permission_id IS NULL
        AND NOT transition.requires_review AND NOT transition.requires_distinct_approver
        AND (transition.effective_from IS NULL OR transition.effective_from<=CURRENT_TIMESTAMP)
        AND (transition.effective_until IS NULL OR transition.effective_until>CURRENT_TIMESTAMP)
    ) THEN RAISE EXCEPTION 'social event workflow transition is not allowed for direct execution' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;
CREATE TRIGGER social_event_workflow_state_integrity
  BEFORE INSERT OR UPDATE OF workflow_state_id, metadata ON social_event
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_social_event_workflow_state();

DO $gate$
DECLARE invalid_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_rows FROM social_event event
  LEFT JOIN workflow_state state ON state.id=event.workflow_state_id
  LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE event.workflow_state_id IS NULL OR state.id IS NULL
    OR state.active IS DISTINCT FROM TRUE
    OR workflow.code IS DISTINCT FROM 'social-event-lifecycle'
    OR workflow.active IS DISTINCT FROM TRUE
    OR event.metadata IS NOT NULL AND event.metadata::jsonb ? 'eventStatus';
  IF invalid_rows<>0 THEN
    RAISE EXCEPTION 'canonical social-event workflow gate failed: invalidRows=%', invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_backfill_run SET
  status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'socialEventRows', (SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'workflowStateReferencesMapped', (SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'legacyMetadataKeysRemoved', (SELECT count(*) FROM catalog_social_event_workflow_cutover_source WHERE run_id=:'backfill_run_id'::uuid AND original_event_status IS NOT NULL),
    'batchSize', current_setting('tdf.catalog_batch_size')::integer,
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
