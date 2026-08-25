\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'social-event-type-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 10000
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-social-event-type-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);

ALTER TABLE social_event ADD COLUMN IF NOT EXISTS event_type_id uuid;

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

CREATE TABLE IF NOT EXISTS catalog_social_event_type_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  social_event_id bigint NOT NULL,
  original_event_type text,
  original_event_type_id uuid,
  original_metadata text,
  target_event_type_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, social_event_id)
);

CREATE TEMP TABLE resolved_social_event_type ON COMMIT DROP AS
SELECT event.id,
  event.event_type_id AS original_event_type_id,
  event.metadata AS original_metadata,
  event.metadata::jsonb ->> 'eventType' AS original_event_type,
  match.candidate_count,
  match.target_id
FROM social_event event
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM event_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='event-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
    AND (item.effective_from IS NULL OR item.effective_from<=CURRENT_DATE)
    AND (item.effective_until IS NULL OR item.effective_until>=CURRENT_DATE)
    AND (
      item.id=event.event_type_id
      OR (
        event.metadata IS NOT NULL
        AND NULLIF(btrim(event.metadata::jsonb ->> 'eventType'), '') IS NOT NULL
        AND lower(btrim(event.metadata::jsonb ->> 'eventType')) IN (
          lower(item.code), lower(item.name_es), lower(item.name_en), lower(COALESCE(item.current_slug, ''))
        )
      )
    )
) match ON TRUE
WHERE event.event_type_id IS NULL
   OR event.metadata IS NOT NULL AND event.metadata::jsonb ? 'eventType';

DO $gate$
DECLARE
  source_rows bigint;
  invalid_rows bigint;
BEGIN
  SELECT count(*), count(*) FILTER (
    WHERE candidate_count<>1 OR target_id IS NULL
      OR (original_event_type_id IS NOT NULL AND original_event_type_id<>target_id)
  ) INTO source_rows, invalid_rows FROM resolved_social_event_type;
  IF source_rows > current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0 THEN
    RAISE EXCEPTION 'social-event type safety gate failed: rows=%, invalidOrAmbiguous=%',
      source_rows, invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_social_event_type_cutover_source (
  run_id, social_event_id, original_event_type, original_event_type_id,
  original_metadata, target_event_type_id, evidence
)
SELECT :'backfill_run_id'::uuid, id, original_event_type, original_event_type_id,
  original_metadata, target_id,
  CASE WHEN original_event_type_id=target_id
    THEN 'existing canonical id confirmed by event-types'
    ELSE 'unique normalized code/name/slug match in event-types'
  END
FROM resolved_social_event_type
ON CONFLICT (run_id, social_event_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'social_event',
  CASE WHEN source.original_event_type IS NULL THEN 'event_type_id' ELSE 'metadata.eventType' END,
  source.social_event_id::text,
  COALESCE(source.original_event_type, source.original_event_type_id::text),
  lower(btrim(COALESCE(source.original_event_type, source.original_event_type_id::text))),
  catalog.id, source.target_event_type_id, 'mapped', source.evidence, 1, now()
FROM catalog_social_event_type_cutover_source source
JOIN catalog_definition catalog ON catalog.code='event-types'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status='mapped', evidence=EXCLUDED.evidence;

DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;
DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;

UPDATE social_event target SET
  event_type_id=source.target_event_type_id,
  metadata=NULLIF((target.metadata::jsonb - 'eventType')::text, '{}')
FROM catalog_social_event_type_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.social_event_id=target.id
  AND (target.event_type_id IS NULL OR target.metadata IS NOT NULL AND target.metadata::jsonb ? 'eventType');

CREATE INDEX IF NOT EXISTS ix_social_event_event_type_start
  ON social_event (event_type_id, start_time DESC);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_social_event_event_type') THEN
    ALTER TABLE social_event ADD CONSTRAINT fk_social_event_event_type
      FOREIGN KEY (event_type_id) REFERENCES event_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE social_event VALIDATE CONSTRAINT fk_social_event_event_type;

CREATE OR REPLACE FUNCTION catalog_validate_social_event_type() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.event_type_id IS NULL THEN
    RAISE EXCEPTION 'social events require event_type_id' USING ERRCODE='23514';
  END IF;
  IF NEW.metadata IS NOT NULL AND NEW.metadata::jsonb ? 'eventType' THEN
    RAISE EXCEPTION 'social events require event_type_id; metadata eventType strings are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM event_type item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='event-types' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
    WHERE item.id=NEW.event_type_id AND item.active AND item.deprecated_at IS NULL
      AND (item.effective_from IS NULL OR item.effective_from<=CURRENT_DATE)
      AND (item.effective_until IS NULL OR item.effective_until>=CURRENT_DATE)
  ) THEN RAISE EXCEPTION 'social event requires an active effective published event type' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;
CREATE TRIGGER social_event_type_integrity
  BEFORE INSERT OR UPDATE OF event_type_id, metadata ON social_event
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_social_event_type();

DO $gate$
DECLARE invalid_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_rows FROM social_event event
  LEFT JOIN event_type item ON item.id=event.event_type_id
  LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id
  LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
  WHERE event.event_type_id IS NULL OR item.id IS NULL
    OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL
    OR catalog.code IS DISTINCT FROM 'event-types' OR catalog.active IS DISTINCT FROM TRUE
    OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE
    OR state.workflow_id IS DISTINCT FROM catalog.workflow_id
    OR item.effective_from>CURRENT_DATE OR item.effective_until<CURRENT_DATE
    OR event.metadata IS NOT NULL AND event.metadata::jsonb ? 'eventType';
  IF invalid_rows<>0 THEN
    RAISE EXCEPTION 'canonical social-event type gate failed: invalidRows=%', invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_backfill_run SET
  status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_social_event_type_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_social_event_type_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'socialEventRows', (SELECT count(*) FROM catalog_social_event_type_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'eventTypeReferencesMapped', (SELECT count(*) FROM catalog_social_event_type_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'legacyMetadataKeysRemoved', (SELECT count(*) FROM catalog_social_event_type_cutover_source WHERE run_id=:'backfill_run_id'::uuid AND original_event_type IS NOT NULL),
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
