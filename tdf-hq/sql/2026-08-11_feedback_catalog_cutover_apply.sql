\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'feedback-catalog-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 1000
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-feedback-catalog-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);

ALTER TABLE feedback ADD COLUMN IF NOT EXISTS category_id uuid;
ALTER TABLE feedback ADD COLUMN IF NOT EXISTS severity_id uuid;

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

CREATE TABLE IF NOT EXISTS catalog_feedback_reference_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  feedback_id uuid NOT NULL,
  original_category text,
  original_severity text,
  original_category_id uuid,
  original_severity_id uuid,
  target_category_id uuid NOT NULL,
  target_severity_id uuid NOT NULL,
  category_evidence text NOT NULL,
  severity_evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, feedback_id)
);

CREATE TEMP TABLE resolved_feedback ON COMMIT DROP AS
SELECT feedback.id,
  feedback.category AS original_category,
  feedback.severity AS original_severity,
  feedback.category_id AS original_category_id,
  feedback.severity_id AS original_severity_id,
  category_match.candidate_count AS category_candidates,
  category_match.target_id AS target_category_id,
  severity_match.candidate_count AS severity_candidates,
  severity_match.target_id AS target_severity_id
FROM feedback
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM feedback_category item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='feedback-categories' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
    AND (item.id=feedback.category_id OR (feedback.category IS NOT NULL
      AND lower(btrim(feedback.category)) IN (lower(item.code), lower(item.name_es), lower(item.name_en))))
) category_match ON TRUE
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM feedback_severity item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='feedback-severities' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL
    AND (item.id=feedback.severity_id OR (feedback.severity IS NOT NULL
      AND lower(btrim(feedback.severity)) IN (lower(item.code), lower(item.name_es), lower(item.name_en))))
) severity_match ON TRUE
WHERE feedback.category IS NOT NULL OR feedback.severity IS NOT NULL
  OR feedback.category_id IS NULL OR feedback.severity_id IS NULL;

DO $gate$
DECLARE
  source_rows bigint;
  invalid_rows bigint;
BEGIN
  SELECT count(*), count(*) FILTER (
    WHERE category_candidates<>1 OR severity_candidates<>1
      OR target_category_id IS NULL OR target_severity_id IS NULL
      OR (original_category_id IS NOT NULL AND original_category_id<>target_category_id)
      OR (original_severity_id IS NOT NULL AND original_severity_id<>target_severity_id)
  ) INTO source_rows, invalid_rows FROM resolved_feedback;
  IF source_rows > current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0 THEN
    RAISE EXCEPTION 'feedback reference safety gate failed: rows=%, invalidOrAmbiguous=%',
      source_rows, invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_feedback_reference_cutover_source (
  run_id, feedback_id, original_category, original_severity,
  original_category_id, original_severity_id, target_category_id,
  target_severity_id, category_evidence, severity_evidence
)
SELECT :'backfill_run_id'::uuid, id, original_category, original_severity,
  original_category_id, original_severity_id, target_category_id,
  target_severity_id,
  'unique normalized code/name match in feedback-categories',
  'unique normalized code/name match in feedback-severities'
FROM resolved_feedback
ON CONFLICT (run_id, feedback_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'feedback', mapping.source_column,
  resolved.id::text, mapping.original_value, lower(btrim(mapping.original_value)),
  catalog.id, mapping.target_id, 'mapped', mapping.evidence, 1, now()
FROM resolved_feedback resolved
CROSS JOIN LATERAL (VALUES
  ('category', resolved.original_category, resolved.target_category_id, 'feedback-categories', 'unique normalized category code/name match'),
  ('severity', resolved.original_severity, resolved.target_severity_id, 'feedback-severities', 'unique normalized severity code/name match')
) mapping(source_column, original_value, target_id, catalog_code, evidence)
JOIN catalog_definition catalog ON catalog.code=mapping.catalog_code
WHERE mapping.original_value IS NOT NULL
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status='mapped', evidence=EXCLUDED.evidence;

UPDATE feedback target SET
  category_id=source.target_category_id,
  severity_id=source.target_severity_id,
  category=NULL,
  severity=NULL
FROM catalog_feedback_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.feedback_id=target.id
  AND (target.category IS NOT NULL OR target.severity IS NOT NULL
    OR target.category_id IS NULL OR target.severity_id IS NULL);

CREATE INDEX IF NOT EXISTS ix_feedback_category ON feedback (category_id, created_at DESC);
CREATE INDEX IF NOT EXISTS ix_feedback_severity ON feedback (severity_id, created_at DESC);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_feedback_category') THEN
    ALTER TABLE feedback ADD CONSTRAINT fk_feedback_category
      FOREIGN KEY (category_id) REFERENCES feedback_category(id) NOT VALID;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_feedback_severity') THEN
    ALTER TABLE feedback ADD CONSTRAINT fk_feedback_severity
      FOREIGN KEY (severity_id) REFERENCES feedback_severity(id) NOT VALID;
  END IF;
END $$;

ALTER TABLE feedback VALIDATE CONSTRAINT fk_feedback_category;
ALTER TABLE feedback VALIDATE CONSTRAINT fk_feedback_severity;

CREATE OR REPLACE FUNCTION catalog_validate_feedback_references() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.category IS NOT NULL OR NEW.severity IS NOT NULL THEN
    RAISE EXCEPTION 'feedback requires category_id and severity_id; copied values are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NEW.category_id IS NULL OR NOT EXISTS (
    SELECT 1 FROM feedback_category item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='feedback-categories' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
    WHERE item.id=NEW.category_id AND item.active AND item.deprecated_at IS NULL
  ) THEN RAISE EXCEPTION 'feedback requires an active published category' USING ERRCODE='23514'; END IF;
  IF NEW.severity_id IS NULL OR NOT EXISTS (
    SELECT 1 FROM feedback_severity item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='feedback-severities' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
    WHERE item.id=NEW.severity_id AND item.active AND item.deprecated_at IS NULL
  ) THEN RAISE EXCEPTION 'feedback requires an active published severity' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS feedback_reference_integrity ON feedback;
CREATE TRIGGER feedback_reference_integrity
  BEFORE INSERT OR UPDATE OF category, severity, category_id, severity_id ON feedback
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_feedback_references();

DO $gate$
DECLARE invalid_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_rows FROM feedback
  WHERE category IS NOT NULL OR severity IS NOT NULL OR category_id IS NULL OR severity_id IS NULL;
  IF invalid_rows<>0 THEN
    RAISE EXCEPTION 'canonical feedback reference gate failed: invalidRows=%', invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_backfill_run SET
  status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_feedback_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_feedback_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'feedbackRows', (SELECT count(*) FROM catalog_feedback_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'categoryReferencesMapped', (SELECT count(*) FROM catalog_feedback_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'severityReferencesMapped', (SELECT count(*) FROM catalog_feedback_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
