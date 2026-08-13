\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'content-reaction-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-content-reaction-cutover-v1',0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);

INSERT INTO catalog_definition (
  id,code,classification,entity_kind,name_es,name_en,public_read,sensitive,
  ordering_mode,workflow_id,cache_revision,active,version
)
SELECT '10000000-0000-4000-8000-000000000046'::uuid,'content-reaction-types',
  'dynamic-business-catalog','content_reaction_type','Reacciones de contenido',
  'Content reactions',TRUE,FALSE,'manual',workflow.id,1,TRUE,1
FROM workflow_definition workflow
WHERE workflow.code='catalog-publication' AND workflow.active
ON CONFLICT (code) DO NOTHING;

DO $catalog_gate$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM catalog_definition catalog
    JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
    WHERE catalog.code='content-reaction-types'
      AND catalog.entity_kind='content_reaction_type'
      AND catalog.classification='dynamic-business-catalog'
      AND catalog.active AND workflow.code='catalog-publication' AND workflow.active
  ) THEN
    RAISE EXCEPTION 'content reaction catalog definition is missing or incompatible' USING ERRCODE='23514';
  END IF;
END
$catalog_gate$;

CREATE TABLE IF NOT EXISTS content_reaction_type (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id uuid NOT NULL REFERENCES catalog_definition(id),
  code text NOT NULL UNIQUE,
  emoji text NOT NULL,
  name_es text NOT NULL,
  name_en text NOT NULL,
  description_es text,
  description_en text,
  current_slug text,
  sort_order integer NOT NULL DEFAULT 0,
  active boolean NOT NULL DEFAULT TRUE,
  workflow_state_id uuid NOT NULL REFERENCES workflow_state(id),
  created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
  deprecated_at timestamptz,
  replacement_id uuid,
  usage_count bigint NOT NULL DEFAULT 0,
  version integer NOT NULL DEFAULT 1
);
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_content_reaction_type_replacement') THEN
    ALTER TABLE content_reaction_type ADD CONSTRAINT fk_content_reaction_type_replacement
      FOREIGN KEY (replacement_id) REFERENCES content_reaction_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE content_reaction_type VALIDATE CONSTRAINT fk_content_reaction_type_replacement;
CREATE UNIQUE INDEX IF NOT EXISTS uq_content_reaction_type_current_slug
  ON content_reaction_type(current_slug) WHERE current_slug IS NOT NULL;
CREATE INDEX IF NOT EXISTS ix_content_reaction_type_catalog_order
  ON content_reaction_type(catalog_id,active,sort_order,code);

INSERT INTO content_reaction_type (
  id,catalog_id,code,emoji,name_es,name_en,current_slug,sort_order,active,workflow_state_id,version
)
SELECT seed.id,catalog.id,seed.code,seed.emoji,seed.name_es,seed.name_en,seed.code,
  seed.sort_order,TRUE,state.id,1
FROM (VALUES
  ('50900000-0000-4000-8000-000000000001'::uuid,'fire','🔥','Fuego','Fire',0),
  ('50900000-0000-4000-8000-000000000002'::uuid,'heart','❤️','Me encanta','Love',1),
  ('50900000-0000-4000-8000-000000000003'::uuid,'clap','👏','Aplauso','Applause',2),
  ('50900000-0000-4000-8000-000000000004'::uuid,'mic_drop','🎤','Mic drop','Mic drop',3),
  ('50900000-0000-4000-8000-000000000005'::uuid,'skull','💀','Me muero','I''m dead',4)
) seed(id,code,emoji,name_es,name_en,sort_order)
JOIN catalog_definition catalog ON catalog.code='content-reaction-types' AND catalog.active
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
WHERE NOT EXISTS (SELECT 1 FROM content_reaction_type existing WHERE existing.code=seed.code);

CREATE TABLE IF NOT EXISTS fan_club_post_reaction (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  post_id bigint NOT NULL REFERENCES fan_club_post(id),
  reactor_party_id bigint NOT NULL REFERENCES party(id),
  reaction_type_id uuid NOT NULL REFERENCES content_reaction_type(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT uq_fan_club_post_reaction UNIQUE(post_id,reactor_party_id)
);
CREATE TABLE IF NOT EXISTS fan_club_memory_reaction (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  memory_id bigint NOT NULL REFERENCES fan_club_memory(id),
  reactor_party_id bigint NOT NULL REFERENCES party(id),
  reaction_type_id uuid NOT NULL REFERENCES content_reaction_type(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT uq_fan_club_memory_reaction UNIQUE(memory_id,reactor_party_id)
);
CREATE INDEX IF NOT EXISTS ix_fan_club_post_reaction_type ON fan_club_post_reaction(reaction_type_id,created_at DESC);
CREATE INDEX IF NOT EXISTS ix_fan_club_memory_reaction_type ON fan_club_memory_reaction(reaction_type_id,created_at DESC);

CREATE TEMP TABLE resolved_content_reaction (
  target_type text NOT NULL,
  target_id bigint NOT NULL,
  reactor_party_id bigint NOT NULL,
  original_reaction text NOT NULL,
  created_at timestamptz NOT NULL,
  target_reaction_type_id uuid,
  target_exists boolean NOT NULL,
  existing_reaction_type_id uuid,
  PRIMARY KEY(target_type,target_id,reactor_party_id)
) ON COMMIT DROP;

DO $load_source$
DECLARE source_table text;
BEGIN
  source_table := CASE
    WHEN to_regclass('public.content_reaction') IS NOT NULL THEN 'content_reaction'
    WHEN to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL THEN 'catalog_content_reaction_legacy_source'
    ELSE NULL
  END;
  IF source_table IS NOT NULL THEN
    EXECUTE format($sql$
      INSERT INTO resolved_content_reaction (
        target_type,target_id,reactor_party_id,original_reaction,created_at,
        target_reaction_type_id,target_exists,existing_reaction_type_id
      )
      SELECT source.target_type,source.target_id,source.reactor_party_id,source.reaction,source.created_at,
        item.id,
        CASE source.target_type
          WHEN 'post' THEN EXISTS (SELECT 1 FROM fan_club_post target WHERE target.id=source.target_id)
          WHEN 'memory' THEN EXISTS (SELECT 1 FROM fan_club_memory target WHERE target.id=source.target_id)
          ELSE FALSE
        END,
        CASE source.target_type
          WHEN 'post' THEN (SELECT reaction.reaction_type_id FROM fan_club_post_reaction reaction WHERE reaction.post_id=source.target_id AND reaction.reactor_party_id=source.reactor_party_id)
          WHEN 'memory' THEN (SELECT reaction.reaction_type_id FROM fan_club_memory_reaction reaction WHERE reaction.memory_id=source.target_id AND reaction.reactor_party_id=source.reactor_party_id)
          ELSE NULL
        END
      FROM %I source
      LEFT JOIN content_reaction_type item ON item.code=lower(btrim(source.reaction))
        AND item.active AND item.deprecated_at IS NULL
        AND EXISTS (
          SELECT 1 FROM catalog_definition catalog
          JOIN workflow_state state ON state.id=item.workflow_state_id
            AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
          WHERE catalog.id=item.catalog_id AND catalog.code='content-reaction-types' AND catalog.active
        )
    $sql$, source_table);
  END IF;
END
$load_source$;

DO $safety_gate$
DECLARE source_rows bigint; invalid_rows bigint; conflict_rows bigint;
BEGIN
  SELECT count(*),count(*) FILTER (
    WHERE target_type NOT IN ('post','memory') OR NOT target_exists OR target_reaction_type_id IS NULL
  ),count(*) FILTER (
    WHERE existing_reaction_type_id IS NOT NULL AND existing_reaction_type_id<>target_reaction_type_id
  ) INTO source_rows,invalid_rows,conflict_rows FROM resolved_content_reaction;
  IF source_rows>current_setting('tdf.catalog_safety_threshold')::bigint
    OR invalid_rows<>0 OR conflict_rows<>0 THEN
    RAISE EXCEPTION 'content reaction safety gate failed: rows=%, unresolvedOrInvalid=%, canonicalConflicts=%',
      source_rows,invalid_rows,conflict_rows USING ERRCODE='23514';
  END IF;
END
$safety_gate$;

INSERT INTO catalog_backfill_run (
  id,run_code,candidate_revision,dry_run,status,safety_threshold,started_at,correlation_id
) VALUES (
  gen_random_uuid(),:'run_code',:'candidate_revision',FALSE,'mapping',:safety_threshold,now(),
  :'run_code'||':'||:'candidate_revision'
)
ON CONFLICT (run_code,candidate_revision,dry_run)
DO UPDATE SET status='mapping',safety_threshold=EXCLUDED.safety_threshold,completed_at=NULL;
SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.content_reaction_backfill_run_id',:'backfill_run_id',TRUE);

CREATE TABLE IF NOT EXISTS catalog_content_reaction_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  target_type text NOT NULL,
  target_id bigint NOT NULL,
  reactor_party_id bigint NOT NULL,
  original_reaction text NOT NULL,
  original_created_at timestamptz NOT NULL,
  target_reaction_type_id uuid NOT NULL REFERENCES content_reaction_type(id),
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(run_id,target_type,target_id,reactor_party_id)
);
CREATE OR REPLACE FUNCTION catalog_reject_content_reaction_evidence_mutation() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  RAISE EXCEPTION 'content reaction cutover evidence is immutable' USING ERRCODE='55000';
END $$;
DROP TRIGGER IF EXISTS catalog_content_reaction_evidence_immutable ON catalog_content_reaction_cutover_source;
CREATE TRIGGER catalog_content_reaction_evidence_immutable
  BEFORE UPDATE OR DELETE ON catalog_content_reaction_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_reject_content_reaction_evidence_mutation();

INSERT INTO catalog_content_reaction_cutover_source (
  run_id,target_type,target_id,reactor_party_id,original_reaction,original_created_at,
  target_reaction_type_id,evidence
)
SELECT :'backfill_run_id'::uuid,target_type,target_id,reactor_party_id,original_reaction,
  created_at,target_reaction_type_id,'unique exact normalized content-reaction code and typed target FK'
FROM resolved_content_reaction
ON CONFLICT (run_id,target_type,target_id,reactor_party_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,'content_reaction','reaction',
  source.target_type||':'||source.target_id||':'||source.reactor_party_id,
  source.original_reaction,lower(btrim(source.original_reaction)),catalog.id,
  source.target_reaction_type_id,'mapped',source.evidence,1,now()
FROM catalog_content_reaction_cutover_source source
JOIN catalog_definition catalog ON catalog.code='content-reaction-types'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,status='mapped',evidence=EXCLUDED.evidence;

INSERT INTO fan_club_post_reaction (id,post_id,reactor_party_id,reaction_type_id,created_at)
SELECT md5('post:'||target_id||':'||reactor_party_id)::uuid,target_id,reactor_party_id,
  target_reaction_type_id,original_created_at
FROM catalog_content_reaction_cutover_source
WHERE run_id=:'backfill_run_id'::uuid AND target_type='post'
ON CONFLICT (post_id,reactor_party_id) DO NOTHING;
INSERT INTO fan_club_memory_reaction (id,memory_id,reactor_party_id,reaction_type_id,created_at)
SELECT md5('memory:'||target_id||':'||reactor_party_id)::uuid,target_id,reactor_party_id,
  target_reaction_type_id,original_created_at
FROM catalog_content_reaction_cutover_source
WHERE run_id=:'backfill_run_id'::uuid AND target_type='memory'
ON CONFLICT (memory_id,reactor_party_id) DO NOTHING;

UPDATE content_reaction_type item SET usage_count=counts.usage_count
FROM (
  SELECT item_id,count(*)::bigint AS usage_count FROM (
    SELECT reaction_type_id AS item_id FROM fan_club_post_reaction
    UNION ALL
    SELECT reaction_type_id FROM fan_club_memory_reaction
  ) usage_refs GROUP BY item_id
) counts
WHERE item.id=counts.item_id AND item.usage_count IS DISTINCT FROM counts.usage_count;
UPDATE content_reaction_type item SET usage_count=0
WHERE item.usage_count<>0
  AND NOT EXISTS (SELECT 1 FROM fan_club_post_reaction reaction WHERE reaction.reaction_type_id=item.id)
  AND NOT EXISTS (SELECT 1 FROM fan_club_memory_reaction reaction WHERE reaction.reaction_type_id=item.id);

DO $post_gate$
BEGIN
  IF EXISTS (
    SELECT 1 FROM catalog_content_reaction_cutover_source source
    LEFT JOIN fan_club_post_reaction reaction ON source.target_type='post'
      AND reaction.post_id=source.target_id AND reaction.reactor_party_id=source.reactor_party_id
    LEFT JOIN fan_club_memory_reaction memory_reaction ON source.target_type='memory'
      AND memory_reaction.memory_id=source.target_id AND memory_reaction.reactor_party_id=source.reactor_party_id
    WHERE source.run_id=current_setting('tdf.content_reaction_backfill_run_id')::uuid AND (
      (source.target_type='post' AND reaction.reaction_type_id IS DISTINCT FROM source.target_reaction_type_id)
      OR (source.target_type='memory' AND memory_reaction.reaction_type_id IS DISTINCT FROM source.target_reaction_type_id)
    )
  ) THEN
    RAISE EXCEPTION 'content reaction post-write verification failed' USING ERRCODE='23514';
  END IF;
END
$post_gate$;

DO $preserve_source$
BEGIN
  IF to_regclass('public.content_reaction') IS NOT NULL THEN
    IF to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL THEN
      RAISE EXCEPTION 'both live and preserved legacy content reaction tables exist' USING ERRCODE='23514';
    END IF;
    ALTER TABLE content_reaction RENAME TO catalog_content_reaction_legacy_source;
  END IF;
END
$preserve_source$;

CREATE OR REPLACE FUNCTION catalog_reject_content_reaction_source_mutation() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  RAISE EXCEPTION 'preserved content reaction source is immutable' USING ERRCODE='55000';
END $$;
DO $source_trigger$
BEGIN
  IF to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL THEN
    DROP TRIGGER IF EXISTS catalog_content_reaction_source_immutable ON catalog_content_reaction_legacy_source;
    CREATE TRIGGER catalog_content_reaction_source_immutable
      BEFORE INSERT OR UPDATE OR DELETE ON catalog_content_reaction_legacy_source
      FOR EACH ROW EXECUTE FUNCTION catalog_reject_content_reaction_source_mutation();
  END IF;
END
$source_trigger$;

CREATE OR REPLACE FUNCTION catalog_validate_fan_club_content_reaction() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM content_reaction_type item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id
      AND catalog.code='content-reaction-types' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id
      AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
    WHERE item.id=NEW.reaction_type_id AND item.active AND item.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'fan club reaction requires an active published content reaction type' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS fan_club_post_reaction_catalog_integrity ON fan_club_post_reaction;
CREATE TRIGGER fan_club_post_reaction_catalog_integrity
  BEFORE INSERT OR UPDATE OF reaction_type_id ON fan_club_post_reaction
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_fan_club_content_reaction();
DROP TRIGGER IF EXISTS fan_club_memory_reaction_catalog_integrity ON fan_club_memory_reaction;
CREATE TRIGGER fan_club_memory_reaction_catalog_integrity
  BEFORE INSERT OR UPDATE OF reaction_type_id ON fan_club_memory_reaction
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_fan_club_content_reaction();

CREATE OR REPLACE FUNCTION catalog_protect_referenced_content_reaction_type() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.catalog_id IS DISTINCT FROM OLD.catalog_id OR NEW.code IS DISTINCT FROM OLD.code THEN
    RAISE EXCEPTION 'referenced content reaction identity is immutable' USING ERRCODE='23514';
  END IF;
  IF OLD.active AND NOT NEW.active AND (
    EXISTS (SELECT 1 FROM fan_club_post_reaction reaction WHERE reaction.reaction_type_id=OLD.id)
    OR EXISTS (SELECT 1 FROM fan_club_memory_reaction reaction WHERE reaction.reaction_type_id=OLD.id)
  ) THEN
    RAISE EXCEPTION 'referenced content reaction type must be replaced before deactivation' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS catalog_content_reaction_type_reference_protection ON content_reaction_type;
CREATE TRIGGER catalog_content_reaction_type_reference_protection
  BEFORE UPDATE OF catalog_id,code,active ON content_reaction_type
  FOR EACH ROW EXECUTE FUNCTION catalog_protect_referenced_content_reaction_type();
DROP TRIGGER IF EXISTS catalog_no_hard_delete ON content_reaction_type;
CREATE TRIGGER catalog_no_hard_delete
  BEFORE DELETE ON content_reaction_type
  FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

UPDATE catalog_backfill_run SET status='completed',completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_content_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_content_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  ambiguous_rows=0,rejected_rows=0,
  report=jsonb_build_object(
    'legacyRows',(SELECT count(*) FROM catalog_content_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'postRows',(SELECT count(*) FROM fan_club_post_reaction),
    'memoryRows',(SELECT count(*) FROM fan_club_memory_reaction)
  )::text
WHERE id=:'backfill_run_id'::uuid;

COMMIT;
