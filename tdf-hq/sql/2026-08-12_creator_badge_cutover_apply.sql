\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'creator-badge-cutover-2026-08-12'
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
SET LOCAL statement_timeout='10min';
SET LOCAL lock_timeout='2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-creator-badge-cutover-v1',0));
SELECT set_config('tdf.catalog_safety_threshold',:'safety_threshold',TRUE);

INSERT INTO catalog_definition (
  id,code,classification,entity_kind,name_es,name_en,public_read,sensitive,
  ordering_mode,workflow_id,cache_revision,active,version
)
SELECT '10000000-0000-4000-8000-000000000047'::uuid,'creator-badge-types',
  'dynamic-business-catalog','creator_badge_type','Insignias de creadores','Creator badges',
  TRUE,FALSE,'manual',workflow.id,1,TRUE,1
FROM workflow_definition workflow
WHERE workflow.code='catalog-publication' AND workflow.active
ON CONFLICT (code) DO NOTHING;

CREATE TABLE IF NOT EXISTS creator_badge_type (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id uuid NOT NULL REFERENCES catalog_definition(id),
  code text NOT NULL UNIQUE,
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
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_creator_badge_type_replacement') THEN
    ALTER TABLE creator_badge_type ADD CONSTRAINT fk_creator_badge_type_replacement
      FOREIGN KEY(replacement_id) REFERENCES creator_badge_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE creator_badge_type VALIDATE CONSTRAINT fk_creator_badge_type_replacement;
CREATE UNIQUE INDEX IF NOT EXISTS uq_creator_badge_type_current_slug
  ON creator_badge_type(current_slug) WHERE current_slug IS NOT NULL;
CREATE INDEX IF NOT EXISTS ix_creator_badge_type_catalog_order
  ON creator_badge_type(catalog_id,active,sort_order,code);

INSERT INTO creator_badge_type (
  id,catalog_id,code,name_es,name_en,current_slug,sort_order,active,workflow_state_id,version
)
SELECT seed.id,catalog.id,seed.code,seed.name_es,seed.name_en,seed.code,
  seed.sort_order,TRUE,state.id,1
FROM (VALUES
  ('50a00000-0000-4000-8000-000000000001'::uuid,'trendsetter','Marcador de tendencia','Trendsetter',10),
  ('50a00000-0000-4000-8000-000000000002'::uuid,'regular','Miembro frecuente','Regular',20),
  ('50a00000-0000-4000-8000-000000000003'::uuid,'og','Miembro fundador','Founding member',30)
) seed(id,code,name_es,name_en,sort_order)
JOIN catalog_definition catalog ON catalog.code='creator-badge-types' AND catalog.active
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
WHERE NOT EXISTS (SELECT 1 FROM creator_badge_type existing WHERE existing.code=seed.code);

DO $catalog_gate$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM catalog_definition catalog
    JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
    WHERE catalog.code='creator-badge-types' AND catalog.entity_kind='creator_badge_type'
      AND catalog.classification='dynamic-business-catalog' AND catalog.active
      AND workflow.code='catalog-publication' AND workflow.active
  ) OR EXISTS (
    SELECT 1
    FROM (VALUES
      ('50a00000-0000-4000-8000-000000000001'::uuid,'trendsetter'),
      ('50a00000-0000-4000-8000-000000000002'::uuid,'regular'),
      ('50a00000-0000-4000-8000-000000000003'::uuid,'og')
    ) seed(id,code)
    LEFT JOIN creator_badge_type item ON item.code=seed.code
    WHERE item.id IS DISTINCT FROM seed.id
  ) THEN
    RAISE EXCEPTION 'creator badge catalog or deterministic seed set is incompatible' USING ERRCODE='23514';
  END IF;
END $catalog_gate$;

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
SELECT set_config('tdf.creator_badge_backfill_run_id',:'backfill_run_id',TRUE);

CREATE TABLE IF NOT EXISTS catalog_creator_badge_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  creator_badge_id bigint NOT NULL,
  original_badge_type text NOT NULL,
  target_badge_type_id uuid NOT NULL REFERENCES creator_badge_type(id),
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(run_id,creator_badge_id)
);
CREATE OR REPLACE FUNCTION catalog_reject_creator_badge_evidence_mutation() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'creator badge cutover evidence is immutable' USING ERRCODE='55000'; END $$;
DROP TRIGGER IF EXISTS catalog_creator_badge_evidence_immutable ON catalog_creator_badge_cutover_source;
CREATE TRIGGER catalog_creator_badge_evidence_immutable
  BEFORE UPDATE OR DELETE ON catalog_creator_badge_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_reject_creator_badge_evidence_mutation();

ALTER TABLE creator_badge ADD COLUMN IF NOT EXISTS badge_type_id uuid;
CREATE TEMP TABLE resolved_creator_badge (
  creator_badge_id bigint PRIMARY KEY,
  original_badge_type text NOT NULL,
  target_badge_type_id uuid,
  existing_badge_type_id uuid
) ON COMMIT DROP;

DO $load_source$ BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type') THEN
    INSERT INTO resolved_creator_badge
    SELECT badge.id,badge.badge_type,item.id,badge.badge_type_id
    FROM creator_badge badge
    LEFT JOIN creator_badge_type item ON item.code=lower(btrim(badge.badge_type))
      AND item.active AND item.deprecated_at IS NULL;
  ELSE
    INSERT INTO resolved_creator_badge
    SELECT badge.id,COALESCE(source.original_badge_type,item.code),item.id,badge.badge_type_id
    FROM creator_badge badge
    LEFT JOIN creator_badge_type item ON item.id=badge.badge_type_id
    LEFT JOIN catalog_creator_badge_cutover_source source
      ON source.run_id=current_setting('tdf.creator_badge_backfill_run_id')::uuid
      AND source.creator_badge_id=badge.id;
  END IF;
END $load_source$;

DO $safety_gate$ DECLARE rows_count bigint; invalid_count bigint; duplicate_count bigint; BEGIN
  SELECT count(*),count(*) FILTER (WHERE target_badge_type_id IS NULL OR (existing_badge_type_id IS NOT NULL AND existing_badge_type_id<>target_badge_type_id))
    INTO rows_count,invalid_count FROM resolved_creator_badge;
  SELECT count(*)-count(DISTINCT (badge.party_id,badge.club_id,resolved.target_badge_type_id)) INTO duplicate_count
  FROM creator_badge badge JOIN resolved_creator_badge resolved ON resolved.creator_badge_id=badge.id;
  IF rows_count>current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_count<>0 OR duplicate_count<>0 THEN
    RAISE EXCEPTION 'creator badge safety gate failed: rows=%, unresolvedOrConflicting=%, canonicalDuplicates=%',
      rows_count,invalid_count,duplicate_count USING ERRCODE='23514';
  END IF;
END $safety_gate$;

INSERT INTO catalog_creator_badge_cutover_source (
  run_id,creator_badge_id,original_badge_type,target_badge_type_id,evidence
)
SELECT :'backfill_run_id'::uuid,creator_badge_id,original_badge_type,target_badge_type_id,
  'unique exact normalized creator badge code'
FROM resolved_creator_badge
ON CONFLICT (run_id,creator_badge_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,'creator_badge','badge_type',source.creator_badge_id::text,
  source.original_badge_type,lower(btrim(source.original_badge_type)),catalog.id,source.target_badge_type_id,
  'mapped',source.evidence,1,now()
FROM catalog_creator_badge_cutover_source source
JOIN catalog_definition catalog ON catalog.code='creator-badge-types'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,status='mapped',evidence=EXCLUDED.evidence;

UPDATE creator_badge badge SET badge_type_id=resolved.target_badge_type_id
FROM resolved_creator_badge resolved
WHERE badge.id=resolved.creator_badge_id AND badge.badge_type_id IS DISTINCT FROM resolved.target_badge_type_id;
ALTER TABLE creator_badge ALTER COLUMN badge_type_id SET NOT NULL;
ALTER TABLE creator_badge DROP COLUMN IF EXISTS badge_type;
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_creator_badge_type') THEN
    ALTER TABLE creator_badge ADD CONSTRAINT fk_creator_badge_type
      FOREIGN KEY(badge_type_id) REFERENCES creator_badge_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE creator_badge VALIDATE CONSTRAINT fk_creator_badge_type;
CREATE UNIQUE INDEX IF NOT EXISTS uq_creator_badge_identity
  ON creator_badge(party_id,club_id,badge_type_id);
CREATE INDEX IF NOT EXISTS ix_creator_badge_type ON creator_badge(badge_type_id,awarded_at DESC);

CREATE OR REPLACE FUNCTION catalog_validate_creator_badge() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM creator_badge_type item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='creator-badge-types' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
    WHERE item.id=NEW.badge_type_id AND item.active AND item.deprecated_at IS NULL
  ) THEN RAISE EXCEPTION 'creator badge requires an active published badge type' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS creator_badge_catalog_integrity ON creator_badge;
CREATE TRIGGER creator_badge_catalog_integrity BEFORE INSERT OR UPDATE OF badge_type_id ON creator_badge
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_creator_badge();
CREATE OR REPLACE FUNCTION catalog_protect_referenced_creator_badge_type() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.catalog_id IS DISTINCT FROM OLD.catalog_id OR NEW.code IS DISTINCT FROM OLD.code THEN
    RAISE EXCEPTION 'referenced creator badge identity is immutable' USING ERRCODE='23514';
  END IF;
  IF OLD.active AND NOT NEW.active AND EXISTS (SELECT 1 FROM creator_badge badge WHERE badge.badge_type_id=OLD.id) THEN
    RAISE EXCEPTION 'referenced creator badge type must be replaced before deactivation' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS catalog_creator_badge_type_reference_protection ON creator_badge_type;
CREATE TRIGGER catalog_creator_badge_type_reference_protection BEFORE UPDATE OF catalog_id,code,active ON creator_badge_type
  FOR EACH ROW EXECUTE FUNCTION catalog_protect_referenced_creator_badge_type();
DROP TRIGGER IF EXISTS catalog_no_hard_delete ON creator_badge_type;
CREATE TRIGGER catalog_no_hard_delete BEFORE DELETE ON creator_badge_type FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

UPDATE creator_badge_type item SET usage_count=counts.usage_count
FROM (SELECT badge_type_id,count(*)::bigint AS usage_count FROM creator_badge GROUP BY badge_type_id) counts
WHERE item.id=counts.badge_type_id AND item.usage_count IS DISTINCT FROM counts.usage_count;
UPDATE creator_badge_type item SET usage_count=0
WHERE item.usage_count<>0 AND NOT EXISTS (SELECT 1 FROM creator_badge badge WHERE badge.badge_type_id=item.id);

DO $post_gate$ BEGIN
  IF EXISTS (
    SELECT 1 FROM catalog_creator_badge_cutover_source source
    LEFT JOIN creator_badge badge ON badge.id=source.creator_badge_id
    WHERE source.run_id=current_setting('tdf.creator_badge_backfill_run_id')::uuid
      AND badge.badge_type_id IS DISTINCT FROM source.target_badge_type_id
  ) THEN RAISE EXCEPTION 'creator badge post-write verification failed' USING ERRCODE='23514'; END IF;
END $post_gate$;

UPDATE catalog_backfill_run SET status='completed',completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_creator_badge_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_creator_badge_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  ambiguous_rows=0,rejected_rows=0,
  report=jsonb_build_object('legacyRows',(SELECT count(*) FROM catalog_creator_badge_cutover_source WHERE run_id=:'backfill_run_id'::uuid),'canonicalRows',(SELECT count(*) FROM creator_badge))::text
WHERE id=:'backfill_run_id'::uuid;

COMMIT;
