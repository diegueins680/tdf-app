\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'event-moment-reaction-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-event-moment-reaction-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);

ALTER TABLE reaction_type ADD COLUMN IF NOT EXISTS current_slug text;
ALTER TABLE reaction_type ADD COLUMN IF NOT EXISTS deprecated_at timestamptz;
ALTER TABLE reaction_type ADD COLUMN IF NOT EXISTS replacement_id uuid;
ALTER TABLE reaction_type ADD COLUMN IF NOT EXISTS usage_count bigint NOT NULL DEFAULT 0;

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_reaction_type_replacement') THEN
    ALTER TABLE reaction_type ADD CONSTRAINT fk_reaction_type_replacement
      FOREIGN KEY (replacement_id) REFERENCES reaction_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE reaction_type VALIDATE CONSTRAINT fk_reaction_type_replacement;
CREATE UNIQUE INDEX IF NOT EXISTS uq_reaction_type_current_slug
  ON reaction_type (current_slug) WHERE current_slug IS NOT NULL;

INSERT INTO reaction_type (
  id, catalog_id, code, emoji, name_es, name_en, current_slug,
  sort_order, active, workflow_state_id, version
)
SELECT seed.id, catalog.id, seed.code, seed.emoji, seed.name_es, seed.name_en,
  seed.code, seed.sort_order, TRUE, state.id, 1
FROM (VALUES
  ('50800000-0000-4000-8000-000000000001'::uuid, 'fire', '🔥', 'Fuego', 'Fire', 0),
  ('50800000-0000-4000-8000-000000000002'::uuid, 'love', '❤️', 'Me encanta', 'Love', 1),
  ('50800000-0000-4000-8000-000000000003'::uuid, 'applause', '👏', 'Aplauso', 'Applause', 2)
) seed(id, code, emoji, name_es, name_en, sort_order)
JOIN catalog_definition catalog ON catalog.code='reaction-types' AND catalog.active
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id
  AND state.code='published' AND state.active
WHERE NOT EXISTS (SELECT 1 FROM reaction_type existing WHERE existing.code=seed.code);

UPDATE reaction_type item SET
  emoji=seed.emoji,
  name_es=seed.name_es,
  name_en=seed.name_en,
  current_slug=COALESCE(item.current_slug, seed.code),
  sort_order=seed.sort_order
FROM (VALUES
  ('fire', '🔥', 'Fuego', 'Fire', 0),
  ('love', '❤️', 'Me encanta', 'Love', 1),
  ('applause', '👏', 'Aplauso', 'Applause', 2)
) seed(code, emoji, name_es, name_en, sort_order)
WHERE item.code=seed.code AND item.version=1;

ALTER TABLE event_moment_reaction ADD COLUMN IF NOT EXISTS id uuid;
ALTER TABLE event_moment_reaction ADD COLUMN IF NOT EXISTS reaction_type_id uuid;
ALTER TABLE event_moment_reaction ALTER COLUMN id SET DEFAULT gen_random_uuid();
UPDATE event_moment_reaction SET id=md5(
  moment_id::text || ':' || COALESCE(reaction, reaction_type_id::text, '') || ':' || reactor_party_id
)::uuid WHERE id IS NULL;
ALTER TABLE event_moment_reaction ALTER COLUMN id SET NOT NULL;
DO $$ BEGIN
  IF EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid='event_moment_reaction'::regclass AND contype='p'
      AND conname='event_moment_reaction_pkey'
  ) AND NOT EXISTS (
    SELECT 1
    FROM pg_constraint constraint_row
    JOIN pg_attribute attribute_row
      ON attribute_row.attrelid=constraint_row.conrelid
      AND attribute_row.attnum=ANY(constraint_row.conkey)
    WHERE constraint_row.conname='event_moment_reaction_pkey'
      AND constraint_row.conrelid='event_moment_reaction'::regclass
    GROUP BY constraint_row.oid
    HAVING array_agg(attribute_row.attname ORDER BY attribute_row.attname)=ARRAY['id']::name[]
  ) THEN
    ALTER TABLE event_moment_reaction DROP CONSTRAINT event_moment_reaction_pkey;
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conrelid='event_moment_reaction'::regclass AND contype='p'
  ) THEN
    ALTER TABLE event_moment_reaction ADD CONSTRAINT event_moment_reaction_pkey PRIMARY KEY (id);
  END IF;
END $$;
ALTER TABLE event_moment_reaction ALTER COLUMN reaction DROP NOT NULL;

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

CREATE TABLE IF NOT EXISTS catalog_event_moment_reaction_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  reaction_id uuid NOT NULL,
  original_reaction text,
  original_reaction_type_id uuid,
  target_reaction_type_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, reaction_id)
);

DO $$ BEGIN
  IF to_regprocedure('catalog_prevent_hard_delete()') IS NOT NULL THEN
    DROP TRIGGER IF EXISTS catalog_event_moment_reaction_cutover_source_no_delete
      ON catalog_event_moment_reaction_cutover_source;
    CREATE TRIGGER catalog_event_moment_reaction_cutover_source_no_delete
      BEFORE DELETE ON catalog_event_moment_reaction_cutover_source
      FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();
  END IF;
END $$;

CREATE TEMP TABLE resolved_event_moment_reaction ON COMMIT DROP AS
SELECT reaction.id,
  reaction.moment_id,
  reaction.reactor_party_id,
  reaction.reaction AS original_reaction,
  reaction.reaction_type_id AS original_reaction_type_id,
  match.candidate_count,
  match.target_id
FROM event_moment_reaction reaction
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM reaction_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE item.active AND item.deprecated_at IS NULL AND (
    item.id=reaction.reaction_type_id OR item.code=CASE lower(btrim(reaction.reaction))
      WHEN 'heart' THEN 'love'
      WHEN 'clap' THEN 'applause'
      ELSE lower(btrim(reaction.reaction))
    END
  )
) match ON TRUE
WHERE reaction.reaction IS NOT NULL OR reaction.reaction_type_id IS NULL;

DO $gate$
DECLARE
  source_rows bigint;
  invalid_rows bigint;
  duplicate_rows bigint;
BEGIN
  SELECT count(*), count(*) FILTER (
    WHERE candidate_count<>1 OR target_id IS NULL
      OR (original_reaction_type_id IS NOT NULL AND original_reaction_type_id<>target_id)
  ) INTO source_rows, invalid_rows FROM resolved_event_moment_reaction;
  SELECT count(*) INTO duplicate_rows FROM (
    SELECT moment_id, reactor_party_id, target_id
    FROM resolved_event_moment_reaction
    GROUP BY moment_id, reactor_party_id, target_id
    HAVING count(*)>1
  ) duplicates;
  IF source_rows > current_setting('tdf.catalog_safety_threshold')::bigint
    OR invalid_rows<>0 OR duplicate_rows<>0 THEN
    RAISE EXCEPTION 'event-moment reaction safety gate failed: rows=%, invalidOrAmbiguous=%, duplicateTargets=%',
      source_rows, invalid_rows, duplicate_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_event_moment_reaction_cutover_source (
  run_id, reaction_id, original_reaction, original_reaction_type_id,
  target_reaction_type_id, evidence
)
SELECT :'backfill_run_id'::uuid, id, original_reaction, original_reaction_type_id,
  target_id,
  CASE
    WHEN original_reaction_type_id=target_id THEN 'existing canonical UUID confirmed in reaction-types'
    WHEN lower(btrim(original_reaction)) IN ('heart','clap') THEN 'reviewed deterministic historical alias'
    ELSE 'unique normalized reaction-type code match'
  END
FROM resolved_event_moment_reaction
ON CONFLICT (run_id, reaction_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'event_moment_reaction',
  CASE WHEN source.original_reaction IS NULL THEN 'reaction_type_id' ELSE 'reaction' END,
  source.reaction_id::text,
  COALESCE(source.original_reaction, source.original_reaction_type_id::text),
  COALESCE(CASE lower(btrim(source.original_reaction)) WHEN 'heart' THEN 'love' WHEN 'clap' THEN 'applause' ELSE lower(btrim(source.original_reaction)) END, source.original_reaction_type_id::text),
  catalog.id, source.target_reaction_type_id, 'mapped', source.evidence, 1, now()
FROM catalog_event_moment_reaction_cutover_source source
JOIN catalog_definition catalog ON catalog.code='reaction-types'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status='mapped', evidence=EXCLUDED.evidence;

UPDATE event_moment_reaction target SET
  reaction_type_id=source.target_reaction_type_id,
  reaction=NULL
FROM catalog_event_moment_reaction_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.reaction_id=target.id
  AND (target.reaction IS NOT NULL OR target.reaction_type_id IS NULL);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_event_moment_reaction_type') THEN
    ALTER TABLE event_moment_reaction ADD CONSTRAINT fk_event_moment_reaction_type
      FOREIGN KEY (reaction_type_id) REFERENCES reaction_type(id) NOT VALID;
  END IF;
END $$;
ALTER TABLE event_moment_reaction VALIDATE CONSTRAINT fk_event_moment_reaction_type;
ALTER TABLE event_moment_reaction ALTER COLUMN reaction_type_id SET NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS uq_event_moment_reaction_identity
  ON event_moment_reaction (moment_id, reaction_type_id, reactor_party_id);
CREATE INDEX IF NOT EXISTS ix_event_moment_reaction_type
  ON event_moment_reaction (reaction_type_id, created_at DESC);

CREATE OR REPLACE FUNCTION catalog_validate_event_moment_reaction() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.reaction IS NOT NULL OR NEW.reaction_type_id IS NULL THEN
    RAISE EXCEPTION 'event moment reactions require reaction_type_id and reject copied reaction strings' USING ERRCODE='23514';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM reaction_type item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='reaction-types' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
    WHERE item.id=NEW.reaction_type_id AND item.active AND item.deprecated_at IS NULL
  ) THEN RAISE EXCEPTION 'event moment reaction requires an active published reaction type' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS event_moment_reaction_catalog_integrity ON event_moment_reaction;
CREATE TRIGGER event_moment_reaction_catalog_integrity
  BEFORE INSERT OR UPDATE OF reaction_type_id, reaction ON event_moment_reaction
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_event_moment_reaction();

CREATE OR REPLACE FUNCTION catalog_protect_referenced_reaction_type() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.catalog_id IS DISTINCT FROM OLD.catalog_id OR NEW.code IS DISTINCT FROM OLD.code THEN
    RAISE EXCEPTION 'referenced reaction type catalog and code are immutable' USING ERRCODE='23514';
  END IF;
  IF OLD.active AND NOT NEW.active AND EXISTS (
    SELECT 1 FROM event_moment_reaction reaction WHERE reaction.reaction_type_id=OLD.id
  ) THEN RAISE EXCEPTION 'a referenced reaction type must be replaced before deactivation' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS catalog_reaction_type_reference_protection ON reaction_type;
CREATE TRIGGER catalog_reaction_type_reference_protection
  BEFORE UPDATE OF catalog_id, code, active ON reaction_type
  FOR EACH ROW EXECUTE FUNCTION catalog_protect_referenced_reaction_type();

DO $gate$
DECLARE invalid_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_rows
  FROM event_moment_reaction reaction
  LEFT JOIN reaction_type item ON item.id=reaction.reaction_type_id
  LEFT JOIN catalog_definition catalog ON catalog.id=item.catalog_id
  LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
  WHERE reaction.reaction IS NOT NULL OR reaction.reaction_type_id IS NULL OR item.id IS NULL
    OR item.active IS DISTINCT FROM TRUE OR item.deprecated_at IS NOT NULL
    OR catalog.code IS DISTINCT FROM 'reaction-types' OR catalog.active IS DISTINCT FROM TRUE
    OR state.code IS DISTINCT FROM 'published' OR state.active IS DISTINCT FROM TRUE
    OR state.workflow_id IS DISTINCT FROM catalog.workflow_id;
  IF invalid_rows<>0 THEN
    RAISE EXCEPTION 'canonical event-moment reaction gate failed: invalidRows=%', invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_definition SET cache_revision=cache_revision+1, updated_at=now(), version=version+1
WHERE code='reaction-types' AND EXISTS (
  SELECT 1 FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid
);

UPDATE catalog_backfill_run SET
  status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'eventMomentReactionRows', (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'canonicalReferencesMapped', (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'copiedStringsRemoved', (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=:'backfill_run_id'::uuid AND original_reaction IS NOT NULL),
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
