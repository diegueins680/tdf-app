\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'instrument-input-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 100
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-instrument-input-cutover-v1', 0));
SELECT set_config('tdf.catalog_safety_threshold', :'safety_threshold', TRUE);

-- Nullable UUID columns are an online-compatible expand step. The candidate
-- application refuses to serve until the deterministic backfill below has
-- populated every required input-row reference.
ALTER TABLE input_row ADD COLUMN IF NOT EXISTS instrument_id uuid;
ALTER TABLE live_session_intake ADD COLUMN IF NOT EXISTS primary_genre_id uuid;
ALTER TABLE live_session_musician ADD COLUMN IF NOT EXISTS instrument_id uuid;

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

CREATE TABLE IF NOT EXISTS catalog_input_reference_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  input_row_id uuid NOT NULL,
  original_instrument text,
  original_instrument_id uuid,
  original_mic_id uuid,
  target_instrument_id uuid NOT NULL,
  target_mic_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, input_row_id)
);

CREATE TABLE IF NOT EXISTS catalog_live_session_reference_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  source_table text NOT NULL CHECK (source_table IN ('live_session_intake','live_session_musician')),
  source_id uuid NOT NULL,
  original_value text,
  original_role text,
  original_entity_id uuid,
  target_entity_id uuid NOT NULL,
  evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id, source_table, source_id)
);

CREATE TEMP TABLE reviewed_input_map (
  track_name text NOT NULL,
  microphone_observation text NOT NULL,
  instrument_code text NOT NULL,
  asset_name text NOT NULL,
  evidence text NOT NULL,
  PRIMARY KEY (track_name, microphone_observation)
) ON COMMIT DROP;

INSERT INTO reviewed_input_map VALUES
  ('Kick In','AKG D112','drums','AKG D112','reviewed kick input plus exact asset name'),
  ('Snare Up','Shure SM57','drums','Shure SM57','reviewed snare input plus exact asset name'),
  ('Snare Down','Shure SM57','drums','Shure SM57','reviewed snare input plus exact asset name'),
  ('Hi-Hat','Sennheiser MKE600','drums','Sennheiser MKE600','reviewed hi-hat input plus exact asset name'),
  ('Tom 1','Sennheiser MD421','drums','Sennheiser MD421','reviewed tom input plus exact asset name'),
  ('Tom Floor','Sennheiser MD421','drums','Sennheiser MD421','reviewed floor-tom input plus exact asset name'),
  ('OH L','AKG C414 (HC)','drums','AKG C414','reviewed overhead channel; HC is placement metadata'),
  ('OH R','AKG C414 (HC)','drums','AKG C414','reviewed overhead channel; HC is placement metadata'),
  ('Bass DI (post)','Neve RNDI','bass-guitar','Neve RNDI','reviewed bass DI plus exact active DI asset'),
  ('Bass Mic 1 (cab)','AKG D112','bass-guitar','AKG D112','reviewed bass cabinet input plus exact asset'),
  ('Bass Mic 2 (ataque)','Neumann KM184','bass-guitar','Neumann KM184','reviewed bass attack input plus exact asset'),
  ('Gtr 1','Sennheiser e906','electric-guitar','Sennheiser e906','reviewed electric-guitar input plus exact asset'),
  ('Gtr 1 Ribbon','Royer R121','electric-guitar','Royer R121','reviewed electric-guitar ribbon input plus exact asset'),
  ('Gtr 2','Sennheiser e906','electric-guitar','Sennheiser e906','reviewed electric-guitar input plus exact asset'),
  ('Gtr 2 Ribbon','Royer R121','electric-guitar','Royer R121','reviewed electric-guitar ribbon input plus exact asset'),
  ('Vox 1','Electro-Voice RE20','voice','Electro-Voice RE20','reviewed voice input plus exact asset'),
  ('Vox 2','Sennheiser e835','voice','Sennheiser e835','reviewed voice input plus exact asset'),
  ('Vox 3','Shure SM58','voice','Shure SM58','reviewed voice input plus exact asset'),
  ('Vox 4','Shure SM58','voice','Shure SM58','reviewed voice input plus exact asset'),
  ('KU-100 L','Neumann KU-100 L','voice','Neumann KU-100','reviewed left channel of one binaural microphone asset'),
  ('KU-100 R','Neumann KU-100 R','voice','Neumann KU-100','reviewed right channel of one binaural microphone asset');

CREATE TEMP TABLE resolved_input_rows ON COMMIT DROP AS
SELECT row.id, row.track_name, row.instrument AS original_instrument,
  row.instrument_id AS original_instrument_id, row.mic_id AS original_mic_id,
  reviewed.evidence,
  instrument_match.candidate_count AS instrument_candidates,
  instrument_match.target_id AS target_instrument_id,
  asset_match.candidate_count AS asset_candidates,
  asset_match.target_id AS target_mic_id
FROM input_row row
LEFT JOIN reviewed_input_map reviewed ON reviewed.track_name=btrim(row.track_name)
  AND reviewed.microphone_observation=btrim(row.instrument)
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM instrument item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='instruments' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE item.code=reviewed.instrument_code AND item.active AND item.deprecated_at IS NULL
) instrument_match ON TRUE
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(item.id ORDER BY item.id))[1] AS target_id
  FROM asset item
  WHERE item.name=reviewed.asset_name AND item.status='Active'
) asset_match ON TRUE
WHERE row.instrument IS NOT NULL;

DO $gate$
DECLARE
  source_rows bigint;
  invalid_candidates bigint;
  instrument_conflicts bigint;
  asset_conflicts bigint;
BEGIN
  SELECT count(*),
    count(*) FILTER (WHERE instrument_candidates<>1 OR asset_candidates<>1),
    count(*) FILTER (WHERE original_instrument_id IS NOT NULL AND original_instrument_id<>target_instrument_id),
    count(*) FILTER (WHERE original_mic_id IS NOT NULL AND original_mic_id<>target_mic_id)
  INTO source_rows, invalid_candidates, instrument_conflicts, asset_conflicts
  FROM resolved_input_rows;
  IF source_rows > current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_candidates<>0
      OR instrument_conflicts<>0 OR asset_conflicts<>0 THEN
    RAISE EXCEPTION
      'input reference safety gate failed: rows=%, invalidCandidates=%, instrumentConflicts=%, assetConflicts=%',
      source_rows, invalid_candidates, instrument_conflicts, asset_conflicts
      USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_input_reference_cutover_source (
  run_id, input_row_id, original_instrument, original_instrument_id, original_mic_id,
  target_instrument_id, target_mic_id, evidence
)
SELECT :'backfill_run_id'::uuid, id, original_instrument, original_instrument_id,
  original_mic_id, target_instrument_id, target_mic_id, evidence
FROM resolved_input_rows
ON CONFLICT (run_id, input_row_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, 'input_row', 'track_name:instrument-id', resolved.id::text,
  resolved.track_name, lower(btrim(resolved.track_name)), catalog.id, resolved.target_instrument_id,
  'mapped', resolved.evidence, 1, now()
FROM resolved_input_rows resolved CROSS JOIN catalog_definition catalog
WHERE catalog.code='instruments'
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status='mapped', evidence=EXCLUDED.evidence;

UPDATE input_row target SET
  instrument_id=resolved.target_instrument_id,
  mic_id=resolved.target_mic_id,
  instrument=NULL
FROM resolved_input_rows resolved
WHERE target.id=resolved.id AND target.instrument IS NOT NULL;

CREATE TEMP TABLE resolved_live_session_references ON COMMIT DROP AS
SELECT 'live_session_intake'::text AS source_table, intake.id AS source_id,
  intake.primary_genre AS original_value, NULL::text AS original_role,
  intake.primary_genre_id AS original_entity_id,
  genre_match.target_id AS target_entity_id,
  'unique normalized genre code/name match'::text AS evidence,
  genre_match.candidate_count
FROM live_session_intake intake
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(candidate.id ORDER BY candidate.id))[1] AS target_id
  FROM genre candidate
  JOIN catalog_definition catalog ON catalog.id=candidate.catalog_id
    AND catalog.code='genres' AND catalog.active
  JOIN workflow_state state ON state.id=candidate.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE candidate.active AND candidate.deprecated_at IS NULL
    AND lower(btrim(intake.primary_genre)) IN (
      lower(candidate.code), lower(candidate.name_es), lower(candidate.name_en)
    )
) genre_match ON TRUE
WHERE intake.primary_genre IS NOT NULL
UNION ALL
SELECT 'live_session_musician', musician.id, musician.instrument, musician.role,
  musician.instrument_id, instrument_match.target_id,
  'unique normalized instrument code/name match', instrument_match.candidate_count
FROM live_session_musician musician
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,
    (array_agg(candidate.id ORDER BY candidate.id))[1] AS target_id
  FROM instrument candidate
  JOIN catalog_definition catalog ON catalog.id=candidate.catalog_id
    AND catalog.code='instruments' AND catalog.active
  JOIN workflow_state state ON state.id=candidate.workflow_state_id
    AND state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE candidate.active AND candidate.deprecated_at IS NULL
    AND lower(btrim(musician.instrument)) IN (
      lower(candidate.code), lower(candidate.name_es), lower(candidate.name_en)
    )
) instrument_match ON TRUE
WHERE (musician.instrument IS NOT NULL OR musician.role IS NOT NULL)
;

DO $gate$
DECLARE
  source_rows bigint;
  invalid_rows bigint;
BEGIN
  SELECT count(*), count(*) FILTER (
    WHERE candidate_count<>1 OR target_entity_id IS NULL
      OR (source_table='live_session_musician' AND original_role IS NOT NULL
          AND lower(btrim(original_role))<>lower(btrim(original_value)))
      OR (original_entity_id IS NOT NULL AND original_entity_id<>target_entity_id)
  ) INTO source_rows, invalid_rows
  FROM resolved_live_session_references;
  IF source_rows > current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0 THEN
    RAISE EXCEPTION
      'live-session reference safety gate failed: rows=%, invalidRows=%',
      source_rows, invalid_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

INSERT INTO catalog_live_session_reference_cutover_source (
  run_id, source_table, source_id, original_value, original_role,
  original_entity_id, target_entity_id, evidence
)
SELECT :'backfill_run_id'::uuid, source_table, source_id, original_value,
  original_role, original_entity_id, target_entity_id, evidence
FROM resolved_live_session_references WHERE candidate_count=1
ON CONFLICT (run_id, source_table, source_id) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id, run_id, source_table, source_column, source_record_id, original_value,
  normalized_value, catalog_id, entity_id, status, evidence, source_count, created_at
)
SELECT gen_random_uuid(), :'backfill_run_id'::uuid, source_table,
  CASE WHEN source_table='live_session_intake' THEN 'primary_genre' ELSE 'instrument' END,
  source_id::text, original_value, lower(btrim(original_value)), catalog.id,
  target_entity_id, 'mapped', evidence, 1, now()
FROM resolved_live_session_references
JOIN catalog_definition catalog ON catalog.code=CASE
  WHEN source_table='live_session_intake' THEN 'genres' ELSE 'instruments' END
WHERE candidate_count=1
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id, status='mapped', evidence=EXCLUDED.evidence;

UPDATE live_session_intake target SET primary_genre_id=source.target_entity_id, primary_genre=NULL
FROM catalog_live_session_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.source_table='live_session_intake'
  AND source.source_id=target.id;

UPDATE live_session_musician target SET instrument_id=source.target_entity_id,
  instrument=NULL, role=NULL
FROM catalog_live_session_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.source_table='live_session_musician'
  AND source.source_id=target.id;

CREATE INDEX IF NOT EXISTS ix_input_row_instrument
  ON input_row (instrument_id, version_id, channel_number);
CREATE INDEX IF NOT EXISTS ix_live_session_musician_instrument
  ON live_session_musician (instrument_id, intake_id);
CREATE INDEX IF NOT EXISTS ix_live_session_intake_primary_genre
  ON live_session_intake (primary_genre_id, created_at DESC);

DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_input_row_instrument') THEN
    ALTER TABLE input_row ADD CONSTRAINT fk_input_row_instrument
      FOREIGN KEY (instrument_id) REFERENCES instrument(id) NOT VALID;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_live_session_musician_instrument') THEN
    ALTER TABLE live_session_musician ADD CONSTRAINT fk_live_session_musician_instrument
      FOREIGN KEY (instrument_id) REFERENCES instrument(id) NOT VALID;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='fk_live_session_intake_primary_genre') THEN
    ALTER TABLE live_session_intake ADD CONSTRAINT fk_live_session_intake_primary_genre
      FOREIGN KEY (primary_genre_id) REFERENCES genre(id) NOT VALID;
  END IF;
END $$;

ALTER TABLE input_row VALIDATE CONSTRAINT fk_input_row_instrument;
ALTER TABLE live_session_musician VALIDATE CONSTRAINT fk_live_session_musician_instrument;
ALTER TABLE live_session_intake VALIDATE CONSTRAINT fk_live_session_intake_primary_genre;

CREATE OR REPLACE FUNCTION catalog_validate_input_row_instrument() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.instrument IS NOT NULL THEN
    RAISE EXCEPTION 'input rows require instrument_id; copied instrument values are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NEW.instrument_id IS NULL OR NOT EXISTS (
    SELECT 1 FROM instrument item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='instruments' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
    WHERE item.id=NEW.instrument_id AND item.active AND item.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'input row requires an active published instrument' USING ERRCODE='23514';
  END IF;
  IF NEW.mic_id IS NULL OR NOT EXISTS (
    SELECT 1 FROM asset item WHERE item.id=NEW.mic_id AND item.status='Active'
  ) THEN
    RAISE EXCEPTION 'input row requires an active microphone or DI asset' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS input_row_instrument_integrity ON input_row;
CREATE TRIGGER input_row_instrument_integrity
  BEFORE INSERT OR UPDATE OF instrument, instrument_id, mic_id ON input_row
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_input_row_instrument();

CREATE OR REPLACE FUNCTION catalog_validate_live_session_musician_instrument() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.instrument IS NOT NULL OR NEW.role IS NOT NULL THEN
    RAISE EXCEPTION 'live-session musicians require instrument_id; copied instrument and role values are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NEW.instrument_id IS NOT NULL AND NOT EXISTS (
    SELECT 1 FROM instrument item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='instruments' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
    WHERE item.id=NEW.instrument_id AND item.active AND item.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'live-session musician requires an active published instrument' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS live_session_musician_instrument_integrity ON live_session_musician;
CREATE TRIGGER live_session_musician_instrument_integrity
  BEFORE INSERT OR UPDATE OF instrument, role, instrument_id ON live_session_musician
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_live_session_musician_instrument();

CREATE OR REPLACE FUNCTION catalog_validate_live_session_primary_genre() RETURNS trigger
LANGUAGE plpgsql AS $$ BEGIN
  IF NEW.primary_genre IS NOT NULL THEN
    RAISE EXCEPTION 'live-session intake requires primary_genre_id; copied genre values are migration evidence only' USING ERRCODE='23514';
  END IF;
  IF NEW.primary_genre_id IS NOT NULL AND NOT EXISTS (
    SELECT 1 FROM genre item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='genres' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id AND state.code='published' AND state.active
    WHERE item.id=NEW.primary_genre_id AND item.active AND item.deprecated_at IS NULL
  ) THEN
    RAISE EXCEPTION 'live-session intake requires an active published genre' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS live_session_intake_primary_genre_integrity ON live_session_intake;
CREATE TRIGGER live_session_intake_primary_genre_integrity
  BEFORE INSERT OR UPDATE OF primary_genre, primary_genre_id ON live_session_intake
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_live_session_primary_genre();

DO $gate$
DECLARE
  invalid_input_rows bigint;
  legacy_genre_rows bigint;
  legacy_musician_rows bigint;
BEGIN
  SELECT count(*) INTO invalid_input_rows
  FROM input_row WHERE instrument IS NOT NULL OR instrument_id IS NULL OR mic_id IS NULL;
  SELECT count(*) INTO legacy_genre_rows
  FROM live_session_intake WHERE primary_genre IS NOT NULL;
  SELECT count(*) INTO legacy_musician_rows
  FROM live_session_musician WHERE instrument IS NOT NULL OR role IS NOT NULL;
  IF invalid_input_rows<>0 OR legacy_genre_rows<>0 OR legacy_musician_rows<>0 THEN
    RAISE EXCEPTION
      'canonical reference gate failed: inputRows=%, genreRows=%, musicianRows=%',
      invalid_input_rows, legacy_genre_rows, legacy_musician_rows
      USING ERRCODE='23514';
  END IF;
END
$gate$;

UPDATE catalog_backfill_run SET
  status='completed', completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_input_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    + (SELECT count(*) FROM catalog_live_session_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_input_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    + (SELECT count(*) FROM catalog_live_session_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0, ambiguous_rows=0,
  report=jsonb_build_object(
    'inputRows', (SELECT count(*) FROM catalog_input_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'instrumentReferencesMapped', (SELECT count(*) FROM catalog_input_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'assetReferencesMapped', (SELECT count(*) FROM catalog_input_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'liveSessionRows', (SELECT count(*) FROM catalog_live_session_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'unresolved', 0, 'ambiguousOrWithheld', 0
  )::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'scanned', scanned_rows, 'mapped', mapped_rows, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
