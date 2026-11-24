-- MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md).
-- Apply after 2026-10-12_music_upload_rebind. Additive: one table, its
-- lifecycle guard, two worker functions, one asset hook and two disabled flag
-- rows. No existing row or function is changed. Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';

-- One row per attempt to measure one master file. kind is 'loudness' today;
-- later audio features (tempo, key, fingerprint) add kinds and keep their
-- values in details. A measured loudness is two integers in tenths.
CREATE TABLE IF NOT EXISTS music_audio_measurement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  asset_id UUID NOT NULL REFERENCES music_asset(id) ON DELETE RESTRICT,
  source_sha256 TEXT NOT NULL CHECK (source_sha256 ~ '^[0-9a-f]{64}$'),
  kind TEXT NOT NULL CHECK (kind = 'loudness'),
  state TEXT NOT NULL DEFAULT 'pending' CHECK (state IN ('pending','measured','failed','superseded')),
  tool TEXT NOT NULL CHECK (tool ~ '^[a-z0-9][a-z0-9.-]{0,63}$'),
  tool_version TEXT CHECK (tool_version ~ '^[A-Za-z0-9.+~:_-]{1,64}$'),
  integrated_lufs_tenths INTEGER CHECK (integrated_lufs_tenths BETWEEN -699 AND 100),
  true_peak_dbtp_tenths INTEGER CHECK (true_peak_dbtp_tenths BETWEEN -1500 AND 200),
  failure_reason TEXT CHECK (failure_reason IN (
    'timeout','decode_error','no_summary','unparseable','out_of_range','silent',
    'tool_unavailable','interrupted')),
  details JSONB NOT NULL DEFAULT '{}'::JSONB,
  job_id UUID REFERENCES music_processing_job(id) ON DELETE RESTRICT,
  job_attempt INTEGER CHECK (job_attempt > 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  finished_at TIMESTAMPTZ,
  superseded_at TIMESTAMPTZ,
  CONSTRAINT music_audio_measurement_values CHECK (
    (integrated_lufs_tenths IS NOT NULL AND true_peak_dbtp_tenths IS NOT NULL
      AND failure_reason IS NULL AND finished_at IS NOT NULL) = (state = 'measured')
    OR (state = 'superseded' AND finished_at IS NOT NULL)),
  CONSTRAINT music_audio_measurement_failure CHECK (
    state <> 'failed' OR (failure_reason IS NOT NULL AND finished_at IS NOT NULL
      AND integrated_lufs_tenths IS NULL AND true_peak_dbtp_tenths IS NULL)),
  CONSTRAINT music_audio_measurement_pending CHECK (
    state <> 'pending' OR (finished_at IS NULL AND failure_reason IS NULL
      AND integrated_lufs_tenths IS NULL AND true_peak_dbtp_tenths IS NULL)),
  CONSTRAINT music_audio_measurement_closed CHECK ((state = 'superseded') = (superseded_at IS NOT NULL))
);

-- At most one row per asset and kind is not superseded.
CREATE UNIQUE INDEX IF NOT EXISTS music_audio_measurement_open
  ON music_audio_measurement(asset_id, kind) WHERE state <> 'superseded';
CREATE INDEX IF NOT EXISTS music_audio_measurement_job
  ON music_audio_measurement(job_id) WHERE job_id IS NOT NULL;

-- The lifecycle of formal/music/MusicLoudnessBinding.tla and
-- TDF.MusicRelease.Loudness.stepMeasurement: a row starts pending for the
-- asset's own SHA-256, closes once, and afterwards only becomes superseded.
CREATE OR REPLACE FUNCTION music_audio_measurement_guard()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NEW.state <> 'pending' THEN
      RAISE EXCEPTION 'A music audio measurement starts pending' USING ERRCODE = 'check_violation';
    END IF;
    PERFORM 1 FROM music_asset asset
      WHERE asset.id = NEW.asset_id AND asset.asset_role = 'master_audio'
        AND asset.sha256 = NEW.source_sha256;
    IF NOT FOUND THEN
      RAISE EXCEPTION 'A music audio measurement belongs to one master file and its SHA-256'
        USING ERRCODE = 'check_violation';
    END IF;
    RETURN NEW;
  END IF;
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Music audio measurements are kept' USING ERRCODE = 'check_violation';
  END IF;
  IF NEW.id IS DISTINCT FROM OLD.id OR NEW.asset_id IS DISTINCT FROM OLD.asset_id
     OR NEW.source_sha256 IS DISTINCT FROM OLD.source_sha256 OR NEW.kind IS DISTINCT FROM OLD.kind
     OR NEW.created_at IS DISTINCT FROM OLD.created_at THEN
    RAISE EXCEPTION 'The identity of a music audio measurement is fixed' USING ERRCODE = 'check_violation';
  END IF;
  IF NEW.state = OLD.state THEN
    IF NEW IS DISTINCT FROM OLD THEN
      RAISE EXCEPTION 'A music audio measurement changes only by a transition' USING ERRCODE = 'check_violation';
    END IF;
    RETURN NEW;
  END IF;
  IF (OLD.state = 'pending' AND NEW.state IN ('measured','failed','superseded'))
     OR (OLD.state = 'measured' AND NEW.state IN ('superseded'))
     OR (OLD.state = 'failed' AND NEW.state IN ('superseded')) THEN
    IF NEW.state = 'superseded' AND OLD.state <> 'pending' AND (
         NEW.integrated_lufs_tenths IS DISTINCT FROM OLD.integrated_lufs_tenths
         OR NEW.true_peak_dbtp_tenths IS DISTINCT FROM OLD.true_peak_dbtp_tenths
         OR NEW.failure_reason IS DISTINCT FROM OLD.failure_reason) THEN
      RAISE EXCEPTION 'Superseding keeps the recorded result' USING ERRCODE = 'check_violation';
    END IF;
    RETURN NEW;
  END IF;
  RAISE EXCEPTION 'Music audio measurement cannot go from % to %', OLD.state, NEW.state
    USING ERRCODE = 'check_violation';
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_audio_measurement_guard
  BEFORE INSERT OR UPDATE OR DELETE ON music_audio_measurement
  FOR EACH ROW EXECUTE FUNCTION music_audio_measurement_guard();

CREATE OR REPLACE FUNCTION music_supersede_audio_measurements(target_asset UUID, target_kind TEXT)
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE
  closed INTEGER;
BEGIN
  UPDATE music_audio_measurement
  SET state = 'superseded', superseded_at = clock_timestamp(),
      finished_at = COALESCE(finished_at, clock_timestamp())
  WHERE asset_id = target_asset AND state <> 'superseded'
    AND (target_kind IS NULL OR kind = target_kind);
  GET DIAGNOSTICS closed = ROW_COUNT;
  RETURN closed;
END;
$$;

-- Opens the pending row of an attempt. Returns NULL, writing nothing, when the
-- asset is not a master with the SHA-256 the worker verified.
CREATE OR REPLACE FUNCTION music_begin_audio_measurement(
  target_asset UUID, verified_sha256 TEXT, target_kind TEXT, tool_name TEXT,
  owner_job UUID, owner_attempt INTEGER)
RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE
  measurement UUID;
BEGIN
  PERFORM 1 FROM music_asset asset
    WHERE asset.id = target_asset AND asset.asset_role = 'master_audio'
      AND asset.sha256 = verified_sha256
    FOR UPDATE;
  IF NOT FOUND THEN
    RETURN NULL;
  END IF;
  PERFORM music_supersede_audio_measurements(target_asset, target_kind);
  INSERT INTO music_audio_measurement(asset_id, source_sha256, kind, tool, job_id, job_attempt)
  VALUES (target_asset, verified_sha256, target_kind, tool_name, owner_job, owner_attempt)
  RETURNING id INTO measurement;
  RETURN measurement;
END;
$$;

-- Closes the pending row an attempt opened, from the wrapper's one JSON
-- object. Anything that is not a complete in-range measurement is a recorded
-- failure. Returns FALSE, writing nothing, when the row is no longer pending
-- or belongs to another attempt.
CREATE OR REPLACE FUNCTION music_finish_audio_measurement(
  measurement UUID, owner_job UUID, owner_attempt INTEGER, outcome JSONB)
RETURNS BOOLEAN LANGUAGE plpgsql AS $$
DECLARE
  integrated INTEGER;
  true_peak INTEGER;
  reason TEXT;
  version TEXT;
BEGIN
  PERFORM 1 FROM music_audio_measurement
    WHERE id = measurement AND state = 'pending'
      AND job_id IS NOT DISTINCT FROM owner_job AND job_attempt IS NOT DISTINCT FROM owner_attempt
    FOR UPDATE;
  IF NOT FOUND THEN
    RETURN FALSE;
  END IF;
  IF jsonb_typeof(outcome) = 'object' AND jsonb_typeof(outcome->'ffmpegVersion') = 'string'
     AND outcome->>'ffmpegVersion' ~ '^[A-Za-z0-9.+~:_-]{1,64}$' THEN
    version := outcome->>'ffmpegVersion';
  END IF;
  IF jsonb_typeof(outcome) = 'object' AND outcome->>'state' = 'measured'
     AND jsonb_typeof(outcome->'integratedLufsTenths') = 'number'
     AND jsonb_typeof(outcome->'truePeakDbtpTenths') = 'number'
     AND outcome->>'integratedLufsTenths' ~ '^-?[0-9]{1,4}$'
     AND outcome->>'truePeakDbtpTenths' ~ '^-?[0-9]{1,4}$' THEN
    integrated := (outcome->>'integratedLufsTenths')::INTEGER;
    true_peak := (outcome->>'truePeakDbtpTenths')::INTEGER;
    IF integrated <= -700 THEN
      reason := 'silent';
    ELSIF integrated > 100 OR true_peak < -1500 OR true_peak > 200 THEN
      reason := 'out_of_range';
    END IF;
  ELSIF jsonb_typeof(outcome) = 'object' AND outcome->>'state' = 'failed'
     AND outcome->>'reason' IN ('timeout','decode_error','no_summary','unparseable',
       'out_of_range','silent','tool_unavailable','interrupted') THEN
    reason := outcome->>'reason';
  ELSE
    reason := 'unparseable';
  END IF;
  IF reason IS NULL THEN
    UPDATE music_audio_measurement
    SET state = 'measured', integrated_lufs_tenths = integrated, true_peak_dbtp_tenths = true_peak,
        tool_version = version, finished_at = clock_timestamp()
    WHERE id = measurement;
  ELSE
    UPDATE music_audio_measurement
    SET state = 'failed', failure_reason = reason, tool_version = version,
        finished_at = clock_timestamp()
    WHERE id = measurement;
  END IF;
  RETURN TRUE;
END;
$$;

-- A master whose stored object or declared content changes is another file:
-- whatever was measured before no longer describes it.
CREATE OR REPLACE FUNCTION music_asset_supersede_measurements()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.sha256 IS DISTINCT FROM OLD.sha256
     OR (NOT OLD.immutable AND (NEW.bucket_name, NEW.object_key) IS DISTINCT FROM (OLD.bucket_name, OLD.object_key)
         AND NEW.processing_state = 'uploaded') THEN
    PERFORM music_supersede_audio_measurements(NEW.id, NULL);
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_asset_supersede_measurements
  AFTER UPDATE OF sha256, bucket_name, object_key ON music_asset
  FOR EACH ROW
  WHEN (NEW.asset_role = 'master_audio')
  EXECUTE FUNCTION music_asset_supersede_measurements();

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('music_releases.loudness_check', FALSE, 'sandbox', 'Enable after the worker image passes the loudness conformance signals'),
  ('music_releases.loudness_check', FALSE, 'production', 'Enable after a music worker is deployed and the owner confirms the limits')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
