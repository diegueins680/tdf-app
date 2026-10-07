-- Additive preview policy. Apply after 2026-09-11_music_release_platform.sql.
-- No master/media rows are rewritten and no published version is backfilled.
BEGIN;

CREATE OR REPLACE FUNCTION music_preview_spec(duration_ms BIGINT, start_ms BIGINT, length_ms BIGINT)
RETURNS JSONB LANGUAGE SQL IMMUTABLE AS $$
  SELECT CASE
    WHEN duration_ms IS NULL OR duration_ms <= 0 THEN NULL
    WHEN start_ms IS NULL AND length_ms IS NULL THEN jsonb_build_object(
      'startMs',CASE WHEN duration_ms > 90000 THEN 30000 ELSE 0 END,
      'durationMs',LEAST(duration_ms,30000),'selection','auto')
    WHEN length_ms > 0 AND COALESCE(start_ms,0) >= 0
      AND COALESCE(start_ms,0) < duration_ms
      AND length_ms <= duration_ms - COALESCE(start_ms,0)
      THEN jsonb_build_object('startMs',COALESCE(start_ms,0),'durationMs',length_ms,'selection','explicit')
    ELSE NULL END;
$$;

CREATE OR REPLACE FUNCTION music_preview_matches(asset_id UUID)
RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM music_asset asset
    JOIN music_release_track track ON track.release_version_id=asset.release_version_id
      AND track.recording_id=asset.recording_id
    JOIN music_recording recording ON recording.id=track.recording_id
    WHERE asset.id=asset_id AND asset.asset_role='preview_audio'
      AND asset.processing_state='ready' AND asset.immutable
      AND asset.technical_metadata->'preview' = music_preview_spec(
        recording.duration_ms,track.preview_start_ms,track.preview_duration_ms)
  );
$$;

DO $$ BEGIN
  IF to_regprocedure('music_check_submission_before_preview(uuid)') IS NULL THEN
    ALTER FUNCTION music_check_submission(UUID) RENAME TO music_check_submission_before_preview;
  END IF;
  IF to_regprocedure('music_public_asset_accessible_before_preview(uuid,text)') IS NULL THEN
    ALTER FUNCTION music_public_asset_accessible(UUID,TEXT) RENAME TO music_public_asset_accessible_before_preview;
  END IF;
END $$;

CREATE OR REPLACE FUNCTION music_check_submission(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT) LANGUAGE SQL STABLE AS $$
  SELECT * FROM music_check_submission_before_preview(version_id)
  UNION ALL
  SELECT 'tracks.'||track.id::text||'.preview',
    CASE WHEN music_preview_spec(recording.duration_ms,track.preview_start_ms,track.preview_duration_ms) IS NULL
      THEN 'preview_range_invalid' ELSE 'preview_processing_required' END,
    CASE WHEN music_preview_spec(recording.duration_ms,track.preview_start_ms,track.preview_duration_ms) IS NULL
      THEN 'Selecciona un inicio y duración dentro de la duración técnica de la pista.'
      ELSE 'El preview no corresponde al rango actual. Guarda el borrador y espera el reprocesamiento.' END
  FROM music_release_track track JOIN music_recording recording ON recording.id=track.recording_id
  WHERE track.release_version_id=version_id
    AND (track.preview_duration_ms IS NOT NULL OR EXISTS (
      SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=version_id
        AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id)
        AND rule.listening_policy='preview'))
    AND NOT EXISTS (SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version_id
      AND asset.recording_id=track.recording_id AND music_preview_matches(asset.id));
$$;

CREATE OR REPLACE FUNCTION music_public_asset_accessible(target_asset_id UUID,territory_code TEXT)
RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT music_public_asset_accessible_before_preview(target_asset_id,territory_code)
    AND EXISTS (SELECT 1 FROM music_asset asset WHERE asset.id=target_asset_id
      AND (asset.asset_role<>'preview_audio' OR music_preview_matches(asset.id)));
$$;

-- Called by the worker loop. Serialize with editorial changes through the version
-- row, retain old assets, and enqueue one job per source/range. A range edited while
-- a job runs will be picked up on the next iteration; stale previews never pass
-- submission/access. Failed jobs retain normal retry/dead-letter semantics.
CREATE OR REPLACE FUNCTION music_queue_preview_jobs(batch_size INTEGER DEFAULT 100)
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE candidate RECORD; spec JSONB; inserted_count INTEGER := 0; inserted_id UUID;
BEGIN
  FOR candidate IN
    SELECT version.id AS version_id,track.recording_id,track.preview_start_ms,
      track.preview_duration_ms,recording.duration_ms,source.id AS source_id
    FROM music_release_version version
    JOIN music_release_track track ON track.release_version_id=version.id
    JOIN music_recording recording ON recording.id=track.recording_id
    JOIN LATERAL (SELECT asset.id FROM music_asset asset
      WHERE asset.release_version_id=version.id AND asset.recording_id=track.recording_id
        AND asset.asset_role='stream_audio' AND asset.media_type='audio/flac'
        AND asset.processing_state='ready' AND asset.immutable
        AND asset.technical_metadata->'normalized'='true'::jsonb
      ORDER BY asset.created_at DESC,asset.id LIMIT 1) source ON TRUE
    WHERE version.state IN ('draft','uploading','processing','validation_failed','changes_requested')
      AND music_preview_spec(recording.duration_ms,track.preview_start_ms,track.preview_duration_ms) IS NOT NULL
      AND NOT EXISTS (SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version.id
        AND asset.recording_id=track.recording_id AND music_preview_matches(asset.id))
      AND NOT EXISTS (SELECT 1 FROM music_processing_job job
        WHERE job.release_version_id=version.id AND job.status IN ('queued','running','retry')
          AND job.job_kind IN ('inspect_audio','create_preview'))
    ORDER BY version.updated_at,track.id
    LIMIT GREATEST(0,LEAST(batch_size,1000)) FOR NO KEY UPDATE OF version SKIP LOCKED
  LOOP
    spec := music_preview_spec(candidate.duration_ms,candidate.preview_start_ms,candidate.preview_duration_ms);
    INSERT INTO music_processing_job(release_version_id,source_asset_id,job_kind,job_key,output)
    VALUES(candidate.version_id,candidate.source_id,'create_preview',
      'preview-v2:'||candidate.source_id::text||':'||md5(spec::text),
      jsonb_build_object('preview',spec)) ON CONFLICT(job_kind,job_key) DO NOTHING RETURNING id INTO inserted_id;
    IF inserted_id IS NOT NULL THEN
      inserted_count := inserted_count + 1;
      -- Existing state machine permits all editable states to enter processing.
      UPDATE music_release_version SET state='processing',assets_valid=FALSE,updated_at=NOW()
        WHERE id=candidate.version_id;
      INSERT INTO music_release_audit_event(release_id,release_version_id,event_type,data)
        SELECT release_id,id,'preview_reprocessing_queued',jsonb_build_object(
          'jobId',inserted_id,'recordingId',candidate.recording_id,'preview',spec)
        FROM music_release_version WHERE id=candidate.version_id;
    END IF;
  END LOOP;
  RETURN inserted_count;
END;
$$;
COMMIT;
