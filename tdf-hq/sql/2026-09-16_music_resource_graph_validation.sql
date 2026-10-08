-- Apply after party_details, correction_asset_graph and correction_concurrency.
-- Quiesce authoring/review. Read-only validation; never rewrite approved data.
BEGIN;
LOCK TABLE music_release_version, music_asset IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION music_check_resource_graph(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT)
LANGUAGE SQL STABLE AS $$
  WITH RECURSIVE assets AS (
    SELECT * FROM music_asset WHERE release_version_id=version_id
      AND asset_role NOT IN ('ddex_xml','ddex_manifest','ddex_package')
  ), rooted AS (
    SELECT id FROM assets WHERE parent_asset_id IS NULL
    UNION
    SELECT child.id FROM assets child JOIN rooted parent ON child.parent_asset_id=parent.id
  )
  SELECT 'assets.'||asset.id||'.parentAssetId','resource_parent_outside_version',
    'El padre del recurso no pertenece al grafo de esta versión. Solicita revisar su procedencia; no se modifica el original.'
  FROM assets asset WHERE asset.parent_asset_id IS NOT NULL
    AND NOT EXISTS (SELECT 1 FROM assets parent WHERE parent.id=asset.parent_asset_id)
  UNION ALL
  SELECT 'assets.'||asset.id||'.parentAssetId','resource_graph_unrooted',
    'El recurso no llega a un original raíz: hay un ciclo o una cadena desconectada. Solicita revisar la procedencia antes de enviar.'
  FROM assets asset WHERE NOT EXISTS (SELECT 1 FROM rooted WHERE rooted.id=asset.id)
  UNION ALL
  SELECT 'assets.'||asset.id||'.recordingId','resource_recording_outside_version',
    'El recurso apunta a una grabación que no está entre las pistas de esta versión.'
  FROM assets asset WHERE asset.recording_id IS NOT NULL AND NOT EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id AND track.recording_id=asset.recording_id)
  UNION ALL
  SELECT 'assets.'||asset.id||'.recordingId','audio_recording_required',
    'Asocia el recurso de audio a una grabación de esta versión.'
  FROM assets asset WHERE asset.asset_role IN ('master_audio','stream_audio','preview_audio')
    AND asset.recording_id IS NULL
  UNION ALL
  SELECT 'assets.'||asset.id||'.parentAssetId','resource_parent_incompatible',
    'El tipo de recurso o su grabación no coincide con el padre declarado. Revisa la cadena de procesamiento.'
  FROM assets asset JOIN assets parent ON parent.id=asset.parent_asset_id
  WHERE (asset.asset_role IN ('stream_audio','preview_audio')
      AND (asset.recording_id IS DISTINCT FROM parent.recording_id
        OR parent.asset_role NOT IN ('master_audio','stream_audio','preview_audio')
        OR (asset.asset_role='stream_audio' AND parent.asset_role='preview_audio')))
    OR (asset.asset_role IN ('cover_display','thumbnail')
      AND (parent.asset_role NOT IN ('cover_original','cover_display','thumbnail')
        OR asset.recording_id IS DISTINCT FROM parent.recording_id))
    OR asset.asset_role IN ('master_audio','cover_original')
  UNION ALL
  SELECT 'rights.'||rights.id||'.recordingId','rights_recording_outside_version',
    'La declaración de derechos apunta a una grabación ajena a esta versión.'
  FROM music_rights_declaration rights WHERE rights.release_version_id=version_id
    AND rights.recording_id IS NOT NULL AND NOT EXISTS (
      SELECT 1 FROM music_release_track track WHERE track.release_version_id=version_id
        AND track.recording_id=rights.recording_id)
  UNION ALL
  SELECT 'rights.'||rights.id||'.evidenceAssetId','rights_evidence_outside_version',
    'La evidencia de derechos debe ser un recurso de esta versión, de la misma grabación o de ámbito general.'
  FROM music_rights_declaration rights WHERE rights.release_version_id=version_id
    AND rights.evidence_asset_id IS NOT NULL AND NOT EXISTS (
      SELECT 1 FROM assets evidence WHERE evidence.id=rights.evidence_asset_id
        AND (evidence.recording_id IS NULL OR evidence.recording_id=rights.recording_id))
  UNION ALL
  SELECT 'credits.'||credit.id||'.recordingId','credit_recording_outside_version',
    'El crédito apunta a una grabación ajena a esta versión.'
  FROM music_credit credit WHERE credit.release_version_id=version_id
    AND credit.recording_id IS NOT NULL AND NOT EXISTS (
      SELECT 1 FROM music_release_track track WHERE track.release_version_id=version_id
        AND track.recording_id=credit.recording_id)
  UNION ALL
  SELECT 'availability.'||rule.id||'.releaseTrackId','availability_track_outside_version',
    'La disponibilidad apunta a una pista ajena a esta versión.'
  FROM music_availability_rule rule WHERE rule.release_version_id=version_id
    AND rule.release_track_id IS NOT NULL AND NOT EXISTS (
      SELECT 1 FROM music_release_track track
      WHERE track.id=rule.release_track_id AND track.release_version_id=version_id)
  UNION ALL
  SELECT 'availability.'||rule.id||'.downloadableAssetId','download_resource_outside_scope',
    'Selecciona un activo descargable de esta versión y, si la regla es por pista, de su grabación.'
  FROM music_availability_rule rule WHERE rule.release_version_id=version_id
    AND rule.downloadable_asset_id IS NOT NULL AND NOT EXISTS (
      SELECT 1 FROM assets asset WHERE asset.id=rule.downloadable_asset_id
        AND (rule.release_track_id IS NULL OR EXISTS (
          SELECT 1 FROM music_release_track track WHERE track.id=rule.release_track_id
            AND track.release_version_id=version_id AND track.recording_id=asset.recording_id)));
$$;

DO $$ BEGIN
  IF to_regprocedure('music_check_submission_before_resource_graph(uuid)') IS NULL THEN
    ALTER FUNCTION music_check_submission(UUID) RENAME TO music_check_submission_before_resource_graph;
  END IF;
  IF to_regprocedure('music_check_ddex_export_before_resource_graph(uuid)') IS NULL THEN
    ALTER FUNCTION music_check_ddex_export(UUID) RENAME TO music_check_ddex_export_before_resource_graph;
  END IF;
  IF to_regprocedure('music_refresh_validation_flags_before_resource_graph(uuid)') IS NULL THEN
    ALTER FUNCTION music_refresh_validation_flags(UUID) RENAME TO music_refresh_validation_flags_before_resource_graph;
  END IF;
END $$;
CREATE OR REPLACE FUNCTION music_check_submission(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT) LANGUAGE SQL STABLE AS $$
  SELECT * FROM music_check_submission_before_resource_graph(version_id)
  UNION ALL SELECT * FROM music_check_resource_graph(version_id);
$$;
CREATE OR REPLACE FUNCTION music_check_ddex_export(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT) LANGUAGE SQL STABLE AS $$
  SELECT * FROM music_check_ddex_export_before_resource_graph(version_id)
  UNION ALL SELECT * FROM music_check_resource_graph(version_id);
$$;

CREATE OR REPLACE FUNCTION music_refresh_validation_flags(version_id UUID)
RETURNS TABLE(metadata_valid BOOLEAN,assets_valid BOOLEAN,rights_valid BOOLEAN,access_valid BOOLEAN)
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM * FROM music_refresh_validation_flags_before_resource_graph(version_id);
  UPDATE music_release_version version SET assets_valid=version.assets_valid
    AND NOT EXISTS (SELECT 1 FROM music_check_resource_graph(version_id))
  WHERE version.id=version_id
    AND version.state IN ('draft','uploading','processing','validation_failed','ready_for_review','changes_requested');
  RETURN QUERY SELECT version.metadata_valid,version.assets_valid,version.rights_valid,version.access_valid
    FROM music_release_version version WHERE version.id=version_id;
END;
$$;

-- Operator-only read model. No queue entries, inferred metadata or repaired bytes.
CREATE OR REPLACE VIEW music_resource_graph_sanitation_queue AS
SELECT version.release_id,version.id AS release_version_id,version.version_number,
  version.state,issue.field_path,issue.error_code,issue.message
FROM music_release_version version
CROSS JOIN LATERAL music_check_resource_graph(version.id) issue;
COMMIT;
