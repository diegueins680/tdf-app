BEGIN;

-- No mutation/backfill. Select the same provided identifier preference as ERN.
CREATE OR REPLACE FUNCTION music_ddex_release_identifier(version_id UUID)
RETURNS TEXT LANGUAGE SQL STABLE AS $$
  SELECT (CASE identifier_type WHEN 'grid' THEN 'grid:' ELSE 'icpn:' END)
    ||upper(translate(identifier_value,' -',''))
  FROM music_identifier WHERE release_version_id=version_id
    AND identifier_type IN ('grid','upc','ean')
    AND verification_status IN ('syntax_valid','authority_verified')
  ORDER BY CASE identifier_type WHEN 'grid' THEN 0 WHEN 'upc' THEN 1 ELSE 2 END,
    created_at,id LIMIT 1;
$$;

CREATE OR REPLACE FUNCTION music_check_ddex_operation(
  version_id UUID, sender_id UUID, recipient_id UUID, requested_operation TEXT)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT)
LANGUAGE SQL STABLE AS $$
  WITH version AS (SELECT * FROM music_release_version WHERE id=version_id)
  SELECT issue.* FROM music_check_ddex_export(version_id) issue
  WHERE NOT (issue.error_code='version_not_approved' AND requested_operation='takedown'
    AND EXISTS (SELECT 1 FROM version WHERE state='suspended' AND approved_at IS NOT NULL
      AND immutable_snapshot IS NOT NULL AND snapshot_sha256 IS NOT NULL))
  UNION ALL
  SELECT 'version','version_missing','La versión solicitada no existe.'
    WHERE NOT EXISTS (SELECT 1 FROM version)
  UNION ALL
  SELECT 'operation','invalid_export_operation','Usa new_release, update o takedown.'
    WHERE requested_operation IS NULL OR requested_operation NOT IN ('new_release','update','takedown')
  UNION ALL
  SELECT 'operation','invalid_export_state','La operación DDEX no corresponde al estado actual de la versión.'
  FROM version WHERE
    (requested_operation IN ('new_release','update') AND state NOT IN ('approved','scheduled','published'))
    OR (requested_operation='update' AND correction_of_version_id IS NULL AND replaces_version_id IS NULL)
    OR (requested_operation='takedown' AND state NOT IN ('suspended','replacement_pending','takedown_scheduled','withdrawn'))
  UNION ALL
  SELECT 'version.takedownAtUtc','takedown_not_due',
    'El mensaje sin deals retira inmediatamente: espera a la fecha programada antes de generarlo.'
  FROM version WHERE requested_operation='takedown' AND state='takedown_scheduled'
    AND takedown_at_utc > NOW()
  UNION ALL
  SELECT 'operation','initial_export_missing',
    'Se requiere un new_release válido del adaptador v5 para este release, identificador, remitente y destinatario. Los paquetes legados requieren revisión; validado no significa entregado.'
  FROM version WHERE requested_operation IN ('update','takedown') AND NOT EXISTS (
    SELECT 1 FROM music_ddex_export prior
    JOIN music_release_version prior_version ON prior_version.id=prior.release_version_id
    WHERE prior_version.release_id=version.release_id AND prior.status='valid'
      AND prior.validation_report->>'adapterVersion'='tdf-ern432-audio-v5'
      AND prior.operation='new_release' AND prior.sender_registry_id=sender_id
      AND prior.recipient_registry_id=recipient_id
      AND music_ddex_release_identifier(prior.release_version_id)=music_ddex_release_identifier(version_id)
  );
$$;
COMMIT;
