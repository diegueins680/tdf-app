-- inventory_session_identity
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object(
 'databaseMatches',current_database()='tdf_hq',
 'roleMatches',current_user='tdf_catalog_inventory',
 'sessionRoleMatches',session_user='tdf_catalog_inventory',
 'localSocket',inet_server_addr() IS NULL,
 'portMatches',current_setting('port')='5432',
 'readOnly',current_setting('transaction_read_only')='on',
 'serverMajorMatches',split_part(current_setting('server_version'),'.',1)='17',
 'restrictedRole',(SELECT NOT rolsuper AND NOT rolbypassrls AND NOT rolcreaterole AND NOT rolcreatedb AND NOT rolinherit FROM pg_catalog.pg_roles WHERE rolname=current_user));
ROLLBACK;

-- relation_capabilities
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
WITH names(name) AS (VALUES ('music_asset'),('music_release'),('music_release_version'),('music_release_track'),('music_recording'),('music_processing_job'),('music_upload_session'),('artist_release')), resolved AS (
 SELECT name,pg_catalog.to_regclass('public.'||name) AS oid FROM names)
 SELECT pg_catalog.jsonb_object_agg(name,
 CASE WHEN resolved.oid IS NULL THEN 'relation_missing'
 WHEN relkind NOT IN ('r','p') THEN 'unexpected_relation_kind'
 WHEN relrowsecurity OR relforcerowsecurity THEN 'rls_coverage_unobserved'
 WHEN NOT pg_catalog.has_table_privilege(current_user,resolved.oid,'SELECT') THEN 'permission_denied'
 ELSE 'select_available' END)
 FROM resolved LEFT JOIN pg_catalog.pg_class ON pg_class.oid=resolved.oid;
ROLLBACK;

-- assets_by_role_and_processing_state
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
WITH counts AS (
 SELECT CASE WHEN asset_role IN ('master_audio','stream_audio','preview_audio','cover_original','cover_display','thumbnail','rights_evidence','ddex_xml','ddex_manifest','ddex_package') THEN asset_role ELSE '__other__' END AS role,
 CASE WHEN processing_state IN ('quarantined','uploaded','inspecting','valid','invalid','processing','ready','failed','deleted') THEN processing_state ELSE '__other__' END AS state,count(*) AS n
 FROM public.music_asset GROUP BY 1,2
 ), roles(role) AS (VALUES ('master_audio'),('stream_audio'),('preview_audio'),('cover_original'),('cover_display'),('thumbnail'),('rights_evidence'),('ddex_xml'),('ddex_manifest'),('ddex_package'),('__other__')), states(state) AS (VALUES ('quarantined'),('uploaded'),('inspecting'),('valid'),('invalid'),('processing'),('ready'),('failed'),('deleted'),('__other__')), role_counts AS (
 SELECT roles.role,pg_catalog.jsonb_object_agg(states.state,coalesce(counts.n,0)) AS states
 FROM roles CROSS JOIN states LEFT JOIN counts ON counts.role=roles.role AND counts.state=states.state GROUP BY roles.role)
 SELECT pg_catalog.jsonb_object_agg(role,states) FROM role_counts;
ROLLBACK;

-- release_totals
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object('total',count(*),'publishedPointerPresent',count(*) FILTER (WHERE published_version_id IS NOT NULL),'withdrawnTimestampPresent',count(*) FILTER (WHERE withdrawn_at IS NOT NULL)) FROM public.music_release;
ROLLBACK;

-- release_versions_by_state
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
WITH counts AS (
 SELECT CASE WHEN state IN ('draft','uploading','processing','validation_failed','ready_for_review','in_review','changes_requested','approved','scheduled','published','suspended','cancelled','replacement_pending','takedown_scheduled','withdrawn') THEN state ELSE '__other__' END AS label,count(*) AS n
 FROM public.music_release_version GROUP BY 1
 ), labels(label) AS (VALUES ('draft'),('uploading'),('processing'),('validation_failed'),('ready_for_review'),('in_review'),('changes_requested'),('approved'),('scheduled'),('published'),('suspended'),('cancelled'),('replacement_pending'),('takedown_scheduled'),('withdrawn'),('__other__'))
 SELECT pg_catalog.jsonb_object_agg(labels.label,coalesce(counts.n,0))
 FROM labels LEFT JOIN counts ON counts.label=labels.label;
ROLLBACK;

-- processing_jobs_by_state
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
WITH counts AS (
 SELECT CASE WHEN status IN ('queued','running','retry','succeeded','failed','dead_letter','cancelled') THEN status ELSE '__other__' END AS label,count(*) AS n
 FROM public.music_processing_job GROUP BY 1
 ), labels(label) AS (VALUES ('queued'),('running'),('retry'),('succeeded'),('failed'),('dead_letter'),('cancelled'),('__other__'))
 SELECT pg_catalog.jsonb_object_agg(labels.label,coalesce(counts.n,0))
 FROM labels LEFT JOIN counts ON counts.label=labels.label;
ROLLBACK;

-- upload_sessions_by_state
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
WITH counts AS (
 SELECT CASE WHEN status IN ('initiated','uploading','completing','completed','cancelled','expired','failed') THEN status ELSE '__other__' END AS label,count(*) AS n
 FROM public.music_upload_session GROUP BY 1
 ), labels(label) AS (VALUES ('initiated'),('uploading'),('completing'),('completed'),('cancelled'),('expired'),('failed'),('__other__'))
 SELECT pg_catalog.jsonb_object_agg(labels.label,coalesce(counts.n,0))
 FROM labels LEFT JOIN counts ON counts.label=labels.label;
ROLLBACK;

-- ready_asset_reference_gaps
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object(
 'readyAssetsMissingVersion',(SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=a.release_version_id)),
 'readyAudioMissingRecording',(SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND a.asset_role IN ('master_audio','stream_audio','preview_audio') AND (a.recording_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=a.recording_id))),
 'readyDerivedAudioMissingParent',(SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND a.asset_role IN ('stream_audio','preview_audio') AND (a.parent_asset_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_asset p WHERE p.id=a.parent_asset_id))),
 'readyDerivedAudioCrossContextParent',(SELECT count(*) FROM public.music_asset a JOIN public.music_asset p ON p.id=a.parent_asset_id WHERE a.processing_state='ready' AND a.asset_role IN ('stream_audio','preview_audio') AND (p.release_version_id IS DISTINCT FROM a.release_version_id OR p.recording_id IS DISTINCT FROM a.recording_id)),
 'readyAssetsWithoutReadyTimestamp',(SELECT count(*) FROM public.music_asset WHERE processing_state='ready' AND ready_at IS NULL),
 'readyAssetsWithBlankObjectReference',(SELECT count(*) FROM public.music_asset WHERE processing_state='ready' AND (bucket_name IS NULL OR object_key IS NULL OR btrim(bucket_name)='' OR btrim(object_key)=''))
);
ROLLBACK;

-- published_reference_gaps
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object(
 'releasePointersMissingOrWrongVersion',(SELECT count(*) FROM public.music_release r WHERE r.published_version_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=r.published_version_id AND v.release_id=r.id)),
 'publishedVersionsMissingRelease',(SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_release r WHERE r.id=v.release_id)),
 'publishedVersionsWithoutTracks',(SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.release_version_id=v.id)),
 'publishedTracksMissingRecording',(SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=t.recording_id)),
 'publishedTracksWithoutValidatedImmutableMaster',(SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.recording_id=t.recording_id AND a.asset_role='master_audio' AND a.processing_state IN ('valid','ready') AND a.immutable)),
 'publishedTracksWithoutReadyStream',(SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.recording_id=t.recording_id AND a.asset_role='stream_audio' AND a.processing_state='ready')),
 'publishedVersionsWithoutValidatedImmutableCoverOriginal',(SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.asset_role='cover_original' AND a.processing_state IN ('valid','ready') AND a.immutable)),
 'publishedVersionsWithoutReadyCoverDisplay',(SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.asset_role='cover_display' AND a.processing_state='ready'))
);
ROLLBACK;

-- structural_orphan_references
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object(
 'versionsMissingRelease',(SELECT count(*) FROM public.music_release_version v WHERE NOT EXISTS(SELECT 1 FROM public.music_release r WHERE r.id=v.release_id)),
 'tracksMissingVersion',(SELECT count(*) FROM public.music_release_track t WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=t.release_version_id)),
 'tracksMissingRecording',(SELECT count(*) FROM public.music_release_track t WHERE NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=t.recording_id)),
 'assetsMissingVersion',(SELECT count(*) FROM public.music_asset a WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=a.release_version_id)),
 'assetsWithDanglingRecording',(SELECT count(*) FROM public.music_asset a WHERE a.recording_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=a.recording_id)),
 'assetsWithDanglingParent',(SELECT count(*) FROM public.music_asset a WHERE a.parent_asset_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_asset p WHERE p.id=a.parent_asset_id)),
 'completedUploadsMissingAsset',(SELECT count(*) FROM public.music_upload_session u WHERE u.status='completed' AND (u.completed_asset_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.id=u.completed_asset_id))),
 'jobsMissingVersion',(SELECT count(*) FROM public.music_processing_job j WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=j.release_version_id)),
 'jobsWithDanglingSourceAsset',(SELECT count(*) FROM public.music_processing_job j WHERE j.source_asset_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.id=j.source_asset_id)),
 'recordingsWithoutTrackReference',(SELECT count(*) FROM public.music_recording r WHERE NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.recording_id=r.id)),
 'audioAssetsWithoutTrackContext',(SELECT count(*) FROM public.music_asset a WHERE a.asset_role IN ('master_audio','stream_audio','preview_audio') AND NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.release_version_id=a.release_version_id AND t.recording_id=a.recording_id))
);
ROLLBACK;

-- legacy_artist_release_reference_totals
BEGIN READ ONLY;
SET LOCAL ROLE tdf_catalog_inventory;
SET LOCAL statement_timeout = '2s';
SET LOCAL lock_timeout = '500ms';
SET LOCAL search_path = pg_catalog, public;
SELECT pg_catalog.jsonb_build_object('total',count(*),'spotifyReferencePresent',count(*) FILTER(WHERE spotify_url IS NOT NULL AND btrim(spotify_url)<>''),'youtubeReferencePresent',count(*) FILTER(WHERE youtube_url IS NOT NULL AND btrim(youtube_url)<>'')) FROM public.artist_release;
ROLLBACK;
