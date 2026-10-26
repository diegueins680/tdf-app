import pathlib,json,hashlib,subprocess,os
ROOT=pathlib.Path('/private/tmp/tdf-player-listening-identity-20261026')
OUT=pathlib.Path('/private/tmp/tdf-audio-live-catalog-prep-20261026')
canonical=(ROOT/'ops/hetzner/inspect-runtime.py').read_bytes();(OUT/'canonical-inspect-runtime.py').write_bytes(canonical)
PREFIX="BEGIN READ ONLY;\nSET LOCAL ROLE tdf_catalog_inventory;\nSET LOCAL statement_timeout = '2s';\nSET LOCAL lock_timeout = '500ms';\nSET LOCAL search_path = pg_catalog, public;\n"
ROLES=['master_audio','stream_audio','preview_audio','cover_original','cover_display','thumbnail','rights_evidence','ddex_xml','ddex_manifest','ddex_package']
ASSET_STATES=['quarantined','uploaded','inspecting','valid','invalid','processing','ready','failed','deleted']
VERSION_STATES=['draft','uploading','processing','validation_failed','ready_for_review','in_review','changes_requested','approved','scheduled','published','suspended','cancelled','replacement_pending','takedown_scheduled','withdrawn']
JOB_STATES=['queued','running','retry','succeeded','failed','dead_letter','cancelled']
UPLOAD_STATES=['initiated','uploading','completing','completed','cancelled','expired','failed']
RELATIONS=['music_asset','music_release','music_release_version','music_release_track','music_recording','music_processing_job','music_upload_session','artist_release']
CAP_STATES=['select_available','permission_denied','relation_missing','rls_coverage_unobserved','unexpected_relation_kind']
def literals(items):return ','.join("'"+s+"'" for s in items)
def values(items):return ','.join("('"+s+"')" for s in items)
def wrapped(sql):return PREFIX+sql.strip().rstrip(';')+';\nROLLBACK;\n'
def count_object(metrics):return 'SELECT pg_catalog.jsonb_build_object(\n'+',\n'.join(" '"+k+"',("+v+")" for k,v in metrics.items())+'\n)'
def spec(name,relations,shape,sql,note):return {'name':name,'requiredRelations':relations,'shape':shape,'sql':wrapped(sql),'interpretation':note}
def state_query(table,column,states):
 return f"""WITH counts AS (
 SELECT CASE WHEN {column} IN ({literals(states)}) THEN {column} ELSE '__other__' END AS label,count(*) AS n
 FROM public.{table} GROUP BY 1
 ), labels(label) AS (VALUES {values(states+['__other__'])})
 SELECT pg_catalog.jsonb_object_agg(labels.label,coalesce(counts.n,0))
 FROM labels LEFT JOIN counts ON counts.label=labels.label"""
identity=spec('inventory_session_identity',[],{k:True for k in ['databaseMatches','roleMatches','sessionRoleMatches','localSocket','portMatches','readOnly','serverMajorMatches','restrictedRole']},"""SELECT pg_catalog.jsonb_build_object(
 'databaseMatches',current_database()='tdf_hq',
 'roleMatches',current_user='tdf_catalog_inventory',
 'sessionRoleMatches',session_user='tdf_catalog_inventory',
 'localSocket',inet_server_addr() IS NULL,
 'portMatches',current_setting('port')='5432',
 'readOnly',current_setting('transaction_read_only')='on',
 'serverMajorMatches',split_part(current_setting('server_version'),'.',1)='17',
 'restrictedRole',(SELECT NOT rolsuper AND NOT rolbypassrls AND NOT rolcreaterole AND NOT rolcreatedb AND NOT rolinherit FROM pg_catalog.pg_roles WHERE rolname=current_user))""",'All booleans must be true; otherwise no business aggregate runs.')
capsql=f"""WITH names(name) AS (VALUES {values(RELATIONS)}), resolved AS (
 SELECT name,pg_catalog.to_regclass('public.'||name) AS oid FROM names)
 SELECT pg_catalog.jsonb_object_agg(name,
 CASE WHEN resolved.oid IS NULL THEN 'relation_missing'
 WHEN relkind NOT IN ('r','p') THEN 'unexpected_relation_kind'
 WHEN relrowsecurity OR relforcerowsecurity THEN 'rls_coverage_unobserved'
 WHEN NOT pg_catalog.has_table_privilege(current_user,resolved.oid,'SELECT') THEN 'permission_denied'
 ELSE 'select_available' END)
 FROM resolved LEFT JOIN pg_catalog.pg_class ON pg_class.oid=resolved.oid"""
capabilities=spec('relation_capabilities',[],{r:CAP_STATES for r in RELATIONS},capsql,'Fixed table allowlist only. RLS,missing SELECT or missing relation means affected counts are UNOBSERVED; do not infer zeros.')
asset_sql=f"""WITH counts AS (
 SELECT CASE WHEN asset_role IN ({literals(ROLES)}) THEN asset_role ELSE '__other__' END AS role,
 CASE WHEN processing_state IN ({literals(ASSET_STATES)}) THEN processing_state ELSE '__other__' END AS state,count(*) AS n
 FROM public.music_asset GROUP BY 1,2
 ), roles(role) AS (VALUES {values(ROLES+['__other__'])}), states(state) AS (VALUES {values(ASSET_STATES+['__other__'])}), role_counts AS (
 SELECT roles.role,pg_catalog.jsonb_object_agg(states.state,coalesce(counts.n,0)) AS states
 FROM roles CROSS JOIN states LEFT JOIN counts ON counts.role=roles.role AND counts.state=states.state GROUP BY roles.role)
 SELECT pg_catalog.jsonb_object_agg(role,states) FROM role_counts"""
queries=[spec('assets_by_role_and_processing_state',['music_asset'],{r:{s:0 for s in ASSET_STATES+['__other__']} for r in ROLES+['__other__']},asset_sql,'Canonical music_asset database rows only; __other__ counts unexpected code values without exporting them. No stored-object existence, MIME validation, rights or playability claim.'),
spec('release_totals',['music_release'],{k:0 for k in ['total','publishedPointerPresent','withdrawnTimestampPresent']},"SELECT pg_catalog.jsonb_build_object('total',count(*),'publishedPointerPresent',count(*) FILTER (WHERE published_version_id IS NOT NULL),'withdrawnTimestampPresent',count(*) FILTER (WHERE withdrawn_at IS NOT NULL)) FROM public.music_release",'Publication pointer/timestamps are database facts,not discoverability or authorization.'),
spec('release_versions_by_state',['music_release_version'],{s:0 for s in VERSION_STATES+['__other__']},state_query('music_release_version','state',VERSION_STATES),'Fixed lifecycle-code counts; published state alone is not an authorized delivery claim.'),
spec('processing_jobs_by_state',['music_processing_job'],{s:0 for s in JOB_STATES+['__other__']},state_query('music_processing_job','status',JOB_STATES),'Status counts only; no job keys,worker IDs,error codes or failure summaries are selected.'),
spec('upload_sessions_by_state',['music_upload_session'],{s:0 for s in UPLOAD_STATES+['__other__']},state_query('music_upload_session','status',UPLOAD_STATES),'Canonical upload statuses only; multipart/object/provider IDs and names never leave the host.')]
ready={
 'readyAssetsMissingVersion':"SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=a.release_version_id)",
 'readyAudioMissingRecording':"SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND a.asset_role IN ('master_audio','stream_audio','preview_audio') AND (a.recording_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=a.recording_id))",
 'readyDerivedAudioMissingParent':"SELECT count(*) FROM public.music_asset a WHERE a.processing_state='ready' AND a.asset_role IN ('stream_audio','preview_audio') AND (a.parent_asset_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_asset p WHERE p.id=a.parent_asset_id))",
 'readyDerivedAudioCrossContextParent':"SELECT count(*) FROM public.music_asset a JOIN public.music_asset p ON p.id=a.parent_asset_id WHERE a.processing_state='ready' AND a.asset_role IN ('stream_audio','preview_audio') AND (p.release_version_id IS DISTINCT FROM a.release_version_id OR p.recording_id IS DISTINCT FROM a.recording_id)",
 'readyAssetsWithoutReadyTimestamp':"SELECT count(*) FROM public.music_asset WHERE processing_state='ready' AND ready_at IS NULL",
 'readyAssetsWithBlankObjectReference':"SELECT count(*) FROM public.music_asset WHERE processing_state='ready' AND (bucket_name IS NULL OR object_key IS NULL OR btrim(bucket_name)='' OR btrim(object_key)='')"}
queries.append(spec('ready_asset_reference_gaps',['music_asset','music_release_version','music_recording'],{k:0 for k in ready},count_object(ready),'Database structural references and blank reference strings only. Zero may follow validated constraints; it does not establish stored-object existence or byte access.'))
published={
 'releasePointersMissingOrWrongVersion':"SELECT count(*) FROM public.music_release r WHERE r.published_version_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=r.published_version_id AND v.release_id=r.id)",
 'publishedVersionsMissingRelease':"SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_release r WHERE r.id=v.release_id)",
 'publishedVersionsWithoutTracks':"SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.release_version_id=v.id)",
 'publishedTracksMissingRecording':"SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=t.recording_id)",
 'publishedTracksWithoutValidatedImmutableMaster':"SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.recording_id=t.recording_id AND a.asset_role='master_audio' AND a.processing_state IN ('valid','ready') AND a.immutable)",
 'publishedTracksWithoutReadyStream':"SELECT count(*) FROM public.music_release_track t JOIN public.music_release_version v ON v.id=t.release_version_id WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.recording_id=t.recording_id AND a.asset_role='stream_audio' AND a.processing_state='ready')",
 'publishedVersionsWithoutValidatedImmutableCoverOriginal':"SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.asset_role='cover_original' AND a.processing_state IN ('valid','ready') AND a.immutable)",
 'publishedVersionsWithoutReadyCoverDisplay':"SELECT count(*) FROM public.music_release_version v WHERE v.state='published' AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.release_version_id=v.id AND a.asset_role='cover_display' AND a.processing_state='ready')"}
queries.append(spec('published_reference_gaps',['music_release','music_release_version','music_release_track','music_recording','music_asset'],{k:0 for k in published},count_object(published),'DB predicates mirror relevant cover/master/stream checks in music validation SQL; not full publication validation,rights,visibility,storage delivery or current API eligibility.'))
orphans={
 'versionsMissingRelease':"SELECT count(*) FROM public.music_release_version v WHERE NOT EXISTS(SELECT 1 FROM public.music_release r WHERE r.id=v.release_id)",
 'tracksMissingVersion':"SELECT count(*) FROM public.music_release_track t WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=t.release_version_id)",
 'tracksMissingRecording':"SELECT count(*) FROM public.music_release_track t WHERE NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=t.recording_id)",
 'assetsMissingVersion':"SELECT count(*) FROM public.music_asset a WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=a.release_version_id)",
 'assetsWithDanglingRecording':"SELECT count(*) FROM public.music_asset a WHERE a.recording_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_recording r WHERE r.id=a.recording_id)",
 'assetsWithDanglingParent':"SELECT count(*) FROM public.music_asset a WHERE a.parent_asset_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_asset p WHERE p.id=a.parent_asset_id)",
 'completedUploadsMissingAsset':"SELECT count(*) FROM public.music_upload_session u WHERE u.status='completed' AND (u.completed_asset_id IS NULL OR NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.id=u.completed_asset_id))",
 'jobsMissingVersion':"SELECT count(*) FROM public.music_processing_job j WHERE NOT EXISTS(SELECT 1 FROM public.music_release_version v WHERE v.id=j.release_version_id)",
 'jobsWithDanglingSourceAsset':"SELECT count(*) FROM public.music_processing_job j WHERE j.source_asset_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM public.music_asset a WHERE a.id=j.source_asset_id)",
 'recordingsWithoutTrackReference':"SELECT count(*) FROM public.music_recording r WHERE NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.recording_id=r.id)",
 'audioAssetsWithoutTrackContext':"SELECT count(*) FROM public.music_asset a WHERE a.asset_role IN ('master_audio','stream_audio','preview_audio') AND NOT EXISTS(SELECT 1 FROM public.music_release_track t WHERE t.release_version_id=a.release_version_id AND t.recording_id=a.recording_id)"}
queries.append(spec('structural_orphan_references',['music_release','music_release_version','music_release_track','music_recording','music_asset','music_upload_session','music_processing_job'],{k:0 for k in orphans},count_object(orphans),'Named structural/reference counts only. Unreferenced draft recordings/assets can be legitimate; no deletion,automatic repair,duplicate-content or storage-orphan claim.'))
queries.append(spec('legacy_artist_release_reference_totals',['artist_release'],{k:0 for k in ['total','spotifyReferencePresent','youtubeReferencePresent']},"SELECT pg_catalog.jsonb_build_object('total',count(*),'spotifyReferencePresent',count(*) FILTER(WHERE spotify_url IS NOT NULL AND btrim(spotify_url)<>''),'youtubeReferencePresent',count(*) FILTER(WHERE youtube_url IS NOT NULL AND btrim(youtube_url)<>'')) FROM public.artist_release",'Only legacy reference presence; provider pages are not automatically native audio,available,owned or authorized. Other uploads/social/podcasts/storage are outside this collector.'))
bundle={'schemaVersion':1,'identity':identity,'capabilities':capabilities,'relations':RELATIONS,'capabilityStates':CAP_STATES,'queries':queries}
(OUT/'query-spec.json').write_text(json.dumps(bundle,indent=2)+'\n')
(OUT/'fixed-queries.sql').write_text('\n'.join('-- '+q['name']+'\n'+q['sql'] for q in [identity,capabilities,*queries]))
(OUT/'source-provenance.json').write_text(json.dumps({'repository':str(ROOT),'sourceRevision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),'canonicalPath':'ops/hetzner/inspect-runtime.py','canonicalSha256':hashlib.sha256(canonical).hexdigest(),'canonicalCopiedBytesEqual':True,'schemaSources':['tdf-hq/sql/2026-09-11_music_release_platform.sql','tdf-hq/src/TDF/Models.hs'],'scope':'Preparation only. Canonical guard functions are copied byte-for-byte,not reimplemented. No SSH execution or production observation.'},indent=2)+'\n')
for p in OUT.iterdir():
 if p.is_file():p.chmod(0o600)
print('Prepared',len(queries),'fixed aggregate families plus2inventory-role preflights;NO SSH')
