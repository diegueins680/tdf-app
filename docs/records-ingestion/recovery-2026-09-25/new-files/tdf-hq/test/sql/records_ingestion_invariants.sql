DO $$
DECLARE
  source bigint;
  run uuid;
  payload jsonb := jsonb_build_object('id','testVIDEO01','channelId','UCx9Jpaw_XDrMtIdzWYlU51g','privacyStatus','public','uploadStatus','processed','eligible',true,'verifiedAt','2026-09-21T00:00:00Z','publishedAt','2026-09-19T00:00:00Z','title','Synthetic fixture','description','Fixture only','durationSeconds',120,'thumbnailUrl','https://i.ytimg.com/vi/testVIDEO01/hqdefault.jpg');
  result text;
  old_count bigint;
BEGIN
  INSERT INTO social_sync_account(platform,external_user_id,records_ingestion) SELECT 'youtube','UCx9Jpaw_XDrMtIdzWYlU51g',jsonb_build_object('enabled',true,'approvalReference','fixture-approved','approvedAt',now(),'collectionId',id) FROM editorial_collection WHERE code='tdf-records-recordings' RETURNING id INTO source;
  INSERT INTO catalog_backfill_run(id,run_code,candidate_revision,dry_run,status,safety_threshold,scanned_rows,mapped_rows,ambiguous_rows,rejected_rows,started_at,report,correlation_id) VALUES(gen_random_uuid(),'runtime-fixture','youtube-api-v1',false,'running',0,0,0,0,0,now(),jsonb_build_object('sourceAccountId',source::text)::text,'fixture') RETURNING id INTO run;
  BEGIN
    PERFORM tdf_ingest_public_video(source,run,payload);
    RAISE EXCEPTION 'TEST: stopped ingestion accepted';
  EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
  UPDATE records_ingestion_control SET enabled=true;
  result:=tdf_ingest_public_video(source,run,payload);
  IF result<>'created' THEN RAISE EXCEPTION 'TEST: not created: %',result; END IF;
  IF NOT EXISTS(SELECT 1 FROM recording r JOIN collection_recording c ON c.recording_id=r.id WHERE r.code='youtube-recording-testVIDEO01' AND r.active AND c.sort_order<0) THEN RAISE EXCEPTION 'TEST: public membership missing'; END IF;
  SELECT count(*) INTO old_count FROM records_ingestion_change;
  result:=tdf_ingest_public_video(source,run,payload||jsonb_build_object('verifiedAt','2026-09-21T01:00:00Z'));
  IF result<>'unchanged' OR (SELECT count(*) FROM records_ingestion_change)<>old_count THEN RAISE EXCEPTION 'TEST: replay side effects'; END IF;
  IF (SELECT count(*) FROM recording WHERE code='youtube-recording-testVIDEO01')<>1 THEN RAISE EXCEPTION 'TEST: duplicate recording'; END IF;
  UPDATE recording SET title_es='Protected editorial title' WHERE code='youtube-recording-testVIDEO01';
  UPDATE record_external_resource SET thumbnail_url='https://example.org/approved-editorial.jpg' WHERE external_code='testVIDEO01';
  PERFORM tdf_ingest_public_video(source,run,payload||jsonb_build_object('title','Updated provider','verifiedAt','2026-09-21T02:00:00Z'));
  IF (SELECT title_es FROM recording WHERE code='youtube-recording-testVIDEO01')<>'Protected editorial title' OR (SELECT thumbnail_url FROM record_external_resource WHERE external_code='testVIDEO01')<>'https://example.org/approved-editorial.jpg' THEN RAISE EXCEPTION 'TEST: editorial edit overwritten'; END IF;
  IF (SELECT title_en FROM recording WHERE code='youtube-recording-testVIDEO01')<>'Updated provider' THEN RAISE EXCEPTION 'TEST: owned title not refreshed'; END IF;
  result:=tdf_ingest_public_video(source,run,payload);
  IF result<>'stale' THEN RAISE EXCEPTION 'TEST: stale response accepted'; END IF;
  BEGIN
    PERFORM tdf_ingest_public_video(source,run,payload||jsonb_build_object('privacyStatus','private'));
    RAISE EXCEPTION 'TEST: private content accepted';
  EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
  BEGIN
    PERFORM tdf_ingest_public_video(source,run,payload||jsonb_build_object('thumbnailUrl','https://i.ytimg.com/vi/WRONGVIDEO1/hqdefault.jpg'));
    RAISE EXCEPTION 'TEST: mismatched thumbnail accepted';
  EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
  UPDATE social_sync_account SET records_ingestion=records_ingestion-'approvedAt' WHERE id=source;
  BEGIN
    PERFORM tdf_ingest_public_video(source,run,payload);
    RAISE EXCEPTION 'TEST: unapproved source accepted';
  EXCEPTION WHEN raise_exception THEN IF SQLERRM LIKE 'TEST:%' THEN RAISE; END IF; END;
END $$;
