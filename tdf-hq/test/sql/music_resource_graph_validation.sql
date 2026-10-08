-- Synthetic, metadata-only graph faults. No trigger disabled; rollback all rows.
BEGIN;
SELECT set_config('test.music_source', :'source_id', true);
SELECT set_config('test.music_actor', :'actor_id', true);
CREATE FUNCTION pg_temp.prepare_graph_fixture(source_id UUID, actor_id BIGINT)
RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE copy_id UUID; release_id UUID;
BEGIN
  SELECT version.release_id INTO release_id FROM music_release_version version WHERE id=source_id;
  copy_id := music_create_release_correction(release_id,source_id,actor_id);
  UPDATE music_release_version_party SET details_source='user_provided' WHERE release_version_id=copy_id;
  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
    VALUES (copy_id,'publication_authority','synthetic-resource-graph',actor_id,'{"fixture":true}');
  RETURN copy_id;
END;
$$;
DO $$
DECLARE
  source_id UUID := current_setting('test.music_source')::UUID;
  actor_id BIGINT := current_setting('test.music_actor')::BIGINT;
  copy_id UUID; master_id UUID; recording_id UUID; bad_id UUID; nested_id UUID;
  source_master UUID; source_recording UUID; source_track UUID;
  fault TEXT; expected_code TEXT;
BEGIN
  SELECT asset.id,asset.recording_id INTO source_master,source_recording
    FROM music_asset asset WHERE release_version_id=source_id AND asset_role='master_audio';
  SELECT id INTO source_track FROM music_release_track WHERE release_version_id=source_id LIMIT 1;
  FOREACH fault IN ARRAY ARRAY['cycle','external_parent','external_recording','missing_recording',
    'incompatible_parent','rights_evidence','credit_recording','rights_recording',
    'availability_track','download_asset'] LOOP
    copy_id := pg_temp.prepare_graph_fixture(source_id,actor_id);
    IF EXISTS (SELECT 1 FROM music_check_resource_graph(copy_id)) THEN
      RAISE EXCEPTION 'Valid cloned graph was rejected'; END IF;
    SELECT asset.id,asset.recording_id INTO master_id,recording_id
      FROM music_asset asset WHERE release_version_id=copy_id AND asset_role='master_audio';
    bad_id := gen_random_uuid();
    INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
      storage_provider,bucket_name,object_key,media_type,byte_size,sha256,
      processing_state,created_by,ready_at)
    VALUES (bad_id,copy_id,recording_id,master_id,'preview_audio','local_private','synthetic-private',
      'synthetic/graph-preview.m4a','audio/mp4',1,repeat('e',64),'ready',actor_id,NOW());
    CASE fault
      WHEN 'cycle' THEN
        UPDATE music_asset SET parent_asset_id=id WHERE id=bad_id;
        expected_code := 'resource_graph_unrooted';
      WHEN 'external_parent' THEN
        UPDATE music_asset SET parent_asset_id=source_master WHERE id=bad_id;
        expected_code := 'resource_parent_outside_version';
      WHEN 'external_recording' THEN
        UPDATE music_asset SET recording_id=source_recording WHERE id=bad_id;
        expected_code := 'resource_recording_outside_version';
      WHEN 'missing_recording' THEN
        UPDATE music_asset SET recording_id=NULL WHERE id=bad_id;
        expected_code := 'audio_recording_required';
      WHEN 'incompatible_parent' THEN
        UPDATE music_asset SET parent_asset_id=(SELECT id FROM music_asset
          WHERE release_version_id=copy_id AND asset_role='cover_original') WHERE id=bad_id;
        expected_code := 'resource_parent_incompatible';
      WHEN 'rights_evidence' THEN
        UPDATE music_rights_declaration SET evidence_asset_id=source_master WHERE release_version_id=copy_id;
        expected_code := 'rights_evidence_outside_version';
      WHEN 'credit_recording' THEN
        UPDATE music_credit SET recording_id=source_recording WHERE release_version_id=copy_id;
        expected_code := 'credit_recording_outside_version';
      WHEN 'rights_recording' THEN
        UPDATE music_rights_declaration SET recording_id=source_recording WHERE release_version_id=copy_id;
        expected_code := 'rights_recording_outside_version';
      WHEN 'availability_track' THEN
        UPDATE music_availability_rule SET release_track_id=source_track WHERE release_version_id=copy_id;
        expected_code := 'availability_track_outside_version';
      ELSE
        UPDATE music_availability_rule SET downloadable_asset_id=source_master WHERE release_version_id=copy_id;
        expected_code := 'download_resource_outside_scope';
    END CASE;
    IF NOT EXISTS (SELECT 1 FROM music_check_submission(copy_id) WHERE error_code=expected_code)
       OR NOT EXISTS (SELECT 1 FROM music_check_ddex_export(copy_id) WHERE error_code=expected_code)
       OR NOT EXISTS (SELECT 1 FROM music_resource_graph_sanitation_queue
         WHERE release_version_id=copy_id AND error_code=expected_code) THEN
      RAISE EXCEPTION 'Fault missing from submission/export/sanitation: %',fault; END IF;
    PERFORM music_refresh_validation_flags(copy_id);
    IF (SELECT assets_valid FROM music_release_version WHERE id=copy_id) THEN
      RAISE EXCEPTION 'Invalid graph retained valid asset flag: %',fault; END IF;
    BEGIN
      UPDATE music_release_version SET state='ready_for_review' WHERE id=copy_id;
      RAISE EXCEPTION 'Invalid graph reached review: %',fault;
    EXCEPTION WHEN check_violation THEN NULL;
    END;
    IF (SELECT state FROM music_release_version WHERE id=copy_id)<>'draft' THEN
      RAISE EXCEPTION 'Failed transition changed state'; END IF;
  END LOOP;
  RAISE NOTICE 'PASS ten resource-graph faults: field errors, export gate, sanitation, flags and database review barrier';

  -- Recheck at approval, not only on initial submission.
  copy_id := pg_temp.prepare_graph_fixture(source_id,actor_id);
  UPDATE music_release_version SET state='ready_for_review' WHERE id=copy_id;
  UPDATE music_release_version SET state='in_review' WHERE id=copy_id;
  SELECT asset.id,asset.recording_id INTO master_id,recording_id
    FROM music_asset asset WHERE release_version_id=copy_id AND asset_role='master_audio';
  bad_id := gen_random_uuid();
  INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
    storage_provider,bucket_name,object_key,media_type,byte_size,sha256,
    processing_state,created_by,ready_at)
  VALUES (bad_id,copy_id,recording_id,bad_id,'preview_audio','local_private','synthetic-private',
    'synthetic/review-cycle.m4a','audio/mp4',1,repeat('e',64),'ready',actor_id,NOW());
  nested_id := gen_random_uuid();
  INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
    storage_provider,bucket_name,object_key,media_type,byte_size,sha256,
    processing_state,created_by,ready_at)
  VALUES (nested_id,copy_id,recording_id,bad_id,'preview_audio','local_private','synthetic-private',
    'synthetic/nested-preview.m4a','audio/mp4',1,repeat('f',64),'ready',actor_id,NOW());
  UPDATE music_asset SET parent_asset_id=nested_id WHERE id=bad_id;
  IF (SELECT count(*) FROM music_check_resource_graph(copy_id)
      WHERE error_code='resource_graph_unrooted')<>2 THEN
    RAISE EXCEPTION 'Two-node cycle was not detected'; END IF;
  BEGIN
    UPDATE music_release_version SET state='approved',approved_by=actor_id,approved_at=NOW() WHERE id=copy_id;
    RAISE EXCEPTION 'Invalid graph approved';
  EXCEPTION WHEN check_violation THEN NULL;
  END;
  IF EXISTS (SELECT 1 FROM music_release_version WHERE id=copy_id
      AND (state<>'in_review' OR approved_at IS NOT NULL OR immutable_snapshot IS NOT NULL)) THEN
    RAISE EXCEPTION 'Failed approval left approval evidence'; END IF;
  UPDATE music_asset SET parent_asset_id=(SELECT id FROM music_asset
    WHERE release_version_id=copy_id AND asset_role='stream_audio' ORDER BY id LIMIT 1)
    WHERE id=bad_id;
  UPDATE music_release_version SET state='approved',approved_by=actor_id,approved_at=NOW() WHERE id=copy_id;
  IF EXISTS (SELECT 1 FROM music_check_resource_graph(copy_id)) THEN RAISE EXCEPTION 'Repair not recognized'; END IF;
  RAISE NOTICE 'PASS two-node cycle blocks approval; four-level valid parent repair restores approval without changing bytes';
END;
$$;
ROLLBACK;
