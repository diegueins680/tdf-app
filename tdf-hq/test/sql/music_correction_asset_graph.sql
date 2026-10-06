-- Disposable migration fixture only. All inserted rows are rolled back.
BEGIN;
SELECT set_config('test.music_source', :'source_id', true);
SELECT set_config('test.music_actor', :'actor_id', true);
SELECT set_config('test.music_fixed', :'expect_fixed', true);

CREATE FUNCTION pg_temp.approve_music_fixture(version_id UUID, actor_id BIGINT)
RETURNS VOID LANGUAGE plpgsql AS $$
BEGIN
  UPDATE music_release_version_party SET details_source='user_provided'
    WHERE release_version_id=version_id;
  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
    VALUES (version_id,'publication_authority','synthetic-chain-test',actor_id,'{"fixture":true}');
  PERFORM music_refresh_validation_flags(version_id);
  UPDATE music_release_version SET state='ready_for_review' WHERE id=version_id;
  UPDATE music_release_version SET state='in_review' WHERE id=version_id;
  UPDATE music_release_version SET state='approved',approved_by=actor_id,approved_at=NOW()
    WHERE id=version_id;
END;
$$;

DO $$
DECLARE
  original_id UUID := current_setting('test.music_source')::UUID;
  actor_id BIGINT := current_setting('test.music_actor')::BIGINT;
  expect_fixed BOOLEAN := current_setting('test.music_fixed')::BOOLEAN;
  release_id UUID;
  source_id UUID;
  copy_id UUID;
  master_id UUID;
  recording_id UUID;
  bad_id UUID;
  before_counts BIGINT[];
  after_counts BIGINT[];
  source_graph JSONB;
  generation INTEGER;
  invalid_kind TEXT;
  constraint_name TEXT;
BEGIN
  SELECT v.release_id INTO release_id FROM music_release_version v WHERE id=original_id;
  source_id := music_create_release_correction(release_id,original_id,actor_id);
  SELECT a.id,a.recording_id INTO master_id,recording_id FROM music_asset a
    WHERE a.release_version_id=source_id AND a.asset_role='master_audio';
  -- Equal timestamps, deliberately reverse UUID order: old sorting must fail,
  -- regardless of random ids in the rest of the graph.
  INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
    storage_provider,bucket_name,object_key,media_type,byte_size,sha256,
    processing_state,immutable,created_by,created_at,ready_at)
  VALUES
    ('f0000000-0000-4000-8000-000000000001',source_id,recording_id,master_id,
      'stream_audio','local_private','synthetic-private','chain/stream.m4a','audio/mp4',
      42,repeat('8',64),'ready',true,actor_id,NOW(),NOW()),
    ('10000000-0000-4000-8000-000000000001',source_id,recording_id,
      'f0000000-0000-4000-8000-000000000001','preview_audio','local_private',
      'synthetic-private','chain/preview.m4a','audio/mp4',21,repeat('9',64),
      'ready',true,actor_id,NOW(),NOW()),
    ('20000000-0000-4000-8000-000000000001',source_id,recording_id,
      '10000000-0000-4000-8000-000000000001','preview_audio','local_private',
      'synthetic-private','chain/short-preview.m4a','audio/mp4',10,repeat('a',64),
      'ready',true,actor_id,NOW(),NOW());
  PERFORM pg_temp.approve_music_fixture(source_id,actor_id);

  IF NOT expect_fixed THEN
    BEGIN
      PERFORM music_create_release_correction(release_id,source_id,actor_id);
      RAISE EXCEPTION 'Legacy correction unexpectedly accepted the reverse-ordered chain';
    EXCEPTION WHEN check_violation THEN
      GET STACKED DIAGNOSTICS constraint_name=CONSTRAINT_NAME;
      IF constraint_name <> 'music_asset_check1' THEN RAISE; END IF;
    END;
    RAISE NOTICE 'PASS legacy defect reproduced deterministically (music_asset_check1)';
    RETURN;
  END IF;

  FOR generation IN 1..5 LOOP
    SELECT jsonb_agg(to_jsonb(a) ORDER BY a.id) INTO source_graph
      FROM music_asset a WHERE a.release_version_id=source_id;
    copy_id := music_create_release_correction(release_id,source_id,actor_id);
    IF (SELECT count(*) FROM music_asset WHERE release_version_id=copy_id) <> 8
       OR (SELECT count(*) FROM music_terms_acceptance WHERE release_version_id=copy_id) <> 0
       OR EXISTS (
         SELECT 1 FROM music_asset copy
         LEFT JOIN music_asset source ON source.id=(copy.provenance->>'correctionSourceAssetId')::UUID
         LEFT JOIN music_asset parent ON parent.id=copy.parent_asset_id
         LEFT JOIN music_release_track track ON track.recording_id=copy.recording_id
           AND track.release_version_id=copy_id
         WHERE copy.release_version_id=copy_id AND (
           source.id IS NULL OR source.release_version_id<>source_id
           OR ROW(copy.storage_provider,copy.storage_class,copy.bucket_name,copy.object_key,
             copy.sha256,copy.byte_size,copy.technical_metadata,copy.immutable,copy.ready_at)
             IS DISTINCT FROM ROW(source.storage_provider,source.storage_class,source.bucket_name,
               source.object_key,source.sha256,source.byte_size,source.technical_metadata,
               source.immutable,source.ready_at)
           OR (source.parent_asset_id IS NOT NULL AND
             (parent.release_version_id IS DISTINCT FROM copy_id OR
               parent.provenance->>'correctionSourceAssetId' IS DISTINCT FROM source.parent_asset_id::TEXT))
           OR (copy.recording_id IS NOT NULL AND
             (track.id IS NULL OR copy.recording_id=source.recording_id))
         )
       ) THEN RAISE EXCEPTION 'Correction lost resources, provenance or parent/recording mapping'; END IF;
    IF source_graph IS DISTINCT FROM (
      SELECT jsonb_agg(to_jsonb(a) ORDER BY a.id) FROM music_asset a WHERE a.release_version_id=source_id
    ) THEN RAISE EXCEPTION 'Correction mutated source assets'; END IF;
    IF (SELECT count(*) FROM music_release_version_party WHERE release_version_id=copy_id) <> 2
       OR EXISTS (
         SELECT 1 FROM music_release_version_party copy
         JOIN music_release_version_party source ON source.release_version_id=source_id
           AND source.music_party_id=copy.music_party_id
         WHERE copy.release_version_id=copy_id
           AND ROW(copy.party_details,copy.details_source) IS DISTINCT FROM
             ROW(source.party_details,source.details_source)
       ) THEN RAISE EXCEPTION 'Correction changed versioned party evidence'; END IF;
    PERFORM pg_temp.approve_music_fixture(copy_id,actor_id);
    source_id := copy_id;
  END LOOP;
  RAISE NOTICE 'PASS five chained corrections: four resource levels, private locators, hashes, parties, no inherited terms';

  FOREACH invalid_kind IN ARRAY ARRAY['cycle','external_parent','excluded_parent','external_recording'] LOOP
    source_id := music_create_release_correction(release_id,original_id,actor_id);
    SELECT a.id,a.recording_id INTO master_id,recording_id FROM music_asset a
      WHERE a.release_version_id=source_id AND a.asset_role='master_audio';
    bad_id := gen_random_uuid();
    INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
      storage_provider,bucket_name,object_key,media_type,byte_size,sha256,
      processing_state,created_by,ready_at)
    VALUES (bad_id,source_id,recording_id,master_id,'preview_audio','local_private',
      'synthetic-private','invalid/preview.m4a','audio/mp4',1,repeat('e',64),'ready',actor_id,NOW());
    IF invalid_kind='cycle' THEN
      UPDATE music_asset SET parent_asset_id=bad_id WHERE id=bad_id;
    ELSIF invalid_kind='external_parent' THEN
      UPDATE music_asset SET parent_asset_id=(SELECT id FROM music_asset
        WHERE release_version_id=original_id AND asset_role='master_audio') WHERE id=bad_id;
    ELSIF invalid_kind='excluded_parent' THEN
      INSERT INTO music_asset(release_version_id,asset_role,storage_provider,bucket_name,
        object_key,media_type,byte_size,sha256,processing_state,created_by,ready_at)
      VALUES (source_id,'ddex_xml','local_private','synthetic-private',
        'invalid/release.xml','application/xml',1,repeat('f',64),'ready',actor_id,NOW())
      RETURNING id INTO master_id;
      UPDATE music_asset SET parent_asset_id=master_id WHERE id=bad_id;
    ELSE
      UPDATE music_asset SET recording_id=(SELECT a.recording_id FROM music_asset a
        WHERE a.release_version_id=original_id AND a.asset_role='master_audio') WHERE id=bad_id;
    END IF;
    PERFORM pg_temp.approve_music_fixture(source_id,actor_id);
    SELECT ARRAY[(SELECT count(*) FROM music_release_version),(SELECT count(*) FROM music_asset),
      (SELECT count(*) FROM music_recording),(SELECT count(*) FROM music_credit),
      (SELECT count(*) FROM music_release_version_party),(SELECT count(*) FROM music_release_audit_event)]
      INTO before_counts;
    BEGIN
      PERFORM music_create_release_correction(release_id,source_id,actor_id);
      RAISE EXCEPTION 'Invalid graph accepted: %',invalid_kind;
    EXCEPTION WHEN check_violation THEN
      IF SQLERRM NOT LIKE 'correction asset%' THEN RAISE; END IF;
    END;
    SELECT ARRAY[(SELECT count(*) FROM music_release_version),(SELECT count(*) FROM music_asset),
      (SELECT count(*) FROM music_recording),(SELECT count(*) FROM music_credit),
      (SELECT count(*) FROM music_release_version_party),(SELECT count(*) FROM music_release_audit_event)]
      INTO after_counts;
    IF before_counts IS DISTINCT FROM after_counts THEN
      RAISE EXCEPTION 'Failed correction left partial rows: %',invalid_kind;
    END IF;
  END LOOP;
  RAISE NOTICE 'PASS cycle/external parent/excluded DDEX parent/external recording: atomic rejection';
END;
$$;
ROLLBACK;
