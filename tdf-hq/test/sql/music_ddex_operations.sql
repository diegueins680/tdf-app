-- Metadata-only operation fixtures, not valid ERN packages or real DPIDs.
BEGIN;
SELECT set_config('test.music_source', :'source_id', true);
SELECT set_config('test.music_actor', :'actor_id', true);
DO $$
DECLARE
  source_id UUID := current_setting('test.music_source')::UUID;
  actor_id BIGINT := current_setting('test.music_actor')::BIGINT;
  copy_id UUID; other_id UUID; sender_id UUID; recipient_id UUID; initial_id UUID; asset_id UUID;
BEGIN
  copy_id := music_create_release_correction(
    (SELECT release_id FROM music_release_version WHERE id=source_id),source_id,actor_id);
  UPDATE music_release_version_party SET details_source='user_provided' WHERE release_version_id=copy_id;
  INSERT INTO music_terms_acceptance(release_version_id,terms_kind,terms_version,accepted_by,evidence)
    VALUES(copy_id,'publication_authority','synthetic-operations',actor_id,'{"fixture":true}');
  UPDATE music_release_version SET state='ready_for_review' WHERE id=copy_id;
  UPDATE music_release_version SET state='in_review' WHERE id=copy_id;
  UPDATE music_release_version SET state='approved',approved_at=NOW(),approved_by=actor_id,
    immutable_snapshot='{"fixture":true}',snapshot_sha256=repeat('a',64) WHERE id=copy_id;
  INSERT INTO music_ddex_party_registry(party_name,dpid,party_role,verification_authority,
    verification_evidence,verified_by,verified_at)
    VALUES('Synthetic sender','SYNTHETICSENDER','sender','fixture','{"fixture":true}',actor_id,NOW())
    RETURNING id INTO sender_id;
  INSERT INTO music_ddex_party_registry(party_name,dpid,party_role,verification_authority,
    verification_evidence,verified_by,verified_at)
    VALUES('Synthetic recipient','SYNTHETICRECEIVER','recipient','fixture','{"fixture":true}',actor_id,NOW())
    RETURNING id INTO recipient_id;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'update')
    WHERE error_code='initial_export_missing') THEN RAISE EXCEPTION 'Missing initial export accepted'; END IF;
  SELECT id INTO asset_id FROM music_asset WHERE release_version_id=source_id LIMIT 1;
  -- Seed validation STATE only. Asset pointers are synthetic test stand-ins.
  INSERT INTO music_ddex_export(release_version_id,operation,standard,ern_version,release_profile,
    release_profile_version,avs_version,structural_dictionary_version,choreography,choreography_version,
    sender_registry_id,recipient_registry_id,sender_dpid,recipient_dpid,message_id,
    canonical_snapshot_sha256,idempotency_key,generated_by,status,xml_asset_id,manifest_asset_id,
    package_asset_id,package_sha256,generated_at,validation_report)
  VALUES(copy_id,'new_release','ERN','4.3.2','Audio','2.3.1','011','DD-ERN-432','Cloud Storage','1.8.1',
    sender_id,recipient_id,'SYNTHETICSENDER','SYNTHETICRECEIVER','operation-fixture',repeat('a',64),
    'operation-fixture',actor_id,'valid',asset_id,asset_id,asset_id,repeat('b',64),NOW(),
    '{"adapterVersion":"tdf-ern432-audio-v5","synthetic":true}') RETURNING id INTO initial_id;
  IF EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'update')
    WHERE error_code IN ('initial_export_missing','invalid_export_state')) THEN RAISE EXCEPTION 'Matching initial export rejected'; END IF;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,gen_random_uuid(),recipient_id,'update')
    WHERE error_code='initial_export_missing') THEN RAISE EXCEPTION 'Cross-sender initial accepted'; END IF;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,gen_random_uuid(),'update')
    WHERE error_code='initial_export_missing') THEN RAISE EXCEPTION 'Cross-recipient initial accepted'; END IF;
  UPDATE music_ddex_export SET validation_report='{"adapterVersion":"tdf-ern432-audio-v4"}' WHERE id=initial_id;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'update')
    WHERE error_code='initial_export_missing') THEN RAISE EXCEPTION 'Legacy unstable track IDs silently accepted'; END IF;
  UPDATE music_ddex_export SET validation_report='{"adapterVersion":"tdf-ern432-audio-v5"}' WHERE id=initial_id;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'takedown')
    WHERE error_code='invalid_export_state') THEN RAISE EXCEPTION 'Approved release takedown accepted'; END IF;
  UPDATE music_release_version SET state='suspended' WHERE id=copy_id;
  IF EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'takedown')
    WHERE error_code IN ('version_not_approved','invalid_export_state','initial_export_missing'))
    THEN RAISE EXCEPTION 'Approved suspended release cannot be withdrawn'; END IF;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'update')
    WHERE error_code='invalid_export_state') THEN RAISE EXCEPTION 'Suspended update accepted'; END IF;
  UPDATE music_release_version SET state='takedown_scheduled',takedown_at_utc=NOW()+INTERVAL '1 day',
    takedown_timezone='America/Guayaquil' WHERE id=copy_id;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'takedown')
    WHERE error_code='takedown_not_due') THEN RAISE EXCEPTION 'Premature deal-less takedown accepted'; END IF;
  UPDATE music_release_version SET state='withdrawn' WHERE id=copy_id;
  IF EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'takedown')
    WHERE error_code IN ('version_not_approved','invalid_export_state','initial_export_missing','takedown_not_due'))
    THEN RAISE EXCEPTION 'Effective withdrawal rejected'; END IF;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(copy_id,sender_id,recipient_id,'unknown')
    WHERE error_code='invalid_export_operation') THEN RAISE EXCEPTION 'Unknown operation accepted'; END IF;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(gen_random_uuid(),sender_id,recipient_id,'new_release')
    WHERE error_code='version_missing') THEN RAISE EXCEPTION 'Missing version accepted'; END IF;
  INSERT INTO music_release_version(release_id,version_number,title,display_artist,created_by,state)
  SELECT release_id,(SELECT MAX(v.version_number)+1 FROM music_release_version v WHERE v.release_id=s.release_id),
    'Synthetic never-approved suspension',display_artist,actor_id,'suspended'
  FROM music_release_version s WHERE id=source_id RETURNING id INTO other_id;
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(other_id,sender_id,recipient_id,'takedown')
    WHERE error_code='version_not_approved') THEN RAISE EXCEPTION 'Unapproved suspension exported'; END IF;
  other_id := music_create_release_correction(
    (SELECT release_id FROM music_release_version WHERE id=source_id),source_id,actor_id);
  DELETE FROM music_identifier WHERE release_version_id=other_id;
  INSERT INTO music_identifier(release_version_id,identifier_type,identifier_value,verification_status)
    VALUES(other_id,'grid','A12425GABC1234002M','syntax_valid');
  IF NOT EXISTS (SELECT 1 FROM music_check_ddex_operation(other_id,sender_id,recipient_id,'update')
    WHERE error_code='initial_export_missing') THEN RAISE EXCEPTION 'Changed product identifier accepted as update'; END IF;
END;
$$;
ROLLBACK;
