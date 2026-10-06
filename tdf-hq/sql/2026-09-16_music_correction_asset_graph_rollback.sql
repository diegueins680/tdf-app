-- Restore the previous function only; retain all versions, assets and bytes. This reintroduces the chained-correction limitation; disable corrections before rollback.
-- Apply with authoring/review quiesced after the existing music migrations.
BEGIN;
LOCK TABLE music_release, music_release_version, music_asset IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION music_create_release_correction(
  target_release_id UUID,
  source_version_id UUID,
  actor_id BIGINT
) RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE
  new_version_id UUID := gen_random_uuid();
  next_version_number INTEGER;
  source_track RECORD;
  source_asset RECORD;
  source_rights RECORD;
  new_recording_id UUID;
  new_asset_id UUID;
  new_rights_id UUID;
  recording_map JSONB := '{}'::JSONB;
  asset_map JSONB := '{}'::JSONB;
BEGIN
  PERFORM 1 FROM music_release_version version
    WHERE version.id=source_version_id AND version.release_id=target_release_id
      AND version.state IN ('approved','scheduled','published','suspended','replacement_pending','takedown_scheduled','withdrawn')
    FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'source music release version is not an immutable correction source' USING ERRCODE='23514';
  END IF;

  SELECT COALESCE(MAX(version_number),0)+1 INTO next_version_number
    FROM music_release_version WHERE release_id=target_release_id;
  INSERT INTO music_release_version(
    id,release_id,version_number,state,title,subtitle,version_title,display_artist,
    title_language,title_script,primary_genre_id,secondary_genre_id,explicit_content,
    original_release_date,label_name,catalog_number,recording_copyright_text,
    work_copyright_text,correction_of_version_id,replaces_version_id,created_by
  )
  SELECT new_version_id,release_id,next_version_number,'draft',title,subtitle,version_title,
    display_artist,title_language,title_script,primary_genre_id,secondary_genre_id,
    explicit_content,original_release_date,label_name,catalog_number,
    recording_copyright_text,work_copyright_text,id,id,actor_id
  FROM music_release_version WHERE id=source_version_id;

  -- Recording metadata and identifiers are part of the immutable published
  -- graph. Clone the logical recording rows so a correction can alter them
  -- without changing what the earlier release version represented.
  FOR source_track IN
    SELECT track.*, recording.canonical_title, recording.subtitle AS recording_subtitle,
      recording.version_title AS recording_version_title,
      recording.title_language AS recording_title_language,
      recording.title_script AS recording_title_script,
      recording.duration_ms, recording.explicit_content AS recording_explicit_content
    FROM music_release_track track
    JOIN music_recording recording ON recording.id=track.recording_id
    WHERE track.release_version_id=source_version_id
    ORDER BY track.disc_number,track.track_number,track.id
  LOOP
    new_recording_id := gen_random_uuid();
    INSERT INTO music_recording(
      id,canonical_title,subtitle,version_title,title_language,title_script,
      duration_ms,explicit_content,created_by
    ) VALUES (
      new_recording_id,source_track.canonical_title,source_track.recording_subtitle,
      source_track.recording_version_title,source_track.recording_title_language,
      source_track.recording_title_script,source_track.duration_ms,
      source_track.recording_explicit_content,actor_id
    );
    INSERT INTO music_release_track(
      release_version_id,recording_id,disc_number,track_number,display_artist,
      is_primary_resource,preview_start_ms,preview_duration_ms
    ) VALUES (
      new_version_id,new_recording_id,source_track.disc_number,source_track.track_number,
      source_track.display_artist,source_track.is_primary_resource,
      source_track.preview_start_ms,source_track.preview_duration_ms
    );
    recording_map := recording_map || jsonb_build_object(source_track.recording_id::TEXT,new_recording_id::TEXT);
  END LOOP;

  INSERT INTO music_credit(
    release_version_id,recording_id,music_party_id,credit_role,display_order,notes
  ) SELECT new_version_id,
      CASE WHEN recording_id IS NULL THEN NULL ELSE (recording_map ->> recording_id::TEXT)::UUID END,
      music_party_id,credit_role,display_order,notes
    FROM music_credit WHERE release_version_id=source_version_id;

  INSERT INTO music_identifier(
    release_version_id,identifier_type,identifier_value,provenance,verification_status,
    verification_authority,verified_at
  ) SELECT new_version_id,identifier_type,identifier_value,provenance,verification_status,
      verification_authority,verified_at
    FROM music_identifier WHERE release_version_id=source_version_id;

  INSERT INTO music_identifier(
    recording_id,identifier_type,identifier_value,provenance,verification_status,
    verification_authority,verified_at
  ) SELECT (recording_map ->> recording_id::TEXT)::UUID,identifier_type,identifier_value,
      provenance,verification_status,verification_authority,verified_at
    FROM music_identifier
    WHERE recording_id IN (
      SELECT recording_id FROM music_release_track WHERE release_version_id=source_version_id
    );

  -- Multiple logical versions may safely reference the same immutable bytes.
  -- Clone locators and provenance, then remap all logical recording and parent
  -- references to the correction graph. The objects themselves are not copied.
  FOR source_asset IN
    SELECT * FROM music_asset
    WHERE release_version_id=source_version_id
      AND asset_role NOT IN ('ddex_xml','ddex_manifest','ddex_package')
    ORDER BY (parent_asset_id IS NOT NULL),created_at,id
  LOOP
    new_asset_id := gen_random_uuid();
    INSERT INTO music_asset(
      id,release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,
      storage_class,bucket_name,object_key,original_filename,media_type,byte_size,
      sha256,etag,processing_state,technical_metadata,provenance,immutable,created_by,ready_at
    ) VALUES (
      new_asset_id,new_version_id,
      CASE WHEN source_asset.recording_id IS NULL THEN NULL
        ELSE (recording_map ->> source_asset.recording_id::TEXT)::UUID END,
      CASE WHEN source_asset.parent_asset_id IS NULL THEN NULL
        ELSE (asset_map ->> source_asset.parent_asset_id::TEXT)::UUID END,
      source_asset.asset_role,source_asset.storage_provider,source_asset.storage_class,
      source_asset.bucket_name,source_asset.object_key,source_asset.original_filename,
      source_asset.media_type,source_asset.byte_size,source_asset.sha256,source_asset.etag,
      source_asset.processing_state,source_asset.technical_metadata,
      source_asset.provenance || jsonb_build_object('correctionSourceAssetId',source_asset.id),
      source_asset.immutable,actor_id,source_asset.ready_at
    );
    asset_map := asset_map || jsonb_build_object(source_asset.id::TEXT,new_asset_id::TEXT);
  END LOOP;

  FOR source_rights IN
    SELECT * FROM music_rights_declaration WHERE release_version_id=source_version_id ORDER BY declared_at,id
  LOOP
    INSERT INTO music_rights_declaration(
      release_version_id,recording_id,rights_scope,authority_basis,territories,
      starts_on,ends_on,declared_by,declared_at,evidence_asset_id
    ) VALUES (
      new_version_id,
      CASE WHEN source_rights.recording_id IS NULL THEN NULL
        ELSE (recording_map ->> source_rights.recording_id::TEXT)::UUID END,
      source_rights.rights_scope,
      source_rights.authority_basis,source_rights.territories,source_rights.starts_on,
      source_rights.ends_on,actor_id,NOW(),
      CASE WHEN source_rights.evidence_asset_id IS NULL THEN NULL
        ELSE COALESCE((asset_map ->> source_rights.evidence_asset_id::TEXT)::UUID,source_rights.evidence_asset_id) END
    ) RETURNING id INTO new_rights_id;
    INSERT INTO music_rights_split(
      declaration_id,rights_holder_id,basis_points,territories,starts_on,ends_on,
      accepted_terms_version,accepted_at
    ) SELECT new_rights_id,rights_holder_id,basis_points,territories,starts_on,ends_on,
        accepted_terms_version,accepted_at
      FROM music_rights_split WHERE declaration_id=source_rights.id;
  END LOOP;

  INSERT INTO music_availability_rule(
    release_version_id,release_track_id,territory_mode,territories,starts_at,ends_at,
    listening_policy,download_policy,purchasable,price_minor,currency,downloadable_asset_id
  )
  SELECT new_version_id,
    CASE WHEN rule.release_track_id IS NULL THEN NULL ELSE new_track.id END,
    rule.territory_mode,rule.territories,rule.starts_at,
    rule.ends_at,rule.listening_policy,rule.download_policy,rule.purchasable,
    rule.price_minor,rule.currency,
    CASE WHEN rule.downloadable_asset_id IS NULL THEN NULL
      ELSE (asset_map ->> rule.downloadable_asset_id::TEXT)::UUID END
  FROM music_availability_rule rule
  LEFT JOIN music_release_track old_track ON old_track.id=rule.release_track_id
  LEFT JOIN music_release_track new_track ON new_track.release_version_id=new_version_id
    AND new_track.recording_id=(recording_map ->> old_track.recording_id::TEXT)::UUID
  WHERE rule.release_version_id=source_version_id;

  PERFORM music_refresh_validation_flags(new_version_id);
  RETURN new_version_id;
END;
$$;

COMMIT;

