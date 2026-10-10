-- After music_version_parties. Quiesce authoring/review during migration.
BEGIN;
LOCK TABLE music_release_version, music_release_version_party IN ACCESS EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION music_legacy_party_details(party_id UUID)
RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT jsonb_build_object('displayName',party.display_name,'legalName',party.legal_name,
    'partyKind',party.party_kind,'identifiers',COALESCE((
      SELECT jsonb_agg(to_jsonb(identifier) ORDER BY identifier.identifier_type,identifier.identifier_value)
      FROM music_party_identifier identifier WHERE identifier.music_party_id=party.id
    ),'[]'::jsonb)) FROM music_party party WHERE party.id=party_id;
$$;

ALTER TABLE music_release_version_party ADD COLUMN IF NOT EXISTS party_details JSONB
  CHECK (party_details IS NULL OR COALESCE(
    jsonb_typeof(party_details)='object'
    AND length(btrim(party_details->>'displayName')) BETWEEN 1 AND 500
    AND party_details->>'partyKind' IN ('person','organization','unknown')
    AND jsonb_typeof(party_details->'identifiers')='array',FALSE));
ALTER TABLE music_release_version_party ADD COLUMN IF NOT EXISTS details_source TEXT
  NOT NULL DEFAULT 'legacy_observed' CHECK (details_source IN ('legacy_observed','user_provided')
    AND (details_source<>'user_provided' OR party_details IS NOT NULL));

-- This is an observation of available legacy data, NOT reconstruction of what
-- was approved historically. Never rewrite old immutable_snapshot or its hash.
DROP TRIGGER trg_music_version_party_immutable ON music_release_version_party;
UPDATE music_release_version_party SET party_details=music_legacy_party_details(music_party_id)
WHERE party_details IS NULL;
CREATE TRIGGER trg_music_version_party_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_release_version_party
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();

CREATE OR REPLACE FUNCTION music_version_parties(version_id UUID)
RETURNS JSONB LANGUAGE SQL STABLE AS $$
  SELECT COALESCE(jsonb_agg(
    COALESCE(member.party_details,music_legacy_party_details(party.id)) ||
    jsonb_build_object('id',party.id,'tdfPartyId',party.tdf_party_id,
      'detailsSource',COALESCE(member.details_source,'legacy_observed')) ORDER BY party.id
  ),'[]'::jsonb)
  FROM music_party party LEFT JOIN music_release_version_party member
    ON member.music_party_id=party.id AND member.release_version_id=version_id
  WHERE party.id IN (
    SELECT music_party_id FROM music_release_version_party WHERE release_version_id=version_id
    UNION SELECT music_party_id FROM music_credit WHERE release_version_id=version_id
    UNION SELECT split.rights_holder_id FROM music_rights_declaration rights
      JOIN music_rights_split split ON split.declaration_id=rights.id WHERE rights.release_version_id=version_id
  );
$$;

-- Only server-built supplied objects enter here; unchanged identifiers retain
-- their existing provenance/authority evidence. Removal really removes them
-- from this version, without modifying the identity directory or other versions.
CREATE OR REPLACE FUNCTION music_merge_party_identifiers(prior JSONB, supplied JSONB)
RETURNS JSONB LANGUAGE SQL IMMUTABLE AS $$
  SELECT COALESCE(jsonb_agg(COALESCE((
    SELECT old.value FROM jsonb_array_elements(COALESCE(prior,'[]'::jsonb)) old
    WHERE old.value->>'identifier_type'=item.value->>'identifier_type'
      AND old.value->>'identifier_value'=item.value->>'identifier_value' LIMIT 1
  ),item.value) ORDER BY item.value->>'identifier_type',item.value->>'identifier_value'),'[]'::jsonb)
  FROM jsonb_array_elements(supplied) item;
$$;

CREATE OR REPLACE FUNCTION music_copy_version_parties()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.correction_of_version_id IS NOT NULL THEN
    IF NOT EXISTS (SELECT 1 FROM music_release_version source
      WHERE source.id=NEW.correction_of_version_id AND source.release_id=NEW.release_id) THEN
      RAISE EXCEPTION 'correction party source must belong to the same release' USING ERRCODE='23514';
    END IF;
    INSERT INTO music_release_version_party(release_version_id,music_party_id,party_details,details_source)
    SELECT NEW.id,member.music_party_id,member.party_details,member.details_source
    FROM music_release_version_party member WHERE member.release_version_id=NEW.correction_of_version_id
    ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$;

DO $$ BEGIN
  IF to_regprocedure('music_check_submission_before_party_details(uuid)') IS NULL THEN
    ALTER FUNCTION music_check_submission(UUID) RENAME TO music_check_submission_before_party_details;
  END IF;
  IF to_regprocedure('music_check_ddex_export_before_party_details(uuid)') IS NULL THEN
    ALTER FUNCTION music_check_ddex_export(UUID) RENAME TO music_check_ddex_export_before_party_details;
  END IF;
END $$;
CREATE OR REPLACE FUNCTION music_check_submission(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT) LANGUAGE SQL STABLE AS $$
  SELECT * FROM music_check_submission_before_party_details(version_id)
  UNION ALL SELECT 'parties.'||(party->>'id'),'party_details_confirmation_required',
    'Revisa los nombres e identificadores legados de este colaborador y guarda el contenido antes de enviar.'
  FROM jsonb_array_elements(music_version_parties(version_id)) party
  WHERE party->>'detailsSource'<>'user_provided';
$$;
CREATE OR REPLACE FUNCTION music_check_ddex_export(version_id UUID)
RETURNS TABLE(field_path TEXT,error_code TEXT,message TEXT) LANGUAGE SQL STABLE AS $$
  SELECT * FROM music_check_ddex_export_before_party_details(version_id)
  UNION ALL SELECT 'version.parties','party_snapshot_missing',
    'La aprobación anterior no conserva partes versionadas. Crea, revisa y aprueba una corrección antes de una exportación nueva.'
  FROM music_release_version version WHERE version.id=version_id
    AND NOT (COALESCE(version.immutable_snapshot->>'schemaVersion','')='2'
      AND COALESCE(jsonb_typeof(version.immutable_snapshot->'parties'),'')='array');
$$;
COMMIT;
