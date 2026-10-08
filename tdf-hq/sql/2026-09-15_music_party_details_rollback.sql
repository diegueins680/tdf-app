-- Prefer retaining additive data with authoring paused when rolling code back.
BEGIN;
LOCK TABLE music_release_version, music_release_version_party IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM music_release_version_party member
      WHERE details_source='user_provided'
        OR party_details IS DISTINCT FROM music_legacy_party_details(music_party_id))
    OR EXISTS (SELECT 1 FROM music_release_version WHERE immutable_snapshot->>'schemaVersion'='2') THEN
    RAISE EXCEPTION 'refusing party details rollback: versioned evidence exists; retain schema and restore compatible code';
  END IF;
END $$;
DROP FUNCTION music_check_submission(UUID);
ALTER FUNCTION music_check_submission_before_party_details(UUID) RENAME TO music_check_submission;
DROP FUNCTION music_check_ddex_export(UUID);
ALTER FUNCTION music_check_ddex_export_before_party_details(UUID) RENAME TO music_check_ddex_export;
CREATE OR REPLACE FUNCTION music_copy_version_parties()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.correction_of_version_id IS NOT NULL THEN
    IF NOT EXISTS (SELECT 1 FROM music_release_version source
      WHERE source.id=NEW.correction_of_version_id AND source.release_id=NEW.release_id) THEN
      RAISE EXCEPTION 'correction party source must belong to the same release' USING ERRCODE='23514';
    END IF;
    INSERT INTO music_release_version_party(release_version_id,music_party_id)
    SELECT NEW.id,member.music_party_id FROM music_release_version_party member
    WHERE member.release_version_id=NEW.correction_of_version_id ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$;
DROP FUNCTION music_version_parties(UUID);
DROP FUNCTION music_merge_party_identifiers(JSONB,JSONB);
ALTER TABLE music_release_version_party DROP COLUMN details_source;
ALTER TABLE music_release_version_party DROP COLUMN party_details;
DROP FUNCTION music_legacy_party_details(UUID);
COMMIT;
