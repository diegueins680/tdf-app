-- Roll code back first with authoring paused. Prefer retaining this additive
-- schema. Removal is safe only when EVERY link is recoverable from old content.
BEGIN;
LOCK TABLE music_release_version_party, music_credit, music_rights_declaration,
  music_rights_split IN SHARE ROW EXCLUSIVE MODE;
DO $$ BEGIN
  IF EXISTS (
    SELECT 1 FROM music_release_version_party member
    WHERE NOT EXISTS (
      SELECT 1 FROM music_credit credit
      WHERE credit.release_version_id=member.release_version_id
        AND credit.music_party_id=member.music_party_id
    ) AND NOT EXISTS (
      SELECT 1 FROM music_rights_declaration rights
      JOIN music_rights_split split ON split.declaration_id=rights.id
      WHERE rights.release_version_id=member.release_version_id
        AND split.rights_holder_id=member.music_party_id
    )
  ) THEN
    RAISE EXCEPTION 'refusing party membership rollback: uncredited collaborators would be lost; retain schema and restore code';
  END IF;
END $$;
DROP TRIGGER trg_music_copy_version_parties ON music_release_version;
DROP FUNCTION music_copy_version_parties();
DROP TABLE music_release_version_party;
COMMIT;
