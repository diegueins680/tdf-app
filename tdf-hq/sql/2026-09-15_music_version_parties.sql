-- Apply after the base music platform (and preview policy when installed).
-- Membership is independent of credits/rights: unfinished collaborators survive.
BEGIN;

CREATE TABLE IF NOT EXISTS music_release_version_party (
  release_version_id UUID NOT NULL REFERENCES music_release_version(id),
  music_party_id UUID NOT NULL REFERENCES music_party(id),
  PRIMARY KEY (release_version_id, music_party_id)
);
CREATE INDEX IF NOT EXISTS music_version_party_party_idx
  ON music_release_version_party(music_party_id, release_version_id);

-- Only recover links for which the old graph provides evidence. Never assign
-- orphan music_party rows to a guessed release. Transactional and restartable;
-- NOT EXISTS also avoids firing the immutable guard on a repeated application.
INSERT INTO music_release_version_party(release_version_id, music_party_id)
SELECT known.release_version_id, known.music_party_id FROM (
  SELECT release_version_id, music_party_id FROM music_credit
  UNION
  SELECT rights.release_version_id, split.rights_holder_id
  FROM music_rights_declaration rights
  JOIN music_rights_split split ON split.declaration_id=rights.id
) known WHERE NOT EXISTS (
  SELECT 1 FROM music_release_version_party member
  WHERE member.release_version_id=known.release_version_id
    AND member.music_party_id=known.music_party_id
) ON CONFLICT DO NOTHING;

CREATE OR REPLACE TRIGGER trg_music_version_party_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_release_version_party
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();

-- Keep the existing correction function/API. Its version INSERT supplies the
-- source id, so this runs in the very same transaction as the correction graph.
CREATE OR REPLACE FUNCTION music_copy_version_parties()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.correction_of_version_id IS NOT NULL THEN
    IF NOT EXISTS (
      SELECT 1 FROM music_release_version source
      WHERE source.id=NEW.correction_of_version_id AND source.release_id=NEW.release_id
    ) THEN
      RAISE EXCEPTION 'correction party source must belong to the same release'
        USING ERRCODE='23514';
    END IF;
    INSERT INTO music_release_version_party(release_version_id,music_party_id)
    SELECT NEW.id, member.music_party_id FROM music_release_version_party member
    WHERE member.release_version_id=NEW.correction_of_version_id
    ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$;

CREATE OR REPLACE TRIGGER trg_music_copy_version_parties
AFTER INSERT ON music_release_version
FOR EACH ROW EXECUTE FUNCTION music_copy_version_parties();

COMMIT;
