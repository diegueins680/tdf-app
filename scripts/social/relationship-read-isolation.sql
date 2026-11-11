BEGIN;
-- Detect unrelated deletes inside the helper, before its fixture rollback can
-- hide them. Include edges with just one endpoint outside the model as well.
INSERT INTO party_follow(follower_party_id,following_party_id)
  VALUES(2,4),(4,2),(4,5),(3,1);
CREATE FUNCTION reject_unrelated_relationship_fixture_delete()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.follower_party_id NOT IN (1,2,3) OR OLD.following_party_id NOT IN (1,2,3) THEN
    RAISE EXCEPTION 'Relationship fixture deleted an unrelated edge';
  END IF;
  RETURN OLD;
END $$;
CREATE TRIGGER relationship_fixture_delete_guard BEFORE DELETE ON party_follow
  FOR EACH ROW EXECUTE FUNCTION reject_unrelated_relationship_fixture_delete();
-- The existing 3 -> 1 model edge must be cleared for this empty observation.
SELECT assert_relationship_read_case(0,false,false,false,false,false,false,true,true,
  'followers','[]'::jsonb,'[]'::jsonb,0);
ROLLBACK;
