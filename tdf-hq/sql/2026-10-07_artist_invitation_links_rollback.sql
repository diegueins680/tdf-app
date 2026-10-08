-- Refuses to discard redemption evidence: once any link has been redeemed,
-- keep the table (older binaries ignore it) instead of rolling back.
BEGIN;

DO $$ BEGIN
  IF to_regclass('artist_invitation_link') IS NOT NULL THEN
    IF EXISTS (SELECT 1 FROM artist_invitation_link WHERE redeemed_at IS NOT NULL) THEN
      RAISE EXCEPTION 'artist_invitation_link holds redemption evidence; keep the table';
    END IF;
  END IF;
END $$;

DROP TABLE IF EXISTS artist_invitation_link;

COMMIT;
