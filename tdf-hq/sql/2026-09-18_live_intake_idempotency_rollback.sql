-- Keep receipts after any accepted submission: dropping them would allow replays.
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM identity_live_intake_request) THEN
    RAISE EXCEPTION 'Intake receipts exist; preserve them during application rollback';
  END IF;
END $$;
DROP TABLE identity_live_intake_request;
