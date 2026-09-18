DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM identity_trial_request) THEN
    RAISE EXCEPTION 'Accepted trial/student requests must retain their replay protection';
  END IF;
END $$;
DROP TABLE identity_trial_request;
