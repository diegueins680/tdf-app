DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM identity_course_registration_request) THEN
    RAISE EXCEPTION 'Accepted course requests must retain their replay protection';
  END IF;
END $$;
DROP TABLE identity_course_registration_request;
