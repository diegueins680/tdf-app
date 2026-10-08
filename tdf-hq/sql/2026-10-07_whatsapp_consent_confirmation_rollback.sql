BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM whats_app_consent WHERE confirmation_requested_at IS NOT NULL) THEN
    RAISE EXCEPTION 'Retain pending WhatsApp confirmation requests; use a forward repair';
  END IF;
END $$;
ALTER TABLE whats_app_consent DROP COLUMN IF EXISTS confirmation_requested_at;
COMMIT;
