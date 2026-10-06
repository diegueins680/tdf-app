BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM identity_ads_request) THEN
    RAISE EXCEPTION 'Accepted ad inquiry receipts require a forward repair, not destructive rollback';
  END IF;
END $$;
DROP TABLE identity_ads_request;
COMMIT;
