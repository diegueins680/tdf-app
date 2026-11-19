-- PostgreSQL refuses this rollback if any demo receipts exist. Do not erase receipts.
ALTER TABLE identity_ads_request
  DROP CONSTRAINT identity_ads_request_request_key_check,
  ADD CONSTRAINT identity_ads_request_request_key_check
  CHECK (request_key ~ '^[A-Za-z0-9_-]{16,128}$');
