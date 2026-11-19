-- Preserve the legacy Ads wire format and allow a disjoint storage-only demo namespace.
ALTER TABLE identity_ads_request
  DROP CONSTRAINT identity_ads_request_request_key_check,
  ADD CONSTRAINT identity_ads_request_request_key_check
  CHECK (request_key ~ '^(demo:)?[A-Za-z0-9_-]{16,128}$');
