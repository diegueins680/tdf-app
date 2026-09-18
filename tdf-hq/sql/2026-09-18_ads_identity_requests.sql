CREATE TABLE IF NOT EXISTS identity_ads_request (
  request_key text PRIMARY KEY CHECK (request_key ~ '^[A-Za-z0-9_-]{16,128}$'),
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  lead_interest_id bigint NOT NULL UNIQUE REFERENCES lead_interest(id) ON DELETE RESTRICT,
  request_payload jsonb NOT NULL,
  response_payload jsonb NOT NULL,
  notification_state text NOT NULL CHECK (notification_state IN ('dispatching','completed','review')),
  created_at timestamptz NOT NULL DEFAULT now()
);
REVOKE ALL ON identity_ads_request FROM PUBLIC;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON identity_ads_request;
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON identity_ads_request
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('party_id');
