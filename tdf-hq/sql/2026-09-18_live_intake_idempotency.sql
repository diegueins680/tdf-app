-- Whole submissions are scoped to their authenticated actor, never an email.
CREATE TABLE IF NOT EXISTS identity_live_intake_request (
  actor_party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  request_key text NOT NULL CHECK (request_key ~ '^[A-Za-z0-9_-]{16,128}$'),
  request_payload jsonb NOT NULL,
  intake_id uuid NOT NULL UNIQUE REFERENCES live_session_intake(id) ON DELETE RESTRICT,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (actor_party_id, request_key)
);
REVOKE ALL ON identity_live_intake_request FROM PUBLIC;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON identity_live_intake_request;
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON identity_live_intake_request
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('actor_party_id');
