-- Public enquiries and authenticated student creation identify source operations,
-- never people by shared names/contact details.
CREATE TABLE IF NOT EXISTS identity_trial_request (
  request_scope text NOT NULL,
  request_key text NOT NULL CHECK (request_key ~ '^[A-Za-z0-9_-]{16,128}$'),
  actor_party_id bigint REFERENCES party(id) ON DELETE RESTRICT,
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  lead_interest_id bigint UNIQUE REFERENCES lead_interest(id) ON DELETE RESTRICT,
  trial_request_id bigint UNIQUE REFERENCES trial_request(id) ON DELETE RESTRICT,
  request_payload jsonb NOT NULL,
  response_payload jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (request_scope, request_key),
  CHECK (
    (request_scope='public-signup' AND actor_party_id IS NULL AND lead_interest_id IS NOT NULL AND trial_request_id IS NULL)
    OR (request_scope='public-trial' AND actor_party_id IS NULL AND trial_request_id IS NOT NULL AND lead_interest_id IS NULL)
    OR (request_scope='school-student:' || actor_party_id::text AND actor_party_id IS NOT NULL AND lead_interest_id IS NULL AND trial_request_id IS NULL)
  )
);
REVOKE ALL ON identity_trial_request FROM PUBLIC;
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON identity_trial_request;
CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON identity_trial_request
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('actor_party_id','party_id');
