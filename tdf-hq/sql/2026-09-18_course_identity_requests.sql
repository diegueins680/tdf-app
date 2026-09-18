-- Source-operation identity for legacy/non-payable registrations. Contact data
-- never selects an account or another person's registration.
CREATE TABLE IF NOT EXISTS identity_course_registration_request (
  request_scope text NOT NULL,
  request_key text NOT NULL CHECK (request_key ~ '^[A-Za-z0-9_-]{16,128}$'),
  request_payload jsonb NOT NULL,
  registration_id bigint NOT NULL UNIQUE REFERENCES course_registration(id) ON DELETE RESTRICT,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (request_scope, request_key)
);
REVOKE ALL ON identity_course_registration_request FROM PUBLIC;
