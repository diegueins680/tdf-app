BEGIN;

CREATE TABLE IF NOT EXISTS feature_access_requests (
  id BIGSERIAL PRIMARY KEY,
  requester_party_id BIGINT NOT NULL REFERENCES party(id),
  feature_id TEXT NOT NULL,
  action TEXT NOT NULL,
  role_context TEXT NOT NULL,
  module_context TEXT NOT NULL,
  justification TEXT,
  status TEXT NOT NULL DEFAULT 'pending',
  reviewer_group TEXT NOT NULL,
  reviewer_party_id BIGINT REFERENCES party(id),
  reviewer_notes TEXT,
  requested_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  decided_at TIMESTAMPTZ,
  cancelled_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  CONSTRAINT feature_access_requests_status_check
    CHECK (status IN ('pending', 'approved', 'rejected', 'cancelled', 'expired')),
  CONSTRAINT feature_access_requests_action_check
    CHECK (action IN (
      'discover', 'view', 'create', 'edit', 'delete', 'archive', 'deactivate',
      'import', 'export', 'submit', 'validate', 'approve', 'reject', 'assign',
      'publish', 'report', 'administer'
    )),
  CONSTRAINT feature_access_requests_justification_length_check
    CHECK (justification IS NULL OR char_length(justification) <= 2000),
  CONSTRAINT feature_access_requests_reviewer_notes_length_check
    CHECK (reviewer_notes IS NULL OR char_length(reviewer_notes) <= 2000)
);

CREATE INDEX IF NOT EXISTS feature_access_requests_requester_idx
  ON feature_access_requests (requester_party_id, requested_at DESC);
CREATE INDEX IF NOT EXISTS feature_access_requests_queue_idx
  ON feature_access_requests (status, reviewer_group, requested_at);
CREATE INDEX IF NOT EXISTS feature_access_requests_duplicate_idx
  ON feature_access_requests (requester_party_id, feature_id, action, status);
CREATE UNIQUE INDEX IF NOT EXISTS feature_access_requests_one_pending_idx
  ON feature_access_requests (requester_party_id, feature_id, action)
  WHERE status = 'pending';

CREATE TABLE IF NOT EXISTS feature_access_request_history (
  id BIGSERIAL PRIMARY KEY,
  request_id BIGINT NOT NULL REFERENCES feature_access_requests(id),
  actor_party_id BIGINT REFERENCES party(id),
  transition TEXT NOT NULL,
  from_status TEXT,
  to_status TEXT NOT NULL,
  note TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT feature_access_request_history_note_length_check
    CHECK (note IS NULL OR char_length(note) <= 2000)
);

CREATE INDEX IF NOT EXISTS feature_access_request_history_request_idx
  ON feature_access_request_history (request_id, created_at);

COMMIT;

-- Rollback (run only after exporting both tables):
-- BEGIN;
-- DROP TABLE IF EXISTS feature_access_request_history;
-- DROP TABLE IF EXISTS feature_access_requests;
-- COMMIT;
