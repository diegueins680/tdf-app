-- Durable replay protection for the public tentative-booking compatibility path.
--
-- The table retains no contact fields or request body. The API stores only the
-- caller-provided idempotency key, a SHA-256 request fingerprint, and the
-- resulting booking reference.
BEGIN;

CREATE TABLE IF NOT EXISTS service_booking_tentative_request (
  idempotency_key TEXT PRIMARY KEY
    CHECK (length(idempotency_key) BETWEEN 16 AND 128)
    CHECK (idempotency_key ~ '^[!-~]+$'),
  request_sha256 TEXT NOT NULL
    CHECK (request_sha256 ~ '^[0-9a-f]{64}$'),
  booking_id BIGINT NOT NULL UNIQUE
    REFERENCES booking(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

COMMENT ON TABLE service_booking_tentative_request IS
  'Idempotency records for unpaid public tentative booking requests; contains no customer contact data.';
COMMENT ON COLUMN service_booking_tentative_request.request_sha256 IS
  'SHA-256 of the normalized request snapshot used only to reject conflicting key reuse.';

COMMIT;
