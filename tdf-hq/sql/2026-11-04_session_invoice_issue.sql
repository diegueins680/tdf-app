-- PAY-INVOICE-ISSUE-001: request-level idempotency for
-- POST /invoices/{sessionId}/generate. Each call used to create a new invoice
-- and issue a new SRI document, so a retry or double submission produced a
-- duplicate legal invoice. The handler now reserves (session, idempotency key)
-- here before any work; the same key and request replays the stored response,
-- a different request with the key is refused, and a reservation without a
-- response (in progress, or interrupted after SRI may have issued) refuses
-- replays instead of issuing again.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE TABLE IF NOT EXISTS session_invoice_issue (
  id               BIGSERIAL PRIMARY KEY,
  session_id       UUID NOT NULL REFERENCES session(id) ON DELETE CASCADE,
  idempotency_key  TEXT NOT NULL,
  request_sha256   TEXT NOT NULL,
  invoice_id       BIGINT NULL REFERENCES invoice(id) ON DELETE SET NULL,
  response         TEXT NULL,
  created_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
  completed_at     TIMESTAMPTZ NULL,
  CONSTRAINT session_invoice_issue_key_shape
    CHECK (char_length(idempotency_key) BETWEEN 16 AND 128 AND idempotency_key ~ '^[!-~]+$'),
  CONSTRAINT session_invoice_issue_sha_shape CHECK (request_sha256 ~ '^[0-9a-f]{64}$'),
  CONSTRAINT session_invoice_issue_completion
    CHECK ((completed_at IS NULL) = (response IS NULL)),
  CONSTRAINT unique_session_invoice_issue UNIQUE (session_id, idempotency_key)
);

COMMIT;
