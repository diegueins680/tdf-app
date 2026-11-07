-- Rollback of 2026-11-04_session_invoice_issue. Dropping the table discards
-- replay reservations only; invoices, session links and SRI numbers stay.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TABLE IF EXISTS session_invoice_issue;
COMMIT;
