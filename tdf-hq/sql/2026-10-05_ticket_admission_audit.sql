-- Additive audit for canonical and legacy tickets. Does not enable sales.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '1min';
CREATE TABLE IF NOT EXISTS event_ticket_admission_audit (
  ticket_id BIGINT PRIMARY KEY REFERENCES event_ticket(id) ON DELETE RESTRICT,
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  order_id BIGINT NOT NULL REFERENCES event_ticket_order(id) ON DELETE RESTRICT,
  actor_party_id TEXT NOT NULL CHECK (actor_party_id ~ '^[1-9][0-9]*$'),
  admitted_at TIMESTAMPTZ NOT NULL
);
CREATE INDEX IF NOT EXISTS event_ticket_admission_audit_event_time
  ON event_ticket_admission_audit(event_id, admitted_at);
COMMIT;
