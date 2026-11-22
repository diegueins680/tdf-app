-- Course follow-up reminders (COURSE-FOLLOWUP-REMINDER-001).
-- One claim row per (follow-up, scheduled time, recipient) so each scheduled
-- reminder reaches the bell once across replicas. The recipient is the party id
-- as text, like event_logistics_alert_delivery, so this claim table stays
-- outside the identity Party reference registry. Additive only: no data seed,
-- grant, visibility change or background work.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS public.course_follow_up_reminder_delivery (
  id BIGSERIAL PRIMARY KEY,
  follow_up_id BIGINT NOT NULL REFERENCES public.course_registration_follow_up(id) ON DELETE CASCADE,
  due_at TIMESTAMPTZ NOT NULL,
  recipient_party_id TEXT NOT NULL,
  delivered_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_course_follow_up_reminder_delivery UNIQUE (follow_up_id, due_at, recipient_party_id)
);

COMMIT;
