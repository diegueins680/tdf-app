CREATE TABLE IF NOT EXISTS course_email_event (
  id BIGSERIAL PRIMARY KEY,
  course_slug TEXT NOT NULL,
  registration_id BIGINT NULL REFERENCES course_registration (id) ON DELETE SET NULL,
  recipient_email TEXT NOT NULL,
  recipient_name TEXT NULL,
  event_type TEXT NOT NULL,
  status TEXT NOT NULL,
  message TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_course_email_event_slug
  ON course_email_event (course_slug);

CREATE INDEX IF NOT EXISTS idx_course_email_event_recipient
  ON course_email_event (recipient_email);

CREATE INDEX IF NOT EXISTS idx_course_email_event_registration
  ON course_email_event (registration_id, created_at DESC);
