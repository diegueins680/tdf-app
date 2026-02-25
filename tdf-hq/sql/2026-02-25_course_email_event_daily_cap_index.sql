CREATE INDEX IF NOT EXISTS idx_course_email_event_sent_recipient_created
  ON course_email_event (recipient_email, created_at DESC)
  WHERE status = 'sent';
