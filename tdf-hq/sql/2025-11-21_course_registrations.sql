CREATE TABLE IF NOT EXISTS course_registration (
  id BIGSERIAL PRIMARY KEY,
  course_slug TEXT NOT NULL,
  full_name TEXT NULL,
  email TEXT NULL,
  phone_e164 TEXT NULL,
  source TEXT NOT NULL,
  status TEXT NOT NULL,
  how_heard TEXT NULL,
  utm_source TEXT NULL,
  utm_medium TEXT NULL,
  utm_campaign TEXT NULL,
  utm_content TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_course_registration_slug ON course_registration (course_slug);
CREATE INDEX IF NOT EXISTS idx_course_registration_email ON course_registration (course_slug, email);
CREATE INDEX IF NOT EXISTS idx_course_registration_phone ON course_registration (course_slug, phone_e164);
