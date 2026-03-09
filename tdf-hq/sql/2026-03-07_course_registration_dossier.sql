ALTER TABLE course_registration
  ADD COLUMN IF NOT EXISTS party_id BIGINT NULL REFERENCES party (id) ON DELETE SET NULL,
  ADD COLUMN IF NOT EXISTS admin_notes TEXT NULL;

CREATE INDEX IF NOT EXISTS idx_course_registration_party
  ON course_registration (party_id, created_at DESC);

CREATE TABLE IF NOT EXISTS course_registration_receipt (
  id BIGSERIAL PRIMARY KEY,
  registration_id BIGINT NOT NULL REFERENCES course_registration (id) ON DELETE CASCADE,
  party_id BIGINT NULL REFERENCES party (id) ON DELETE SET NULL,
  file_url TEXT NOT NULL,
  file_name TEXT NULL,
  mime_type TEXT NULL,
  notes TEXT NULL,
  uploaded_by BIGINT NULL REFERENCES party (id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_course_registration_receipt_registration
  ON course_registration_receipt (registration_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_course_registration_receipt_party
  ON course_registration_receipt (party_id, created_at DESC);

CREATE TABLE IF NOT EXISTS course_registration_follow_up (
  id BIGSERIAL PRIMARY KEY,
  registration_id BIGINT NOT NULL REFERENCES course_registration (id) ON DELETE CASCADE,
  party_id BIGINT NULL REFERENCES party (id) ON DELETE SET NULL,
  entry_type TEXT NOT NULL,
  subject TEXT NULL,
  notes TEXT NOT NULL,
  attachment_url TEXT NULL,
  attachment_name TEXT NULL,
  next_follow_up_at TIMESTAMPTZ NULL,
  created_by BIGINT NULL REFERENCES party (id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_course_registration_follow_up_registration
  ON course_registration_follow_up (registration_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_course_registration_follow_up_party
  ON course_registration_follow_up (party_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_course_registration_follow_up_next
  ON course_registration_follow_up (next_follow_up_at)
  WHERE next_follow_up_at IS NOT NULL;
