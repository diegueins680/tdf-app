-- Academy & Referrals
CREATE TABLE IF NOT EXISTS academy_user(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email TEXT NOT NULL UNIQUE,
  role TEXT CHECK (role IN ('artist','manager')) NOT NULL,
  platform TEXT,
  created_at TIMESTAMPTZ DEFAULT now()
);
CREATE TABLE IF NOT EXISTS academy_microcourse(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  slug TEXT UNIQUE NOT NULL,
  title TEXT NOT NULL,
  summary TEXT,
  created_at TIMESTAMPTZ DEFAULT now()
);
CREATE TABLE IF NOT EXISTS academy_lesson(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  microcourse_id UUID REFERENCES academy_microcourse(id) ON DELETE CASCADE,
  day INT NOT NULL,
  title TEXT NOT NULL,
  body TEXT NOT NULL
);
CREATE UNIQUE INDEX ON academy_lesson(microcourse_id, day);
CREATE TABLE IF NOT EXISTS academy_progress(
  user_id UUID REFERENCES academy_user(id) ON DELETE CASCADE,
  lesson_id UUID REFERENCES academy_lesson(id) ON DELETE CASCADE,
  completed_at TIMESTAMPTZ DEFAULT now(),
  PRIMARY KEY (user_id, lesson_id)
);
CREATE TABLE IF NOT EXISTS referral_code(
  code TEXT PRIMARY KEY,
  owner_user_id UUID REFERENCES academy_user(id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ DEFAULT now()
);
CREATE TABLE IF NOT EXISTS referral_claim(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  code TEXT REFERENCES referral_code(code) ON DELETE CASCADE,
  claimant_user_id UUID REFERENCES academy_user(id) ON DELETE SET NULL,
  email TEXT NOT NULL,
  claimed_at TIMESTAMPTZ DEFAULT now(),
  UNIQUE (code, email)
);
CREATE TABLE IF NOT EXISTS cohort(
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  slug TEXT UNIQUE NOT NULL,
  title TEXT NOT NULL,
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  seat_cap INT NOT NULL DEFAULT 30
);
CREATE TABLE IF NOT EXISTS cohort_enrollment(
  cohort_id UUID REFERENCES cohort(id) ON DELETE CASCADE,
  user_id UUID REFERENCES academy_user(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ DEFAULT now(),
  PRIMARY KEY (cohort_id, user_id)
);
