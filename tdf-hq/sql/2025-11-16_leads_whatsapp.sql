-- leads & whatsapp audit
CREATE TABLE IF NOT EXISTS course_edition (
  id                SERIAL PRIMARY KEY,
  slug              TEXT UNIQUE NOT NULL,         -- e.g. 'produccion-musical-dic-2025'
  name              TEXT NOT NULL,                -- 'Curso de Producción Musical'
  registration_url  TEXT,
  starts_on         DATE NOT NULL,
  ends_on           DATE NOT NULL,
  created_at        TIMESTAMPTZ DEFAULT now()
);

CREATE TABLE IF NOT EXISTS lead (
  id                BIGSERIAL PRIMARY KEY,
  phone_e164        TEXT NOT NULL,                -- '+59398...'
  display_name      TEXT,
  email             TEXT,
  source            TEXT NOT NULL DEFAULT 'whatsapp',
  course_edition_id INTEGER NOT NULL REFERENCES course_edition(id) ON DELETE RESTRICT,
  status            TEXT NOT NULL DEFAULT 'NEW',  -- NEW | LINK_SENT | COMPLETED | COLD
  token             TEXT NOT NULL,                -- short opaque token
  created_at        TIMESTAMPTZ DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_lead_token ON lead(token);

CREATE TABLE IF NOT EXISTS whatsapp_message_log (
  id                BIGSERIAL PRIMARY KEY,
  direction         TEXT NOT NULL,                -- 'in' | 'out'
  wa_msg_id         TEXT,
  phone_e164        TEXT NOT NULL,
  payload           JSONB NOT NULL,
  created_at        TIMESTAMPTZ DEFAULT now()
);

-- Seed (edición de diciembre 2025/enero 2026)
INSERT INTO course_edition (slug, name, registration_url, starts_on, ends_on)
VALUES (
  'produccion-musical-dic-2025',
  'Curso de Producción Musical (Dic 2025 / Ene 2026)',
  NULL,
  '2025-12-13',
  '2026-01-03'
)
ON CONFLICT (slug) DO NOTHING;
