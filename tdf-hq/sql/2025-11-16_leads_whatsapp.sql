-- leads & whatsapp audit
CREATE TABLE IF NOT EXISTS course_edition (
  id                SERIAL PRIMARY KEY,
  slug              TEXT UNIQUE NOT NULL,         -- e.g. 'produccion-musical-feb-2026'
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

CREATE UNIQUE INDEX IF NOT EXISTS idx_lead_token ON lead(token) WHERE token IS NOT NULL;

-- Audit log table for tracking all WhatsApp messages sent/received.
-- This table is created for future implementation of message auditing.
-- Currently not populated by the application code but reserved for logging
-- inbound/outbound messages for compliance and debugging purposes.
CREATE TABLE IF NOT EXISTS whatsapp_message_log (
  id                BIGSERIAL PRIMARY KEY,
  direction         TEXT NOT NULL,                -- 'in' | 'out'
  wa_msg_id         TEXT,
  phone_e164        TEXT NOT NULL,
  payload           JSONB NOT NULL,
  created_at        TIMESTAMPTZ DEFAULT now()
);

-- Seed (edición de febrero 2026/marzo 2026)
INSERT INTO course_edition (slug, name, registration_url, starts_on, ends_on)
VALUES (
  'produccion-musical-feb-2026',
  'Curso de Producción Musical (Feb 2026 / Mar 2026)',
  NULL,
  '2026-02-28',
  '2026-03-21'
)
ON CONFLICT (slug) DO NOTHING;
