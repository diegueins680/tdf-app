-- Additive Persistent-compatible schema for the existing admin calendar API.
-- No OAuth credentials, calendar selection or external synchronization is seeded.
BEGIN;
CREATE TABLE IF NOT EXISTS public.google_calendar_config (
  id BIGSERIAL PRIMARY KEY,
  owner_id BIGINT REFERENCES public.party(id),
  calendar_id VARCHAR NOT NULL,
  access_token VARCHAR,
  refresh_token VARCHAR,
  token_type VARCHAR,
  token_expires_at TIMESTAMPTZ,
  sync_cursor VARCHAR,
  synced_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT unique_calendar UNIQUE (calendar_id)
);
-- A database initialized by an older binary may lack the later optional owner.
ALTER TABLE public.google_calendar_config
  ADD COLUMN IF NOT EXISTS owner_id BIGINT REFERENCES public.party(id);
CREATE TABLE IF NOT EXISTS public.google_calendar_event (
  id BIGSERIAL PRIMARY KEY,
  calendar_id VARCHAR NOT NULL,
  google_id VARCHAR NOT NULL,
  status VARCHAR NOT NULL,
  summary VARCHAR,
  description VARCHAR,
  location VARCHAR,
  start_at TIMESTAMPTZ,
  end_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ,
  html_link VARCHAR,
  attendees VARCHAR,
  raw_payload VARCHAR,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_local TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT unique_calendar_event UNIQUE (calendar_id, google_id)
);
-- The established identity migration could not attach this guard to an absent
-- table. Keep its archived-account rule when creating the missing dependency.
DROP TRIGGER IF EXISTS identity_archive_reference_guard ON public.google_calendar_config;
CREATE TRIGGER identity_archive_reference_guard
  BEFORE INSERT OR UPDATE ON public.google_calendar_config
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference('owner_id');
COMMIT;
