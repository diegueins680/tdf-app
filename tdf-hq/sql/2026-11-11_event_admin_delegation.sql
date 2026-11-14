-- Delegated event administrators and the single-use invitation links that
-- grant them. Additive: creates no events, accounts, grants or invitations.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS public.event_admin_invitation (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  token_sha256 TEXT NOT NULL,
  invitee_name TEXT NOT NULL,
  created_by TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMPTZ NOT NULL,
  accepted_by TEXT,
  accepted_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  CONSTRAINT unique_event_admin_invitation_token UNIQUE (token_sha256),
  CONSTRAINT event_admin_invitation_token_check CHECK (token_sha256 ~ '^[0-9a-f]{64}$'),
  CONSTRAINT event_admin_invitation_name_check CHECK (btrim(invitee_name) <> ''),
  CONSTRAINT event_admin_invitation_window_check CHECK (expires_at > created_at),
  CONSTRAINT event_admin_invitation_accept_check
    CHECK ((accepted_by IS NULL) = (accepted_at IS NULL))
);

CREATE INDEX IF NOT EXISTS event_admin_invitation_event_idx
  ON public.event_admin_invitation (event_id, created_at DESC);

CREATE TABLE IF NOT EXISTS public.event_admin_member (
  id BIGSERIAL PRIMARY KEY,
  event_id BIGINT NOT NULL REFERENCES public.social_event(id) ON DELETE CASCADE,
  party_id TEXT NOT NULL,
  granted_by TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_event_admin_member UNIQUE (event_id, party_id),
  CONSTRAINT event_admin_member_party_check CHECK (party_id ~ '^[1-9][0-9]*$')
);

COMMIT;
