-- AUTH-ARTIST-INVITE-001: staff-issued, single-use artist invitation links.
-- Only a SHA-256 digest of each link token is stored. A link binds to the first
-- account that redeems it; revoked or expired links grant nothing.
BEGIN;

CREATE TABLE IF NOT EXISTS artist_invitation_link (
  id BIGSERIAL PRIMARY KEY,
  token_sha256 TEXT NOT NULL UNIQUE CHECK (token_sha256 ~ '^[0-9a-f]{64}$'),
  campaign TEXT NOT NULL CHECK (campaign ~ '^[a-z0-9_]{1,80}$'),
  invitee_label TEXT NOT NULL CHECK (char_length(btrim(invitee_label)) BETWEEN 1 AND 120),
  created_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  expires_at TIMESTAMPTZ NOT NULL,
  redeemed_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  redeemed_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  CONSTRAINT artist_invitation_link_window CHECK (
    expires_at > created_at AND expires_at <= created_at + INTERVAL '90 days'
  ),
  CONSTRAINT artist_invitation_link_redemption CHECK (
    (redeemed_by_party_id IS NULL) = (redeemed_at IS NULL)
  ),
  CONSTRAINT artist_invitation_link_redeemed_or_revoked CHECK (
    redeemed_at IS NULL OR revoked_at IS NULL
  )
);

CREATE INDEX IF NOT EXISTS artist_invitation_link_created_idx
  ON artist_invitation_link (created_at DESC, id DESC);

COMMIT;
