-- Independent, revocable consent state.  The event log is append-only so a
-- withdrawal never destroys the evidence needed to demonstrate the change.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_consent_state (
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  consent_kind TEXT NOT NULL CHECK (consent_kind IN ('pilot_participation','public_visibility','public_rankings','rating_reminders')),
  granted BOOLEAN NOT NULL DEFAULT FALSE,
  version INTEGER NOT NULL DEFAULT 0 CHECK (version >= 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (party_id, consent_kind)
);

CREATE TABLE IF NOT EXISTS reputation_consent_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  consent_kind TEXT NOT NULL CHECK (consent_kind IN ('pilot_participation','public_visibility','public_rankings','rating_reminders')),
  granted BOOLEAN NOT NULL,
  version INTEGER NOT NULL CHECK (version > 0),
  source TEXT NOT NULL CHECK (source IN ('self_service','admin','migration')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (party_id, consent_kind, version)
);

CREATE INDEX IF NOT EXISTS reputation_consent_state_public_idx
  ON reputation_consent_state(party_id) WHERE consent_kind='public_visibility' AND granted;

COMMIT;
