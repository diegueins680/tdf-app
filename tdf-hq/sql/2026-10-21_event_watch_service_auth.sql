BEGIN;

-- This purpose-specific identity never has security_role or api_token grants.
CREATE TABLE IF NOT EXISTS event_watch_client (
  client_id TEXT PRIMARY KEY CHECK (client_id ~ '^[a-z0-9][a-z0-9_-]{2,47}$'),
  secret_hash TEXT NOT NULL CHECK (secret_hash ~ '^[0-9a-f]{64}$'),
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  allow_publish BOOLEAN NOT NULL DEFAULT FALSE,
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp()
);

CREATE TABLE IF NOT EXISTS event_watch_session (
  token_hash TEXT PRIMARY KEY CHECK (token_hash ~ '^[0-9a-f]{64}$'),
  client_id TEXT NOT NULL REFERENCES event_watch_client(client_id) ON DELETE CASCADE,
  issued_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  expires_at TIMESTAMPTZ NOT NULL,
  CHECK (expires_at > issued_at),
  CHECK (expires_at <= issued_at + interval '15 minutes')
);
CREATE INDEX IF NOT EXISTS event_watch_session_client_idx
  ON event_watch_session(client_id);
CREATE INDEX IF NOT EXISTS event_watch_session_expiry_idx
  ON event_watch_session(expires_at);

COMMENT ON TABLE event_watch_client IS
  'Operator-provisioned event research clients. Rotate secret_hash or set active=false to revoke all sessions. No interactive/admin access.';
-- A rotation invalidates outstanding sessions as part of the same transaction.
CREATE OR REPLACE FUNCTION revoke_event_watch_sessions() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.secret_hash IS DISTINCT FROM OLD.secret_hash
     OR NEW.active IS DISTINCT FROM OLD.active
     OR NEW.actor_party_id IS DISTINCT FROM OLD.actor_party_id
     OR NEW.allow_publish IS DISTINCT FROM OLD.allow_publish
     OR NEW.expires_at IS DISTINCT FROM OLD.expires_at THEN
    DELETE FROM event_watch_session WHERE client_id=OLD.client_id;
  END IF;
  RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS event_watch_client_revoke_sessions ON event_watch_client;
CREATE TRIGGER event_watch_client_revoke_sessions
  AFTER UPDATE ON event_watch_client
  FOR EACH ROW EXECUTE FUNCTION revoke_event_watch_sessions();
COMMIT;
