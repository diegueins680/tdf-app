-- The provider-generated file ID is persisted before any file creation. Retrying
-- a timed-out create cannot allocate a second object, even without a live lease.
CREATE TABLE IF NOT EXISTS content_storage_drive_attempt (
  owner_party_id bigint NOT NULL REFERENCES party(id),
  request_key text NOT NULL CHECK (request_key ~ '^[a-f0-9]{64}$'),
  fingerprint text NOT NULL CHECK (fingerprint ~ '^[a-f0-9]{64}$'),
  provider_account_id text NOT NULL,
  provider_file_id text NOT NULL UNIQUE,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(owner_party_id,request_key)
);
