-- Additive; never rewrite the five already integrated storage migrations.
CREATE TABLE IF NOT EXISTS content_storage_credential (
  id uuid PRIMARY KEY,
  owner_party_id bigint NOT NULL UNIQUE REFERENCES party(id),
  sealed bytea NOT NULL CHECK (octet_length(sealed) >= 29),
  expires_at timestamptz NOT NULL,
  revision bigint NOT NULL DEFAULT 0 CHECK (revision >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);
REVOKE ALL ON content_storage_credential FROM PUBLIC;
COMMENT ON COLUMN content_storage_credential.sealed IS
  'Versioned AES-256-GCM envelope; owner/provider/connection bound AAD; key is server-only, outside database and backups.';

-- Exact canonical object reference. A URL alone never grants another account
-- access. Domain writers verify ownership and bind the resource atomically.
CREATE TABLE IF NOT EXISTS content_storage_reference (
  resource_kind text NOT NULL CHECK (resource_kind IN ('course_receipt','course_follow_up','payment')),
  resource_id bigint NOT NULL CHECK (resource_id > 0),
  file_id uuid NOT NULL REFERENCES content_storage_upload(id),
  bound_by bigint NOT NULL REFERENCES party(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(resource_kind,resource_id)
);
CREATE INDEX IF NOT EXISTS content_storage_reference_file ON content_storage_reference(file_id);
