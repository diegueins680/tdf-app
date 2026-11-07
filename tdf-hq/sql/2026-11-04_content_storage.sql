-- Additive foundation. GB is decimal; existing higher entitlements are retained.
CREATE TABLE IF NOT EXISTS content_storage_account (
  party_id bigint PRIMARY KEY REFERENCES party(id),
  allowance_bytes bigint NOT NULL DEFAULT 5000000000 CHECK (allowance_bytes >= 0),
  default_destination text CHECK (default_destination IN ('tdf','google_drive')),
  revision bigint NOT NULL DEFAULT 0 CHECK (revision >= 0),
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS content_storage_upload (
  id uuid PRIMARY KEY,
  owner_party_id bigint NOT NULL REFERENCES content_storage_account(party_id),
  request_key text NOT NULL CHECK (request_key ~ '^[a-f0-9]{64}$'),
  fingerprint text NOT NULL CHECK (fingerprint ~ '^[a-f0-9]{64}$'),
  byte_size bigint NOT NULL CHECK (byte_size > 0),
  destination text NOT NULL CHECK (destination IN ('tdf','google_drive')),
  visibility text NOT NULL CHECK (visibility IN ('private','public')),
  file_name text NOT NULL,
  media_type text NOT NULL,
  sha256 text NOT NULL CHECK (sha256 ~ '^[a-f0-9]{64}$'),
  status text NOT NULL CHECK (status IN ('uploading','completed','cancelled')),
  lease_id uuid NOT NULL,
  lease_until timestamptz NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  completed_at timestamptz,
  UNIQUE(owner_party_id,request_key),
  CHECK ((status = 'completed') = (completed_at IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS content_storage_upload_usage
  ON content_storage_upload(owner_party_id,destination,status);

-- Serializes all reservations of one account, including exact-key replays.
-- Expired leases keep their bytes reserved until recovery or explicit deletion.
CREATE OR REPLACE FUNCTION content_storage_reserve(
  p_owner bigint, p_key text, p_fingerprint text, p_bytes bigint,
  p_destination text, p_visibility text, p_name text, p_media_type text,
  p_sha256 text, p_id uuid, p_lease uuid
) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE account content_storage_account; existing content_storage_upload;
  occupied bigint;
BEGIN
  INSERT INTO content_storage_account(party_id) VALUES(p_owner) ON CONFLICT DO NOTHING;
  SELECT * INTO STRICT account FROM content_storage_account WHERE party_id=p_owner FOR UPDATE;
  SELECT * INTO existing FROM content_storage_upload WHERE owner_party_id=p_owner AND request_key=p_key FOR UPDATE;
  IF FOUND THEN
    IF existing.fingerprint <> p_fingerprint THEN
      RETURN jsonb_build_object('code','storage_upload_conflict');
    END IF;
    IF existing.status='completed' THEN
      RETURN jsonb_build_object('id',existing.id,'status','completed','lease',existing.lease_id);
    END IF;
    IF existing.status='cancelled' THEN
      RETURN jsonb_build_object('code','storage_upload_cancelled');
    END IF;
    IF existing.lease_until > clock_timestamp() THEN
      RETURN jsonb_build_object('code','storage_upload_in_progress');
    END IF;
    UPDATE content_storage_upload SET lease_id=p_lease,lease_until=clock_timestamp()+interval '2 minutes' WHERE id=existing.id;
    RETURN jsonb_build_object('id',existing.id,'status','uploading','lease',p_lease);
  END IF;
  SELECT COALESCE(sum(byte_size),0) INTO occupied FROM content_storage_upload
    WHERE owner_party_id=p_owner AND destination='tdf' AND status<>'cancelled';
  IF p_destination='tdf' AND (p_bytes > GREATEST(account.allowance_bytes,5000000000) - occupied) THEN
    RETURN jsonb_build_object('code','storage_quota_exceeded');
  END IF;
  INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
    VALUES(p_id,p_owner,p_key,p_fingerprint,p_bytes,p_destination,p_visibility,p_name,p_media_type,p_sha256,'uploading',p_lease,clock_timestamp()+interval '2 minutes');
  RETURN jsonb_build_object('id',p_id,'status','uploading','lease',p_lease);
END $$;

-- The writer must hold the current lease and recheck it under FOR UPDATE before
-- publishing its file. Stale attempts may not finalize another attempt's bytes.
COMMENT ON TABLE content_storage_upload IS 'Canonical file accounting only; legacy unmetered files require explicit inventory/reconciliation before claiming whole-account coverage.';
