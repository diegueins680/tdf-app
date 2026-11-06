-- Existing objects were not consistently metered by account. Never infer zero.
ALTER TABLE content_storage_account ADD COLUMN IF NOT EXISTS legacy_bytes bigint NOT NULL DEFAULT 0 CHECK (legacy_bytes>=0);
ALTER TABLE content_storage_account ADD COLUMN IF NOT EXISTS legacy_usage_verified boolean NOT NULL DEFAULT false;
ALTER TABLE content_storage_account DROP CONSTRAINT IF EXISTS content_storage_account_party_id_fkey;
ALTER TABLE content_storage_account ADD CONSTRAINT content_storage_account_party_id_fkey FOREIGN KEY(party_id) REFERENCES party(id) ON DELETE CASCADE;
CREATE TABLE IF NOT EXISTS content_storage_reconciliation (
  party_id bigint NOT NULL REFERENCES content_storage_account(party_id),
  evidence_sha256 text NOT NULL CHECK(evidence_sha256 ~ '^[a-f0-9]{64}$'),
  legacy_bytes bigint NOT NULL CHECK(legacy_bytes>=0),
  prior_legacy_bytes bigint NOT NULL CHECK(prior_legacy_bytes>=0),
  applied_revision bigint NOT NULL,
  database_actor name NOT NULL DEFAULT session_user,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(party_id,evidence_sha256)
);
-- Accounts are created on first use, never per party: identity merges refuse
-- to run while any row references the retired party, so an account row for
-- every party would block every merge. Parties created after this migration
-- cannot own unmetered legacy objects, so their lazily created account starts
-- verified; parties that existed before it must be reconciled first.
CREATE TABLE IF NOT EXISTS content_storage_legacy_cutoff (
  singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
  max_legacy_party_id bigint NOT NULL CHECK (max_legacy_party_id >= 0)
);
INSERT INTO content_storage_legacy_cutoff(max_legacy_party_id)
  SELECT COALESCE(max(id),0) FROM party ON CONFLICT DO NOTHING;
CREATE OR REPLACE FUNCTION content_storage_new_party(p_party bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT COALESCE((SELECT p_party > max_legacy_party_id FROM content_storage_legacy_cutoff), false)
$$;
CREATE OR REPLACE FUNCTION content_storage_ensure_account(p_party bigint) RETURNS void
LANGUAGE sql AS $$
  INSERT INTO content_storage_account(party_id,legacy_usage_verified)
    VALUES(p_party, content_storage_new_party(p_party)) ON CONFLICT DO NOTHING
$$;
DROP TRIGGER IF EXISTS content_storage_new_account ON party;
DROP FUNCTION IF EXISTS content_storage_new_account();
-- Invoker-security operator primitive, not an HTTP endpoint. Requires the
-- reviewed DB writer and a byte-verified ownership/size report before activation.
CREATE OR REPLACE FUNCTION content_storage_reconcile(p_party bigint,p_bytes bigint,p_evidence text,p_revision bigint)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE account content_storage_account; prior content_storage_reconciliation;
BEGIN
  IF p_bytes<0 OR p_evidence !~ '^[a-f0-9]{64}$' THEN RETURN jsonb_build_object('code','invalid_reconciliation'); END IF;
  PERFORM content_storage_ensure_account(p_party);
  SELECT * INTO STRICT account FROM content_storage_account WHERE party_id=p_party FOR UPDATE;
  SELECT * INTO prior FROM content_storage_reconciliation WHERE party_id=p_party AND evidence_sha256=p_evidence;
  IF FOUND THEN
    IF prior.legacy_bytes<>p_bytes THEN RETURN jsonb_build_object('code','reconciliation_conflict'); END IF;
    RETURN jsonb_build_object('status','already_recorded','revision',prior.applied_revision);
  END IF;
  IF account.revision<>p_revision THEN RETURN jsonb_build_object('code','reconciliation_conflict'); END IF;
  IF p_bytes > 9223372036854775807::numeric - (SELECT COALESCE(sum(byte_size),0) FROM content_storage_upload WHERE owner_party_id=p_party AND destination='tdf' AND status<>'cancelled') THEN
    RETURN jsonb_build_object('code','invalid_reconciliation_size');
  END IF;
  INSERT INTO content_storage_reconciliation(party_id,evidence_sha256,legacy_bytes,prior_legacy_bytes,applied_revision)
    VALUES(p_party,p_evidence,p_bytes,account.legacy_bytes,account.revision+1);
  UPDATE content_storage_account SET legacy_bytes=p_bytes,legacy_usage_verified=true,revision=revision+1 WHERE party_id=p_party;
  RETURN jsonb_build_object('status','recorded','revision',account.revision+1);
END $$;

CREATE OR REPLACE FUNCTION content_storage_reserve(
  p_owner bigint, p_key text, p_fingerprint text, p_bytes bigint,
  p_destination text, p_visibility text, p_name text, p_media_type text,
  p_sha256 text, p_id uuid, p_lease uuid
) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE account content_storage_account; existing content_storage_upload;
  occupied bigint;
BEGIN
  PERFORM content_storage_ensure_account(p_owner);
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
  IF p_destination='tdf' AND NOT account.legacy_usage_verified THEN
    RETURN jsonb_build_object('code','storage_inventory_required');
  END IF;
  SELECT COALESCE(sum(byte_size),0) INTO occupied FROM content_storage_upload
    WHERE owner_party_id=p_owner AND destination='tdf' AND status<>'cancelled';
  occupied := occupied + account.legacy_bytes;
  IF p_destination='tdf' AND (p_bytes > GREATEST(account.allowance_bytes,5000000000) - occupied) THEN
    RETURN jsonb_build_object('code','storage_quota_exceeded');
  END IF;
  INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
    VALUES(p_id,p_owner,p_key,p_fingerprint,p_bytes,p_destination,p_visibility,p_name,p_media_type,p_sha256,'uploading',p_lease,clock_timestamp()+interval '2 minutes');
  RETURN jsonb_build_object('id',p_id,'status','uploading','lease',p_lease);
END $$;

