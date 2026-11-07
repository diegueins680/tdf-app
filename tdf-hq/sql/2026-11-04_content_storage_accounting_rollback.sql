DO $$ BEGIN
  IF EXISTS(SELECT 1 FROM content_storage_upload) OR EXISTS(SELECT 1 FROM content_storage_reconciliation) THEN
    RAISE EXCEPTION 'Refusing to discard content accounting or reconciliation evidence';
  END IF;
END $$;
DROP TRIGGER IF EXISTS content_storage_new_account ON party;
DROP FUNCTION IF EXISTS content_storage_new_account();
DROP FUNCTION IF EXISTS content_storage_ensure_account(bigint);
DROP FUNCTION IF EXISTS content_storage_new_party(bigint);
DROP TABLE IF EXISTS content_storage_legacy_cutoff;
DROP FUNCTION IF EXISTS content_storage_reconcile(bigint,bigint,text,bigint);
DROP TABLE IF EXISTS content_storage_reconciliation;
ALTER TABLE content_storage_account DROP COLUMN IF EXISTS legacy_bytes;
ALTER TABLE content_storage_account DROP COLUMN IF EXISTS legacy_usage_verified;
ALTER TABLE content_storage_account DROP CONSTRAINT IF EXISTS content_storage_account_party_id_fkey;
ALTER TABLE content_storage_account ADD CONSTRAINT content_storage_account_party_id_fkey FOREIGN KEY(party_id) REFERENCES party(id);

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

