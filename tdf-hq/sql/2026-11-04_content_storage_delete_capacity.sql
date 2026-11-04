-- Pending physical deletion still occupies capacity. Apply after accounting.
CREATE OR REPLACE FUNCTION content_storage_reconcile(p_party bigint,p_bytes bigint,p_evidence text,p_revision bigint)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE account content_storage_account; prior content_storage_reconciliation;
BEGIN
  IF p_bytes<0 OR p_evidence !~ '^[a-f0-9]{64}$' THEN RETURN jsonb_build_object('code','invalid_reconciliation'); END IF;
  INSERT INTO content_storage_account(party_id) VALUES(p_party) ON CONFLICT DO NOTHING;
  SELECT * INTO STRICT account FROM content_storage_account WHERE party_id=p_party FOR UPDATE;
  SELECT * INTO prior FROM content_storage_reconciliation WHERE party_id=p_party AND evidence_sha256=p_evidence;
  IF FOUND THEN
    IF prior.legacy_bytes<>p_bytes THEN RETURN jsonb_build_object('code','reconciliation_conflict'); END IF;
    RETURN jsonb_build_object('status','already_recorded','revision',prior.applied_revision);
  END IF;
  IF account.revision<>p_revision THEN RETURN jsonb_build_object('code','reconciliation_conflict'); END IF;
  IF p_bytes > 9223372036854775807::numeric - (SELECT COALESCE(sum(byte_size),0) FROM content_storage_upload WHERE owner_party_id=p_party AND destination='tdf' AND (status<>'cancelled' OR deleted_at IS NULL)) THEN
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
  IF p_destination='tdf' AND NOT account.legacy_usage_verified THEN
    RETURN jsonb_build_object('code','storage_inventory_required');
  END IF;
  SELECT COALESCE(sum(byte_size),0) INTO occupied FROM content_storage_upload
    WHERE owner_party_id=p_owner AND destination='tdf' AND (status<>'cancelled' OR deleted_at IS NULL);
  occupied := occupied + account.legacy_bytes;
  IF p_destination='tdf' AND (p_bytes > GREATEST(account.allowance_bytes,5000000000) - occupied) THEN
    RETURN jsonb_build_object('code','storage_quota_exceeded');
  END IF;
  INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
    VALUES(p_id,p_owner,p_key,p_fingerprint,p_bytes,p_destination,p_visibility,p_name,p_media_type,p_sha256,'uploading',p_lease,clock_timestamp()+interval '2 minutes');
  RETURN jsonb_build_object('id',p_id,'status','uploading','lease',p_lease);
END $$;

