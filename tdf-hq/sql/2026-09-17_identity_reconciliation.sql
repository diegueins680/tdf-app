-- Operator-only identity decisions. Contact attributes are hints, never identity proof.
-- No legacy rows are merged or constrained by email/phone by this migration.
BEGIN;
CREATE TABLE IF NOT EXISTS identity_contact_request (
  actor_party_id bigint NOT NULL REFERENCES party(id),
  request_key text NOT NULL CHECK(length(request_key) BETWEEN 16 AND 128),
  request_body jsonb NOT NULL,
  party_id bigint NOT NULL REFERENCES party(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(actor_party_id,request_key)
);
CREATE TABLE IF NOT EXISTS identity_reconciliation_case (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  member_ids bigint[] NOT NULL,
  status text NOT NULL DEFAULT 'review' CHECK(status IN ('review','separate','confirmed','applied','reverted')),
  evidence jsonb NOT NULL,
  before_parties jsonb NOT NULL,
  reason text NOT NULL CHECK(length(trim(reason)) BETWEEN 10 AND 4000),
  reviewed_by bigint REFERENCES party(id),
  reviewed_at timestamptz,
  created_at timestamptz NOT NULL DEFAULT now(),
  CHECK(cardinality(member_ids)>=2),
  CHECK(jsonb_typeof(evidence)='object'),
  CHECK(jsonb_typeof(before_parties)='array')
);
CREATE TABLE IF NOT EXISTS identity_merge_history (
  operation_id uuid PRIMARY KEY,
  case_id uuid NOT NULL REFERENCES identity_reconciliation_case(id),
  canonical_party_id bigint NOT NULL REFERENCES party(id),
  retired_party_ids bigint[] NOT NULL,
  before_parties jsonb NOT NULL,
  after_parties jsonb NOT NULL,
  applied_at timestamptz NOT NULL DEFAULT now(),
  reverted_at timestamptz,
  actor text NOT NULL DEFAULT session_user
);
CREATE TABLE IF NOT EXISTS identity_party_archive (
  party_id bigint PRIMARY KEY REFERENCES party(id),
  canonical_party_id bigint NOT NULL REFERENCES party(id),
  operation_id uuid NOT NULL REFERENCES identity_merge_history(operation_id),
  archived_at timestamptz NOT NULL DEFAULT now(),
  CHECK(party_id<>canonical_party_id)
);
REVOKE ALL ON identity_reconciliation_case,identity_merge_history,identity_party_archive,identity_contact_request FROM PUBLIC;

-- This function is called only after the existing CRM authorization check.
-- A key is scoped to the authenticated actor, and a changed payload is rejected.
CREATE OR REPLACE FUNCTION identity_create_contact(actor_id bigint, request_id text, body jsonb)
RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE prior identity_contact_request%ROWTYPE; result_id bigint;
BEGIN
  IF actor_id IS NULL OR request_id IS NULL OR length(request_id) NOT BETWEEN 16 AND 128
    OR request_id !~ '^[A-Za-z0-9_-]+$' OR jsonb_typeof(body)<>'object'
    OR coalesce(length(trim(body->>'display_name')),0)=0 THEN
    RAISE EXCEPTION 'invalid contact request' USING ERRCODE='22023';
  END IF;
  PERFORM pg_advisory_xact_lock(hashtextextended('identity-contact:'||actor_id||':'||request_id,0));
  SELECT * INTO prior FROM identity_contact_request WHERE actor_party_id=actor_id AND request_key=request_id;
  IF FOUND THEN
    IF prior.request_body IS DISTINCT FROM body THEN
      RAISE EXCEPTION 'contact request key already used with another payload' USING ERRCODE='22023';
    END IF;
    -- Do not silently reuse an archived identity, or grant access via its alias.
    IF EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=prior.party_id) THEN
      RAISE EXCEPTION 'contact request was archived; resolve through administrative review' USING ERRCODE='55000';
    END IF;
    RETURN prior.party_id;
  END IF;
  INSERT INTO party(display_name,legal_name,is_org,tax_id,primary_email,primary_phone,
    whatsapp,instagram,emergency_contact,notes,created_at)
  VALUES(body->>'display_name',body->>'legal_name',(body->>'is_org')::boolean,
    body->>'tax_id',body->>'primary_email',body->>'primary_phone',body->>'whatsapp',
    body->>'instagram',body->>'emergency_contact',body->>'notes',now()) RETURNING id INTO result_id;
  INSERT INTO identity_contact_request(actor_party_id,request_key,request_body,party_id)
  VALUES(actor_id,request_id,body,result_id);
  RETURN result_id;
END $$;

-- Every declared FK plus likely legacy scalar identity references is examined.
-- Unknown dependencies block a merge; financial/audit/permission rows are never rewritten.
CREATE OR REPLACE FUNCTION identity_party_dependencies(candidate bigint)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE item record; n bigint; result jsonb:='[]'::jsonb;
BEGIN
  FOR item IN
    SELECT DISTINCT ns.nspname AS schema_name,cl.relname AS table_name,a.attname AS column_name
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    JOIN pg_namespace ns ON ns.oid=cl.relnamespace
    WHERE ns.nspname='public' AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f'
           AND fk.confrelid='party'::regclass AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$'
        OR (a.attname IN ('user_id','owner_user_id','claimant_user_id','actor_id','created_by','updated_by')
            AND cl.relname<>'party'))
    ORDER BY 1,2,3
  LOOP
    EXECUTE format('SELECT count(*) FROM %I.%I WHERE %I::text=$1',item.schema_name,item.table_name,item.column_name)
      INTO n USING candidate::text;
    IF n>0 THEN result:=result||jsonb_build_array(jsonb_build_object('table',item.table_name,'column',item.column_name,'count',n)); END IF;
  END LOOP;
  RETURN result;
END $$;

CREATE OR REPLACE FUNCTION identity_merge_plan(case_key uuid)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE c identity_reconciliation_case%ROWTYPE; survivor bigint; members bigint[];
  snapshots jsonb; blockers jsonb:='[]'::jsonb; changes jsonb:='{}'::jsonb;
  source_row record; field_name text; values_found jsonb; target_body jsonb; deps jsonb;
BEGIN
  SELECT * INTO c FROM identity_reconciliation_case WHERE id=case_key;
  IF NOT FOUND THEN RAISE EXCEPTION 'identity case not found' USING ERRCODE='P0002'; END IF;
  SELECT array_agg(DISTINCT v ORDER BY v) INTO members FROM unnest(c.member_ids) v;
  SELECT jsonb_agg(to_jsonb(p) ORDER BY p.id) INTO snapshots FROM party p WHERE id=ANY(members);
  IF cardinality(members)<>cardinality(c.member_ids) OR jsonb_array_length(snapshots)<>cardinality(members) THEN
    blockers:=blockers||'"invalid-members"'::jsonb;
  END IF;
  -- Established accounts survive; stable oldest ID breaks ties. Creation time is
  -- used only for identity stability, never as profile verification/freshness.
  SELECT p.id INTO survivor FROM party p WHERE p.id=ANY(members)
  ORDER BY EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=p.id AND u.active) DESC,p.created_at,p.id LIMIT 1;
  SELECT to_jsonb(p) INTO target_body FROM party p WHERE id=survivor;
  IF c.status<>'confirmed' OR c.reviewed_by IS NULL OR c.reviewed_at IS NULL THEN
    blockers:=blockers||'"identity-review-required"'::jsonb;
  END IF;
  -- One reviewed issuing system, scope and subject must identify the WHOLE group.
  -- Names, handles, emails, phones and pairwise chains cannot satisfy this gate.
  IF coalesce(c.evidence->>'basis','') NOT IN ('verified-source-subject','authenticated-owner-attestation')
    OR coalesce(length(c.evidence->>'issuer'),0)=0
    OR coalesce(length(c.evidence->>'scope'),0)=0
    OR coalesce(length(c.evidence->>'subject'),0)=0
    OR coalesce(length(c.evidence->>'evidence_reference'),0)<10
    OR c.evidence->'member_ids' IS DISTINCT FROM to_jsonb(members)
    OR c.evidence->>'external_reference_review' IS DISTINCT FROM 'no-unresolved-references' THEN
    blockers:=blockers||'"whole-group-proof-required"'::jsonb;
  END IF;
  IF snapshots IS DISTINCT FROM c.before_parties THEN blockers:=blockers||'"stale-party-evidence"'::jsonb; END IF;
  IF EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=ANY(members) OR canonical_party_id=ANY(members)) THEN
    blockers:=blockers||'"existing-canonical-mapping"'::jsonb;
  END IF;
  IF EXISTS(SELECT 1 FROM party WHERE id=ANY(members) AND is_org) THEN
    blockers:=blockers||'"organization-or-artist-review-required"'::jsonb;
  END IF;
  FOR source_row IN SELECT p.* FROM party p WHERE p.id=ANY(members) AND p.id<>survivor LOOP
    deps:=identity_party_dependencies(source_row.id);
    IF deps<>'[]'::jsonb THEN blockers:=blockers||jsonb_build_array(jsonb_build_object('party_id',source_row.id,'dependencies',deps)); END IF;
    IF source_row.stripe_customer_id IS NOT NULL THEN blockers:=blockers||'"payment-identity-conflict"'::jsonb; END IF;
  END LOOP;
  FOREACH field_name IN ARRAY ARRAY['display_name','legal_name','tax_id','primary_email','primary_phone','whatsapp','instagram','emergency_contact','notes','country_id','country_code'] LOOP
    SELECT jsonb_agg(DISTINCT row_value->field_name) INTO values_found FROM jsonb_array_elements(snapshots) row_value
      WHERE row_value->>field_name IS NOT NULL AND trim(row_value->>field_name)<>'';
    IF jsonb_array_length(values_found)>1 THEN
      blockers:=blockers||jsonb_build_array(jsonb_build_object('field',field_name,'reason','conflicting-unverified-values'));
    ELSIF jsonb_array_length(values_found)=1 AND coalesce(trim(target_body->>field_name),'')='' THEN
      -- Contact email is an authentication lookup input in the legacy system.
      IF field_name='primary_email' AND EXISTS(SELECT 1 FROM user_credential WHERE party_id=survivor) THEN
        blockers:=blockers||'"authentication-email-change"'::jsonb;
      ELSE changes:=changes||jsonb_build_object(field_name,values_found->0); END IF;
    END IF;
  END LOOP;
  RETURN jsonb_build_object('case_id',case_key,'survivor',survivor,'members',members,'blockers',blockers,
    'changes',changes,'before',snapshots,'can_execute',blockers='[]'::jsonb,
    'fingerprint',encode(digest(coalesce(snapshots::text,'null')||c.evidence::text||c.status||coalesce(c.reviewed_by::text,''),'sha256'),'hex'));
END $$;

CREATE OR REPLACE FUNCTION identity_execute_merge(operation_key uuid,case_key uuid,expected_fingerprint text)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE plan jsonb; prior identity_merge_history%ROWTYPE; members bigint[]; survivor bigint; after_state jsonb;
  relation record; changed party%ROWTYPE;
BEGIN
  -- Serialize this maintenance operation, and fence every public-table writer.
  -- Batches are one small group with a caller-supplied lock_timeout. No network
  -- operation or notification occurs while these locks are held.
  PERFORM pg_advisory_xact_lock(hashtextextended('identity-reconciliation',0));
  SELECT * INTO prior FROM identity_merge_history WHERE operation_id=operation_key;
  IF FOUND THEN
    IF prior.case_id<>case_key OR prior.reverted_at IS NOT NULL THEN RAISE EXCEPTION 'operation replay conflict'; END IF;
    RETURN jsonb_build_object('operation_id',operation_key,'status','already-applied');
  END IF;
  FOR relation IN SELECT oid::regclass name FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind='r' ORDER BY oid LOOP
    EXECUTE format('LOCK TABLE %s IN SHARE ROW EXCLUSIVE MODE',relation.name);
  END LOOP;
  plan:=identity_merge_plan(case_key);
  IF plan->>'can_execute'<>'true' OR plan->>'fingerprint' IS DISTINCT FROM expected_fingerprint THEN
    RAISE EXCEPTION 'merge plan is blocked or stale' USING ERRCODE='55000';
  END IF;
  SELECT array_agg(v::bigint ORDER BY v::bigint) INTO members FROM jsonb_array_elements_text(plan->'members') v;
  survivor:=(plan->>'survivor')::bigint;
  SELECT * INTO changed FROM jsonb_populate_record(NULL::party,
    (SELECT to_jsonb(p) FROM party p WHERE id=survivor)||(plan->'changes'));
  UPDATE party SET display_name=changed.display_name,legal_name=changed.legal_name,tax_id=changed.tax_id,
    primary_email=changed.primary_email,primary_phone=changed.primary_phone,whatsapp=changed.whatsapp,
    instagram=changed.instagram,emergency_contact=changed.emergency_contact,notes=changed.notes,
    country_id=changed.country_id,country_code=changed.country_code WHERE id=survivor;
  SELECT jsonb_agg(to_jsonb(p) ORDER BY p.id) INTO after_state FROM party p WHERE id=ANY(members);
  INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)
  VALUES(operation_key,case_key,survivor,array_remove(members,survivor),plan->'before',after_state);
  INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)
    SELECT v,survivor,operation_key FROM unnest(members) v WHERE v<>survivor;
  UPDATE identity_reconciliation_case SET status='applied' WHERE id=case_key;
  RETURN jsonb_build_object('operation_id',operation_key,'status','applied','archived',cardinality(members)-1);
END $$;

CREATE OR REPLACE FUNCTION identity_rollback_merge(operation_key uuid)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE h identity_merge_history%ROWTYPE; before_row jsonb; after_row jsonb; current_row jsonb;
  changed_fields jsonb:='{}'::jsonb; item record; restored party%ROWTYPE;
BEGIN
  PERFORM pg_advisory_xact_lock(hashtextextended('identity-reconciliation',0));
  SELECT * INTO h FROM identity_merge_history WHERE operation_id=operation_key FOR UPDATE;
  IF NOT FOUND THEN RAISE EXCEPTION 'merge operation not found'; END IF;
  IF h.reverted_at IS NOT NULL THEN RETURN jsonb_build_object('status','already-reverted'); END IF;
  PERFORM 1 FROM party WHERE id=h.canonical_party_id OR id=ANY(h.retired_party_ids) ORDER BY id FOR UPDATE;
  SELECT v INTO before_row FROM jsonb_array_elements(h.before_parties) v WHERE (v->>'id')::bigint=h.canonical_party_id;
  SELECT v INTO after_row FROM jsonb_array_elements(h.after_parties) v WHERE (v->>'id')::bigint=h.canonical_party_id;
  SELECT to_jsonb(p) INTO current_row FROM party p WHERE p.id=h.canonical_party_id;
  FOR item IN SELECT key,value FROM jsonb_each(before_row) LOOP
    IF item.value IS DISTINCT FROM after_row->item.key THEN
      IF current_row->item.key IS DISTINCT FROM after_row->item.key THEN
        RAISE EXCEPTION 'rollback conflict on field %; administrative review required',item.key USING ERRCODE='55000';
      END IF;
      changed_fields:=changed_fields||jsonb_build_object(item.key,item.value);
    END IF;
  END LOOP;
  IF changed_fields ? 'primary_email' AND EXISTS(SELECT 1 FROM user_credential WHERE party_id=h.canonical_party_id) THEN
    RAISE EXCEPTION 'rollback would change a current authentication email; review required' USING ERRCODE='55000';
  END IF;
  SELECT * INTO restored FROM jsonb_populate_record(NULL::party,current_row||changed_fields);
  UPDATE party SET display_name=restored.display_name,legal_name=restored.legal_name,tax_id=restored.tax_id,
    primary_email=restored.primary_email,primary_phone=restored.primary_phone,whatsapp=restored.whatsapp,
    instagram=restored.instagram,emergency_contact=restored.emergency_contact,notes=restored.notes,
    country_id=restored.country_id,country_code=restored.country_code WHERE id=h.canonical_party_id;
  DELETE FROM identity_party_archive WHERE operation_id=operation_key;
  UPDATE identity_merge_history SET reverted_at=now() WHERE operation_id=operation_key;
  UPDATE identity_reconciliation_case SET status='reverted' WHERE id=h.case_id;
  RETURN jsonb_build_object('status','reverted');
END $$;
-- Archived contacts cannot acquire new relationships or credentials through any
-- legacy writer. Historical references on unrelated updates remain untouched.
CREATE OR REPLACE FUNCTION identity_reject_archived_reference()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE column_name text; new_value text;
BEGIN
  FOREACH column_name IN ARRAY TG_ARGV LOOP
    new_value:=to_jsonb(NEW)->>column_name;
    IF TG_OP='UPDATE' THEN
      IF new_value IS NOT DISTINCT FROM to_jsonb(OLD)->>column_name THEN CONTINUE; END IF;
    END IF;
    IF EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id::text=new_value) THEN
      RAISE EXCEPTION 'archived contact requires administrative resolution' USING ERRCODE='55000';
    END IF;
  END LOOP;
  RETURN NEW;
END $$;
CREATE OR REPLACE FUNCTION identity_reject_archived_party_write()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=OLD.id) THEN
    RAISE EXCEPTION 'archived contact cannot be modified' USING ERRCODE='55000';
  END IF;
  IF TG_OP='DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS identity_archive_write_guard ON party;
CREATE TRIGGER identity_archive_write_guard BEFORE UPDATE OR DELETE ON party
  FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_party_write();
DO $$
DECLARE item record;
BEGIN
  FOR item IN
    SELECT cl.oid::regclass table_name,string_agg(quote_literal(a.attname),',' ORDER BY a.attnum) columns
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    WHERE cl.relnamespace='public'::regnamespace AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f' AND fk.confrelid='party'::regclass
           AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$')
    GROUP BY cl.oid ORDER BY cl.oid
  LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',item.table_name);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',item.table_name,item.columns);
  END LOOP;
END $$;
REVOKE ALL ON FUNCTION identity_party_dependencies(bigint),identity_merge_plan(uuid),identity_execute_merge(uuid,uuid,text),identity_rollback_merge(uuid) FROM PUBLIC;
COMMIT;
