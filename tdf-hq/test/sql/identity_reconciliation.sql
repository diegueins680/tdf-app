-- Synthetic, transactional integration coverage. Roll back fixture rows.
BEGIN;
SET LOCAL lock_timeout='5s';
DO $$
DECLARE a bigint; b bigint; c bigint; actor_id bigint; case_key uuid:=gen_random_uuid();
  operation_key uuid:=gen_random_uuid(); plan jsonb; result jsonb; count_before bigint;
BEGIN
  INSERT INTO party(display_name,is_org,created_at) VALUES('Identity test operator',false,now()) RETURNING id INTO actor_id;
  a:=identity_create_contact(actor_id,'request-test-0001','{"display_name":"Shared Name","is_org":false}');
  b:=identity_create_contact(actor_id,'request-test-0002','{"display_name":"Shared Name","is_org":false}');
  IF a=b THEN RAISE EXCEPTION 'same name incorrectly collapsed distinct requests'; END IF;
  IF identity_create_contact(actor_id,'request-test-0001','{"display_name":"Shared Name","is_org":false}')<>a THEN RAISE EXCEPTION 'contact retry created a second identity'; END IF;
  BEGIN
    PERFORM identity_create_contact(actor_id,'request-test-0001','{"display_name":"Changed","is_org":false}');
    RAISE EXCEPTION 'changed-payload replay accepted';
  EXCEPTION WHEN invalid_parameter_value THEN NULL; END;
  UPDATE party SET primary_phone='+10000000001' WHERE id=b;
  INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)
    SELECT case_key,ARRAY[a,b],'{}',jsonb_agg(to_jsonb(p) ORDER BY p.id),'Synthetic exact identity test' FROM party p WHERE id IN(a,b);
  plan:=identity_merge_plan(case_key);
  IF (plan->>'can_execute')::boolean THEN RAISE EXCEPTION 'unreviewed identity merged'; END IF;
  UPDATE identity_reconciliation_case SET status='confirmed',reviewed_by=actor_id,reviewed_at=now(),evidence=jsonb_build_object(
    'basis','verified-source-subject','issuer','fixture-issuer','scope','fixture-tenant','subject','fixture-subject',
    'evidence_reference','synthetic-test-evidence-only','external_reference_review','no-unresolved-references','member_ids',ARRAY[a,b]) WHERE id=case_key;
  UPDATE identity_reconciliation_case SET evidence=evidence||jsonb_build_object('member_ids',ARRAY[a]) WHERE id=case_key;
  IF (identity_merge_plan(case_key)->>'can_execute')::boolean THEN RAISE EXCEPTION 'partial-group proof authorized a merge'; END IF;
  UPDATE identity_reconciliation_case SET evidence=evidence||jsonb_build_object('member_ids',ARRAY[a,b]) WHERE id=case_key;
  -- A credential on a redundant record cannot be moved or discarded, even if disabled.
  INSERT INTO user_credential(party_id,username,password_hash,active) VALUES(b,'identity-fixture-disabled','not-a-real-password',false);
  IF (identity_merge_plan(case_key)->>'can_execute')::boolean THEN RAISE EXCEPTION 'disabled authentication identity was ignored'; END IF;
  DELETE FROM user_credential WHERE party_id=b;
  -- Privilege/ownership references, including those with no FK, block execution.
  INSERT INTO party_security_role(party_id,role_id) VALUES(b,gen_random_uuid());
  IF (identity_merge_plan(case_key)->>'can_execute')::boolean THEN RAISE EXCEPTION 'soft ownership reference ignored'; END IF;
  DELETE FROM party_security_role WHERE party_id=b;
  INSERT INTO booking(party_id) VALUES(b);
  IF (identity_merge_plan(case_key)->>'can_execute')::boolean THEN RAISE EXCEPTION 'booking reference ignored'; END IF;
  DELETE FROM booking WHERE party_id=b;
  plan:=identity_merge_plan(case_key);
  IF NOT (plan->>'can_execute')::boolean THEN RAISE EXCEPTION 'valid synthetic case blocked: %',plan->'blockers'; END IF;
  SELECT count(*) INTO count_before FROM party;
  -- Stale evidence must fail before changing any rows.
  UPDATE party SET notes='concurrent edit' WHERE id=b;
  BEGIN
    PERFORM identity_execute_merge(operation_key,case_key,plan->>'fingerprint');
    RAISE EXCEPTION 'stale plan executed';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  UPDATE party SET notes=NULL WHERE id=b;
  result:=identity_execute_merge(operation_key,case_key,plan->>'fingerprint');
  IF result->>'status'<>'applied' THEN RAISE EXCEPTION 'merge did not apply'; END IF;
  IF (SELECT count(*) FROM party)<>count_before THEN RAISE EXCEPTION 'party was deleted'; END IF;
  IF (SELECT primary_phone FROM party WHERE id=a)<>'+10000000001' THEN RAISE EXCEPTION 'compatible field was lost'; END IF;
  IF NOT EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=b AND canonical_party_id=a) THEN RAISE EXCEPTION 'mapping missing'; END IF;
  IF identity_execute_merge(operation_key,case_key,plan->>'fingerprint')->>'status'<>'already-applied' THEN RAISE EXCEPTION 'merge retry repeated effects'; END IF;
  BEGIN
    INSERT INTO booking(party_id) VALUES(b);
    RAISE EXCEPTION 'new archived reference allowed';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  BEGIN
    UPDATE party SET notes='illegal edit' WHERE id=b;
    RAISE EXCEPTION 'archived party write allowed';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  UPDATE party SET notes='unrelated later edit',primary_phone='+10000000002' WHERE id=a;
  BEGIN
    PERFORM identity_rollback_merge(operation_key);
    RAISE EXCEPTION 'conflicting rollback succeeded';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  UPDATE party SET primary_phone='+10000000001' WHERE id=a;
  PERFORM identity_rollback_merge(operation_key);
  IF (SELECT notes FROM party WHERE id=a)<>'unrelated later edit' OR (SELECT primary_phone FROM party WHERE id=a) IS NOT NULL THEN
    RAISE EXCEPTION 'rollback lost later edit or failed to undo merged field';
  END IF;
  IF EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=b) THEN RAISE EXCEPTION 'rollback left archived record'; END IF;
  IF identity_rollback_merge(operation_key)->>'status'<>'already-reverted' THEN RAISE EXCEPTION 'rollback retry repeated effects'; END IF;
  -- Conflicting values cannot be settled from creation/import timestamps.
  c:=identity_create_contact(actor_id,'request-test-0003','{"display_name":"Shared Name","is_org":false,"primary_phone":"+19999999999"}');
  UPDATE identity_reconciliation_case SET member_ids=ARRAY[b,c],status='confirmed',evidence=evidence||jsonb_build_object('member_ids',ARRAY[b,c]),
    before_parties=(SELECT jsonb_agg(to_jsonb(p) ORDER BY p.id) FROM party p WHERE id IN(b,c)) WHERE id=case_key;
  IF (identity_merge_plan(case_key)->>'can_execute')::boolean THEN RAISE EXCEPTION 'conflicting values merged'; END IF;
  RAISE NOTICE 'identity integration: retry, false positives, whole-group proof, ownership, relationships, stale plans, archival, field provenance and rollback passed';
END $$;
ROLLBACK;
