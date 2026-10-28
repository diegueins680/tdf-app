-- Synthetic full-schema authorization regression; all effects are rolled back.
BEGIN;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
 SELECT n,'Synthetic unassigned source '||n,false,now() FROM generate_series(919146001,919146005) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
 SELECT n,'unassigned-source-'||n,'not-a-login-hash',true FROM generate_series(919146001,919146005) n;
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 919146005,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919146001,NULL,'Synthetic unassigned public event',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":true}' FROM event_type WHERE code='concert';
INSERT INTO event_moment(id,event_id,author_party_id,author_name,caption,media_url,media_type,created_at,updated_at)
 VALUES(919146001,919146001,'919146002','Synthetic author','Synthetic unassigned memory','','text',now(),now());
DO $$ DECLARE target uuid; context jsonb; payload jsonb; result_value jsonb; op text; key uuid; audit_count bigint;
BEGIN
 target:=(interaction_summary(919146003,'event_moment','919146001')->>'id')::uuid;
 ASSERT target IS NOT NULL;
 context:=event_memory_source_context(target,919146003);
 payload:=jsonb_build_object('operation','source.hide','reason','Synthetic unauthorized attempt',
  'expectedVersion',(SELECT version FROM interaction_target WHERE id=target),'expectedSourceRevision',context->>'sourceRevision');
 result_value:=interaction_command(919146003,target,gen_random_uuid(),payload);
 RAISE NOTICE 'Synthetic ownerless command result=%, state=%',result_value,
  (SELECT source_moderation_state FROM event_moment WHERE id=919146001);
 ASSERT result_value->>'error' IS NOT DISTINCT FROM 'forbidden',
  'A reader must not hide a memory when the organizer is NULL';
 ASSERT (SELECT source_moderation_state='visible' FROM event_moment WHERE id=919146001);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_audit WHERE target_id=target);
 ASSERT context->'canHide' IS NOT DISTINCT FROM 'false'::jsonb;
 ASSERT context->'canRestore' IS NOT DISTINCT FROM 'false'::jsonb;
 ASSERT context->'canRemove' IS NOT DISTINCT FROM 'false'::jsonb;
 ASSERT context->'canReport' IS NOT DISTINCT FROM 'true'::jsonb;
 result_value:=interaction_command(919146003,target,gen_random_uuid(),jsonb_build_object(
  'operation','source.report','reason','Synthetic report on unassigned event','expectedSourceRevision',context->>'sourceRevision'));
 ASSERT result_value->>'reported' IS NOT DISTINCT FROM 'true','Ordinary reader can still report';
 ASSERT event_memory_source_reports(919146003,919146001,NULL,20) IS NOT DISTINCT FROM
  '{"supported":true,"canReview":false,"items":[],"nextCursor":null}'::jsonb,'No review rows or reasons leak to reader';
 FOREACH op IN ARRAY ARRAY['source.hide','source.restore','source.report.resolve','source.remove'] LOOP
  payload:=payload||jsonb_build_object('operation',op);
  IF op='source.report.resolve' THEN payload:=payload||'{"decision":"dismissed"}'::jsonb; ELSE payload:=payload-'decision'; END IF;
  ASSERT interaction_command(919146003,target,gen_random_uuid(),payload)->>'error' IS NOT DISTINCT FROM 'forbidden',
   'Unassigned organizer never confers enforcement';
 END LOOP;
 ASSERT (SELECT state='open' FROM interaction_report WHERE target_id=target AND comment_id IS NULL);
 ASSERT (SELECT count(*)=1 FROM interaction_audit WHERE target_id=target),'Only the authorized report created audit';
 -- Positive control: the existing scoped platform moderator retains authority.
 context:=event_memory_source_context(target,919146005);
 ASSERT context->'canHide' IS NOT DISTINCT FROM 'true'::jsonb;
 payload:=jsonb_build_object('operation','source.hide','reason','Synthetic moderator decision',
  'expectedVersion',(SELECT version FROM interaction_target WHERE id=target),'expectedSourceRevision',context->>'sourceRevision');
 ASSERT interaction_command(919146005,target,gen_random_uuid(),payload)->>'state' IS NOT DISTINCT FROM 'hidden';
 ASSERT interaction_resolve('event_moment','919146001',919146003) IS NULL;
 context:=event_memory_source_context(target,919146005);
 payload:=payload||jsonb_build_object('operation','source.restore','expectedVersion',(SELECT version FROM interaction_target WHERE id=target),
  'expectedSourceRevision',context->>'sourceRevision');
 ASSERT interaction_command(919146005,target,gen_random_uuid(),payload)->>'state' IS NOT DISTINCT FROM 'visible';
 -- Revoking an organizer to NULL must also revoke an already recorded replay.
 UPDATE social_event SET organizer_party_id='919146001' WHERE id=919146001;
 context:=event_memory_source_context(target,919146001);
 payload:=jsonb_build_object('operation','source.hide','reason','Synthetic former organizer',
  'expectedVersion',(SELECT version FROM interaction_target WHERE id=target),'expectedSourceRevision',context->>'sourceRevision');
 key:=gen_random_uuid();
 ASSERT interaction_command(919146001,target,key,payload)->>'state' IS NOT DISTINCT FROM 'hidden';
 UPDATE social_event SET organizer_party_id=NULL WHERE id=919146001;
 SELECT count(*) INTO audit_count FROM interaction_audit WHERE target_id=target;
 ASSERT interaction_command(919146001,target,key,payload)->>'error' IS NOT DISTINCT FROM 'unavailable',
  'A former organizer without current read/enforcement authority cannot replay';
 ASSERT (SELECT count(*)=audit_count FROM interaction_audit WHERE target_id=target);
 ASSERT event_memory_source_reports(919146001,919146001,NULL,20) IS NOT DISTINCT FROM
  '{"supported":true,"canReview":false,"items":[],"nextCursor":null}'::jsonb;
END $$;
ROLLBACK;
