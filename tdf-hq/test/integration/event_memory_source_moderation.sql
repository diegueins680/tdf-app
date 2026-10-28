-- Synthetic, actual full-schema policy checks. No session/HTTP claim here.
BEGIN;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
 SELECT n,'Synthetic source moderation '||n,false,now() FROM generate_series(919145001,919145006) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
 SELECT n,'source-moderation-'||n,'not-a-login-hash',true FROM generate_series(919145001,919145006) n;
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 919145005,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919145001,'919145001','Public synthetic source policy',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":true}' FROM event_type WHERE code='concert';
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919145002,'919145004','Private synthetic source policy',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":false}' FROM event_type WHERE code='concert';
INSERT INTO event_logistics_member(event_id,party_id,member_role) VALUES(919145001,'919145006','viewer');
INSERT INTO event_moment(id,event_id,author_party_id,author_name,caption,media_url,media_type,created_at,updated_at) VALUES
 (919145001,919145001,'919145002','Synthetic author','Public synthetic memory','','text',now(),now()),
 (919145002,919145002,'919145002','Synthetic author','Private synthetic memory','','text',now(),now());
DO $$ DECLARE target uuid; private_target uuid; summary jsonb; context jsonb; payload jsonb;
 report_key uuid:=gen_random_uuid(); hide_key uuid:=gen_random_uuid(); restore_key uuid:=gen_random_uuid();
 first jsonb; result_value jsonb; reports_before bigint; guarded_resolver text;
BEGIN
 target:=(interaction_summary(919145003,'event_moment','919145001')->>'id')::uuid;
 private_target:=interaction_register('event_moment','919145002',919145004);
 ASSERT target IS NOT NULL AND private_target IS NOT NULL;
 ASSERT interaction_resolve('event_moment','000919145001',919145003) IS NULL,'Noncanonical aliases are rejected before any moderation';
 context:=event_memory_source_context(target,919145003);
 ASSERT (context->>'canReport')::boolean AND NOT (context->>'canHide')::boolean,'Readers can report, never enforce';
 ASSERT NOT (event_memory_source_context(target,919145002)->>'canReport')::boolean,'Cannot report own source';
 ASSERT event_memory_source_context(private_target,919145003) IS NULL,'Private parent access is required';
 ASSERT event_memory_source_context(private_target,919145005) IS NULL,'Admin cannot invent private-event membership';
 ASSERT event_memory_source_context(target,NULL) IS NULL,'No anonymous reporting authority';
 ASSERT event_memory_source_reports(919145003,919145001,NULL,20)->>'canReview'='false','No reader report-inbox disclosure';
 ASSERT event_memory_source_reports(919145001,919145002,NULL,20)->>'error'='unavailable','No cross-event inbox';
 payload:=jsonb_build_object('operation','source.report','reason','Synthetic image-rights concern',
  'expectedSourceRevision',context->>'sourceRevision');
 first:=interaction_command(919145003,target,report_key,payload);
 ASSERT first->>'reported'='true','Source report accepted';
 ASSERT interaction_command(919145003,target,report_key,payload)=first,'Exact report replay';
 ASSERT interaction_command(919145003,target,gen_random_uuid(),payload)->>'reported'='true','Semantic duplicate acknowledged';
 ASSERT (SELECT count(*)=1 FROM interaction_report WHERE target_id=target AND comment_id IS NULL),'One source report';
 ASSERT (SELECT count(*)=1 FROM interaction_audit WHERE target_id=target AND operation='source.report'),'One report audit';
 ASSERT interaction_command(919145003,target,report_key,payload||'{"reason":"Different payload"}')->>'error'='request_key_conflict';
 summary:=event_memory_source_reports(919145001,919145001,NULL,20);
 ASSERT jsonb_array_length(summary->'items')=1 AND summary->'items'->0->>'caption'='Public synthetic memory';
 ASSERT NOT (summary::text LIKE '%reporter_id%' OR summary::text LIKE '%reporterId%'),'Reviewer needs no reporter identities';
 ASSERT event_memory_source_reports(919145001,919145001,private_target,20)->>'error'='invalid_cursor';
 context:=event_memory_source_context(target,919145001);
 payload:=jsonb_build_object('operation','source.hide','reason','Synthetic scope review',
  'expectedSourceRevision',context->>'sourceRevision','expectedVersion',(SELECT version FROM interaction_target WHERE id=target));
 ASSERT interaction_command(919145006,target,gen_random_uuid(),payload)->>'error'='forbidden','Logistics is not organizer';
 ASSERT interaction_command(919145002,target,gen_random_uuid(),payload)->>'error'='forbidden','Source authorship is not enforcement';
 INSERT INTO social_v2_pair(party_a,party_b,block_b) VALUES(919145001,919145002,true);
 ASSERT interaction_resolve('event_moment','919145001',919145001) IS NULL,'Block constrains ordinary social reads';
 ASSERT (event_memory_source_context(target,919145001)->>'canHide')::boolean,'Blocking cannot revoke current organizer enforcement';
 UPDATE event_moment SET caption='Edited synthetic memory',updated_at=clock_timestamp() WHERE id=919145001;
 ASSERT interaction_command(919145001,target,hide_key,payload)->>'error'='revision_conflict','Caption edit invalidates moderation revision';
 payload:=payload||jsonb_build_object('expectedSourceRevision',event_memory_source_context(target,919145001)->>'sourceRevision');
 first:=interaction_command(919145001,target,hide_key,payload);
 ASSERT first->>'state'='hidden','Organizer can hide source';
 ASSERT interaction_command(919145001,target,hide_key,payload)=first,'Hide retries are idempotent';
 ASSERT interaction_resolve('event_moment','919145001',919145003) IS NULL,'Hidden source denied';
 -- Negative control: deliberately restore the pre-moderation authority inside
 -- this rolled-back synthetic transaction. The hidden source becomes readable;
 -- the same safety predicate detects it before restoring the guarded authority.
 guarded_resolver:=pg_get_functiondef('interaction_resolve(text,text,bigint)'::regprocedure);
 EXECUTE (SELECT definition FROM event_memory_moderation_authority WHERE signature='interaction_resolve(text,text,bigint)');
 ASSERT interaction_resolve('event_moment','919145001',919145003) IS NOT NULL,'Negative control must expose the hidden source';
 EXECUTE guarded_resolver;
 ASSERT interaction_resolve('event_moment','919145001',919145003) IS NULL,'Restored guard must deny the hidden source';

 ASSERT interaction_resolve('event_moment','000919145001',919145003) IS NULL,'Alias cannot bypass hiding';
 ASSERT interaction_resolve('event_moment','919145001',NULL) IS NULL,'Anonymous source denied';
 ASSERT interaction_target_context(target,919145003) IS NULL,'Discussion and notification authority denied';
 ASSERT interaction_comments_page(target,919145003,NULL,'newest',NULL,20)->>'error'='unavailable';
 ASSERT interaction_destination(919145003,'target',target)->>'error'='unavailable';
 ASSERT interaction_command(919145003,target,gen_random_uuid(),'{"operation":"comment.create","body":"Forbidden"}')->>'error'='unavailable';
 UPDATE interaction_runtime SET enabled=false WHERE singleton;
 ASSERT interaction_resolve('event_moment','919145001',919145003) IS NULL,'Pause cannot bypass source policy';
 ASSERT jsonb_array_length(event_memory_source_reports(919145001,919145001,NULL,20)->'items')=1,'Authorized review remains readable during pause';
 UPDATE interaction_runtime SET enabled=true WHERE singleton;
 context:=event_memory_source_context(target,919145001);
 payload:=jsonb_build_object('operation','source.restore','reason','Synthetic review completed',
  'expectedSourceRevision',context->>'sourceRevision','expectedVersion',(SELECT version FROM interaction_target WHERE id=target));
 first:=interaction_command(919145001,target,restore_key,payload);
 ASSERT first->>'state'='visible';
 ASSERT interaction_command(919145001,target,restore_key,payload)=first,'Restore replay survives changed state';
 ASSERT interaction_resolve('event_moment','919145001',919145003) IS NOT NULL,'Restore restores canonical reads';
 payload:=jsonb_build_object('operation','source.report','reason','Same reviewed source',
  'expectedSourceRevision',event_memory_source_context(target,919145003)->>'sourceRevision');
 PERFORM interaction_command(919145003,target,gen_random_uuid(),payload);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_report WHERE target_id=target AND comment_id IS NULL AND state='open'),'Hide/restore cannot reopen resolved reports';
 UPDATE event_moment SET caption=caption,updated_at=clock_timestamp() WHERE id=919145001;
 payload:=payload||jsonb_build_object('expectedSourceRevision',event_memory_source_context(target,919145003)->>'sourceRevision');
 PERFORM interaction_command(919145003,target,gen_random_uuid(),payload);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_report WHERE target_id=target AND comment_id IS NULL AND state='open'),'No-op edits cannot reopen reports';
 UPDATE event_moment SET caption='New content after review',updated_at=clock_timestamp() WHERE id=919145001;
 payload:=payload||jsonb_build_object('expectedSourceRevision',event_memory_source_context(target,919145003)->>'sourceRevision');
 PERFORM interaction_command(919145003,target,gen_random_uuid(),payload);
 ASSERT EXISTS(SELECT 1 FROM interaction_report WHERE target_id=target AND comment_id IS NULL AND state='open'),'A real content change can reopen a report';
 context:=event_memory_source_context(target,919145001);
 payload:=jsonb_build_object('operation','source.report.resolve','reason','Synthetic review: publication may remain visible','decision','dismissed',
  'expectedSourceRevision',context->>'sourceRevision','expectedVersion',(SELECT version FROM interaction_target WHERE id=target));
 ASSERT interaction_command(919145001,target,gen_random_uuid(),payload)->>'state'='visible';
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_report WHERE target_id=target AND comment_id IS NULL AND state='open'),'Close without hiding content';
 context:=event_memory_source_context(target,919145005);
 payload:=jsonb_build_object('operation','source.remove','reason','Synthetic platform enforcement',
  'expectedSourceRevision',context->>'sourceRevision','expectedVersion',(SELECT version FROM interaction_target WHERE id=target));
 ASSERT interaction_command(919145001,target,gen_random_uuid(),payload)->>'error'='forbidden','Organizer cannot remove';
 ASSERT interaction_command(919145005,target,gen_random_uuid(),payload)->>'state'='removed';
 context:=event_memory_source_context(target,919145001);
 payload:=jsonb_build_object('operation','source.restore','reason','Forbidden downgrade',
  'expectedSourceRevision',context->>'sourceRevision','expectedVersion',(SELECT version FROM interaction_target WHERE id=target));
 ASSERT interaction_command(919145001,target,gen_random_uuid(),payload)->>'error'='forbidden','Organizer cannot reverse platform removal';
 ASSERT interaction_command(919145005,target,gen_random_uuid(),payload)->>'state'='visible','Moderator restoration';
 UPDATE party_security_role SET active=false WHERE party_id=919145005;
 payload:=payload||jsonb_build_object('operation','source.remove','expectedVersion',(SELECT version FROM interaction_target WHERE id=target),
  'expectedSourceRevision',event_memory_source_context(target,919145005)->>'sourceRevision');
 ASSERT interaction_command(919145005,target,gen_random_uuid(),payload)->>'error'='forbidden','Live moderator revocation';
 DELETE FROM event_moment WHERE id=919145001;
 ASSERT event_memory_source_context(target,919145001) IS NULL,'Deletion retires source moderation access';
 ASSERT jsonb_array_length(event_memory_source_reports(919145001,919145001,NULL,20)->'items')=0;
END $$;
ROLLBACK;
