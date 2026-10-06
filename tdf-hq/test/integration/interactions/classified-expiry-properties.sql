BEGIN;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Classified expiry actor',false,now() FROM generate_series(917500001,917500003) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'classified-expiry-'||n,'not-a-login-hash',true FROM generate_series(917500001,917500003) n;
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
SELECT 917500003,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status)
VALUES('91750000-0000-4000-8000-000000000001',917500001,'person','Expiry fixture','interaction-expiry-fixture','published','public','allowed');
INSERT INTO classified(id,author_profile_id,category_id,title,slug,description,status,expires_at,created_at)
SELECT '91750000-0000-4000-8000-000000000002','91750000-0000-4000-8000-000000000001',id,
 'Expiry fixture opportunity','interaction-expiry-opportunity','Synthetic discussion expiry regression fixture','published',now()+interval '1 day',now()-interval '2 days'
 FROM classified_category ORDER BY id LIMIT 1;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
DO $$
DECLARE source_id text:='91750000-0000-4000-8000-000000000002'; target_id_value uuid; comment_id_value uuid; result_value jsonb; expiry_value timestamptz;
BEGIN
 ASSERT interaction_resolve('classified',source_id,NULL)->>'route'='/clasificados/interaction-expiry-opportunity';
 target_id_value:=interaction_register('classified',source_id,917500002);
 result_value:=interaction_command(917500002,target_id_value,gen_random_uuid(),'{"operation":"comment.create","body":"Preserve existing opportunity discussion"}');
 ASSERT NOT result_value ? 'error',result_value::text;
 comment_id_value:=(result_value->>'id')::uuid;
 FOREACH expiry_value IN ARRAY ARRAY[NULL::timestamptz,now(),now()-interval '1 second',now()-interval '1 day'] LOOP
   UPDATE classified SET expires_at=expiry_value WHERE id=source_id::uuid;
   ASSERT interaction_resolve('classified',source_id,NULL) IS NULL, 'Null or elapsed expiry must not expose a discussion missing from public detail';
   ASSERT interaction_resolve('classified',source_id,917500001) IS NULL, 'Ownership cannot bypass public-detail expiry';
   ASSERT interaction_resolve_scoped('classified',source_id,917500003,true) IS NULL, 'Moderation cannot publish an expired classified';
   ASSERT interaction_register('classified',source_id,917500002) IS NULL;
   ASSERT interaction_summary(NULL,'classified',source_id)->>'error'='unavailable';
   ASSERT interaction_destination(917500002,'comment',comment_id_value)->>'error'='unavailable';
   ASSERT interaction_command(917500002,target_id_value,gen_random_uuid(),'{"operation":"comment.create","body":"Must be denied"}')->>'error'='unavailable';
   ASSERT interaction_command(917500002,target_id_value,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":"50900000-0000-4000-8000-000000000001"}')->>'error'='unavailable';
   ASSERT (SELECT body='Preserve existing opportunity discussion' FROM interaction_comment WHERE id=comment_id_value);
 END LOOP;
 UPDATE classified SET expires_at=now()+interval '1 day' WHERE id=source_id::uuid;
 ASSERT interaction_register('classified',source_id,917500002)=target_id_value;
 ASSERT interaction_destination(917500002,'comment',comment_id_value)->>'commentId'=comment_id_value::text;
 ASSERT interaction_summary(NULL,'classified',source_id)->>'commentCount'='1';
END $$;
ROLLBACK;
