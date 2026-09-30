BEGIN;
INSERT INTO party(id,display_name,is_org,created_at) VALUES(917100001,'Publication boundary fixture',false,now());
INSERT INTO user_credential(party_id,username,password_hash,active) VALUES(917100001,'interaction-publication-boundary','not-a-login-hash',true);
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
SELECT 917100001,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
UPDATE interaction_runtime SET enabled=true WHERE singleton;
DO $$
DECLARE kind_value text; membership_table text; membership_key text; collection_kind text; source_id uuid;
 target_value uuid; comment_value uuid; failure_mode text; draft uuid; result_value jsonb;
BEGIN
 SELECT s.id INTO draft FROM workflow_state s JOIN workflow_definition w ON w.id=s.workflow_id
   WHERE w.code='catalog-publication' AND s.code='draft';
 ASSERT draft IS NOT NULL;
 FOREACH kind_value IN ARRAY ARRAY['recording','recording_session','record_release'] LOOP
   membership_table:=CASE kind_value WHEN 'recording' THEN 'collection_recording' WHEN 'recording_session' THEN 'collection_session' ELSE 'collection_release' END;
   membership_key:=CASE kind_value WHEN 'recording' THEN 'recording_id' WHEN 'recording_session' THEN 'session_id' ELSE 'release_id' END;
   collection_kind:=CASE kind_value WHEN 'recording' THEN 'recording' WHEN 'recording_session' THEN 'session' ELSE 'release' END;
   EXECUTE format('SELECT r.id FROM %I r JOIN %I m ON m.%I=r.id JOIN editorial_collection c ON c.id=m.collection_id
     JOIN workflow_state s ON s.id=r.workflow_state_id JOIN workflow_definition w ON w.id=s.workflow_id
     WHERE r.active AND c.active AND c.workflow_state_id=s.id AND s.code=''published'' AND w.code=''catalog-publication''
       AND c.collection_type=$1 ORDER BY r.id LIMIT 1',kind_value,membership_table,membership_key) INTO source_id USING collection_kind;
   ASSERT source_id IS NOT NULL, 'Published editorial fixture required for '||kind_value;
   ASSERT interaction_resolve(kind_value,source_id::text,NULL) IS NOT NULL;
   target_value:=interaction_register(kind_value,source_id::text,917100001);
   result_value:=interaction_command(917100001,target_value,gen_random_uuid(),'{"operation":"comment.create","body":"Retain discussion across publication withdrawal"}');
   ASSERT NOT result_value ? 'error',result_value::text;
   comment_value:=(result_value->>'id')::uuid;
   FOREACH failure_mode IN ARRAY ARRAY['inactive','draft','wrong_kind','detached'] LOOP
     BEGIN
       IF failure_mode='detached' THEN
         EXECUTE format('DELETE FROM %I WHERE %I=$1',membership_table,membership_key) USING source_id;
       ELSE
         EXECUTE format('UPDATE editorial_collection SET %s WHERE id IN (SELECT collection_id FROM %I WHERE %I=$1)',
           CASE failure_mode WHEN 'inactive' THEN 'active=false' WHEN 'draft' THEN 'workflow_state_id='||quote_literal(draft) ELSE 'collection_type='||quote_literal(CASE collection_kind WHEN 'recording' THEN 'session' ELSE 'recording' END) END,
           membership_table,membership_key) USING source_id;
       END IF;
       ASSERT interaction_resolve(kind_value,source_id::text,NULL) IS NULL, kind_value||' '||failure_mode||' leaked anonymously';
       ASSERT interaction_resolve(kind_value,source_id::text,917100001) IS NULL, 'Catalog management cannot publish through discussion';
       ASSERT interaction_resolve_scoped(kind_value,source_id::text,917100001,true) IS NULL, 'Moderation retains publication boundary';
       ASSERT interaction_register(kind_value,source_id::text,917100001) IS NULL;
       ASSERT interaction_summary(NULL,kind_value,source_id::text)->>'error'='unavailable';
       ASSERT interaction_destination(917100001,'comment',comment_value)->>'error'='unavailable';
       ASSERT interaction_command(917100001,target_value,gen_random_uuid(),'{"operation":"comment.create","body":"Must fail"}')->>'error'='unavailable';
       ASSERT interaction_command(917100001,target_value,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":"50900000-0000-4000-8000-000000000001"}')->>'error'='unavailable';
       ASSERT (SELECT body='Retain discussion across publication withdrawal' FROM interaction_comment WHERE id=comment_value), 'Withdrawal preserves engagement';
       RAISE EXCEPTION 'publication fixture rollback';
     EXCEPTION WHEN raise_exception THEN ASSERT SQLERRM='publication fixture rollback';
     END;
     ASSERT interaction_resolve(kind_value,source_id::text,NULL) IS NOT NULL, 'Restored publication returns the same discussion';
     ASSERT interaction_destination(917100001,'comment',comment_value)->>'commentId'=comment_value::text;
   END LOOP;
 END LOOP;
END $$;
ROLLBACK;
