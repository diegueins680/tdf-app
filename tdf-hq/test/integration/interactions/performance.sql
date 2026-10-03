\set ON_ERROR_STOP on
BEGIN;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Interaction performance '||n,false,now() FROM generate_series(920000001,920002001) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-perf-'||n,'not-a-login-hash',true FROM generate_series(920000001,920002001) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(920000001,920000001,'Interaction performance club');
INSERT INTO fan_club_post(id,club_id,fan_party_id,content,created_at)
VALUES(920000001,920000001,920000001,'Synthetic large discussion',now());
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at)
SELECT n,920000001,now() FROM generate_series(920000002,920002001) n;
SELECT interaction_register('club_post','920000001',920000001) AS target \gset
INSERT INTO interaction_comment(id,target_id,author_id,root_id,body,created_at)
SELECT ('92000000-0000-4000-8000-'||lpad(n::text,12,'0'))::uuid,:'target',920000001+(n%100),
 ('92000000-0000-4000-8000-'||lpad(n::text,12,'0'))::uuid,'Synthetic parent',now()-make_interval(secs=>n)
 FROM generate_series(1,1000) n;
INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body,created_at)
SELECT gen_random_uuid(),:'target',920000001+(n%100),
 ('92000000-0000-4000-8000-'||lpad((1+n%1000)::text,12,'0'))::uuid,
 ('92000000-0000-4000-8000-'||lpad((1+n%1000)::text,12,'0'))::uuid,1,'Synthetic reply',now()+make_interval(secs=>n)
 FROM generate_series(1,9000) n;
INSERT INTO interaction_reaction(target_id,actor_id,reaction_type_id)
SELECT :'target',n,'50900000-0000-4000-8000-000000000001' FROM generate_series(920000002,920002001) n;
ANALYZE interaction_comment;
ANALYZE interaction_comment_total;
ANALYZE interaction_reaction;
ANALYZE party;
ANALYZE user_credential;
EXPLAIN(ANALYZE,BUFFERS) SELECT interaction_summary(920000002,'club_post','920000001');
EXPLAIN(ANALYZE,BUFFERS) SELECT interaction_comments_page(:'target',920000002,NULL,'newest',NULL,20);
EXPLAIN(ANALYZE,BUFFERS) SELECT interaction_comments_page(:'target',920000002,'92000000-0000-4000-8000-000000000001','oldest',NULL,20);
EXPLAIN(ANALYZE,BUFFERS) SELECT interaction_comment_context(:'target',920000002,'92000000-0000-4000-8000-000000000999');
ROLLBACK;
