-- Synthetic identities and content only; explicit columns match the complete schema.
INSERT INTO party(id,display_name,is_org,created_at)
  SELECT n,'Synthetic schema '||n,false,now() FROM generate_series(900000001,900000003) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
  SELECT n,'synthetic-schema-'||n,'not-a-login-hash',true FROM generate_series(900000001,900000003) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(900000001,900000003,'Synthetic club');
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(900000001,900000003,now());
INSERT INTO fan_club_post(id,club_id,fan_party_id,title,content,created_at) VALUES
  (900000001,900000001,900000003,'Synthetic first','fixture','2026-01-01'),
  (900000002,900000001,900000003,'Synthetic second','fixture','2026-01-01');
DO $$ BEGIN ASSERT social_v2_me(900000001)->>'error'='disabled'; END $$;
UPDATE social_v2_runtime SET enabled=true;
SELECT social_v2_publish_batch(); -- Commit before reads, as the HTTP handler does.
DO $$ BEGIN
  ASSERT jsonb_array_length(social_v2_feed(900000001,NULL,20)->'items')=2;
  ASSERT social_v2_mutate(900000001,900000002,'request',0,'request')->>'requested'='true';
  ASSERT social_v2_mutate(900000002,900000001,'accept',1,'accept')->>'connected'='true';
  ASSERT social_v2_mutate(900000001,900000002,'block',2,'block')->>'blocked'='true';
  ASSERT social_v2_preferences(900000001,true,false,0)->>'personalized'='false';
  ASSERT social_v2_relationship(900000002,900000001)->>'error'='unavailable';
END $$;
CREATE TABLE social_schema_preserved AS SELECT
  (SELECT jsonb_agg(to_jsonb(p) ORDER BY position) FROM social_v2_publication p) AS publications,
  (SELECT jsonb_agg(to_jsonb(c) ORDER BY actor,request_key) FROM social_v2_command c) AS commands;
-- An old source writer still edits and inserts using its original schema contract.
UPDATE fan_club_post SET title='Edited original',created_at='2020-01-01' WHERE id=900000001;
INSERT INTO fan_club_post(id,club_id,fan_party_id,title,content,created_at)
  VALUES(900000003,900000001,900000003,'Backdated new source','fixture','2019-01-01');
SELECT social_v2_publish_batch();
SELECT social_v2_publish_batch();
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM social_v2_publication WHERE post_id BETWEEN 900000001 AND 900000003)=3;
  ASSERT (SELECT position FROM social_v2_publication WHERE post_id=900000003)>(SELECT position FROM social_v2_publication WHERE post_id=900000002);
  ASSERT (SELECT jsonb_agg(to_jsonb(p) ORDER BY position) FROM social_v2_publication p WHERE post_id<>900000003)=(SELECT publications FROM social_schema_preserved);
END $$;
UPDATE social_schema_preserved SET publications=(SELECT jsonb_agg(to_jsonb(p) ORDER BY position) FROM social_v2_publication p);
