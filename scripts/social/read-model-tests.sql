UPDATE social_v2_runtime SET enabled=true;
SELECT social_v2_publish_batch();
DO $$ DECLARE page jsonb; BEGIN
  ASSERT social_v2_publish_batch()=0;
  page:=social_v2_feed(2,NULL,1);
  ASSERT page->'items'->0->>'postId'='3';
  ASSERT page->>'nextCursor'='3';
  ASSERT social_v2_feed(2,3,1)->'items'->0->>'postId'='2';
  ASSERT social_v2_feed(2,2,1)->'items'->0->>'postId'='1';
  ASSERT social_v2_feed(2,2,1)->>'nextCursor' IS NULL;
  ASSERT social_v2_feed(3,NULL,10)->'items'='[]'::jsonb;
  ASSERT social_v2_feed(2,NULL,-1)->>'error'='invalid';
  ASSERT social_v2_feed(2,-1,1)->>'error'='invalid';
END $$;
-- Edit must not move a published row; a late/backdated commit gets NEW order.
UPDATE fan_club_post SET created_at='2099-01-01',content='edited' WHERE id=1;
INSERT INTO fan_club_post VALUES(6,1,5,NULL,'late','backdated insert',false,'2020-01-01');
SELECT social_v2_publish_batch();
DO $$ BEGIN
  ASSERT social_v2_feed(2,NULL,1)->'items'->0->>'postId'='6';
  ASSERT social_v2_feed(2,3,1)->'items'->0->>'postId'='2';
END $$;
DELETE FROM fan_club_post WHERE id=2;
DO $$ BEGIN ASSERT social_v2_feed(2,3,1)->'items'->0->>'postId'='1'; END $$;
DELETE FROM fan_follow WHERE fan_party_id=2;
DO $$ BEGIN ASSERT social_v2_feed(2,NULL,50)->'items'='[]'::jsonb; END $$;
INSERT INTO fan_follow VALUES(2,5);
SELECT social_v2_preferences(5,true,true,0);
DO $$ BEGIN
  ASSERT social_v2_discover(2,10)->'items'->0->>'reason'='shared_interests';
  ASSERT social_v2_preferences(2,false,false,0)->>'personalized'='false';
  ASSERT social_v2_discover(2,10)->'items'->0->>'reason'='public_profile';
  ASSERT social_v2_mutate(2,5,'dismiss',0,'dismiss-candidate')->>'dismissed'='true';
  ASSERT social_v2_discover(2,10)->'items'='[]'::jsonb;
  ASSERT social_v2_mutate(2,5,'mute',1,'mute-author')->>'muted'='true';
  ASSERT social_v2_feed(2,NULL,50)->'items'='[]'::jsonb;
  ASSERT social_v2_mutate(2,5,'unmute',2,'unmute-author')->>'muted'='false';
  ASSERT social_v2_mutate(5,2,'block',3,'block-viewer')->>'blocked'='true';
  ASSERT social_v2_feed(2,NULL,50)->'items'='[]'::jsonb;
  ASSERT social_v2_discover(2,10)->'items'='[]'::jsonb;
END $$;
DO $$ BEGIN
  ASSERT jsonb_typeof(social_v2_me(2)->'relationships')='array';
  ASSERT social_v2_me(1)->>'error'='unavailable';
  ASSERT NOT social_v2_pair_json(2,5,(SELECT p FROM social_v2_pair p WHERE party_a=2 AND party_b=5))->>'connected'='true';
END $$;
