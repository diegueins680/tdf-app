-- Additional directed-follow, retry, rate-limit and storage integrity cases.
-- Isolated transaction preserves the preceding fixtures and benchmark distribution.
BEGIN;
INSERT INTO party SELECT n,'Synthetic mutation '||n,false FROM generate_series(200001,200004) n;
INSERT INTO user_credential SELECT n,n,true FROM generate_series(200001,200004) n;
DO $$ DECLARE n integer; BEGIN
  ASSERT social_v2_mutate(200001,200002,'follow',0,'follow')->>'following'='true';
  ASSERT social_v2_state(200002,200001)->>'following'='false';
  ASSERT social_v2_mutate(200001,200002,'follow',0,'follow')->>'revision'='1';
  ASSERT social_v2_mutate(200002,200001,'follow',1,'reverse')->>'following'='true';
  ASSERT social_v2_mutate(200001,200002,'unfollow',2,'remove')->>'following'='false';
  ASSERT social_v2_state(200002,200001)->>'following'='true';
  ASSERT social_v2_mutate(200001,200002,'follow',0,'follow')->>'following'='false';
  UPDATE user_credential SET active=false WHERE party_id=200002;
  ASSERT social_v2_mutate(200001,200002,'follow',0,'follow')->>'error'='unavailable';
  ASSERT social_v2_relationship(200001,200002)->>'error'='unavailable';
  UPDATE user_credential SET active=true WHERE party_id=200002;
  ASSERT social_v2_close(200002)->>'closed'='true';
  ASSERT NOT social_v2_allowed(200001,200002);
  ASSERT NOT EXISTS(SELECT 1 FROM social_v2_pair WHERE party_a=200001 AND party_b=200002 AND (follow_a OR follow_b));
  FOR n IN 0..59 LOOP
    ASSERT NOT (social_v2_mutate(200003,200004,CASE WHEN n%2=0 THEN 'follow' ELSE 'unfollow' END,n,'budget-'||n) ? 'error');
  END LOOP;
  ASSERT social_v2_mutate(200003,200004,'follow',60,'over-budget')->>'error'='rate_limited';
  ASSERT social_v2_mutate(200003,200004,'follow',0,'budget-0')->>'following'='false';
  ASSERT (SELECT count(*) FROM social_v2_command WHERE actor=200003)=60;
  BEGIN
    INSERT INTO social_v2_pair(party_a,party_b) VALUES(200003,200003);
    RAISE EXCEPTION 'Self-edge constraint missing';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    INSERT INTO social_v2_pair(party_a,party_b) VALUES(200003,200004);
    RAISE EXCEPTION 'Pair uniqueness constraint missing';
  EXCEPTION WHEN unique_violation THEN NULL; END;
  BEGIN
    INSERT INTO social_v2_pair(party_a,party_b) VALUES(200003,999999);
    RAISE EXCEPTION 'Target foreign key missing';
  EXCEPTION WHEN foreign_key_violation THEN NULL; END;
  BEGIN
    UPDATE social_v2_pair SET block_a=true,consent_b=true WHERE party_a=200003 AND party_b=200004;
    RAISE EXCEPTION 'Block/consent constraint missing';
  EXCEPTION WHEN check_violation THEN NULL; END;
END $$;
ROLLBACK;
SELECT 'PASS: directed follows, retries, revocation, 60-command budget, uniqueness, foreign keys, block constraint';
