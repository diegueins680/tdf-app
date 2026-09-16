-- Generated from checked LegacyDm Insert observations; do not hand-edit.
-- Checked case 1
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=true, 'LegacyDm observed case 1 mismatch';
END $$;
ROLLBACK;
-- Checked case 2
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 2 mismatch';
END $$;
ROLLBACK;
-- Checked case 3
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 3 mismatch';
END $$;
ROLLBACK;
-- Checked case 4
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 4 mismatch';
END $$;
ROLLBACK;
-- Checked case 5
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 5 mismatch';
END $$;
ROLLBACK;
-- Checked case 6
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=true, 'LegacyDm observed case 6 mismatch';
END $$;
ROLLBACK;
-- Checked case 7
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 7 mismatch';
END $$;
ROLLBACK;
-- Checked case 8
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 8 mismatch';
END $$;
ROLLBACK;
-- Checked case 9
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 9 mismatch';
END $$;
ROLLBACK;
-- Checked case 10
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 10 mismatch';
END $$;
ROLLBACK;
-- Checked case 11
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 11 mismatch';
END $$;
ROLLBACK;
-- Checked case 12
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 12 mismatch';
END $$;
ROLLBACK;
-- Checked case 13
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 13 mismatch';
END $$;
ROLLBACK;
-- Checked case 14
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 14 mismatch';
END $$;
ROLLBACK;
-- Checked case 15
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=true, 'LegacyDm observed case 15 mismatch';
END $$;
ROLLBACK;
-- Checked case 16
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 16 mismatch';
END $$;
ROLLBACK;
-- Checked case 17
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 17 mismatch';
END $$;
ROLLBACK;
-- Checked case 18
BEGIN;
UPDATE social_v2_runtime SET enabled=false,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 18 mismatch';
END $$;
ROLLBACK;
-- Checked case 19
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 19 mismatch';
END $$;
ROLLBACK;
-- Checked case 20
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE false;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 20 mismatch';
END $$;
ROLLBACK;
-- Checked case 21
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 21 mismatch';
END $$;
ROLLBACK;
-- Checked case 22
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 22 mismatch';
END $$;
ROLLBACK;
-- Checked case 23
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 23 mismatch';
END $$;
ROLLBACK;
-- Checked case 24
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,true,true,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=true, 'LegacyDm observed case 24 mismatch';
END $$;
ROLLBACK;
-- Checked case 25
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,false WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 25 mismatch';
END $$;
ROLLBACK;
-- Checked case 26
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE false;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 26 mismatch';
END $$;
ROLLBACK;
-- Checked case 27
BEGIN;
UPDATE social_v2_runtime SET enabled=true,activated_once=true;
INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a) SELECT 1,2,false,false,true WHERE true;
INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE true;
DO $$ DECLARE permitted boolean := true; BEGIN
  BEGIN INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,1,'synthetic model case');
  EXCEPTION WHEN insufficient_privilege THEN permitted := false; END;
  ASSERT permitted=false, 'LegacyDm observed case 27 mismatch';
END $$;
ROLLBACK;
