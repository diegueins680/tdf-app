-- The enclosing generated case helper rolls back its fixture subtransaction.
CREATE FUNCTION assert_dm_read_case(case_id integer,feature_enabled boolean,was_activated boolean,
  pair_exists boolean,is_blocked boolean,closed_a boolean,closed_b boolean,
  live_a boolean,live_b boolean,consent_a boolean,consent_b boolean,viewer bigint,
  route text,cursor_kind text,fields_expected boolean,visible_expected boolean)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE result jsonb; fields_returned boolean;
BEGIN
  BEGIN
    UPDATE social_v2_runtime SET enabled=feature_enabled,activated_once=was_activated;
    INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a)
      SELECT 1,2,consent_a,consent_b,is_blocked WHERE pair_exists;
    INSERT INTO social_v2_preference(party_id,closed) SELECT 1,true WHERE closed_a;
    INSERT INTO social_v2_preference(party_id,closed) SELECT 2,true WHERE closed_b;
    UPDATE user_credential SET active=live_a WHERE party_id=1;
    UPDATE user_credential SET active=live_b WHERE party_id=2;
    IF route='threads' THEN
      result := social_v2_chat_threads(viewer);
      fields_returned := EXISTS(SELECT 1 FROM jsonb_array_elements(result->'result') r WHERE r->>'ctThreadId'='1');
    ELSE
      result := social_v2_chat_messages(viewer,1,CASE cursor_kind WHEN 'local' THEN 2 WHEN 'foreign' THEN 3 ELSE NULL END,NULL,50);
      fields_returned := result ? 'result' AND jsonb_array_length(result->'result')>0;
      ASSERT (visible_expected OR result->>'error'='unavailable') IS TRUE, 'Denied request leaked cursor context';
    END IF;
    ASSERT (fields_returned=fields_expected) IS TRUE, format('DmReads observed case %s mismatch',case_id);
    RAISE EXCEPTION USING ERRCODE='ZZ001',MESSAGE='fixture rollback';
  EXCEPTION WHEN SQLSTATE 'ZZ001' THEN NULL; END;
END $$;
ALTER FUNCTION assert_dm_read_case(integer,boolean,boolean,boolean,boolean,boolean,boolean,boolean,boolean,boolean,boolean,bigint,text,text,boolean,boolean)
  SET plpgsql.variable_conflict=use_variable;
