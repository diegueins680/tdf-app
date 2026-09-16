DO $$ BEGIN
  ASSERT (SELECT provolatile='s' FROM pg_proc WHERE oid='social_v2_relationship_rows(bigint,text)'::regprocedure);
  ASSERT (SELECT provolatile='s' FROM pg_proc WHERE oid='social_v2_legacy_suggestions(bigint)'::regprocedure);
  ASSERT (SELECT provolatile='s' FROM pg_proc WHERE oid='social_v2_legacy_suggestions_enabled()'::regprocedure);
END $$;
CREATE FUNCTION assert_relationship_read_case(case_id integer,feature_enabled boolean,was_activated boolean,
  pair_exists boolean,is_blocked boolean,closed_a boolean,closed_b boolean,
  live_a boolean,live_b boolean,route text,edges jsonb,expected jsonb,expected_count bigint)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE actual jsonb; actual_count bigint;
BEGIN
  BEGIN
    UPDATE social_v2_runtime SET enabled=feature_enabled,activated_once=was_activated;
    INSERT INTO social_v2_pair(party_a,party_b,block_a) SELECT 1,2,is_blocked WHERE pair_exists;
    INSERT INTO social_v2_preference(party_id,closed) VALUES(1,closed_a),(2,closed_b);
    UPDATE user_credential SET active=live_a WHERE party_id=1;
    UPDATE user_credential SET active=live_b WHERE party_id=2;
    DELETE FROM party_follow;
    INSERT INTO party_follow(follower_party_id,following_party_id,created_at)
      SELECT (e->>0)::bigint,(e->>1)::bigint,'2026-01-01T23:00:00Z'::timestamptz
      FROM jsonb_array_elements(edges) e;
    IF route='suggestions' THEN
      SELECT coalesce(sum(mutual_count),0) INTO actual_count FROM social_v2_legacy_suggestions(1);
      ASSERT (actual_count=expected_count) IS TRUE,format('RelationshipReads observed case %s count mismatch',case_id);
      ASSERT NOT EXISTS(SELECT 1 FROM social_v2_legacy_suggestions(1) WHERE party_id<>3), 'Unexpected recommendation';
    ELSE
      SELECT coalesce(jsonb_agg(jsonb_build_array(follower_id,following_id) ORDER BY follower_id,following_id),'[]')
        INTO actual FROM social_v2_relationship_rows(1,route);
      ASSERT (actual=expected) IS TRUE,format('RelationshipReads observed case %s rows mismatch',case_id);
      ASSERT NOT EXISTS(SELECT 1 FROM social_v2_relationship_rows(1,route) r
        WHERE r.follower_name<>'Synthetic '||r.follower_id OR r.following_name<>'Synthetic '||r.following_id
          OR r.via_nfc OR r.started_at<>'2026-01-01'::date), 'Relationship payload mismatch';
    END IF;
    RAISE EXCEPTION USING ERRCODE='ZZ001',MESSAGE='fixture rollback';
  EXCEPTION WHEN SQLSTATE 'ZZ001' THEN NULL; END;
END $$;
-- Prove the legacy UTC day contract despite a session date already one day ahead.
ALTER FUNCTION assert_relationship_read_case(integer,boolean,boolean,boolean,boolean,
  boolean,boolean,boolean,boolean,text,jsonb,jsonb,bigint) SET TimeZone='Pacific/Kiritimati';
