DO $$ BEGIN
  ASSERT (SELECT provolatile='s' FROM pg_proc WHERE oid='social_v2_profiles(bigint,bigint[])'::regprocedure);
  ASSERT (SELECT provolatile='s' FROM pg_proc WHERE oid='social_v2_profile_eligible(bigint,bigint[])'::regprocedure);
  BEGIN
    PERFORM * FROM social_v2_profiles(1,ARRAY[2::bigint,2]);
    RAISE EXCEPTION 'duplicate SQL identifiers unexpectedly accepted';
  EXCEPTION WHEN invalid_parameter_value THEN NULL; END;
  BEGIN
    PERFORM * FROM social_v2_profiles(1,ARRAY[NULL::bigint]);
    RAISE EXCEPTION 'NULL SQL identifier unexpectedly accepted';
  EXCEPTION WHEN invalid_parameter_value THEN NULL; END;
END $$;
CREATE FUNCTION assert_profile_read_case(case_id integer,feature_enabled boolean,was_activated boolean,
  pair_exists boolean,is_blocked boolean,closed_a boolean,closed_b boolean,
  live_a boolean,live_b boolean,discoverable_b boolean,muted_b boolean,
  requested bigint[],expected bigint[])
RETURNS void LANGUAGE plpgsql AS $$
DECLARE actual bigint[];
BEGIN
  BEGIN
    UPDATE social_v2_runtime SET enabled=feature_enabled,activated_once=was_activated;
    INSERT INTO social_v2_pair(party_a,party_b,block_a,mute_a)
      SELECT 1,2,is_blocked,muted_b WHERE pair_exists;
    INSERT INTO social_v2_preference(party_id,closed) VALUES(1,closed_a);
    INSERT INTO social_v2_preference(party_id,closed,discoverable) VALUES(2,closed_b,discoverable_b);
    UPDATE user_credential SET active=live_a WHERE party_id=1;
    UPDATE user_credential SET active=live_b WHERE party_id=2;
    SELECT coalesce(array_agg(profile_id),'{}'::bigint[]) INTO actual FROM social_v2_profiles(1,requested);
    ASSERT (actual=expected) IS TRUE, format('ProfileReads observed case %s mismatch',case_id);
    -- Every emitted row contains only the expected synthetic profile fields.
    ASSERT NOT EXISTS(SELECT 1 FROM social_v2_profiles(1,requested) p
      WHERE p.party_name<>'Synthetic '||p.profile_id OR p.bio IS DISTINCT FROM 'profile bio '||p.profile_id),
      'Profile payload mismatch';
    RAISE EXCEPTION USING ERRCODE='ZZ001',MESSAGE='fixture rollback';
  EXCEPTION WHEN SQLSTATE 'ZZ001' THEN NULL; END;
END $$;
