-- Additive read boundary. Apply after foundation + DM write boundary; no flag changes.
-- Never remove this policy as a rollback after activation or recorded pair/closure.
BEGIN;
-- Bulk form of the existing activation/pair/closure and account-only policy.
-- Materialize viewer authority once; indexed joins avoid one policy function
-- evaluation (and nested statements) per candidate. No projection grants rights.
CREATE OR REPLACE FUNCTION social_v2_profile_eligible(actor bigint,requested bigint[])
RETURNS TABLE(profile_id bigint,ordinal bigint) LANGUAGE sql STABLE AS $$
  WITH viewer AS MATERIALIZED (
    SELECT social_v2_live(actor) AS live,
      coalesce((SELECT activated_once FROM social_v2_runtime WHERE singleton),true) AS activated,
      coalesce((SELECT closed FROM social_v2_preference WHERE party_id=actor),false) AS closed
  )
  SELECT p.id,r.ordinal
  FROM unnest(requested) WITH ORDINALITY AS r(id,ordinal)
  JOIN party p ON p.id=r.id
  CROSS JOIN viewer v
  LEFT JOIN social_v2_preference t ON t.party_id=p.id
  LEFT JOIN social_v2_pair e ON e.party_a=least(actor,p.id) AND e.party_b=greatest(actor,p.id)
  WHERE NOT (v.activated OR e.party_a IS NOT NULL OR v.closed OR coalesce(t.closed,false))
    OR (v.live AND NOT p.is_org AND NOT coalesce(t.closed,false)
        AND NOT coalesce(e.block_a OR e.block_b,false)
        AND EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=p.id AND u.active))
$$;
CREATE OR REPLACE FUNCTION social_v2_profiles(actor bigint,requested bigint[])
RETURNS TABLE(profile_id bigint,party_name text,profile_name text,avatar_url text,bio text,city text)
LANGUAGE plpgsql STABLE AS $$
BEGIN
  IF requested IS NULL OR cardinality(requested)>100 OR
     EXISTS(SELECT 1 FROM unnest(requested) n WHERE n IS NULL OR n<=0) OR
     cardinality(requested)<>(SELECT count(DISTINCT n) FROM unnest(requested) n) THEN
    RAISE EXCEPTION 'invalid profile identifiers' USING ERRCODE='22023';
  END IF;
  RETURN QUERY
    SELECT p.id,p.display_name::text,f.display_name::text,f.avatar_url::text,f.bio::text,f.city::text
    FROM social_v2_profile_eligible(actor,requested) r
    JOIN party p ON p.id=r.profile_id
    LEFT JOIN fan_profile f ON f.fan_party_id=p.id
    ORDER BY r.ordinal;
END $$;
COMMIT;
