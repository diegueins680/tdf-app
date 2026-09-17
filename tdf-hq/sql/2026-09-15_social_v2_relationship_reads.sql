-- Additive compatibility readers after profile_reads. No activation or data rewrite.
BEGIN;
CREATE INDEX IF NOT EXISTS social_v2_legacy_follow_incoming
  ON party_follow(following_party_id,follower_party_id);
CREATE OR REPLACE FUNCTION social_v2_legacy_suggestions_enabled()
RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT NOT coalesce((SELECT activated_once FROM social_v2_runtime WHERE singleton),true)
    AND NOT EXISTS(SELECT 1 FROM social_v2_pair)
    AND NOT EXISTS(SELECT 1 FROM social_v2_preference WHERE closed)
$$;
CREATE OR REPLACE FUNCTION social_v2_relationship_rows(actor bigint,kind text)
RETURNS TABLE(follower_id bigint,following_id bigint,follower_name text,following_name text,
  via_nfc boolean,started_at date)
LANGUAGE plpgsql STABLE AS $$
BEGIN
  IF kind IS NULL OR kind NOT IN ('followers','following','friends') THEN
    RAISE EXCEPTION 'invalid relationship collection' USING ERRCODE='22023';
  END IF;
  RETURN QUERY
    WITH edges AS MATERIALIZED (
      SELECT f.*,CASE WHEN f.follower_party_id=actor THEN f.following_party_id ELSE f.follower_party_id END AS peer
      FROM party_follow f
      WHERE f.follower_party_id<>f.following_party_id AND
        ((kind='followers' AND f.following_party_id=actor) OR
         (kind IN ('following','friends') AND f.follower_party_id=actor))
        AND (kind<>'friends' OR EXISTS(SELECT 1 FROM party_follow r
          WHERE r.follower_party_id=f.following_party_id AND r.following_party_id=actor))
    ), eligible AS MATERIALIZED (
      SELECT profile_id FROM social_v2_profile_eligible(actor,ARRAY(SELECT DISTINCT peer FROM edges))
    )
    SELECT e.follower_party_id,e.following_party_id,p.display_name::text,q.display_name::text,
      e.via_nfc,(e.created_at AT TIME ZONE 'UTC')::date
    FROM edges e JOIN eligible v ON v.profile_id=e.peer
    JOIN party p ON p.id=e.follower_party_id JOIN party q ON q.id=e.following_party_id
    ORDER BY e.created_at DESC,e.id DESC;
END $$;
CREATE OR REPLACE FUNCTION social_v2_legacy_suggestions(actor bigint)
RETURNS TABLE(party_id bigint,mutual_count bigint) LANGUAGE plpgsql STABLE AS $$
BEGIN
  -- The legacy DTO can express only inferred mutual counts. These edges have no
  -- publication consent and may be manufactured reciprocally by one writer.
  -- Once enforcement starts, canonical Discover is the recommendation surface.
  IF NOT social_v2_legacy_suggestions_enabled() THEN RETURN; END IF;
  RETURN QUERY
    WITH direct AS MATERIALIZED (
      SELECT actor AS id
      UNION SELECT following_party_id FROM party_follow WHERE follower_party_id=actor
      UNION SELECT follower_party_id FROM party_follow WHERE following_party_id=actor
    )
    SELECT f.following_party_id,count(*)
    FROM direct d JOIN party_follow f ON f.follower_party_id=d.id
    WHERE d.id<>actor AND NOT EXISTS(SELECT 1 FROM direct x WHERE x.id=f.following_party_id)
    GROUP BY f.following_party_id ORDER BY count(*) DESC,f.following_party_id ASC LIMIT 20;
END $$;
COMMIT;
