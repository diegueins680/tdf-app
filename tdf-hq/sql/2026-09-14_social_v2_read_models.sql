BEGIN;
ALTER TABLE social_v2_runtime ADD COLUMN IF NOT EXISTS publication_position bigint NOT NULL DEFAULT 0;
CREATE TABLE IF NOT EXISTS social_v2_publication (
  post_id bigint PRIMARY KEY REFERENCES fan_club_post(id) ON DELETE CASCADE,
  position bigint NOT NULL UNIQUE,
  published_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE INDEX IF NOT EXISTS social_v2_club_artist ON fan_club(artist_party_id,id);
CREATE INDEX IF NOT EXISTS social_v2_officer_actor ON fan_club_officer(fan_party_id,club_id);
CREATE INDEX IF NOT EXISTS social_v2_visible_post_club ON fan_club_post(club_id,id)
  WHERE parent_id IS NULL AND NOT is_hidden;
-- Publication position is assigned while holding the runtime row until COMMIT.
-- It is a derived publication order, NOT the original author-supplied date or a sequence.
CREATE OR REPLACE FUNCTION social_v2_publish_batch() RETURNS integer LANGUAGE plpgsql AS $$
DECLARE n integer; start_position bigint;
BEGIN
  SELECT publication_position INTO start_position FROM social_v2_runtime WHERE enabled FOR UPDATE;
  IF NOT FOUND THEN RETURN 0; END IF;
  WITH candidates AS (
    SELECT p.id,p.created_at FROM fan_club_post p
    WHERE p.parent_id IS NULL AND NOT EXISTS
      (SELECT 1 FROM social_v2_publication s WHERE s.post_id=p.id)
    ORDER BY p.created_at,p.id LIMIT 500
  ) INSERT INTO social_v2_publication(post_id,position)
    SELECT id,start_position+row_number() OVER(ORDER BY created_at,id) FROM candidates;
  GET DIAGNOSTICS n=ROW_COUNT;
  UPDATE social_v2_runtime SET publication_position=start_position+n WHERE singleton;
  RETURN n;
END $$;

CREATE OR REPLACE FUNCTION social_v2_feed(actor bigint, before_position bigint, page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE result_value jsonb;
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 OR before_position<=0 THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  IF NOT social_v2_live(actor) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  WITH membership AS MATERIALIZED (
    -- Start from the viewer's authoritative clubs. An empty/sparse membership
    -- must not inspect the global publication history to discover no matches.
    SELECT c.id,c.artist_party_id FROM fan_follow f JOIN fan_club c ON c.artist_party_id=f.artist_party_id
      WHERE f.fan_party_id=actor
    UNION SELECT c.id,c.artist_party_id FROM fan_club c WHERE c.artist_party_id=actor
    UNION SELECT c.id,c.artist_party_id FROM fan_club_officer o JOIN fan_club c ON c.id=o.club_id
      WHERE o.fan_party_id=actor
  ), clubs AS MATERIALIZED (
    SELECT c.* FROM membership c JOIN party artist ON artist.id=c.artist_party_id
      LEFT JOIN social_v2_preference pref ON pref.party_id=artist.id
      LEFT JOIN social_v2_pair r ON r.party_a=least(actor,artist.id) AND r.party_b=greatest(actor,artist.id)
    WHERE NOT artist.is_org AND NOT coalesce(pref.closed,false)
      AND EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=artist.id AND u.active)
      AND NOT coalesce(r.block_a OR r.block_b,false)
      AND NOT coalesce(CASE WHEN actor=r.party_a THEN r.mute_a ELSE r.mute_b END,false)
  ), eligible AS (
    SELECT s.position,s.published_at,p.id,p.title,p.content,p.created_at,
      p.fan_party_id AS author_id,a.display_name,c.artist_party_id
    FROM clubs c JOIN fan_club_post p ON p.club_id=c.id
      JOIN social_v2_publication s ON s.post_id=p.id JOIN party a ON a.id=p.fan_party_id
      LEFT JOIN social_v2_preference pref ON pref.party_id=a.id
      LEFT JOIN social_v2_pair r ON r.party_a=least(actor,a.id) AND r.party_b=greatest(actor,a.id)
    WHERE (before_position IS NULL OR s.position<before_position)
      AND NOT p.is_hidden AND p.parent_id IS NULL
      AND NOT a.is_org AND NOT coalesce(pref.closed,false)
      AND EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=a.id AND u.active)
      AND NOT coalesce(r.block_a OR r.block_b,false)
      AND NOT coalesce(CASE WHEN actor=r.party_a THEN r.mute_a ELSE r.mute_b END,false)
      -- Membership above grants access; follows here select subscriptions only.
      AND (EXISTS(SELECT 1 FROM fan_follow f WHERE f.fan_party_id=actor AND f.artist_party_id=c.artist_party_id)
        OR coalesce(CASE WHEN actor=r.party_a THEN r.follow_a ELSE r.follow_b END,false))
    ORDER BY s.position DESC LIMIT page_size+1
  ), page AS (SELECT * FROM eligible ORDER BY position DESC LIMIT page_size)
  SELECT jsonb_build_object('items',coalesce((SELECT jsonb_agg(jsonb_build_object(
    'postId',id,'position',position::text,'publishedAt',published_at,'createdAt',created_at,
    'title',title,'content',content,'authorId',author_id,'authorName',display_name,
    'artistId',artist_party_id) ORDER BY position DESC) FROM page),'[]'::jsonb),
    'nextCursor',CASE WHEN (SELECT count(*) FROM eligible)>page_size
      THEN (SELECT min(position)::text FROM page) ELSE NULL END) INTO result_value;
  RETURN result_value;
END $$;

CREATE OR REPLACE FUNCTION social_v2_discover(actor bigint, page_size integer) RETURNS jsonb
LANGUAGE plpgsql STABLE AS $$
DECLARE use_personalization boolean;
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  IF NOT social_v2_live(actor) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  SELECT personalized INTO use_personalization FROM social_v2_preference WHERE party_id=actor;
  use_personalization := coalesce(use_personalization,true);
  RETURN (WITH public_sample AS MATERIALIZED (
    -- Bound expensive policy evaluations; rotate the opt-in pool daily without
    -- using popularity, private connections or purchases. Eligibility still
    -- precedes every returned candidate/reason. Sampling can reduce recall.
    SELECT party_id FROM social_v2_preference WHERE discoverable AND NOT closed
    ORDER BY md5(party_id::text || current_date::text),party_id LIMIT 200
  ), candidates AS (
    SELECT a.id,a.display_name,r AS pair_state,
      CASE WHEN use_personalization AND EXISTS (
        SELECT 1 FROM fan_profile_genre_membership f JOIN artist_profile_genre_membership g
          ON g.genre_id=f.genre_id WHERE f.fan_party_id=actor AND g.artist_party_id=a.id
      ) THEN 1 ELSE 0 END AS shared_interest
    FROM public_sample s JOIN party a ON a.id=s.party_id
      LEFT JOIN social_v2_pair r ON r.party_a=least(actor,a.id) AND r.party_b=greatest(actor,a.id)
    WHERE a.id<>actor AND NOT a.is_org AND social_v2_live(actor)
      AND EXISTS (SELECT 1 FROM user_credential u WHERE u.party_id=a.id AND u.active)
      AND NOT coalesce(r.block_a OR r.block_b,false)
      AND NOT coalesce(r.consent_a AND r.consent_b,false)
      AND NOT coalesce(CASE WHEN actor=r.party_a THEN r.mute_a OR r.dismiss_a OR r.follow_a
        ELSE r.mute_b OR r.dismiss_b OR r.follow_b END,false)
  ), ranked AS (
    SELECT * FROM candidates ORDER BY shared_interest DESC,
      md5(id::text || current_date::text || CASE WHEN use_personalization THEN actor::text ELSE '' END),id
    LIMIT page_size
  ) SELECT jsonb_build_object('personalized',use_personalization,'items',
    coalesce(jsonb_agg(jsonb_build_object('partyId',id,'displayName',display_name,
      'reason',CASE WHEN shared_interest=1 THEN 'shared_interests' ELSE 'public_profile' END,
      'relationship',social_v2_pair_json(actor,id,pair_state))), '[]'::jsonb))
    FROM ranked);
END $$;
REVOKE ALL ON FUNCTION social_v2_publish_batch() FROM PUBLIC;
REVOKE ALL ON FUNCTION social_v2_feed(bigint,bigint,integer) FROM PUBLIC;
REVOKE ALL ON FUNCTION social_v2_discover(bigint,integer) FROM PUBLIC;

CREATE OR REPLACE FUNCTION social_v2_me(actor bigint) RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF NOT social_v2_live(actor) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  RETURN jsonb_build_object('discoverable',coalesce((SELECT discoverable FROM social_v2_preference
    WHERE party_id=actor),false),'personalized',coalesce((SELECT personalized FROM social_v2_preference
    WHERE party_id=actor),true),'revision',coalesce((SELECT revision FROM social_v2_preference
    WHERE party_id=actor),0),'relationships',coalesce((
      SELECT jsonb_agg(social_v2_pair_json(actor,id,pair_state)||jsonb_build_object('displayName',display_name)) FROM (
        SELECT a.id,a.display_name,p AS pair_state FROM social_v2_pair p JOIN party a
          ON a.id=CASE WHEN p.party_a=actor THEN p.party_b ELSE p.party_a END
        WHERE (p.party_a=actor OR p.party_b=actor) AND social_v2_live(a.id)
          AND (social_v2_allowed(actor,a.id) OR CASE WHEN actor=p.party_a THEN p.block_a ELSE p.block_b END)
        ORDER BY p.updated_at DESC,a.id LIMIT 50
      ) visible), '[]'::jsonb));
END $$;
REVOKE ALL ON FUNCTION social_v2_me(bigint) FROM PUBLIC;

-- A blocked recipient cannot poll the other party's relationship revision.
-- The blocking party retains its own control so it can unblock deliberately.
CREATE OR REPLACE FUNCTION social_v2_relationship(actor bigint, target bigint)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF actor IS NULL OR target IS NULL OR actor<=0 OR target<=0 OR actor=target THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  IF NOT social_v2_live(actor) OR NOT social_v2_live(target) THEN
    RETURN '{"error":"unavailable"}'::jsonb;
  END IF;
  IF NOT social_v2_allowed(actor,target) AND NOT EXISTS (
    SELECT 1 FROM social_v2_pair p
    WHERE p.party_a=least(actor,target) AND p.party_b=greatest(actor,target)
      AND CASE WHEN actor=p.party_a THEN p.block_a ELSE p.block_b END
  ) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  RETURN social_v2_state(actor,target);
END $$;
REVOKE ALL ON FUNCTION social_v2_relationship(bigint,bigint) FROM PUBLIC;

COMMIT;
