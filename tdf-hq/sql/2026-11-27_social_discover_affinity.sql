-- Affinity order of the people in Discover (SOCIAL-DISCOVER-AFFINITY-001,
-- formal/system/social-discover-affinity.md). Additive: a three-argument form of
-- social_v2_discover and two indexes for its legacy evidence reads. The
-- two-argument function is not touched and stays the only one called while
-- SOCIAL_DISCOVER_AFFINITY_ENABLED is false.
BEGIN;

-- The actor's own legacy reactions and replies, read by actor and date.
CREATE INDEX IF NOT EXISTS social_v2_post_reaction_reactor
  ON fan_club_post_reaction(reactor_party_id,created_at);
CREATE INDEX IF NOT EXISTS social_v2_reply_author ON fan_club_post(fan_party_id,created_at)
  WHERE parent_id IS NOT NULL AND NOT is_hidden;

-- public_sample, candidates and the base order key are the text of
-- social_v2_discover(bigint,integer); scripts/test-social-discover-affinity-sql.py
-- fails when they differ. Affinity only moves candidates inside that set.
CREATE OR REPLACE FUNCTION social_v2_discover(actor bigint, page_size integer, use_affinity boolean)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE use_personalization boolean; read_behaviour boolean; canonical boolean := false;
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
  -- An actor who switched personalisation off has no behaviour read at all.
  read_behaviour := coalesce(use_affinity,false) AND use_personalization;
  IF read_behaviour THEN
    -- Same authority switch as TDF.Interactions.Legacy.activated.
    SELECT activated_once INTO canonical FROM interaction_runtime WHERE singleton;
    canonical := coalesce(canonical,false);
  END IF;
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
  ), interaction AS (
    -- The actor's own interactions of the last 90 days: weight 1 per post the
    -- actor reacts to now, weight 2 per visible reply. Nothing here is read
    -- from another party's behaviour.
    SELECT 1 AS weight,CASE WHEN t.entity_key ~ '^[0-9]{1,18}$' THEN t.entity_key::bigint END AS post_id
      FROM interaction_reaction x JOIN interaction_target t ON t.id=x.target_id
      WHERE read_behaviour AND canonical AND x.actor_id=actor
        AND x.created_at>=now()-interval '90 days' AND t.entity_kind='club_post'
    UNION ALL
    SELECT 2,CASE WHEN t.entity_key ~ '^[0-9]{1,18}$' THEN t.entity_key::bigint END
      FROM interaction_comment x JOIN interaction_target t ON t.id=x.target_id
      WHERE read_behaviour AND canonical AND x.author_id=actor AND x.state='visible'
        AND x.created_at>=now()-interval '90 days' AND t.entity_kind='club_post'
    UNION ALL
    SELECT 1,x.post_id FROM fan_club_post_reaction x
      WHERE read_behaviour AND NOT canonical AND x.reactor_party_id=actor
        AND x.created_at>=now()-interval '90 days'
    UNION ALL
    SELECT 2,x.parent_id FROM fan_club_post x
      WHERE read_behaviour AND NOT canonical AND x.fan_party_id=actor
        AND x.parent_id IS NOT NULL AND NOT x.is_hidden
        AND x.created_at>=now()-interval '90 days'
  ), evidence AS (
    -- Count an interaction only on a top-level post that is not hidden, written
    -- by a candidate, in a club the actor may read now (the rule of
    -- social_v2_feed). 20 units is the most a candidate can have.
    SELECT p.fan_party_id AS author_id,least(sum(i.weight),20)::integer AS units
    FROM interaction i JOIN fan_club_post p ON p.id=i.post_id
      JOIN fan_club club ON club.id=p.club_id JOIN party artist ON artist.id=club.artist_party_id
      LEFT JOIN social_v2_preference pref ON pref.party_id=artist.id
      LEFT JOIN social_v2_pair r ON r.party_a=least(actor,artist.id) AND r.party_b=greatest(actor,artist.id)
    WHERE p.parent_id IS NULL AND NOT p.is_hidden
      AND p.fan_party_id IN (SELECT id FROM candidates)
      AND (club.artist_party_id=actor
        OR EXISTS(SELECT 1 FROM fan_follow f
          WHERE f.fan_party_id=actor AND f.artist_party_id=club.artist_party_id)
        OR EXISTS(SELECT 1 FROM fan_club_officer o WHERE o.club_id=club.id AND o.fan_party_id=actor))
      AND NOT artist.is_org AND NOT coalesce(pref.closed,false)
      AND EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=artist.id AND u.active)
      AND NOT coalesce(r.block_a OR r.block_b,false)
      AND NOT coalesce(CASE WHEN actor=r.party_a THEN r.mute_a ELSE r.mute_b END,false)
    GROUP BY p.fan_party_id
  ), scored AS (
    SELECT c.*,coalesce(e.units,0) AS units,
      row_number() OVER (ORDER BY coalesce(e.units,0) DESC,c.id) AS affinity_rank
    FROM candidates c LEFT JOIN evidence e ON e.author_id=c.id
  ), ranked AS (
    -- At most page_size/2 candidates with evidence lead; all others keep the
    -- base order, so none moves down more than page_size/2 places.
    SELECT * FROM scored ORDER BY (units>0 AND affinity_rank<=page_size/2) DESC,
      CASE WHEN units>0 AND affinity_rank<=page_size/2 THEN affinity_rank END,
      shared_interest DESC,
      md5(id::text || current_date::text || CASE WHEN use_personalization THEN actor::text ELSE '' END),id
    LIMIT page_size
  ) SELECT jsonb_build_object('personalized',use_personalization,'items',
    coalesce(jsonb_agg(jsonb_build_object('partyId',id,'displayName',display_name,
      'reason',CASE WHEN shared_interest=1 THEN 'shared_interests' ELSE 'public_profile' END,
      'relationship',social_v2_pair_json(actor,id,pair_state))), '[]'::jsonb))
    FROM ranked);
END $$;
REVOKE ALL ON FUNCTION social_v2_discover(bigint,integer,boolean) FROM PUBLIC;

COMMIT;
