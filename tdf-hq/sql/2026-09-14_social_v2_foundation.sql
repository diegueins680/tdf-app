-- Additive, opt-in social authority. Apply separately; not in boot migrations yet.
-- Caller identity is supplied only by an authenticated server, never request JSON.
BEGIN;
CREATE TABLE IF NOT EXISTS social_v2_runtime (
  singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
  enabled boolean NOT NULL DEFAULT false
);
INSERT INTO social_v2_runtime(singleton) VALUES (true) ON CONFLICT DO NOTHING;
CREATE TABLE IF NOT EXISTS social_v2_preference (
  party_id bigint PRIMARY KEY REFERENCES party(id) ON DELETE RESTRICT,
  discoverable boolean NOT NULL DEFAULT false,
  personalized boolean NOT NULL DEFAULT true,
  closed boolean NOT NULL DEFAULT false,
  revision bigint NOT NULL DEFAULT 0 CHECK (revision >= 0),
  updated_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE TABLE IF NOT EXISTS social_v2_pair (
  party_a bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  party_b bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  consent_a boolean NOT NULL DEFAULT false,
  consent_b boolean NOT NULL DEFAULT false,
  follow_a boolean NOT NULL DEFAULT false,
  follow_b boolean NOT NULL DEFAULT false,
  block_a boolean NOT NULL DEFAULT false,
  block_b boolean NOT NULL DEFAULT false,
  mute_a boolean NOT NULL DEFAULT false,
  mute_b boolean NOT NULL DEFAULT false,
  dismiss_a boolean NOT NULL DEFAULT false,
  dismiss_b boolean NOT NULL DEFAULT false,
  revision bigint NOT NULL DEFAULT 0 CHECK (revision >= 0),
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  updated_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(party_a, party_b), CHECK (party_a < party_b),
  CHECK (NOT (block_a OR block_b) OR NOT (consent_a OR consent_b OR follow_a OR follow_b))
);
CREATE INDEX IF NOT EXISTS social_v2_pair_reverse ON social_v2_pair(party_b, party_a);
CREATE TABLE IF NOT EXISTS social_v2_command (
  actor bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  request_key text NOT NULL CHECK (length(request_key) BETWEEN 1 AND 80),
  target bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  operation text NOT NULL,
  expected_revision bigint NOT NULL,
  result jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(actor,request_key)
);
CREATE INDEX IF NOT EXISTS social_v2_command_rate ON social_v2_command(actor,created_at);

CREATE INDEX IF NOT EXISTS social_v2_active_credential_party
  ON user_credential(party_id) WHERE active;

CREATE OR REPLACE FUNCTION social_v2_live(p bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT EXISTS (SELECT 1 FROM party a JOIN user_credential u ON u.party_id=a.id
    WHERE a.id=p AND NOT a.is_org AND u.active)
    AND NOT EXISTS (SELECT 1 FROM social_v2_preference s WHERE s.party_id=p AND s.closed)
$$;
CREATE OR REPLACE FUNCTION social_v2_allowed(viewer bigint, target bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
  SELECT social_v2_live(viewer) AND social_v2_live(target)
    AND NOT EXISTS (SELECT 1 FROM social_v2_pair p
      WHERE p.party_a=least(viewer,target) AND p.party_b=greatest(viewer,target)
        AND (p.block_a OR p.block_b))
$$;
CREATE OR REPLACE FUNCTION social_v2_state(viewer bigint, target bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
  SELECT jsonb_build_object('partyId',target,'revision',coalesce(p.revision,0),
    'following',CASE WHEN viewer=p.party_a THEN p.follow_a ELSE coalesce(p.follow_b,false) END,
    'requested',CASE WHEN viewer=p.party_a THEN p.consent_a ELSE coalesce(p.consent_b,false) END,
    'incoming',CASE WHEN viewer=p.party_a THEN p.consent_b ELSE coalesce(p.consent_a,false) END,
    'connected',coalesce(p.consent_a AND p.consent_b,false) AND social_v2_allowed(viewer,target),
    'blocked',CASE WHEN viewer=p.party_a THEN p.block_a ELSE coalesce(p.block_b,false) END,
    'muted',CASE WHEN viewer=p.party_a THEN p.mute_a ELSE coalesce(p.mute_b,false) END,
    'dismissed',CASE WHEN viewer=p.party_a THEN p.dismiss_a ELSE coalesce(p.dismiss_b,false) END)
  FROM (VALUES(1)) seed(n) LEFT JOIN social_v2_pair p
    ON p.party_a=least(viewer,target) AND p.party_b=greatest(viewer,target)
$$;
CREATE OR REPLACE FUNCTION social_v2_mutate(actor bigint, target bigint, op text,
  expected bigint, request_id text) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE p social_v2_pair%ROWTYPE; previous social_v2_command%ROWTYPE;
  side_a boolean; result_value jsonb;
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF actor IS NULL OR target IS NULL OR actor <= 0 OR target <= 0 OR actor=target
    OR expected IS NULL OR expected < 0 OR request_id IS NULL
    OR length(request_id) NOT BETWEEN 1 AND 80 OR op IS NULL
    OR op NOT IN ('request','accept','disconnect','follow','unfollow','block','unblock',
                  'mute','unmute','dismiss','undismiss') THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  -- Both account and preference revocation take these locks in the same order.
  PERFORM id FROM party WHERE id IN (actor,target) ORDER BY id FOR UPDATE;
  -- Fence credential revocation as well as actor preference changes.
  PERFORM id FROM user_credential WHERE party_id IN (actor,target) ORDER BY id FOR UPDATE;
  IF NOT social_v2_live(actor) OR NOT social_v2_live(target) THEN
    RETURN '{"error":"unavailable"}'::jsonb;
  END IF;
  INSERT INTO social_v2_pair(party_a,party_b)
    VALUES(least(actor,target),greatest(actor,target)) ON CONFLICT DO NOTHING;
  SELECT * INTO STRICT p FROM social_v2_pair
    WHERE party_a=least(actor,target) AND party_b=greatest(actor,target) FOR UPDATE;
  side_a := actor=p.party_a;
  -- Revalidate before replay. Never return a previous success through a current denial.
  IF (p.block_a OR p.block_b) AND op NOT IN ('block','unblock','disconnect','unfollow') THEN
    RETURN '{"error":"unavailable"}'::jsonb;
  END IF;
  SELECT * INTO previous FROM social_v2_command c WHERE c.actor=actor AND c.request_key=request_id;
  IF FOUND THEN
    IF previous.target<>target OR previous.operation<>op OR previous.expected_revision<>expected THEN
      RETURN '{"error":"request_key_conflict"}'::jsonb;
    END IF;
    -- Return current state, not old consent. No repeated side effect.
    RETURN social_v2_state(actor,target);
  END IF;
  IF p.revision<>expected THEN RETURN '{"error":"revision_conflict"}'::jsonb; END IF;
  IF (SELECT count(*) FROM social_v2_command c WHERE c.actor=actor
      AND c.created_at > clock_timestamp()-interval '1 minute') >= 60 THEN
    RETURN '{"error":"rate_limited"}'::jsonb;
  END IF;
  IF op='accept' AND NOT (CASE WHEN side_a THEN p.consent_b ELSE p.consent_a END) THEN
    RETURN '{"error":"no_request"}'::jsonb;
  END IF;
  UPDATE social_v2_pair SET
    consent_a=CASE WHEN op='block' OR (side_a AND op='disconnect') THEN false
      WHEN side_a AND op IN ('request','accept') THEN true ELSE consent_a END,
    consent_b=CASE WHEN op='block' OR (NOT side_a AND op='disconnect') THEN false
      WHEN NOT side_a AND op IN ('request','accept') THEN true ELSE consent_b END,
    follow_a=CASE WHEN op='block' THEN false WHEN side_a AND op IN ('follow','unfollow')
      THEN op='follow' ELSE follow_a END,
    follow_b=CASE WHEN op='block' THEN false WHEN NOT side_a AND op IN ('follow','unfollow')
      THEN op='follow' ELSE follow_b END,
    block_a=CASE WHEN side_a AND op IN ('block','unblock') THEN op='block' ELSE block_a END,
    block_b=CASE WHEN NOT side_a AND op IN ('block','unblock') THEN op='block' ELSE block_b END,
    mute_a=CASE WHEN side_a AND op IN ('mute','unmute') THEN op='mute' ELSE mute_a END,
    mute_b=CASE WHEN NOT side_a AND op IN ('mute','unmute') THEN op='mute' ELSE mute_b END,
    dismiss_a=CASE WHEN side_a AND op IN ('dismiss','undismiss') THEN op='dismiss' ELSE dismiss_a END,
    dismiss_b=CASE WHEN NOT side_a AND op IN ('dismiss','undismiss') THEN op='dismiss' ELSE dismiss_b END,
    revision=revision+1, updated_at=clock_timestamp()
    WHERE party_a=p.party_a AND party_b=p.party_b;
  result_value := social_v2_state(actor,target);
  INSERT INTO social_v2_command(actor,request_key,target,operation,expected_revision,result)
    VALUES(actor,request_id,target,op,expected,result_value);
  RETURN result_value;
END $$;
-- PL/pgSQL argument names intentionally public contract; resolve SQL column collisions explicitly.
ALTER FUNCTION social_v2_mutate(bigint,bigint,text,bigint,text) SET plpgsql.variable_conflict = use_variable;

CREATE OR REPLACE FUNCTION social_v2_preferences(actor bigint, discover boolean,
  personalize boolean, expected bigint) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE rev bigint;
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  IF discover IS NULL OR personalize IS NULL OR expected IS NULL OR expected<0 THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  PERFORM id FROM party WHERE id=actor FOR UPDATE;
  PERFORM id FROM user_credential WHERE party_id=actor ORDER BY id FOR UPDATE;
  IF NOT social_v2_live(actor) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  INSERT INTO social_v2_preference(party_id) VALUES(actor) ON CONFLICT DO NOTHING;
  SELECT revision INTO rev FROM social_v2_preference WHERE party_id=actor FOR UPDATE;
  IF rev<>expected THEN RETURN '{"error":"revision_conflict"}'::jsonb; END IF;
  UPDATE social_v2_preference SET discoverable=discover,personalized=personalize,
    revision=revision+1,updated_at=clock_timestamp() WHERE party_id=actor;
  RETURN jsonb_build_object('revision',rev+1,'discoverable',discover,'personalized',personalize);
END $$;
CREATE OR REPLACE FUNCTION social_v2_close(actor bigint) RETURNS jsonb LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS(SELECT 1 FROM social_v2_runtime WHERE enabled) THEN
    RETURN '{"error":"disabled"}'::jsonb;
  END IF;
  PERFORM id FROM party WHERE id=actor FOR UPDATE;
  IF NOT social_v2_live(actor) THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  INSERT INTO social_v2_preference(party_id,closed) VALUES(actor,true)
    ON CONFLICT(party_id) DO UPDATE SET closed=true,discoverable=false,
      revision=social_v2_preference.revision+1,updated_at=clock_timestamp();
  UPDATE social_v2_pair SET consent_a=false,consent_b=false,follow_a=false,follow_b=false,
    revision=revision+1,updated_at=clock_timestamp() WHERE party_a=actor OR party_b=actor;
  RETURN '{"closed":true}'::jsonb;
END $$;
-- Closed means social participation is disabled, not account deletion or data erasure.
-- Retain pair/command tombstones; no backfill of manufactured legacy consent.
REVOKE ALL ON FUNCTION social_v2_mutate(bigint,bigint,text,bigint,text) FROM PUBLIC;
REVOKE ALL ON FUNCTION social_v2_preferences(bigint,boolean,boolean,bigint) FROM PUBLIC;
REVOKE ALL ON FUNCTION social_v2_close(bigint) FROM PUBLIC;
COMMIT;
