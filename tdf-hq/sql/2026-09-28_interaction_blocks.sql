BEGIN;
CREATE OR REPLACE FUNCTION interaction_block_state(actor bigint,peer bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object('partyId',peer,'blocked',CASE WHEN p.party_a=actor THEN coalesce(p.block_a,false)
   ELSE coalesce(p.block_b,false) END,'version',coalesce(p.revision,0))
 FROM (VALUES(1)) seed(n) LEFT JOIN social_v2_pair p
 ON p.party_a=least(actor,peer) AND p.party_b=greatest(actor,peer)
$$;
CREATE OR REPLACE FUNCTION interaction_block(actor bigint,peer bigint,blocked boolean,
 expected bigint,request_id uuid) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE p social_v2_pair%ROWTYPE; old social_v2_command%ROWTYPE; op text;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF actor IS NULL OR peer IS NULL OR actor=peer OR peer<=0 OR blocked IS NULL OR expected IS NULL OR expected<0 OR request_id IS NULL
 THEN RETURN '{"error":"invalid"}'; END IF;
 PERFORM id FROM party WHERE id IN (actor,peer) ORDER BY id FOR UPDATE;
 PERFORM id FROM user_credential WHERE party_id IN (actor,peer) ORDER BY id FOR UPDATE;
 IF NOT interaction_actor_live(actor) OR NOT EXISTS(SELECT 1 FROM party WHERE id=peer) THEN RETURN '{"error":"unavailable"}'; END IF;
 INSERT INTO social_v2_pair(party_a,party_b) VALUES(least(actor,peer),greatest(actor,peer)) ON CONFLICT DO NOTHING;
 SELECT * INTO STRICT p FROM social_v2_pair WHERE party_a=least(actor,peer) AND party_b=greatest(actor,peer) FOR UPDATE;
 op:=CASE WHEN blocked THEN 'block' ELSE 'unblock' END;
 SELECT * INTO old FROM social_v2_command c WHERE c.actor=interaction_block.actor
   AND c.request_key='interaction:'||request_id;
 IF FOUND THEN
   IF old.target<>peer OR old.operation<>op OR old.expected_revision<>expected THEN RETURN '{"error":"request_key_conflict"}'; END IF;
   RETURN interaction_block_state(actor,peer)||'{"replay":true}'::jsonb;
 END IF;
 IF p.revision<>expected THEN RETURN '{"error":"revision_conflict"}'; END IF;
 IF (SELECT count(*) FROM social_v2_command c WHERE c.actor=interaction_block.actor
   AND c.created_at>now()-interval '1 minute')>=30 THEN RETURN '{"error":"rate_limited"}'; END IF;
 UPDATE social_v2_pair SET
   block_a=CASE WHEN actor=party_a THEN blocked ELSE block_a END,
   block_b=CASE WHEN actor=party_b THEN blocked ELSE block_b END,
   consent_a=CASE WHEN blocked THEN false ELSE consent_a END,
   consent_b=CASE WHEN blocked THEN false ELSE consent_b END,
   follow_a=CASE WHEN blocked THEN false ELSE follow_a END,
   follow_b=CASE WHEN blocked THEN false ELSE follow_b END,
   revision=revision+1,updated_at=now()
 WHERE party_a=p.party_a AND party_b=p.party_b;
 INSERT INTO social_v2_command(actor,request_key,target,operation,expected_revision,result)
 VALUES(actor,'interaction:'||request_id,peer,op,expected,interaction_block_state(actor,peer));
 INSERT INTO interaction_audit(actor_id,operation,reason) VALUES(actor,'user.'||op,'Account block updated');
 RETURN interaction_block_state(actor,peer);
END $$;
COMMIT;
