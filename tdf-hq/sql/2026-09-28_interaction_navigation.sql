BEGIN;
-- Opaque stable URLs resolve through the current domain adapter, never a cached
-- title, route or authorization decision captured at notification creation.
CREATE OR REPLACE FUNCTION interaction_destination(actor bigint,destination_kind text,destination uuid)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE t interaction_target%ROWTYPE; context jsonb; comment_value jsonb;
BEGIN
 IF destination_kind='comment' THEN
   SELECT target.* INTO t FROM interaction_comment c JOIN interaction_target target ON target.id=c.target_id
     WHERE c.id=destination;
 ELSIF destination_kind='target' THEN SELECT * INTO t FROM interaction_target WHERE id=destination;
 ELSE RETURN '{"error":"invalid"}'; END IF;
 IF NOT FOUND THEN RETURN '{"error":"unavailable"}'; END IF;
 context:=interaction_target_context(t.id,actor);
 IF context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF destination_kind='comment' THEN
   comment_value:=interaction_comment_context(t.id,actor,destination);
   IF comment_value ? 'error' THEN RETURN comment_value; END IF;
 END IF;
 RETURN context||jsonb_build_object('targetId',t.id,'commentId',CASE WHEN destination_kind='comment' THEN destination END,
   'context',comment_value);
END $$;

CREATE OR REPLACE FUNCTION interaction_mention_candidates(actor bigint,target uuid,query_text text,after_party bigint,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE result_value jsonb; cursor_value bigint; term text;
BEGIN
 IF NOT interaction_actor_live(actor) OR interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF length(btrim(query_text)) NOT BETWEEN 2 AND 120 OR page_size NOT BETWEEN 1 AND 50
   OR after_party<0 THEN RETURN '{"error":"invalid"}'; END IF;
 term:='%'||replace(replace(replace(lower(btrim(query_text)),chr(92),chr(92)||chr(92)),'%',chr(92)||'%'),'_',chr(92)||'_')||'%';
 WITH page AS MATERIALIZED (
   SELECT p.id,p.display_name,f.avatar_url,u.username FROM party p
   LEFT JOIN fan_profile f ON f.fan_party_id=p.id
   JOIN LATERAL (SELECT c.username FROM user_credential c WHERE c.party_id=p.id AND c.active ORDER BY c.id LIMIT 1) u ON true
   WHERE p.id>coalesce(after_party,0) AND p.id<>actor AND NOT p.is_org
     AND (lower(p.display_name) LIKE term OR lower(u.username) LIKE term)
     AND interaction_actor_live(p.id) AND NOT interaction_blocked(actor,p.id)
     AND interaction_target_context(target,p.id) IS NOT NULL
     AND (EXISTS(SELECT 1 FROM social_v2_preference pref WHERE pref.party_id=p.id AND pref.discoverable)
       OR EXISTS(SELECT 1 FROM social_v2_pair pair WHERE pair.party_a=least(actor,p.id) AND pair.party_b=greatest(actor,p.id)
         AND pair.consent_a AND pair.consent_b))
   ORDER BY p.id LIMIT page_size+1
 ), numbered AS (SELECT *,row_number() OVER(ORDER BY id) ordinal FROM page)
 SELECT coalesce(jsonb_agg(jsonb_build_object('partyId',id,'partyType','person','displayName',display_name,
   'username',username,'avatarUrl',avatar_url,'accountStatus','active','secondaryLabel',NULL) ORDER BY id)
     FILTER(WHERE ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(id ORDER BY id))[page_size] END
 INTO result_value,cursor_value FROM numbered;
 RETURN jsonb_build_object('items',result_value,'nextCursor',cursor_value);
END $$;

CREATE OR REPLACE FUNCTION interaction_preferences(actor bigint,settings jsonb DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE result_value jsonb;
BEGIN
 IF NOT interaction_actor_live(actor) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF settings IS NOT NULL THEN
   IF jsonb_typeof(settings)<>'object' OR settings-ARRAY['reactions','comments','replies','mentions']<>'{}'::jsonb
     OR NOT (settings ?& ARRAY['reactions','comments','replies','mentions'])
     OR EXISTS(SELECT 1 FROM jsonb_each(settings) s WHERE jsonb_typeof(s.value)<>'boolean')
   THEN RETURN '{"error":"invalid"}'; END IF;
   INSERT INTO interaction_notification_preference(party_id,reactions,comments,replies,mentions)
   VALUES(actor,(settings->>'reactions')::boolean,(settings->>'comments')::boolean,
     (settings->>'replies')::boolean,(settings->>'mentions')::boolean)
   ON CONFLICT(party_id) DO UPDATE SET reactions=excluded.reactions,comments=excluded.comments,
     replies=excluded.replies,mentions=excluded.mentions,updated_at=now();
 END IF;
 SELECT jsonb_build_object('reactions',p.reactions,'comments',p.comments,'replies',p.replies,'mentions',p.mentions)
 INTO result_value FROM interaction_notification_preference p WHERE p.party_id=actor;
 RETURN coalesce(result_value,'{"reactions":true,"comments":true,"replies":true,"mentions":true}'::jsonb);
END $$;

CREATE OR REPLACE FUNCTION interaction_moderation_page(actor bigint,target uuid,before_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE context jsonb; pivot interaction_comment%ROWTYPE; items jsonb; cursor_value uuid;
BEGIN
 context:=interaction_target_context(target,actor);
 IF context IS NULL OR NOT ((context->>'canManage')::boolean OR interaction_is_moderator(actor)) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE target_id=target AND id=before_id;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 WITH page AS MATERIALIZED (
   SELECT c.* FROM interaction_comment c WHERE c.target_id=target
     AND NOT interaction_blocked(actor,c.author_id)
     AND (c.state='hidden' OR (interaction_is_moderator(actor) AND EXISTS(
       SELECT 1 FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open')))
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ), numbered AS (SELECT p.*,row_number() OVER(ORDER BY p.created_at DESC,p.id DESC) ordinal FROM page p)
 SELECT coalesce(jsonb_agg(interaction_comment_json(c,actor)||jsonb_build_object('moderationBody',c.body,
   'openReports',(SELECT count(*) FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open'))
   ORDER BY n.ordinal) FILTER(WHERE n.ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(n.id ORDER BY n.ordinal))[page_size] END
 INTO items,cursor_value FROM numbered n JOIN interaction_comment c ON c.id=n.id;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
COMMIT;
