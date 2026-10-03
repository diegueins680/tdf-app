BEGIN;
SET LOCAL lock_timeout='5s';
-- Legacy adapters use the same mutation function and permission evaluator. The
-- only compatibility behavior retained here is the old optional toggle contract.
CREATE OR REPLACE FUNCTION interaction_legacy_command(actor bigint,kind text,entity text,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; context jsonb; requested uuid; current_reaction uuid; mapped uuid; desired boolean;
 command jsonb; result_value jsonb; alias_value bigint; c interaction_comment%ROWTYPE; reply_to uuid;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF kind='club_post' AND payload->>'operation' IN ('legacy.comment','legacy.hide','legacy.restore') THEN
   SELECT comment_id INTO reply_to FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id=entity;
   IF reply_to IS NOT NULL THEN
     SELECT * INTO c FROM interaction_comment WHERE id=reply_to;
     SELECT entity_key INTO entity FROM interaction_target WHERE id=c.target_id;
   END IF;
   IF payload ? 'artistId' AND NOT EXISTS(SELECT 1 FROM fan_club_post p JOIN fan_club club ON club.id=p.club_id
       WHERE p.id=entity::bigint AND club.artist_party_id=(payload->>'artistId')::bigint) THEN RETURN '{"error":"unavailable"}'; END IF;
 END IF;
 target:=interaction_register(kind,entity,actor);
 IF target IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 context:=interaction_target_context(target,actor);
 PERFORM id FROM party WHERE id IN(actor,(context->>'ownerId')::bigint) ORDER BY id FOR SHARE;
 PERFORM interaction_lock_source(target); PERFORM interaction_lock_permissions(target,actor);
 PERFORM pg_advisory_xact_lock(hashtextextended('interaction-actor:'||actor,0));
 PERFORM id FROM interaction_target WHERE id=target FOR UPDATE;
 IF payload->>'operation'='legacy.reaction' THEN
   BEGIN requested:=(payload->>'reactionTypeId')::uuid; EXCEPTION WHEN invalid_text_representation THEN RETURN '{"error":"invalid"}'; END;
   mapped:=requested;
   IF kind='event_moment' THEN
     SELECT c.id INTO mapped FROM reaction_type r JOIN content_reaction_type c
       ON c.code=CASE r.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE r.code END WHERE r.id=requested;
   END IF;
   IF mapped IS NULL THEN RETURN '{"error":"invalid_reaction"}'; END IF;
   SELECT reaction_type_id INTO current_reaction FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
   desired:=coalesce((payload->>'active')::boolean,current_reaction IS DISTINCT FROM mapped);
   command:=jsonb_build_object('operation','reaction.set','reactionTypeId',CASE WHEN desired THEN mapped
     WHEN current_reaction=mapped THEN NULL ELSE current_reaction END);
   RETURN interaction_command(actor,target,gen_random_uuid(),command);
 ELSIF payload->>'operation' IN ('legacy.hide','legacy.restore') THEN
   IF reply_to IS NOT NULL THEN
     PERFORM id FROM interaction_comment WHERE id=reply_to FOR UPDATE;
     SELECT * INTO c FROM interaction_comment WHERE id=reply_to;
     RETURN interaction_command(actor,target,gen_random_uuid(),jsonb_build_object('operation',
       CASE payload->>'operation' WHEN 'legacy.hide' THEN 'comment.hide' ELSE 'comment.restore' END,
       'commentId',reply_to,'expectedVersion',c.version,'reason','Content owner moderation'));
   END IF;
   IF NOT (context->>'canManage')::boolean THEN RETURN '{"error":"forbidden"}'; END IF;
   UPDATE fan_club_post SET is_hidden=(payload->>'operation'='legacy.hide') WHERE id=entity::bigint;
   RETURN jsonb_build_object('ok',true);
 ELSIF payload->>'operation'='legacy.comment' THEN
   result_value:=interaction_command(actor,target,gen_random_uuid(),jsonb_build_object('operation','comment.create',
     'body',payload->>'body','mentions','[]'::jsonb)||CASE WHEN reply_to IS NULL THEN '{}'::jsonb ELSE jsonb_build_object('parentId',reply_to) END);
   IF result_value ? 'error' THEN RETURN result_value; END IF;
   SELECT * INTO c FROM interaction_comment WHERE id=(result_value->>'id')::uuid;
   IF kind='club_post' THEN
     alias_value:=nextval(pg_get_serial_sequence('fan_club_post','id'));
     INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
     VALUES('club_reply',alias_value::text,target,c.id,jsonb_build_object('title',payload->'title','mediaUrls',payload->'mediaUrls'));
     RETURN jsonb_build_object('fcpId',alias_value,'fcpParentId',entity::bigint,'fcpTitle',payload->'title',
       'fcpContent',c.body,'fcpMediaUrls',coalesce(payload->'mediaUrls','[]'::jsonb),'fcpAuthorId',actor,
       'fcpAuthorName',result_value->'author'->>'displayName','fcpAvatarUrl',result_value->'author'->'avatarUrl',
       'fcpIsPinned',false,'fcpIsHidden',false,'fcpReplies',0,
       'fcpReactions',jsonb_build_object('rsItems','[]'::jsonb,'rsTotal',0,'rsMyReactionTypeId',NULL),
       'fcpCreatedAt',c.created_at,'fcpUpdatedAt',NULL);
   ELSIF kind='event_moment' THEN
     alias_value:=nextval(pg_get_serial_sequence('event_moment_comment','id'));
     INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
     VALUES('moment_comment',alias_value::text,target,c.id,'{}');
     RETURN jsonb_build_object('emcId',alias_value::text,'emcMomentId',entity,'emcAuthorPartyId',actor::text,
       'emcAuthorName',result_value->'author'->>'displayName','emcBody',c.body,'emcCreatedAt',c.created_at,'emcUpdatedAt',c.updated_at);
   END IF;
 END IF;
 RETURN '{"error":"invalid"}';
END $$;
CREATE OR REPLACE FUNCTION interaction_legacy_reaction_summary(actor bigint,kind text,entity text) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE summary jsonb;
BEGIN
 summary:=interaction_summary(actor,kind,entity);
 IF summary ? 'error' THEN RETURN jsonb_build_object('rsItems','[]'::jsonb,'rsTotal',0,'rsMyReactionTypeId',NULL); END IF;
 RETURN jsonb_build_object('rsItems',coalesce((SELECT jsonb_agg(jsonb_build_object(
   'rsiReactionTypeId',x->'id','rsiCode',x->>'code','rsiNameEs',r.name_es,'rsiNameEn',r.name_en,
   'rsiDisplaySymbol',x->>'emoji','rsiCount',x->'count')) FROM jsonb_array_elements(summary->'reactions') x
   JOIN content_reaction_type r ON r.id=(x->>'id')::uuid),'[]'::jsonb),
   'rsTotal',coalesce((SELECT sum((x->>'count')::bigint) FROM jsonb_array_elements(summary->'reactions') x),0),
   'rsMyReactionTypeId',summary->'myReactionTypeId');
END $$;
-- Old clients receive a bounded preview and privacy-redacted reaction identities.
-- New clients consume counts and cursor pages, never an unbounded DTO tree.
CREATE OR REPLACE FUNCTION interaction_legacy_moment(actor bigint,entity text) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; comments jsonb; reactions jsonb;
BEGIN
 target:=interaction_register('event_moment',entity,actor);
 IF target IS NULL THEN RETURN jsonb_build_object('comments','[]'::jsonb,'reactions','[]'::jsonb); END IF;
 SELECT coalesce(jsonb_agg(jsonb_build_object('emcId',coalesce(m.legacy_id,c.id::text),'emcMomentId',entity,
   'emcAuthorPartyId',c.author_id::text,'emcAuthorName',a.value->>'displayName','emcBody',c.body,
   'emcCreatedAt',c.created_at,'emcUpdatedAt',c.updated_at) ORDER BY c.created_at,c.id),'[]'::jsonb) INTO comments
 FROM (SELECT c.* FROM interaction_comment c WHERE c.target_id=target AND c.state='visible'
   AND interaction_comment_readable(c,actor) ORDER BY c.created_at DESC,c.id DESC LIMIT 20) c
 CROSS JOIN LATERAL (SELECT interaction_public_author(actor,c.author_id) value) a
 LEFT JOIN interaction_legacy_mapping m ON m.comment_id=c.id AND m.legacy_kind='moment_comment';
 SELECT coalesce(jsonb_agg(jsonb_build_object('emrReactionTypeId',coalesce(old.id,r.reaction_type_id)::text,
   'emrReactionCode',coalesce(old.code,c.code),'emrReactionNameEs',c.name_es,'emrReactionNameEn',c.name_en,
   'emrReactionEmoji',c.emoji,'emrPartyId',CASE WHEN r.actor_id=actor THEN actor::text END,
   'emrCreatedAt',CASE WHEN r.actor_id=actor THEN r.created_at END)),'[]'::jsonb) INTO reactions
 FROM (SELECT r.* FROM interaction_reaction r WHERE r.target_id=target
   AND interaction_actor_live(r.actor_id) AND NOT interaction_blocked(actor,r.actor_id)
   ORDER BY (r.actor_id=actor) DESC,r.created_at DESC,r.actor_id LIMIT 100) r
 JOIN content_reaction_type c ON c.id=r.reaction_type_id
 LEFT JOIN reaction_type old ON old.code=CASE c.code WHEN 'heart' THEN 'love' WHEN 'clap' THEN 'applause' ELSE c.code END;
 RETURN jsonb_build_object('comments',comments,'reactions',reactions);
END $$;
COMMIT;
