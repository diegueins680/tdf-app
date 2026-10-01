-- Preserve legacy moment reaction mapping without shadowing the comment record.
BEGIN;
CREATE OR REPLACE FUNCTION interaction_legacy_command(actor bigint,kind text,entity text,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; context jsonb; requested uuid; current_reaction uuid; mapped uuid; desired boolean;
 command jsonb; result_value jsonb; alias_value bigint; c interaction_comment%ROWTYPE; reply_to uuid; requested_entity text:=entity; source_entity text:=entity;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF kind='club_post' AND payload->>'operation' IN ('legacy.comment','legacy.hide','legacy.restore','legacy.reaction') THEN
   SELECT comment_id INTO reply_to FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id=entity;
   IF reply_to IS NOT NULL THEN
     SELECT * INTO c FROM interaction_comment WHERE id=reply_to;
     SELECT entity_key INTO source_entity FROM interaction_target WHERE id=c.target_id;
     IF payload->>'operation'<>'legacy.reaction' THEN entity:=source_entity; END IF;
   END IF;
   IF payload ? 'artistId' AND NOT EXISTS(SELECT 1 FROM fan_club_post p JOIN fan_club club ON club.id=p.club_id
       WHERE p.id=source_entity::bigint AND club.artist_party_id=(payload->>'artistId')::bigint) THEN RETURN '{"error":"unavailable"}'; END IF;
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
     SELECT reaction_choice.id INTO mapped FROM reaction_type r JOIN content_reaction_type reaction_choice
       ON reaction_choice.code=CASE r.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE r.code END WHERE r.id=requested;
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
     RETURN jsonb_build_object('fcpId',alias_value,'fcpParentId',requested_entity::bigint,'fcpTitle',payload->'title',
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
COMMIT;
