BEGIN;
SET LOCAL lock_timeout='5s';
LOCK TABLE interaction_comment IN SHARE MODE;
CREATE TABLE IF NOT EXISTS interaction_target_comment_total (
 target_id uuid NOT NULL REFERENCES interaction_target(id),
 author_id bigint NOT NULL REFERENCES party(id),
 comments bigint NOT NULL DEFAULT 0 CHECK(comments>=0),
 roots bigint NOT NULL DEFAULT 0 CHECK(roots>=0 AND roots<=comments),
 PRIMARY KEY(target_id,author_id)
);
INSERT INTO interaction_target_comment_total(target_id,author_id,comments,roots)
 SELECT target_id,author_id,sum(comments),sum(roots) FROM interaction_comment_total GROUP BY target_id,author_id
 ON CONFLICT(target_id,author_id) DO UPDATE SET comments=excluded.comments,roots=excluded.roots;
CREATE INDEX IF NOT EXISTS interaction_comment_total_root ON interaction_comment_total(root_id,author_id) INCLUDE(comments,roots);
CREATE OR REPLACE FUNCTION interaction_count_comment() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_OP IN ('UPDATE','DELETE') AND OLD.state='visible' AND OLD.author_id IS NOT NULL THEN
   UPDATE interaction_comment_total SET comments=comments-1,roots=roots-CASE WHEN OLD.parent_id IS NULL THEN 1 ELSE 0 END
   WHERE target_id=OLD.target_id AND root_id=OLD.root_id AND author_id=OLD.author_id;
   IF NOT FOUND THEN RAISE EXCEPTION 'Missing comment counter' USING ERRCODE='23514'; END IF;
   UPDATE interaction_target_comment_total SET comments=comments-1,roots=roots-CASE WHEN OLD.parent_id IS NULL THEN 1 ELSE 0 END
   WHERE target_id=OLD.target_id AND author_id=OLD.author_id;
   IF NOT FOUND THEN RAISE EXCEPTION 'Missing target comment counter' USING ERRCODE='23514'; END IF;
 END IF;
 IF TG_OP IN ('INSERT','UPDATE') AND NEW.state='visible' AND NEW.author_id IS NOT NULL THEN
   INSERT INTO interaction_comment_total(target_id,root_id,author_id,comments,roots)
   VALUES(NEW.target_id,NEW.root_id,NEW.author_id,1,CASE WHEN NEW.parent_id IS NULL THEN 1 ELSE 0 END)
   ON CONFLICT(target_id,root_id,author_id) DO UPDATE SET
     comments=interaction_comment_total.comments+1,roots=interaction_comment_total.roots+excluded.roots;
   INSERT INTO interaction_target_comment_total(target_id,author_id,comments,roots)
   VALUES(NEW.target_id,NEW.author_id,1,CASE WHEN NEW.parent_id IS NULL THEN 1 ELSE 0 END)
   ON CONFLICT(target_id,author_id) DO UPDATE SET
     comments=interaction_target_comment_total.comments+1,roots=interaction_target_comment_total.roots+excluded.roots;
 END IF;
 RETURN NULL;
END $$;

-- Batch exactly the candidate identities, with indexed anti-joins for bilateral
-- blocks, closure and archival. Never evaluate a policy function for every body.
CREATE OR REPLACE FUNCTION interaction_author_batch(viewer bigint,candidates bigint[])
RETURNS TABLE(party_id bigint,author jsonb) LANGUAGE sql STABLE AS $$
 SELECT p.id,jsonb_build_object('id',p.id,'displayName',coalesce(nullif(btrim(f.display_name),''),p.display_name),'avatarUrl',f.avatar_url)
 FROM party p LEFT JOIN fan_profile f ON f.fan_party_id=p.id
 WHERE p.id=ANY(candidates) AND NOT p.is_org
   AND EXISTS(SELECT 1 FROM user_credential c WHERE c.party_id=p.id AND c.active)
   AND NOT EXISTS(SELECT 1 FROM social_v2_preference s WHERE s.party_id=p.id AND s.closed)
   AND NOT EXISTS(SELECT 1 FROM identity_party_archive a WHERE a.party_id=p.id)
   AND NOT EXISTS(SELECT 1 FROM social_v2_pair pair WHERE pair.party_a=least(viewer,p.id) AND pair.party_b=greatest(viewer,p.id)
     AND viewer IS NOT NULL AND (pair.block_a OR pair.block_b))
   AND NOT EXISTS(SELECT 1 FROM directory_profile_block b JOIN directory_profile x ON x.id=b.blocker_profile_id
     JOIN directory_profile y ON y.id=b.blocked_profile_id
     WHERE (x.subject_party_id=viewer AND y.subject_party_id=p.id) OR (y.subject_party_id=viewer AND x.subject_party_id=p.id))
$$;
CREATE OR REPLACE FUNCTION interaction_comments_json(comment_keys uuid[],viewer bigint)
RETURNS TABLE(comment_id uuid,value jsonb) LANGUAGE sql STABLE AS $$
 WITH selected AS MATERIALIZED (SELECT * FROM interaction_comment WHERE id=ANY(comment_keys)),
 candidates AS (SELECT author_id party_id FROM selected UNION SELECT m.party_id FROM interaction_comment_mention m WHERE m.comment_id=ANY(comment_keys)
   UNION SELECT t.author_id FROM interaction_comment_total t WHERE t.root_id=ANY(comment_keys)),
 authors AS MATERIALIZED (SELECT * FROM interaction_author_batch(viewer,ARRAY(SELECT party_id FROM candidates))),
 mentions AS (SELECT m.comment_id,jsonb_agg(jsonb_build_object('partyId',m.party_id,'start',m.start_offset,'end',m.end_offset) ORDER BY m.start_offset) value
   FROM interaction_comment_mention m JOIN authors a ON a.party_id=m.party_id WHERE m.comment_id=ANY(comment_keys) GROUP BY m.comment_id),
 replies AS (SELECT t.root_id,sum(t.comments-t.roots) total FROM interaction_comment_total t JOIN authors a ON a.party_id=t.author_id
   WHERE t.root_id=ANY(comment_keys) GROUP BY t.root_id)
 SELECT c.id,jsonb_build_object('id',c.id,'targetId',c.target_id,'parentId',c.parent_id,'rootId',c.root_id,
   'depth',least(c.depth,2),'version',c.version,'createdAt',c.created_at,'editedAt',c.edited_at,
   'state',CASE WHEN c.state<>'visible' THEN c.state WHEN a.author IS NULL THEN 'unavailable' ELSE 'visible' END,
   'body',CASE WHEN c.state='visible' AND a.author IS NOT NULL THEN c.body ELSE '' END,
   'author',CASE WHEN c.state='visible' THEN a.author ELSE NULL END,
   'legacyPresentation',CASE WHEN c.state='visible' AND a.author IS NOT NULL THEN interaction_legacy_presentation(legacy.source_value) END,
   'canEdit',coalesce(c.state='visible' AND viewer=c.author_id,false),'canDelete',coalesce(c.state IN ('visible','hidden') AND viewer=c.author_id,false),
   'mentions',CASE WHEN c.state='visible' AND a.author IS NOT NULL THEN coalesce(m.value,'[]'::jsonb) ELSE '[]'::jsonb END,
   'replyCount',coalesce(r.total,0))
 FROM selected c LEFT JOIN interaction_legacy_mapping legacy ON legacy.comment_id=c.id AND legacy.legacy_kind='club_reply' LEFT JOIN authors a ON a.party_id=c.author_id LEFT JOIN mentions m ON m.comment_id=c.id LEFT JOIN replies r ON r.root_id=c.id
$$;
CREATE OR REPLACE FUNCTION interaction_comments_page(target uuid,actor bigint,thread uuid,
 order_code text,before_id uuid,page_size integer) RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE pivot interaction_comment%ROWTYPE; page_keys uuid[]; items jsonb; next_id uuid; author_ids bigint[];
BEGIN
 IF interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 OR order_code IS NULL OR order_code NOT IN ('newest','oldest','relevant') THEN RETURN '{"error":"invalid"}'; END IF;
 IF order_code='relevant' THEN order_code:='newest'; END IF;
 IF thread IS NOT NULL AND NOT EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=thread AND c.target_id=target AND c.parent_id IS NULL)
 THEN RETURN '{"error":"unavailable"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE id=before_id AND target_id=target
     AND CASE WHEN thread IS NULL THEN parent_id IS NULL ELSE root_id=thread AND parent_id IS NOT NULL END;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 SELECT array_agg(party_id) INTO author_ids FROM interaction_author_batch(actor,ARRAY(
   SELECT author_id FROM interaction_target_comment_total WHERE target_id=target AND comments>0));
 SELECT array_agg(id) INTO page_keys FROM (
   SELECT c.id FROM interaction_comment c WHERE c.target_id=target
     AND CASE WHEN thread IS NULL THEN c.parent_id IS NULL ELSE c.root_id=thread AND c.parent_id IS NOT NULL END
     AND ((c.state='visible' AND c.author_id=ANY(author_ids)) OR (c.parent_id IS NULL AND EXISTS(
       SELECT 1 FROM interaction_comment_total t WHERE t.target_id=target AND t.root_id=c.root_id AND t.comments>t.roots AND t.author_id=ANY(author_ids))))
     AND (before_id IS NULL OR CASE WHEN order_code='oldest' THEN (c.created_at,c.id)>(pivot.created_at,pivot.id) ELSE (c.created_at,c.id)<(pivot.created_at,pivot.id) END)
   ORDER BY CASE WHEN order_code='oldest' THEN c.created_at END ASC,CASE WHEN order_code='oldest' THEN c.id END ASC,
     CASE WHEN order_code='newest' THEN c.created_at END DESC,CASE WHEN order_code='newest' THEN c.id END DESC LIMIT page_size+1
 ) page;
 IF array_length(page_keys,1)>page_size THEN next_id:=page_keys[page_size]; END IF;
 SELECT coalesce(jsonb_agg(value ORDER BY array_position(page_keys,comment_id)),'[]'::jsonb) INTO items
 FROM interaction_comments_json(page_keys[1:page_size],actor);
 RETURN jsonb_build_object('items',items,'nextCursor',next_id,'sort',order_code);
END $$;
CREATE OR REPLACE FUNCTION interaction_summary(actor bigint,kind text,entity text)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; t interaction_target%ROWTYPE; context jsonb; choices jsonb;
 total_comments bigint; total_roots bigint; selected uuid; mode_value text; author_ids bigint[];
BEGIN
 target:=interaction_register(kind,entity,actor);
 IF target IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO t FROM interaction_target WHERE id=target;
 context:=interaction_target_context(target,actor);
 SELECT array_agg(party_id) INTO author_ids FROM interaction_author_batch(actor,ARRAY(
   SELECT author_id FROM interaction_target_comment_total WHERE target_id=target AND comments>0
   UNION SELECT actor_id FROM interaction_reaction WHERE target_id=target));
 SELECT coalesce(sum(c.comments),0),coalesce(sum(c.roots),0) INTO total_comments,total_roots
 FROM interaction_target_comment_total c WHERE c.target_id=target AND c.author_id=ANY(author_ids);
 WITH counts AS (
   SELECT reaction_type_id,count(*) total FROM interaction_reaction WHERE target_id=target AND actor_id=ANY(author_ids) GROUP BY reaction_type_id
 ) SELECT coalesce(jsonb_agg(jsonb_build_object('id',r.id,'code',r.code,'emoji',r.emoji,'label',r.name_es,'count',coalesce(c.total,0),
   'selectable',choice.reaction_type_id IS NOT NULL AND r.active AND r.deprecated_at IS NULL AND w.active AND w.code='published')
     ORDER BY choice.default_order NULLS LAST,r.sort_order),'[]'::jsonb)
 INTO choices FROM content_reaction_type r JOIN workflow_state w ON w.id=r.workflow_state_id
 LEFT JOIN interaction_reaction_choice choice ON choice.reaction_type_id=r.id LEFT JOIN counts c ON c.reaction_type_id=r.id
 WHERE (choice.reaction_type_id IS NOT NULL AND r.active AND r.deprecated_at IS NULL AND w.active AND w.code='published') OR c.total>0;
 SELECT reaction_type_id INTO selected FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
 SELECT mode INTO mode_value FROM interaction_subscription WHERE target_id=target AND party_id=actor;
 RETURN context||jsonb_build_object('id',target,'version',t.version,'commentPolicy',t.comment_policy,
   'canReact',actor IS NOT NULL AND interaction_domain_write(target,actor) AND (context->>'reactable')::boolean,'canComment',coalesce(interaction_can_comment(target,actor),false),
   'mentionedPeople',CASE WHEN (context->>'canManage')::boolean THEN coalesce((SELECT jsonb_agg(a.author ORDER BY a.party_id)
     FROM interaction_author_batch(actor,ARRAY(SELECT party_id FROM interaction_target_mention WHERE target_id=target)) a),'[]'::jsonb) ELSE '[]'::jsonb END,
   'canModerate',coalesce(interaction_is_moderator(actor),false),'commentCount',total_comments,'rootCount',total_roots,
   'reactions',choices,'myReactionTypeId',selected,'subscription',coalesce(mode_value,'participating'),'defaultSort','newest');
END $$;
CREATE OR REPLACE FUNCTION interaction_comment_context(target uuid,actor bigint,comment_key uuid)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE c interaction_comment%ROWTYPE; context jsonb; keys uuid[]; authors bigint[]; mapped jsonb;
BEGIN
 context:=interaction_target_context(target,actor);
 IF context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO c FROM interaction_comment WHERE id=comment_key AND target_id=target;
 IF NOT FOUND OR interaction_blocked(actor,c.author_id)
   OR (c.state IN ('hidden','removed') AND actor IS DISTINCT FROM c.author_id
     AND NOT coalesce((context->>'canManage')::boolean,false) AND NOT interaction_is_moderator(actor))
 THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT array_agg(party_id) INTO authors FROM interaction_author_batch(actor,ARRAY(
   SELECT author_id FROM interaction_comment_total WHERE target_id=target AND root_id=c.root_id AND comments>0));
 SELECT array_agg(id) INTO keys FROM (SELECT x.id FROM interaction_comment x
   WHERE x.target_id=target AND x.root_id=c.root_id AND x.state='visible' AND x.author_id=ANY(authors)
     AND (x.created_at,x.id)>=(c.created_at,c.id) ORDER BY x.created_at,x.id LIMIT 11) page;
 SELECT jsonb_object_agg(comment_id::text,value) INTO mapped
 FROM interaction_comments_json(coalesce(keys,'{}'::uuid[])||ARRAY[c.id,c.root_id,c.parent_id],actor);
 RETURN jsonb_build_object('target',context,'root',mapped->c.root_id::text,'comment',mapped->c.id::text,
   'parent',mapped->c.parent_id::text,'surrounding',coalesce((SELECT jsonb_agg(mapped->key::text ORDER BY ordinal)
     FROM unnest(keys) WITH ORDINALITY k(key,ordinal)),'[]'::jsonb));
END $$;
COMMIT;
