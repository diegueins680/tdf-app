BEGIN;
CREATE OR REPLACE FUNCTION interaction_summary(actor bigint,kind text,entity text)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; t interaction_target%ROWTYPE; context jsonb; choices jsonb;
 total_comments bigint; total_roots bigint; selected uuid; mode_value text;
BEGIN
 target:=interaction_register(kind,entity,actor);
 IF target IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO t FROM interaction_target WHERE id=target;
 context:=interaction_target_context(target,actor);
 -- Single grouped aggregate, independent of comment bodies/tree size. The
 -- performance fixture inspects these plans with large synthetic discussions.
 SELECT coalesce(sum(c.comments),0),coalesce(sum(c.roots),0) INTO total_comments,total_roots
 FROM interaction_comment_total c WHERE c.target_id=target
   AND interaction_actor_live(c.author_id) AND NOT interaction_blocked(actor,c.author_id);
 WITH counts AS (
   SELECT r.reaction_type_id,count(*) total FROM interaction_reaction r WHERE r.target_id=target
     AND interaction_actor_live(r.actor_id) AND NOT interaction_blocked(actor,r.actor_id)
   GROUP BY r.reaction_type_id
 ) SELECT coalesce(jsonb_agg(jsonb_build_object('id',r.id,'code',r.code,'emoji',r.emoji,
     'label',r.name_es,'count',coalesce(c.total,0),'selectable',choice.reaction_type_id IS NOT NULL)
     ORDER BY choice.default_order NULLS LAST,r.sort_order),'[]'::jsonb)
 INTO choices FROM content_reaction_type r
 LEFT JOIN interaction_reaction_choice choice ON choice.reaction_type_id=r.id
 LEFT JOIN counts c ON c.reaction_type_id=r.id
 WHERE (choice.reaction_type_id IS NOT NULL AND r.active AND r.deprecated_at IS NULL) OR c.total>0;
 SELECT reaction_type_id INTO selected FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
 SELECT mode INTO mode_value FROM interaction_subscription WHERE target_id=target AND party_id=actor;
 RETURN context||jsonb_build_object('id',target,'version',t.version,'commentPolicy',t.comment_policy,
   'canReact',actor IS NOT NULL AND interaction_domain_write(target,actor) AND (context->>'reactable')::boolean,
   'canComment',coalesce(interaction_can_comment(target,actor),false),
   'canModerate',coalesce(interaction_is_moderator(actor),false),'commentCount',total_comments,
   'rootCount',total_roots,'reactions',choices,'myReactionTypeId',selected,
   'subscription',coalesce(mode_value,'participating'),'defaultSort','newest');
END $$;
CREATE OR REPLACE FUNCTION interaction_reactors(target uuid,actor bigint,after_actor bigint,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE items jsonb; cursor_value bigint;
BEGIN
 IF actor IS NULL OR interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 OR after_actor<=0 THEN RETURN '{"error":"invalid"}'; END IF;
 WITH page AS MATERIALIZED (
   SELECT r.actor_id,r.reaction_type_id FROM interaction_reaction r WHERE r.target_id=target
     AND (after_actor IS NULL OR r.actor_id>after_actor)
     AND interaction_actor_live(r.actor_id) AND NOT interaction_blocked(actor,r.actor_id)
     -- Reveal only identities explicitly discoverable, already connected, or self.
     AND (r.actor_id=actor OR EXISTS(SELECT 1 FROM social_v2_preference p WHERE p.party_id=r.actor_id AND p.discoverable)
       OR EXISTS(SELECT 1 FROM social_v2_pair p WHERE p.party_a=least(actor,r.actor_id)
         AND p.party_b=greatest(actor,r.actor_id) AND p.consent_a AND p.consent_b))
   ORDER BY r.actor_id LIMIT page_size+1
 ), numbered AS (SELECT p.*,row_number() OVER(ORDER BY p.actor_id) ordinal FROM page p)
 SELECT coalesce(jsonb_agg(jsonb_build_object('author',interaction_public_author(actor,n.actor_id),
   'reactionTypeId',n.reaction_type_id) ORDER BY n.actor_id) FILTER(WHERE ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(actor_id ORDER BY actor_id))[page_size] END
 INTO items,cursor_value FROM numbered n;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
COMMIT;
