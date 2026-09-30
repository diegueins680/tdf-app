BEGIN;
SET LOCAL lock_timeout='5s';
-- One bounded aggregate per author and root. Allows excluding blocked identities
-- using indexed aggregates instead of rescanning all comment text or full trees.
CREATE TABLE IF NOT EXISTS interaction_comment_total (
 target_id uuid NOT NULL REFERENCES interaction_target(id),
 root_id uuid NOT NULL REFERENCES interaction_comment(id),
 author_id bigint NOT NULL REFERENCES party(id),
 comments bigint NOT NULL DEFAULT 0 CHECK(comments>=0),
 roots bigint NOT NULL DEFAULT 0 CHECK(roots BETWEEN 0 AND 1),
 PRIMARY KEY(target_id,root_id,author_id)
);
CREATE INDEX IF NOT EXISTS interaction_comment_total_actor ON interaction_comment_total(author_id,target_id);
CREATE OR REPLACE FUNCTION interaction_count_comment() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_OP IN ('UPDATE','DELETE') AND OLD.state='visible' AND OLD.author_id IS NOT NULL THEN
   UPDATE interaction_comment_total SET comments=comments-1,roots=roots-CASE WHEN OLD.parent_id IS NULL THEN 1 ELSE 0 END
   WHERE target_id=OLD.target_id AND root_id=OLD.root_id AND author_id=OLD.author_id;
   IF NOT FOUND THEN RAISE EXCEPTION 'Missing comment counter' USING ERRCODE='23514'; END IF;
 END IF;
 IF TG_OP IN ('INSERT','UPDATE') AND NEW.state='visible' AND NEW.author_id IS NOT NULL THEN
   INSERT INTO interaction_comment_total(target_id,root_id,author_id,comments,roots)
   VALUES(NEW.target_id,NEW.root_id,NEW.author_id,1,CASE WHEN NEW.parent_id IS NULL THEN 1 ELSE 0 END)
   ON CONFLICT(target_id,root_id,author_id) DO UPDATE SET
     comments=interaction_comment_total.comments+1,roots=interaction_comment_total.roots+excluded.roots;
 END IF;
 RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS interaction_comment_count ON interaction_comment;
CREATE TRIGGER interaction_comment_count AFTER INSERT OR UPDATE OF state OR DELETE ON interaction_comment
FOR EACH ROW EXECUTE FUNCTION interaction_count_comment();

CREATE OR REPLACE FUNCTION interaction_public_author(actor bigint,author bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object('id',p.id,'displayName',coalesce(nullif(btrim(f.display_name),''),p.display_name),
   'avatarUrl',f.avatar_url)
 FROM party p LEFT JOIN fan_profile f ON f.fan_party_id=p.id
 WHERE p.id=author AND interaction_actor_live(author) AND NOT interaction_blocked(actor,author)
$$;
CREATE OR REPLACE FUNCTION interaction_legacy_presentation(source_value jsonb) RETURNS jsonb
LANGUAGE sql IMMUTABLE AS $$
 SELECT CASE WHEN source_value IS NULL THEN NULL ELSE jsonb_build_object('title',source_value->'title','mediaUrls',
   CASE jsonb_typeof(source_value->'mediaUrls') WHEN 'array' THEN source_value->'mediaUrls'
     WHEN 'string' THEN to_jsonb(string_to_array(source_value->>'mediaUrls',',')) ELSE '[]'::jsonb END) END
$$;
CREATE OR REPLACE FUNCTION interaction_comment_json(c interaction_comment,actor bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 WITH author AS (SELECT interaction_public_author(actor,c.author_id) value)
 SELECT jsonb_build_object('id',c.id,'targetId',c.target_id,'parentId',c.parent_id,'rootId',c.root_id,
   'depth',least(c.depth,2),'version',c.version,'createdAt',c.created_at,'editedAt',c.edited_at,
   'state',CASE WHEN c.state<>'visible' THEN c.state WHEN author.value IS NULL THEN 'unavailable' ELSE 'visible' END,
   'body',CASE WHEN c.state='visible' AND author.value IS NOT NULL THEN c.body ELSE '' END,
   'author',CASE WHEN c.state='visible' THEN author.value ELSE NULL END,
   'legacyPresentation',CASE WHEN c.state='visible' AND author.value IS NOT NULL THEN (SELECT interaction_legacy_presentation(m.source_value) FROM interaction_legacy_mapping m WHERE m.comment_id=c.id AND m.legacy_kind='club_reply') END,
   'canEdit',c.state='visible' AND actor=c.author_id,
   'canDelete',c.state IN ('visible','hidden') AND actor=c.author_id,
   'mentions',CASE WHEN c.state='visible' AND author.value IS NOT NULL THEN
     coalesce((SELECT jsonb_agg(jsonb_build_object('partyId',m.party_id,'start',m.start_offset,'end',m.end_offset)
       ORDER BY m.start_offset) FROM interaction_comment_mention m
       WHERE m.comment_id=c.id AND interaction_public_author(actor,m.party_id) IS NOT NULL),'[]'::jsonb)
     ELSE '[]'::jsonb END)
 FROM author
$$;
CREATE OR REPLACE FUNCTION interaction_comment_readable(c interaction_comment,actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT (c.state='visible' AND interaction_actor_live(c.author_id) AND NOT interaction_blocked(actor,c.author_id))
   OR (c.state='deleted' AND EXISTS(SELECT 1 FROM interaction_comment_total t
     WHERE t.target_id=c.target_id AND t.root_id=c.root_id AND t.comments>t.roots
       AND interaction_actor_live(t.author_id) AND NOT interaction_blocked(actor,t.author_id)))
$$;
CREATE OR REPLACE FUNCTION interaction_comments_page(target uuid,actor bigint,thread uuid,
 order_code text,before_id uuid,page_size integer) RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE pivot interaction_comment%ROWTYPE; items jsonb; next_id uuid;
BEGIN
 IF interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 OR order_code IS NULL OR order_code NOT IN ('newest','oldest','relevant')
   THEN RETURN '{"error":"invalid"}'; END IF;
 IF order_code='relevant' THEN order_code:='newest'; END IF;
 IF thread IS NOT NULL AND NOT EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=thread
   AND c.target_id=target AND c.parent_id IS NULL) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE id=before_id AND target_id=target
     AND CASE WHEN thread IS NULL THEN parent_id IS NULL ELSE root_id=thread AND parent_id IS NOT NULL END;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 WITH page AS MATERIALIZED (
   SELECT c.* FROM interaction_comment c WHERE c.target_id=target
     AND CASE WHEN thread IS NULL THEN c.parent_id IS NULL ELSE c.root_id=thread AND c.parent_id IS NOT NULL END
     AND interaction_comment_readable(c,actor)
     AND (before_id IS NULL OR CASE WHEN order_code='oldest'
       THEN (c.created_at,c.id)>(pivot.created_at,pivot.id) ELSE (c.created_at,c.id)<(pivot.created_at,pivot.id) END)
   ORDER BY CASE WHEN order_code='oldest' THEN c.created_at END ASC,
     CASE WHEN order_code='oldest' THEN c.id END ASC,
     CASE WHEN order_code='newest' THEN c.created_at END DESC,
     CASE WHEN order_code='newest' THEN c.id END DESC LIMIT page_size+1
 ), numbered AS (SELECT p.*,row_number() OVER () ordinal FROM page p)
 SELECT coalesce(jsonb_agg(interaction_comment_json(c,actor)||jsonb_build_object('replyCount',
   coalesce((SELECT sum(t.comments-t.roots) FROM interaction_comment_total t WHERE t.target_id=target
     AND t.root_id=c.id AND interaction_actor_live(t.author_id) AND NOT interaction_blocked(actor,t.author_id)),0))
   ORDER BY n.ordinal) FILTER(WHERE n.ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(n.id ORDER BY n.ordinal))[page_size] END
 INTO items,next_id FROM numbered n JOIN interaction_comment c ON c.id=n.id;
 RETURN jsonb_build_object('items',items,'nextCursor',next_id,'sort',order_code);
END $$;

CREATE OR REPLACE FUNCTION interaction_comment_context(target uuid,actor bigint,comment_key uuid)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE c interaction_comment%ROWTYPE; root interaction_comment%ROWTYPE;
BEGIN
 IF interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO c FROM interaction_comment WHERE id=comment_key AND target_id=target;
 IF NOT FOUND OR interaction_blocked(actor,c.author_id)
   OR (c.state IN ('hidden','removed') AND actor IS DISTINCT FROM c.author_id
     AND NOT coalesce((interaction_target_context(target,actor)->>'canManage')::boolean,false)
     AND NOT interaction_is_moderator(actor))
   THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO root FROM interaction_comment WHERE id=c.root_id AND target_id=target;
 RETURN jsonb_build_object('target',interaction_target_context(target,actor),'root',interaction_comment_json(root,actor),
   'comment',interaction_comment_json(c,actor),
   'parent',CASE WHEN c.parent_id IS NOT NULL THEN (SELECT interaction_comment_json(p,actor)
      FROM interaction_comment p WHERE p.id=c.parent_id AND p.target_id=target) ELSE NULL END,
   'surrounding',coalesce((SELECT jsonb_agg(interaction_comment_json(p,actor) ORDER BY p.created_at,p.id)
     FROM (SELECT x.* FROM interaction_comment x WHERE x.target_id=target AND x.root_id=c.root_id
       AND interaction_comment_readable(x,actor) AND (x.created_at,x.id)>=(c.created_at,c.id)
       ORDER BY x.created_at,x.id LIMIT 11) p),'[]'::jsonb));
END $$;
COMMIT;
