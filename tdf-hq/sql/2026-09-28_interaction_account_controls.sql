BEGIN;
SET LOCAL lock_timeout='5s';
CREATE OR REPLACE FUNCTION interaction_block_list(actor bigint,after_party bigint,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE items jsonb; cursor_value bigint;
BEGIN
 IF NOT interaction_actor_live(actor) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 OR after_party<0 THEN RETURN '{"error":"invalid"}'; END IF;
 WITH blocked AS (
   SELECT CASE WHEN party_a=actor THEN party_b ELSE party_a END peer,revision FROM social_v2_pair
   WHERE (party_a=actor AND block_a) OR (party_b=actor AND block_b)
 ), page AS MATERIALIZED (
   SELECT b.peer,b.revision,p.display_name FROM blocked b JOIN party p ON p.id=b.peer
   WHERE b.peer>coalesce(after_party,0) ORDER BY b.peer LIMIT page_size+1
 ), numbered AS (SELECT *,row_number() OVER(ORDER BY peer) ordinal FROM page)
 SELECT coalesce(jsonb_agg(jsonb_build_object('partyId',peer,'displayName',display_name,'version',revision,'blocked',true)
   ORDER BY peer) FILTER(WHERE ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(peer ORDER BY peer))[page_size] END INTO items,cursor_value FROM numbered;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
CREATE OR REPLACE FUNCTION interaction_report_inbox(actor bigint,before_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE pivot interaction_comment%ROWTYPE; keys uuid[]; items jsonb; cursor_value uuid;
BEGIN
 IF NOT interaction_is_moderator(actor) THEN RETURN '{"error":"forbidden"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE id=before_id;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 SELECT array_agg(id) INTO keys FROM (
   SELECT c.id FROM interaction_comment c WHERE EXISTS(SELECT 1 FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open')
     AND interaction_target_context(c.target_id,actor) IS NOT NULL AND NOT interaction_blocked(actor,c.author_id)
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ) page;
 IF array_length(keys,1)>page_size THEN cursor_value:=keys[page_size]; END IF;
 SELECT coalesce(jsonb_agg(j.value||jsonb_build_object('moderationBody',CASE WHEN c.state IN ('visible','hidden') THEN c.body ELSE '' END,
   'openReports',(SELECT count(*) FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open'))
   ORDER BY array_position(keys,c.id)),'[]'::jsonb) INTO items FROM interaction_comments_json(keys[1:page_size],actor) j
 JOIN interaction_comment c ON c.id=j.comment_id;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
COMMIT;
