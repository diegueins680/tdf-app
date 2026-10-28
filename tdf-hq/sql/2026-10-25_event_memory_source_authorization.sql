-- Forward-only repair: a missing organizer must never grant review/enforcement.
-- Preserve migration 215, function OIDs, all content, canonical reports and audit.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL search_path=public,pg_catalog;
DO $guard$ DECLARE item record; installed pg_proc%ROWTYPE; BEGIN
 FOR item IN SELECT * FROM (VALUES
 ('public.event_memory_source_context(uuid,bigint)','e5b7c716f31f2bc8f9a91579eac83a98','8c616f3e4848cbcf20ed5065feba2306','s'),
 ('public.event_memory_source_reports(bigint,bigint,uuid,integer)','e3bce1a9fe8462c122c48cdd1890cf19','ada2bda3951bc62b0fdde2ac13cd0e57','s'),
 ('public.event_memory_source_command(bigint,uuid,uuid,jsonb)','7b59334fd8ace45b18d8410e7726fccf','39a34f6efbb2843c9a3ffeabab7fd219','v'),
 ('public.interaction_command(bigint,uuid,uuid,jsonb)','ccc976b3dcea7fb5ead882956eecf2d0','7d33d28fc9379c7b83328f0ca0397eeb','v')
 ) expected(signature,before_digest,after_digest,volatility) LOOP
  SELECT * INTO STRICT installed FROM pg_proc WHERE oid=item.signature::regprocedure;
  IF installed.prosecdef OR installed.proconfig IS NOT NULL
   OR installed.prolang<>(SELECT oid FROM pg_language WHERE lanname='plpgsql')
   OR installed.provolatile::text<>item.volatility OR installed.prorettype<>'jsonb'::regtype
   OR md5(installed.prosrc) NOT IN(item.before_digest,item.after_digest) THEN
   RAISE EXCEPTION 'Unexpected source authorization successor: %',item.signature;
  END IF;
 END LOOP;
END $guard$;

CREATE OR REPLACE FUNCTION public.event_memory_source_context(target uuid,actor bigint) RETURNS jsonb
LANGUAGE plpgsql STABLE AS $$
DECLARE m event_moment%ROWTYPE; e social_event%ROWTYPE; managing boolean; moderating boolean; reading boolean;
BEGIN
 IF actor IS NULL OR NOT coalesce(interaction_actor_live(actor),false) THEN RETURN NULL; END IF;
 SELECT source.* INTO m FROM interaction_target t JOIN event_moment source ON source.id::text=t.entity_key
  WHERE t.id=target AND t.entity_kind='event_moment' AND t.retired_at IS NULL;
 IF NOT FOUND THEN RETURN NULL; END IF;
 SELECT * INTO STRICT e FROM social_event WHERE id=m.event_id;
 moderating:=coalesce(interaction_is_moderator(actor),false);
 IF interaction_event_access_scoped(e.id,actor,moderating) IS NOT TRUE THEN RETURN NULL; END IF;
 managing:=coalesce(e.organizer_party_id=actor::text,false);
 reading:=interaction_resolve('event_moment',m.id::text,actor) IS NOT NULL;
 IF (managing OR moderating OR reading) IS NOT TRUE THEN RETURN NULL; END IF;
 RETURN jsonb_build_object('supported',true,'eventId',e.id::text,'state',m.source_moderation_state,
  'sourceRevision',extract(epoch FROM m.updated_at)::text,
  'canReport',reading AND m.author_party_id IS DISTINCT FROM actor::text,
  'canHide',(managing OR moderating) AND m.source_moderation_state='visible',
  'canRestore',(managing AND m.source_moderation_state='hidden') OR (moderating AND m.source_moderation_state<>'visible'),
  'canRemove',moderating AND m.source_moderation_state<>'removed');
END $$;

CREATE OR REPLACE FUNCTION public.event_memory_source_reports(actor bigint,event_key bigint,after_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE can_review boolean; items jsonb; next_id uuid; parent social_event%ROWTYPE;
BEGIN
 IF actor IS NULL OR NOT coalesce(interaction_actor_live(actor),false) OR interaction_event_access_scoped(event_key,actor,coalesce(interaction_is_moderator(actor),false)) IS NOT TRUE THEN
  RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO STRICT parent FROM social_event WHERE id=event_key;
 can_review:=coalesce(parent.organizer_party_id=actor::text,false) OR coalesce(interaction_is_moderator(actor),false);
 IF can_review IS NOT TRUE THEN RETURN '{"supported":true,"canReview":false,"items":[],"nextCursor":null}'; END IF;
 IF page_size IS NULL OR page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF after_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM interaction_target t JOIN event_moment m ON m.id::text=t.entity_key
  WHERE t.id=after_id AND t.entity_kind='event_moment' AND m.event_id=event_key) THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 WITH candidates AS (
  SELECT t.id FROM interaction_report r JOIN interaction_target t ON t.id=r.target_id
   JOIN event_moment m ON m.id::text=t.entity_key
   WHERE r.comment_id IS NULL AND r.state='open' AND t.entity_kind='event_moment' AND m.event_id=event_key
  UNION SELECT t.id FROM event_moment m JOIN interaction_target t ON t.entity_key=m.id::text AND t.entity_kind='event_moment'
   WHERE m.event_id=event_key AND m.source_moderation_state<>'visible'
 ), selected AS MATERIALIZED (
  SELECT t.*,event_memory_source_context(t.id,actor) AS context FROM candidates c JOIN interaction_target t ON t.id=c.id
  WHERE t.retired_at IS NULL AND (after_id IS NULL OR t.id>after_id) ORDER BY t.id LIMIT page_size+1
 ), numbered AS (SELECT *,row_number() OVER(ORDER BY id) AS ordinal FROM selected)
 SELECT coalesce(jsonb_agg(jsonb_build_object('id',n.id,'entityKey',m.id::text,'eventId',event_key::text,
  'version',n.version,'source',n.context,'caption',m.caption,'mediaType',m.media_type,'mediaUrl',m.media_url,
  'author',CASE WHEN m.author_party_id ~ '^[1-9][0-9]{0,17}$' THEN interaction_public_author(actor,m.author_party_id::bigint) END,
  'openReports',(SELECT count(*) FROM interaction_report r WHERE r.target_id=n.id AND r.comment_id IS NULL AND r.state='open'),
  'reportsReferToEarlierContent',EXISTS(SELECT 1 FROM interaction_report r WHERE r.target_id=n.id AND r.comment_id IS NULL AND r.state='open' AND r.reported_version<m.source_content_version),
  'reasons',coalesce((SELECT jsonb_agg(x.reason ORDER BY x.created_at,x.id) FROM
   (SELECT r.id,r.reason,r.created_at FROM interaction_report r WHERE r.target_id=n.id AND r.comment_id IS NULL AND r.state='open'
    ORDER BY r.created_at,r.id LIMIT 20) x),'[]'::jsonb),
  'audit',coalesce((SELECT jsonb_agg(jsonb_build_object('operation',a.operation,'reason',a.reason,'previousState',a.previous_state,
    'newState',a.new_state,'createdAt',a.created_at) ORDER BY a.id DESC) FROM
    (SELECT * FROM interaction_audit WHERE target_id=n.id AND comment_id IS NULL AND operation LIKE 'source.%' ORDER BY id DESC LIMIT 5) a),'[]'::jsonb)
  ) ORDER BY n.id) FILTER(WHERE ordinal<=page_size AND n.context IS NOT NULL),'[]'::jsonb),
  CASE WHEN count(*)>page_size THEN (array_agg(n.id ORDER BY n.id))[page_size] END INTO items,next_id
 FROM numbered n JOIN event_moment m ON m.id::text=n.entity_key;
 RETURN jsonb_build_object('supported',true,'canReview',true,'items',items,'nextCursor',next_id);
END $$;

CREATE OR REPLACE FUNCTION public.event_memory_source_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE t interaction_target%ROWTYPE; m event_moment%ROWTYPE; initial_event bigint; context jsonb;
 op text:=payload->>'operation'; allowed text[]; reason_value text; fingerprint text; prior interaction_request%ROWTYPE;
 next_state text; result_value jsonb; parent social_event%ROWTYPE; inserted_report uuid;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled AND activated_once) THEN RETURN '{"error":"disabled"}'; END IF;
 IF actor IS NULL OR request_id IS NULL OR jsonb_typeof(payload) IS DISTINCT FROM 'object'
  OR pg_column_size(payload)>32768 OR interaction_actor_live(actor) IS NOT TRUE THEN RETURN '{"error":"invalid"}'; END IF;
 allowed:=CASE op WHEN 'source.report' THEN ARRAY['operation','reason','expectedSourceRevision']
  WHEN 'source.hide' THEN ARRAY['operation','reason','expectedVersion','expectedSourceRevision']
  WHEN 'source.remove' THEN ARRAY['operation','reason','expectedVersion','expectedSourceRevision']
  WHEN 'source.restore' THEN ARRAY['operation','reason','expectedVersion','expectedSourceRevision'] END;
 IF op='source.report.resolve' THEN allowed:=ARRAY['operation','reason','expectedVersion','expectedSourceRevision','decision']; END IF;
 IF allowed IS NULL OR payload-allowed<>'{}'::jsonb OR jsonb_typeof(payload->'reason') IS DISTINCT FROM 'string'
  OR length(btrim(payload->>'reason')) NOT BETWEEN 1 AND 1000
  OR payload->>'reason' ~ '[[:cntrl:]]' OR jsonb_typeof(payload->'expectedSourceRevision') IS DISTINCT FROM 'string' THEN RETURN '{"error":"invalid"}'; END IF;
 reason_value:=btrim(payload->>'reason');
 SELECT source.event_id INTO initial_event FROM interaction_target it JOIN event_moment source ON source.id::text=it.entity_key
  WHERE it.id=target AND it.entity_kind='event_moment' AND it.retired_at IS NULL;
 IF NOT FOUND THEN RETURN '{"error":"unavailable"}'; END IF;
 PERFORM id FROM party WHERE id IN (actor,(SELECT organizer_party_id::bigint FROM social_event
  WHERE id=initial_event AND organizer_party_id ~ '^[1-9][0-9]{0,17}$')) ORDER BY id FOR SHARE;
 SELECT * INTO parent FROM social_event WHERE id=initial_event FOR SHARE;
 SELECT source.* INTO m FROM interaction_target it JOIN event_moment source ON source.id::text=it.entity_key
  WHERE it.id=target AND it.entity_kind='event_moment' AND source.event_id=initial_event FOR UPDATE OF source;
 IF NOT FOUND THEN RETURN '{"error":"unavailable"}'; END IF;
 PERFORM pg_advisory_xact_lock(hashtextextended('interaction-actor:'||actor,0));
 SELECT * INTO t FROM interaction_target WHERE id=target AND retired_at IS NULL FOR UPDATE;
 IF NOT FOUND THEN RETURN '{"error":"unavailable"}'; END IF;
 context:=event_memory_source_context(target,actor);
 IF context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 -- Replays still require live authority, but not the original pre-mutation
 -- state (a successful restore itself makes canRestore false).
 IF (op='source.report' AND (context->>'canReport')::boolean IS NOT TRUE)
  OR (op='source.remove' AND interaction_is_moderator(actor) IS NOT TRUE)
  OR (op IN ('source.hide','source.restore','source.report.resolve') AND
   (coalesce(parent.organizer_party_id=actor::text,false) OR coalesce(interaction_is_moderator(actor),false)) IS NOT TRUE)
  OR (op IN ('source.hide','source.restore') AND m.source_moderation_state='removed' AND interaction_is_moderator(actor) IS NOT TRUE)
  THEN RETURN '{"error":"forbidden"}'; END IF;
 fingerprint:=encode(digest(payload::text,'sha256'),'hex');
 SELECT * INTO prior FROM interaction_request WHERE actor_id=actor AND request_key=request_id;
 IF FOUND THEN
  IF prior.target_id<>target OR prior.payload_hash<>fingerprint THEN RETURN '{"error":"request_key_conflict"}'; END IF;
  IF prior.result->>'_sourceContentVersion' IS DISTINCT FROM m.source_content_version::text
   OR (op<>'source.report' AND prior.result->>'state' IS DISTINCT FROM m.source_moderation_state) THEN
   RETURN '{"error":"revision_conflict"}';
  END IF;
  RETURN (prior.result-'_sourceContentVersion')||CASE WHEN op='source.report' THEN '{}'::jsonb ELSE jsonb_build_object('version',t.version) END;
 END IF;
 IF context->>'sourceRevision' IS DISTINCT FROM payload->>'expectedSourceRevision' THEN RETURN '{"error":"revision_conflict"}'; END IF;
 IF (op='source.hide' AND (context->>'canHide')::boolean IS NOT TRUE)
  OR (op='source.remove' AND (context->>'canRemove')::boolean IS NOT TRUE)
  OR (op='source.restore' AND (context->>'canRestore')::boolean IS NOT TRUE) THEN RETURN '{"error":"revision_conflict"}'; END IF;
 IF op='source.report' THEN
  INSERT INTO interaction_report(target_id,comment_id,reporter_id,reason,reported_version) VALUES(target,NULL,actor,reason_value,m.source_content_version)
   ON CONFLICT(target_id,reporter_id) WHERE comment_id IS NULL DO UPDATE SET
    state='open',reason=excluded.reason,created_at=clock_timestamp(),reported_version=excluded.reported_version
    WHERE interaction_report.state<>'open' AND interaction_report.reported_version<excluded.reported_version
    RETURNING id INTO inserted_report;
  IF inserted_report IS NOT NULL THEN
   INSERT INTO interaction_audit(target_id,actor_id,operation,reason,new_state) VALUES(target,actor,op,reason_value,'open');
  END IF;
  result_value:='{"reported":true}';
 ELSE
  IF coalesce(payload->>'expectedVersion','') !~ '^[1-9][0-9]{0,17}$' THEN RETURN '{"error":"invalid"}'; END IF;
  IF (payload->>'expectedVersion')::bigint<>t.version THEN RETURN '{"error":"revision_conflict"}'; END IF;
  IF op='source.report.resolve' THEN
   IF coalesce(payload->>'decision','') NOT IN ('reviewed','dismissed') THEN RETURN '{"error":"invalid"}'; END IF;
   UPDATE interaction_report SET state=payload->>'decision',reported_version=m.source_content_version
    WHERE target_id=target AND comment_id IS NULL AND state='open';
   IF NOT FOUND THEN RETURN '{"error":"revision_conflict"}'; END IF;
   next_state:=m.source_moderation_state;
  ELSE
   next_state:=CASE op WHEN 'source.hide' THEN 'hidden' WHEN 'source.remove' THEN 'removed' ELSE 'visible' END;
   UPDATE event_moment SET source_moderation_state=next_state,updated_at=clock_timestamp() WHERE id=m.id;
   UPDATE interaction_report SET state='reviewed',reported_version=m.source_content_version
    WHERE target_id=target AND comment_id IS NULL AND state='open';
  END IF;
  UPDATE interaction_target SET version=version+1,updated_at=clock_timestamp() WHERE id=target RETURNING * INTO t;
  INSERT INTO interaction_audit(target_id,actor_id,operation,reason,previous_state,new_state)
   VALUES(target,actor,op,reason_value,CASE WHEN op='source.report.resolve' THEN 'open' ELSE m.source_moderation_state END,
    CASE WHEN op='source.report.resolve' THEN payload->>'decision' ELSE next_state END);
  result_value:=jsonb_build_object('state',next_state,'version',t.version);
 END IF;
 INSERT INTO interaction_request(actor_id,request_key,target_id,payload_hash,result)
  VALUES(actor,request_id,target,fingerprint,result_value||jsonb_build_object('_sourceContentVersion',m.source_content_version));
 RETURN result_value;
END $$;

CREATE OR REPLACE FUNCTION public.interaction_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
BEGIN
 -- Authorization repair 216: keep the hardened source authority on recovery.
 IF payload->>'operation' IN ('source.report','source.hide','source.remove','source.restore','source.report.resolve') THEN
  RETURN event_memory_source_command(actor,target,request_id,payload);
 END IF;
 RETURN interaction_command_before_event_source(actor,target,request_id,payload);
END $$;

-- Keep the 215 captured-authority digest unchanged: its reapply/rollback must
-- reject this successor instead of silently restoring nullable authorization.
COMMIT;
