-- Reuse canonical reports/audit/requests. Installation hides nothing and sends nothing.
BEGIN;
SET LOCAL lock_timeout='5s';
ALTER TABLE event_moment ADD COLUMN IF NOT EXISTS source_moderation_state text NOT NULL DEFAULT 'visible'
 CHECK(source_moderation_state IN ('visible','hidden','removed'));
ALTER TABLE event_moment ADD COLUMN IF NOT EXISTS source_content_version bigint NOT NULL DEFAULT 1 CHECK(source_content_version>=1);
CREATE OR REPLACE FUNCTION event_memory_content_version() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 NEW.source_content_version:=OLD.source_content_version;
 IF ROW(NEW.caption,NEW.media_url,NEW.media_type,NEW.media_width,NEW.media_height,NEW.media_duration_ms)
  IS DISTINCT FROM ROW(OLD.caption,OLD.media_url,OLD.media_type,OLD.media_width,OLD.media_height,OLD.media_duration_ms) THEN
  NEW.source_content_version:=OLD.source_content_version+1;
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_memory_source_content_version ON event_moment;
CREATE TRIGGER event_memory_source_content_version BEFORE UPDATE ON event_moment FOR EACH ROW EXECUTE FUNCTION event_memory_content_version();
CREATE INDEX IF NOT EXISTS event_memory_hidden_source ON event_moment(event_id,id)
 WHERE source_moderation_state<>'visible';
ALTER TABLE interaction_report ALTER COLUMN comment_id DROP NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS interaction_report_source_reporter
 ON interaction_report(target_id,reporter_id) WHERE comment_id IS NULL;
CREATE INDEX IF NOT EXISTS interaction_report_source_open ON interaction_report(target_id,created_at,id)
 WHERE comment_id IS NULL AND state='open';
CREATE TABLE IF NOT EXISTS event_memory_moderation_authority (
 signature text PRIMARY KEY, definition text NOT NULL, installed_digest text, helper_digest text
);
DO $$ DECLARE function_signature text; helper_signature text; original text; saved event_memory_moderation_authority%ROWTYPE; BEGIN
 FOREACH function_signature IN ARRAY ARRAY['interaction_resolve(text,text,bigint)',
  'interaction_command(bigint,uuid,uuid,jsonb)','interaction_summary(bigint,text,text)'] LOOP
  original:=pg_get_functiondef(function_signature::regprocedure);
  INSERT INTO event_memory_moderation_authority(signature,definition,installed_digest) VALUES(function_signature,original,NULL) ON CONFLICT DO NOTHING;
  SELECT * INTO STRICT saved FROM event_memory_moderation_authority a WHERE a.signature=function_signature;
  IF saved.installed_digest IS NOT NULL AND md5(original) NOT IN (saved.installed_digest,md5(saved.definition)) THEN
   RAISE EXCEPTION 'A successor changed the source moderation authority: %',function_signature;
  END IF;
  -- Copy the implementation; keep the public function's OID so existing callers
  -- cannot retain an unfiltered resolver through a renamed dependency.
  original:=replace(saved.definition,'FUNCTION public.'||split_part(function_signature,'(',1)||'(',
   'FUNCTION public.'||split_part(function_signature,'(',1)||'_before_event_source(');
  IF original=saved.definition THEN RAISE EXCEPTION 'Unexpected function declaration'; END IF;
  helper_signature:=replace(function_signature,split_part(function_signature,'(',1)||'(',split_part(function_signature,'(',1)||'_before_event_source(');
  IF (saved.helper_digest IS NULL AND to_regprocedure(helper_signature) IS NOT NULL)
   OR (saved.helper_digest IS NOT NULL AND md5(pg_get_functiondef(to_regprocedure(helper_signature))) IS DISTINCT FROM saved.helper_digest) THEN
   RAISE EXCEPTION 'A successor changed the captured source authority: %',helper_signature;
  END IF;
  EXECUTE original;
  UPDATE event_memory_moderation_authority SET helper_digest=md5(pg_get_functiondef(helper_signature::regprocedure))
   WHERE signature=function_signature;
 END LOOP;
END $$;

CREATE OR REPLACE FUNCTION event_memory_guard_source_report() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF NEW.comment_id IS NULL AND NOT EXISTS(SELECT 1 FROM interaction_target
  WHERE id=NEW.target_id AND entity_kind='event_moment') THEN
  RAISE EXCEPTION 'Source reports require an event moment' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_memory_source_report_guard ON interaction_report;
CREATE TRIGGER event_memory_source_report_guard BEFORE INSERT OR UPDATE OF comment_id,target_id ON interaction_report
 FOR EACH ROW EXECUTE FUNCTION event_memory_guard_source_report();

CREATE OR REPLACE FUNCTION interaction_resolve(kind text,entity text,actor bigint)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE context jsonb;
BEGIN
 context:=interaction_resolve_before_event_source(kind,entity,actor);
 IF context IS NOT NULL AND context->>'kind'='event_moment' AND NOT EXISTS(
  SELECT 1 FROM event_moment WHERE id=(context->>'key')::bigint AND source_moderation_state='visible') THEN RETURN NULL; END IF;
 RETURN context;
END $$;

CREATE OR REPLACE FUNCTION event_memory_source_context(target uuid,actor bigint) RETURNS jsonb
LANGUAGE plpgsql STABLE AS $$
DECLARE m event_moment%ROWTYPE; e social_event%ROWTYPE; managing boolean; moderating boolean; reading boolean;
BEGIN
 IF actor IS NULL OR NOT coalesce(interaction_actor_live(actor),false) THEN RETURN NULL; END IF;
 SELECT source.* INTO m FROM interaction_target t JOIN event_moment source ON source.id::text=t.entity_key
  WHERE t.id=target AND t.entity_kind='event_moment' AND t.retired_at IS NULL;
 IF NOT FOUND THEN RETURN NULL; END IF;
 SELECT * INTO STRICT e FROM social_event WHERE id=m.event_id;
 moderating:=interaction_is_moderator(actor);
 IF NOT interaction_event_access_scoped(e.id,actor,moderating) THEN RETURN NULL; END IF;
 managing:=e.organizer_party_id=actor::text;
 reading:=interaction_resolve('event_moment',m.id::text,actor) IS NOT NULL;
 IF NOT (managing OR moderating OR reading) THEN RETURN NULL; END IF;
 RETURN jsonb_build_object('supported',true,'eventId',e.id::text,'state',m.source_moderation_state,
  'sourceRevision',extract(epoch FROM m.updated_at)::text,
  'canReport',reading AND m.author_party_id IS DISTINCT FROM actor::text,
  'canHide',(managing OR moderating) AND m.source_moderation_state='visible',
  'canRestore',(managing AND m.source_moderation_state='hidden') OR (moderating AND m.source_moderation_state<>'visible'),
  'canRemove',moderating AND m.source_moderation_state<>'removed');
END $$;

CREATE OR REPLACE FUNCTION event_memory_source_reports(actor bigint,event_key bigint,after_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE can_review boolean; items jsonb; next_id uuid; parent social_event%ROWTYPE;
BEGIN
 IF actor IS NULL OR NOT coalesce(interaction_actor_live(actor),false) OR NOT interaction_event_access_scoped(event_key,actor,interaction_is_moderator(actor)) THEN
  RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO STRICT parent FROM social_event WHERE id=event_key;
 can_review:=parent.organizer_party_id=actor::text OR interaction_is_moderator(actor);
 IF NOT can_review THEN RETURN '{"supported":true,"canReview":false,"items":[],"nextCursor":null}'; END IF;
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

CREATE OR REPLACE FUNCTION event_memory_source_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE t interaction_target%ROWTYPE; m event_moment%ROWTYPE; initial_event bigint; context jsonb;
 op text:=payload->>'operation'; allowed text[]; reason_value text; fingerprint text; prior interaction_request%ROWTYPE;
 next_state text; result_value jsonb; parent social_event%ROWTYPE; inserted_report uuid;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled AND activated_once) THEN RETURN '{"error":"disabled"}'; END IF;
 IF actor IS NULL OR request_id IS NULL OR jsonb_typeof(payload) IS DISTINCT FROM 'object'
  OR pg_column_size(payload)>32768 OR NOT interaction_actor_live(actor) THEN RETURN '{"error":"invalid"}'; END IF;
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
 IF (op='source.report' AND NOT (context->>'canReport')::boolean)
  OR (op='source.remove' AND NOT interaction_is_moderator(actor))
  OR (op IN ('source.hide','source.restore','source.report.resolve') AND NOT
   (parent.organizer_party_id=actor::text OR interaction_is_moderator(actor)))
  OR (op IN ('source.hide','source.restore') AND m.source_moderation_state='removed' AND NOT interaction_is_moderator(actor))
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
 IF (op='source.hide' AND NOT (context->>'canHide')::boolean)
  OR (op='source.remove' AND NOT (context->>'canRemove')::boolean)
  OR (op='source.restore' AND NOT (context->>'canRestore')::boolean) THEN RETURN '{"error":"revision_conflict"}'; END IF;
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

CREATE OR REPLACE FUNCTION interaction_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
BEGIN
 IF payload->>'operation' IN ('source.report','source.hide','source.remove','source.restore','source.report.resolve') THEN
  RETURN event_memory_source_command(actor,target,request_id,payload);
 END IF;
 RETURN interaction_command_before_event_source(actor,target,request_id,payload);
END $$;

CREATE OR REPLACE FUNCTION interaction_summary(actor bigint,kind text,entity text)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE value jsonb; context jsonb;
BEGIN
 value:=interaction_summary_before_event_source(actor,kind,entity);
 IF value->>'error' IS NOT NULL THEN RETURN value; END IF;
 IF kind='event_moment' THEN
  context:=event_memory_source_context((value->>'id')::uuid,actor);
  IF context IS NOT NULL THEN value:=value||jsonb_build_object('sourceModeration',context); END IF;
 END IF;
 RETURN value;
END $$;
UPDATE event_memory_moderation_authority SET installed_digest=md5(pg_get_functiondef(signature::regprocedure));
COMMIT;
