-- Serialize source enforcement with assignment revocation.
-- Preserve all prior SQL, functions/OIDs, publications and canonical audit.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL search_path=public,pg_catalog;
DO $guard$ DECLARE item record; installed pg_proc%ROWTYPE; BEGIN
 FOR item IN SELECT * FROM (VALUES
 ('public.event_memory_source_context(uuid,bigint)','8c616f3e4848cbcf20ed5065feba2306','8c616f3e4848cbcf20ed5065feba2306','s'),
 ('public.event_memory_source_reports(bigint,bigint,uuid,integer)','ada2bda3951bc62b0fdde2ac13cd0e57','ada2bda3951bc62b0fdde2ac13cd0e57','s'),
 ('public.interaction_command(bigint,uuid,uuid,jsonb)','7d33d28fc9379c7b83328f0ca0397eeb','7d33d28fc9379c7b83328f0ca0397eeb','v'),
 ('public.event_memory_source_command(bigint,uuid,uuid,jsonb)','39a34f6efbb2843c9a3ffeabab7fd219','791fb81a35d313775e0d403e66dc6b29','v')
 ) expected(signature,before_digest,after_digest,volatility) LOOP
  SELECT * INTO STRICT installed FROM pg_proc WHERE oid=item.signature::regprocedure;
  IF installed.prosecdef OR installed.proconfig IS NOT NULL
   OR installed.prolang<>(SELECT oid FROM pg_language WHERE lanname='plpgsql')
   OR installed.provolatile::text<>item.volatility OR installed.prorettype<>'jsonb'::regtype
   OR md5(installed.prosrc) NOT IN(item.before_digest,item.after_digest) THEN
   RAISE EXCEPTION 'Unexpected source permission-lock successor: %',item.signature;
  END IF;
 END LOOP;
END $guard$;

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
 -- Reuse canonical membership/assignment locking before computing authority.
 PERFORM interaction_lock_permissions(target,actor);
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
COMMIT;
