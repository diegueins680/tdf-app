-- TC01–07: docs/event-operations/task-completion-contract.md; formal gate precedes SQL.
-- Requires the existing task revision/read and RACI scoped-authority primitives.
-- Private, disabled by the existing event API flag; not in the production manifest.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION event_operation_complete_task(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT,
  target_command_id UUID, expected_revision BIGINT, requested_reason TEXT, correlation_reference TEXT
) RETURNS JSONB LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path = public, pg_temp AS $$
DECLARE
  event_state TEXT;
  task_state TEXT;
  old_version INTEGER;
  current_revision BIGINT;
  result_revision BIGINT;
  checked_at TIMESTAMPTZ;
  operation_namespace TEXT := 'event.task.complete/' || target_activity_id::TEXT;
  request_hash BYTEA;
  prior event_operation_command_receipt%ROWTYPE;
  response_payload JSONB;
BEGIN
  PERFORM 1 FROM event_operation_feature_flag
    WHERE feature_code='event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','feature_disabled'); END IF;
  IF EXISTS (SELECT 1 FROM unnest(ARRAY[target_event_id,target_activity_id,target_actor_party_id]) ident(value)
      WHERE value IS NULL OR value<=0 OR value>9007199254740991)
    OR target_command_id IS NULL OR expected_revision IS NULL OR expected_revision<=0
    OR NULLIF(btrim(requested_reason),'') IS NULL OR length(requested_reason)>2000
    OR NULLIF(btrim(correlation_reference),'') IS NULL OR length(correlation_reference)>200 THEN
    RETURN jsonb_build_object('error','invalid_request');
  END IF;

  SELECT canonical_state INTO event_state FROM event_operation_event_state
    WHERE event_id=target_event_id FOR SHARE;
  IF NOT FOUND OR NOT event_operation_actor_can_read_task(
    target_event_id,target_activity_id,target_actor_party_id,clock_timestamp()
  ) THEN RETURN jsonb_build_object('error','not_found'); END IF;

  -- Same WRITE fence as all canonical task writers, including legacy routes.
  -- A stale RR/Serializable snapshot aborts instead of using stale dependencies.
  INSERT INTO event_operation_task_write_fence(event_id) VALUES(target_event_id)
    ON CONFLICT(event_id) DO UPDATE SET revision=event_operation_task_write_fence.revision+1;
  SELECT metadata.revision,activity.status,activity.version
    INTO current_revision,task_state,old_version
    FROM event_operation_task_revision metadata
    JOIN event_logistics_activity activity ON activity.id=metadata.activity_id
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id
    FOR UPDATE OF metadata,activity;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','not_found'); END IF;
  -- Audit/receipt FK locking must not introduce an unchecked pre-decision wait.
  PERFORM 1 FROM party WHERE id=target_actor_party_id FOR KEY SHARE;
  checked_at := clock_timestamp();
  IF NOT event_operation_actor_can_read_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) THEN
    RETURN jsonb_build_object('error','not_found');
  END IF;
  request_hash := digest(convert_to(jsonb_build_object(
    'operation',operation_namespace,'eventId',target_event_id::TEXT,'activityId',target_activity_id::TEXT,
    'actorPartyId',target_actor_party_id::TEXT,'expectedRevision',expected_revision::TEXT,
    'reason',requested_reason,'correlationId',correlation_reference)::TEXT,'UTF8'),'sha256');
  SELECT * INTO prior FROM event_operation_command_receipt
    WHERE event_id=target_event_id AND operation_code=operation_namespace AND command_id=target_command_id;
  IF FOUND THEN
    IF prior.actor_party_id=target_actor_party_id AND prior.request_sha256=request_hash THEN
      RETURN prior.response || jsonb_build_object('replayed',TRUE);
    END IF;
    RETURN jsonb_build_object('error','idempotency_conflict');
  END IF;
  IF NOT event_operation_actor_can_manage_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) THEN
    RETURN jsonb_build_object('error','forbidden');
  END IF;
  IF event_state NOT IN ('draft','planning') OR task_state NOT IN ('planned','confirmed') OR old_version<1
    OR NOT EXISTS (SELECT 1 FROM event_operation_task_policy WHERE activity_id=target_activity_id
      AND requires_accountability AND dependencies_gate_completion) THEN
    RETURN jsonb_build_object('error','operation_not_ready');
  END IF;
  IF current_revision<>expected_revision THEN RETURN jsonb_build_object('error','version_conflict'); END IF;
  IF (SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=target_activity_id
      AND raci_role='accountable' AND revoked_at IS NULL AND valid_from<=checked_at
      AND (valid_until IS NULL OR valid_until>checked_at))<>1
    OR NOT EXISTS (SELECT 1 FROM event_operation_raci_assignment WHERE activity_id=target_activity_id
      AND raci_role='responsible' AND revoked_at IS NULL AND valid_from<=checked_at
      AND (valid_until IS NULL OR valid_until>checked_at)) THEN
    RETURN jsonb_build_object('error','accountability_not_ready');
  END IF;
  -- No override in this command, including previously recorded trusted overrides.
  -- Failures do not reveal identities or states of private prerequisite tasks.
  IF EXISTS (SELECT 1 FROM event_logistics_dependency dependency
    JOIN event_logistics_activity prerequisite ON prerequisite.id=dependency.depends_on_activity_id
    WHERE dependency.activity_id=target_activity_id AND prerequisite.status<>'completed') THEN
    RETURN jsonb_build_object('error','dependencies_not_ready');
  END IF;

  UPDATE event_logistics_activity SET status='completed',version=version+1,updated_at=checked_at
    WHERE id=target_activity_id;
  PERFORM event_operation_validate_task_event(target_event_id);
  SELECT revision INTO result_revision FROM event_operation_task_revision WHERE activity_id=target_activity_id;
  response_payload := jsonb_build_object('eventId',target_event_id,'activityId',target_activity_id,
    'commandId',target_command_id,'status','completed','activityVersion',old_version+1,
    'aggregateRevision',result_revision::TEXT,'replayed',FALSE);
  INSERT INTO event_operation_audit_event(event_id,actor_party_id,actor_reference,operation_code,command_id,
    resource_kind,resource_id,outcome,reason,before_state,after_state,correlation_id)
    VALUES(target_event_id,target_actor_party_id,'party:' || target_actor_party_id::TEXT,
      'event.task.complete',target_command_id,'task',target_activity_id::TEXT,'accepted',requested_reason,
      jsonb_build_object('status',task_state,'activityVersion',old_version,'aggregateRevision',current_revision::TEXT),
      jsonb_build_object('status','completed','activityVersion',old_version+1,'aggregateRevision',result_revision::TEXT),
      correlation_reference);
  INSERT INTO event_operation_command_receipt(event_id,actor_party_id,operation_code,command_id,
    request_sha256,expected_version,result_version,outcome,response)
    VALUES(target_event_id,target_actor_party_id,operation_namespace,target_command_id,
      request_hash,expected_revision,result_revision,'accepted',response_payload);
  RETURN response_payload;
END $$;

REVOKE ALL ON FUNCTION event_operation_complete_task(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,TEXT) FROM PUBLIC;
COMMIT;
