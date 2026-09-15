-- Private planning-stage command. Not an HTTP endpoint or production manifest entry.
-- RC01–06: docs/event-operations/raci-reassignment-contract.md, checked before implementation.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION event_operation_actor_can_manage_task(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT,
  evaluated_at TIMESTAMPTZ
) RETURNS BOOLEAN LANGUAGE sql STABLE SECURITY INVOKER
SET search_path = public, pg_temp AS $$
  SELECT EXISTS (
    SELECT 1 FROM event_logistics_activity activity
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id
      AND (EXISTS (
        SELECT 1 FROM event_operation_actor_capabilities(target_event_id,target_actor_party_id,evaluated_at)
          capability WHERE capability.scope_code='task.manage'
      ) OR EXISTS (
        SELECT 1 FROM event_operation_grant grant_record
        WHERE grant_record.event_id=target_event_id AND grant_record.grantee_party_id=target_actor_party_id
          AND grant_record.scope_code='task.manage' AND grant_record.resource_kind='task'
          AND grant_record.resource_id=target_activity_id::TEXT AND grant_record.revoked_at IS NULL
          AND grant_record.valid_from<=evaluated_at
          AND (grant_record.valid_until IS NULL OR grant_record.valid_until>evaluated_at)
      ))
  )
$$;

CREATE OR REPLACE FUNCTION event_operation_reassign_raci(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT,
  target_command_id UUID, expected_revision BIGINT, requested_role TEXT,
  source_party_id BIGINT, recipient_party_id BIGINT, requested_reason TEXT, correlation_reference TEXT
) RETURNS JSONB LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path = public, pg_temp AS $$
DECLARE
  event_state TEXT;
  task_state TEXT;
  current_revision BIGINT;
  result_revision BIGINT;
  checked_at TIMESTAMPTZ;
  operation_namespace TEXT := 'event.task.raci.reassign/' || target_activity_id::TEXT;
  request_hash BYTEA;
  prior event_operation_command_receipt%ROWTYPE;
  source_assignment event_operation_raci_assignment%ROWTYPE;
  has_source BOOLEAN;
  has_recipient BOOLEAN;
  new_assignment_id UUID;
  response_payload JSONB;
BEGIN
  PERFORM 1 FROM event_operation_feature_flag
    WHERE feature_code='event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','feature_disabled'); END IF;
  IF EXISTS (SELECT 1 FROM unnest(ARRAY[target_event_id,target_activity_id,target_actor_party_id,
        source_party_id,recipient_party_id]) ident(value)
      WHERE value IS NULL OR value<=0 OR value>9007199254740991)
    OR target_command_id IS NULL OR expected_revision IS NULL OR expected_revision<=0
    OR requested_role IS NULL OR requested_role NOT IN ('responsible','accountable','consulted','informed')
    OR source_party_id=recipient_party_id
    OR NULLIF(btrim(requested_reason),'') IS NULL OR length(requested_reason)>2000
    OR NULLIF(btrim(correlation_reference),'') IS NULL OR length(correlation_reference)>200 THEN
    RETURN jsonb_build_object('error','invalid_request');
  END IF;

  SELECT canonical_state INTO event_state FROM event_operation_event_state
    WHERE event_id=target_event_id FOR SHARE;
  IF NOT FOUND OR NOT event_operation_actor_can_read_task(
    target_event_id,target_activity_id,target_actor_party_id,clock_timestamp()
  ) THEN RETURN jsonb_build_object('error','not_found'); END IF;

  -- Same WRITE fence as legacy task/RACI writers; stale RR snapshots abort here.
  -- Receipt lookup must precede expected-version comparison, so acquire separately
  -- from event_operation_lock_task_revision rather than rejecting a valid replay.
  INSERT INTO event_operation_task_write_fence(event_id) VALUES(target_event_id)
    ON CONFLICT(event_id) DO UPDATE SET revision=event_operation_task_write_fence.revision+1;
  SELECT metadata.revision, activity.status INTO current_revision,task_state
    FROM event_operation_task_revision metadata
    JOIN event_logistics_activity activity ON activity.id=metadata.activity_id
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id
    FOR UPDATE OF metadata;
  IF NOT FOUND THEN RETURN jsonb_build_object('error','not_found'); END IF;
  checked_at := clock_timestamp();
  IF NOT event_operation_actor_can_read_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) THEN
    RETURN jsonb_build_object('error','not_found');
  END IF;

  -- The canonical receipt ledger is reused; no caller-provided hash or second ledger.
  request_hash := digest(convert_to(jsonb_build_object(
    'operation',operation_namespace,'eventId',target_event_id::TEXT,'activityId',target_activity_id::TEXT,
    'actorPartyId',target_actor_party_id::TEXT,'expectedRevision',expected_revision::TEXT,
    'role',requested_role,'fromPartyId',source_party_id::TEXT,'toPartyId',recipient_party_id::TEXT,
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
  IF event_state NOT IN ('draft','planning') OR task_state NOT IN ('planned','confirmed') THEN
    RETURN jsonb_build_object('error','operation_not_ready');
  END IF;
  IF current_revision<>expected_revision THEN RETURN jsonb_build_object('error','version_conflict'); END IF;

  SELECT * INTO source_assignment FROM event_operation_raci_assignment
    WHERE activity_id=target_activity_id AND party_id=source_party_id AND raci_role=requested_role
      AND revoked_at IS NULL FOR UPDATE;
  has_source := FOUND;
  PERFORM 1 FROM party WHERE id=recipient_party_id FOR KEY SHARE;
  has_recipient := FOUND;
  -- Tuple/FK waits can outlast a grant even though the authorization fence is held.
  checked_at := clock_timestamp();
  IF NOT event_operation_actor_can_read_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) THEN
    RETURN jsonb_build_object('error','not_found');
  END IF;
  IF NOT event_operation_actor_can_manage_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) THEN
    RETURN jsonb_build_object('error','forbidden');
  END IF;
  IF NOT has_source OR source_assignment.valid_from>checked_at OR source_assignment.valid_until IS NOT NULL THEN
    RETURN jsonb_build_object('error','assignment_not_replaceable');
  END IF;
  IF NOT has_recipient OR NOT event_operation_actor_can_read_task(
    target_event_id,target_activity_id,recipient_party_id,checked_at
  ) THEN RETURN jsonb_build_object('error','assignee_unavailable'); END IF;
  IF EXISTS(SELECT 1 FROM event_operation_raci_assignment
    WHERE activity_id=target_activity_id AND party_id=recipient_party_id AND raci_role=requested_role
      AND revoked_at IS NULL) THEN RETURN jsonb_build_object('error','assignment_conflict'); END IF;

  -- The swap preserves role cardinality at this decision instant. It cannot repair
  -- a different expired responsibility by silently claiming that the task is ready.
  IF EXISTS(SELECT 1 FROM event_operation_task_policy WHERE activity_id=target_activity_id AND requires_accountability)
    AND ((SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=target_activity_id
      AND raci_role='accountable' AND revoked_at IS NULL AND valid_from<=checked_at
      AND (valid_until IS NULL OR valid_until>checked_at))<>1
      OR NOT EXISTS(SELECT 1 FROM event_operation_raci_assignment WHERE activity_id=target_activity_id
        AND raci_role='responsible' AND revoked_at IS NULL AND valid_from<=checked_at
        AND (valid_until IS NULL OR valid_until>checked_at))) THEN
    RETURN jsonb_build_object('error','accountability_not_ready');
  END IF;

  UPDATE event_operation_raci_assignment SET revoked_at=checked_at,revoked_by_party_id=target_actor_party_id,
    revocation_reason=requested_reason WHERE id=source_assignment.id;
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id,valid_from)
    VALUES(target_activity_id,recipient_party_id,requested_role,target_actor_party_id,checked_at)
    RETURNING id INTO new_assignment_id;
  PERFORM event_operation_validate_task_event(target_event_id);
  SELECT revision INTO result_revision FROM event_operation_task_revision WHERE activity_id=target_activity_id;
  response_payload := jsonb_build_object('eventId',target_event_id,'activityId',target_activity_id,
    'commandId',target_command_id,'role',requested_role,'fromPartyId',source_party_id,
    'toPartyId',recipient_party_id,'aggregateRevision',result_revision::TEXT,'replayed',FALSE);
  INSERT INTO event_operation_audit_event(event_id,actor_party_id,actor_reference,operation_code,command_id,
    resource_kind,resource_id,outcome,reason,before_state,after_state,correlation_id)
    VALUES(target_event_id,target_actor_party_id,'party:' || target_actor_party_id::TEXT,
      'event.task.raci.reassign',target_command_id,'task',target_activity_id::TEXT,'accepted',requested_reason,
      jsonb_build_object('assignmentId',source_assignment.id,'partyId',source_party_id,
        'role',requested_role,'aggregateRevision',current_revision::TEXT),
      jsonb_build_object('assignmentId',new_assignment_id,'partyId',recipient_party_id,
        'role',requested_role,'aggregateRevision',result_revision::TEXT),correlation_reference);
  INSERT INTO event_operation_command_receipt(event_id,actor_party_id,operation_code,command_id,
    request_sha256,expected_version,result_version,outcome,response)
    VALUES(target_event_id,target_actor_party_id,operation_namespace,target_command_id,
      request_hash,expected_revision,result_revision,'accepted',response_payload);
  RETURN response_payload;
END $$;

REVOKE ALL ON FUNCTION event_operation_actor_can_manage_task(BIGINT,BIGINT,BIGINT,TIMESTAMPTZ) FROM PUBLIC;
REVOKE ALL ON FUNCTION event_operation_reassign_raci(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,BIGINT,BIGINT,TEXT,TEXT) FROM PUBLIC;
COMMIT;
