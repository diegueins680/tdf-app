BEGIN;
SET LOCAL lock_timeout='10s';
SET LOCAL statement_timeout='10min';
-- EC01–06: read-only, exact task context; never a write-authorization certificate.
CREATE OR REPLACE FUNCTION event_operation_read_raci_editor_context(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT,
  after_party_id BIGINT DEFAULT 0
) RETURNS JSONB LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path=public,pg_temp AS $$
DECLARE checked_at TIMESTAMPTZ; result JSONB;
BEGIN
  IF EXISTS(SELECT 1 FROM unnest(ARRAY[target_event_id,target_activity_id,target_actor_party_id]) ids(value)
      WHERE value IS NULL OR value<=0 OR value>9007199254740991)
    OR after_party_id IS NULL OR after_party_id<0 OR after_party_id>9007199254740991 THEN RETURN NULL; END IF;
  PERFORM 1 FROM event_operation_feature_flag WHERE feature_code='event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  PERFORM 1 FROM event_operation_event_state WHERE event_id=target_event_id FOR SHARE;
  IF NOT FOUND OR NOT event_operation_actor_can_read_task(
    target_event_id,target_activity_id,target_actor_party_id,clock_timestamp()) THEN RETURN NULL; END IF;
  PERFORM 1 FROM event_operation_task_revision metadata
    JOIN event_logistics_activity activity ON activity.id=metadata.activity_id
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id FOR SHARE OF metadata;
  IF NOT FOUND THEN RETURN NULL; END IF;
  -- A single post-wait clock and statement snapshot for the entire context.
  checked_at := clock_timestamp();
  WITH current_raci AS MATERIALIZED (
    SELECT party_id,raci_role,valid_until FROM event_operation_raci_assignment
    WHERE activity_id=target_activity_id AND revoked_at IS NULL AND valid_from<=checked_at
      AND (valid_until IS NULL OR valid_until>checked_at)
  ), authority AS MATERIALIZED (
    SELECT activity.event_id,activity.id,metadata.revision,activity.status,state.canonical_state,
      COALESCE(policy.requires_accountability,FALSE) AS requires_accountability,
      event_operation_actor_can_manage_task(target_event_id,target_activity_id,target_actor_party_id,checked_at) AS can_manage
    FROM event_logistics_activity activity
    JOIN event_operation_task_revision metadata ON metadata.activity_id=activity.id
    JOIN event_operation_event_state state ON state.event_id=activity.event_id
    LEFT JOIN event_operation_task_policy policy ON policy.activity_id=activity.id
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id
      AND event_operation_actor_can_read_task(target_event_id,target_activity_id,target_actor_party_id,checked_at)
  ), readiness AS MATERIALIZED (
    SELECT *,can_manage AND canonical_state IN ('draft','planning') AND status IN ('planned','confirmed')
      AND (NOT requires_accountability OR (
        (SELECT count(*) FROM current_raci WHERE raci_role='accountable')=1
        AND EXISTS(SELECT 1 FROM current_raci WHERE raci_role='responsible'))) AS operation_ready
    FROM authority
  ), candidate_ids AS (
    SELECT relationship.party_id FROM event_operation_relationship relationship
    WHERE relationship.event_id=target_event_id AND relationship.relationship_kind IN ('primary_owner','co_owner')
      AND relationship.revoked_at IS NULL AND relationship.valid_from<=checked_at
      AND (relationship.valid_until IS NULL OR relationship.valid_until>checked_at)
      AND EXISTS(SELECT 1 FROM readiness WHERE operation_ready)
    UNION
    SELECT grant_record.grantee_party_id FROM event_operation_grant grant_record
    WHERE grant_record.event_id=target_event_id AND grant_record.scope_code IN ('task.read','task.manage')
      AND ((grant_record.resource_kind='event' AND grant_record.resource_id IS NULL)
        OR (grant_record.resource_kind='task' AND grant_record.resource_id=target_activity_id::TEXT))
      AND grant_record.revoked_at IS NULL AND grant_record.valid_from<=checked_at
      AND (grant_record.valid_until IS NULL OR grant_record.valid_until>checked_at)
      AND EXISTS(SELECT 1 FROM readiness WHERE operation_ready)
  ), candidate_page AS MATERIALIZED (
    SELECT party_id FROM candidate_ids WHERE party_id>after_party_id AND party_id<=9007199254740991
      AND event_operation_actor_can_read_task(target_event_id,target_activity_id,party_id,checked_at)
    ORDER BY party_id LIMIT 101
  ), visible_page AS MATERIALIZED (SELECT party_id FROM candidate_page ORDER BY party_id LIMIT 100)
  SELECT jsonb_strip_nulls(jsonb_build_object(
    'eventId',event_id,'activityId',id,'aggregateRevision',revision::TEXT,
    'canManage',can_manage,'operationReady',operation_ready,
    'replaceableAssignments',CASE WHEN operation_ready THEN COALESCE((SELECT jsonb_agg(
      jsonb_build_object('partyId',party_id,'role',raci_role) ORDER BY raci_role,party_id)
      FROM current_raci WHERE valid_until IS NULL),'[]'::JSONB) ELSE '[]'::JSONB END,
    'eligiblePartyIds',COALESCE((SELECT jsonb_agg(party_id ORDER BY party_id) FROM visible_page),'[]'::JSONB),
    'nextAfterPartyId',CASE WHEN (SELECT count(*) FROM candidate_page)>100
      THEN (SELECT max(party_id) FROM visible_page) ELSE NULL END
  )) INTO result FROM readiness;
  RETURN result;
END $$;
REVOKE ALL ON FUNCTION event_operation_read_raci_editor_context(BIGINT,BIGINT,BIGINT,BIGINT) FROM PUBLIC;
COMMIT;
