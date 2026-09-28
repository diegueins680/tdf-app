BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Trusted internal predicate. No actor identity or evaluated_at from an HTTP client.
CREATE OR REPLACE FUNCTION event_operation_actor_can_read_task(
  target_event_id BIGINT, target_activity_id BIGINT,
  target_actor_party_id BIGINT, evaluated_at TIMESTAMPTZ
)
RETURNS BOOLEAN
LANGUAGE sql STABLE SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
  SELECT EXISTS (
    SELECT 1 FROM event_logistics_activity activity
    WHERE activity.id = target_activity_id AND activity.event_id = target_event_id
      AND (
        EXISTS (
          SELECT 1 FROM event_operation_actor_capabilities(
            target_event_id, target_actor_party_id, evaluated_at
          ) capability WHERE capability.scope_code IN ('task.read', 'task.manage')
        ) OR EXISTS (
          SELECT 1 FROM event_operation_grant grant_record
          WHERE grant_record.event_id = target_event_id
            AND grant_record.grantee_party_id = target_actor_party_id
            AND grant_record.scope_code IN ('task.read', 'task.manage')
            AND grant_record.resource_kind = 'task'
            -- Do not cast untrusted resource text: malformed/alternate spellings deny.
            AND grant_record.resource_id = target_activity_id::TEXT
            AND grant_record.revoked_at IS NULL
            AND grant_record.valid_from <= evaluated_at
            AND (grant_record.valid_until IS NULL OR grant_record.valid_until > evaluated_at)
        )
      )
  )
$$;

CREATE OR REPLACE FUNCTION event_operation_read_task(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT
)
RETURNS JSONB
LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  checked_at TIMESTAMPTZ;
  snapshot JSONB;
BEGIN
  PERFORM 1 FROM event_operation_feature_flag
    WHERE feature_code = 'event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;

  -- Permission writers update this row. Evaluate only AFTER the locking statement.
  PERFORM 1 FROM event_operation_event_state WHERE event_id = target_event_id FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  checked_at := clock_timestamp();

  -- One statement snapshot for task, policy, current RACI and authorization.
  -- This authorization fence does NOT lock task writers. Never split this SELECT
  -- into independently refreshed task and assignment queries.
  WITH current_raci AS MATERIALIZED (
    SELECT party_id, raci_role FROM event_operation_raci_assignment
    WHERE activity_id = target_activity_id AND revoked_at IS NULL
      AND valid_from <= checked_at AND (valid_until IS NULL OR valid_until > checked_at)
  )
  SELECT jsonb_strip_nulls(jsonb_build_object(
    'eventId', activity.event_id,
    'activityId', activity.id,
    'status', activity.status,
    'version', activity.version,
    'policy', CASE WHEN policy.activity_id IS NULL THEN NULL ELSE jsonb_build_object(
      'requiresAccountability', policy.requires_accountability,
      'dependenciesGateCompletion', policy.dependencies_gate_completion,
      'version', policy.version
    ) END,
    'raci', COALESCE((
      SELECT jsonb_agg(jsonb_build_object('partyId', party_id, 'role', raci_role)
                       ORDER BY raci_role, party_id) FROM current_raci
    ), '[]'::JSONB),
    'accountabilityNeedsAttention', COALESCE(policy.requires_accountability, FALSE) AND (
      (SELECT count(*) FROM current_raci WHERE raci_role = 'accountable') <> 1
      OR NOT EXISTS (SELECT 1 FROM current_raci WHERE raci_role = 'responsible')
    )
  )) INTO snapshot
  FROM event_logistics_activity activity
  LEFT JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
  WHERE activity.id = target_activity_id AND activity.event_id = target_event_id
    AND event_operation_actor_can_read_task(
      target_event_id, target_activity_id, target_actor_party_id, checked_at
    );
  RETURN snapshot;
END
$$;

COMMIT;
