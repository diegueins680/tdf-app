BEGIN;

UPDATE event_operation_feature_flag
SET enabled = FALSE,
    updated_at = now(),
    updated_by_party_id = NULL,
    change_reason = 'application rollback: event operations API disabled'
WHERE feature_code = 'event.operations.api'
  AND (
    enabled
    OR updated_by_party_id IS NOT NULL
    OR change_reason IS DISTINCT FROM 'application rollback: event operations API disabled'
  );

DROP FUNCTION IF EXISTS event_operation_apply_transition(
  BIGINT, BIGINT, UUID, BIGINT, TEXT, TEXT, TEXT, TEXT
);

COMMIT;
