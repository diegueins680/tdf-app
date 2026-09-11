-- Non-destructive application rollback. RSVP rows, consent decisions,
-- deduplication evidence, uniqueness, status checks, and privacy-safe public
-- projection are intentionally preserved. Old binaries ignore additive fields.
\set ON_ERROR_STOP on
BEGIN;

UPDATE workflow_state_capability capability
SET enabled=FALSE,updated_at=now(),version=version+1
FROM workflow_state state
JOIN workflow_definition workflow ON workflow.id=state.workflow_id
WHERE capability.state_id=state.id
  AND workflow.code='social-event-lifecycle'
  AND capability.capability_code='rsvp';

COMMIT;
