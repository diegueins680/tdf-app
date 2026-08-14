\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

SELECT jsonb_build_object(
  'report', 'social-event-workflow-schema-readiness',
  'workflowTablePresent', to_regclass('public.workflow_definition') IS NOT NULL,
  'workflowStateIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='social_event' AND column_name='workflow_state_id'
  ),
  'workflowMappingTablePresent', to_regclass('public.workflow_migration_mapping') IS NOT NULL,
  'metadataValidation', 'strict jsonb cast; malformed metadata aborts this transaction',
  'legacyMetadataRows', (
    SELECT count(*) FROM social_event
    WHERE metadata IS NOT NULL
      AND metadata::jsonb ? 'eventStatus'
  )
);

WITH source AS (
  SELECT event.id,
    event.workflow_state_id AS original_workflow_state_id,
    event.metadata AS original_metadata,
    CASE WHEN event.metadata IS NOT NULL THEN event.metadata::jsonb ->> 'eventStatus' END AS original_event_status
  FROM social_event event
  WHERE event.workflow_state_id IS NULL
     OR event.metadata IS NOT NULL
        AND event.metadata::jsonb ? 'eventStatus'
), normalized AS (
  SELECT source.*,
    CASE
      WHEN NULLIF(btrim(original_event_status), '') IS NULL THEN 'planning'
      WHEN lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))='canceled' THEN 'cancelled'
      WHEN lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))='onsale' THEN 'on_sale'
      ELSE lower(regexp_replace(btrim(original_event_status), '[[:space:]-]+', '_', 'g'))
    END AS normalized_status
  FROM source
), resolved AS (
  SELECT normalized.*,
    match.candidate_count,
    match.target_id,
    match.target_code
  FROM normalized
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      (array_agg(state.id ORDER BY state.id))[1] AS target_id,
      (array_agg(state.code ORDER BY state.id))[1] AS target_code
    FROM workflow_state state
    JOIN workflow_definition workflow ON workflow.id=state.workflow_id
      AND workflow.code='social-event-lifecycle' AND workflow.active
    WHERE state.active AND state.code=normalized.normalized_status
  ) match ON TRUE
)
SELECT jsonb_build_object(
  'report', 'social-event-workflow-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (
    WHERE candidate_count=1
      AND (original_workflow_state_id IS NULL OR original_workflow_state_id=target_id)
  ),
  'unresolved', count(*) FILTER (WHERE candidate_count=0),
  'ambiguous', count(*) FILTER (WHERE candidate_count>1),
  'conflicts', count(*) FILTER (
    WHERE original_workflow_state_id IS NOT NULL AND original_workflow_state_id<>target_id
  ),
  'rows', COALESCE(jsonb_agg(jsonb_build_object(
    'id', id,
    'originalEventStatus', original_event_status,
    'normalizedStatus', normalized_status,
    'originalWorkflowStateId', original_workflow_state_id,
    'candidateCount', candidate_count,
    'targetWorkflowStateId', target_id,
    'targetCode', target_code,
    'evidence', CASE
      WHEN original_workflow_state_id=target_id THEN 'existing canonical UUID confirmed in social-event-lifecycle'
      WHEN original_event_status IS NULL OR btrim(original_event_status)='' THEN 'legacy API read/default semantics deterministically resolve an absent status to planning'
      ELSE 'reviewed normalized status/alias resolves to one active state in social-event-lifecycle'
    END
  ) ORDER BY id), '[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report', 'social-event-persisted-workflow',
  'workflow', jsonb_build_object(
    'id', workflow.id,
    'code', workflow.code,
    'publicRead', workflow.public_read,
    'sensitive', workflow.sensitive,
    'cacheRevision', workflow.cache_revision
  ),
  'states', (
    SELECT jsonb_agg(jsonb_build_object(
      'id', state.id,
      'code', state.code,
      'nameEs', state.name_es,
      'nameEn', state.name_en,
      'terminal', state.terminal,
      'initialContexts', COALESCE((
        SELECT jsonb_agg(default_state.context ORDER BY default_state.context)
        FROM workflow_default_state default_state
        WHERE default_state.state_id=state.id AND default_state.active
      ), '[]'::jsonb),
      'capabilities', COALESCE((
        SELECT jsonb_agg(capability.capability_code ORDER BY capability.capability_code)
        FROM workflow_state_capability capability
        WHERE capability.state_id=state.id AND capability.enabled
      ), '[]'::jsonb)
    ) ORDER BY state.sort_order, state.id)
    FROM workflow_state state WHERE state.workflow_id=workflow.id AND state.active
  ),
  'transitions', (
    SELECT count(*) FROM workflow_transition transition
    WHERE transition.workflow_id=workflow.id AND transition.active
  )
) FROM workflow_definition workflow
WHERE workflow.code='social-event-lifecycle' AND workflow.active;

ROLLBACK;
