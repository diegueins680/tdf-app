\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

WITH source AS (
  SELECT card.id, card.service_kind::text AS original_service_kind,
    card.stage AS original_stage, card.service_offering_id AS original_service_offering_id,
    card.workflow_state_id AS original_workflow_state_id,
    COALESCE(card.service_offering_id, service.id) AS target_service_offering_id,
    CASE
      WHEN card.stage IS NULL THEN state.code
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='make-up-needed' THEN 'makeup-needed'
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='pre-prod' THEN 'pre-production'
      WHEN lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))='post-prod' THEN 'post-production'
      ELSE lower(regexp_replace(btrim(card.stage), '[^[:alnum:]]+', '-', 'g'))
    END AS normalized_stage
  FROM pipeline_card card
  LEFT JOIN service_offering service ON service.code=CASE card.service_kind::text
    WHEN 'Recording' THEN 'recording' WHEN 'Mixing' THEN 'mixing'
    WHEN 'Mastering' THEN 'mastering' WHEN 'Rehearsal' THEN 'rehearsal'
    WHEN 'Classes' THEN 'classes' WHEN 'EventProduction' THEN 'event-production' END
    AND service.active
  LEFT JOIN workflow_state state ON state.id=card.workflow_state_id
  WHERE card.service_kind IS NOT NULL OR card.stage IS NOT NULL
    OR card.service_offering_id IS NULL OR card.workflow_state_id IS NULL
), resolved AS (
  SELECT source.*, match.workflow_id, match.target_state_id, match.candidate_count
  FROM source
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      (array_agg(binding.workflow_id ORDER BY binding.workflow_id))[1] AS workflow_id,
      (array_agg(state.id ORDER BY state.id))[1] AS target_state_id
    FROM pipeline_workflow_binding binding
    JOIN workflow_definition workflow ON workflow.id=binding.workflow_id
      AND workflow.active AND workflow.code LIKE 'pipeline-%'
    JOIN workflow_state state ON state.workflow_id=workflow.id
      AND state.active AND state.code=source.normalized_stage
    WHERE binding.service_offering_id=source.target_service_offering_id AND binding.active
  ) match ON TRUE
)
SELECT jsonb_build_object(
  'report', 'pipeline-workflow-cutover-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (WHERE candidate_count=1
    AND (original_workflow_state_id IS NULL OR original_workflow_state_id=target_state_id)),
  'unresolved', count(*) FILTER (WHERE candidate_count=0),
  'ambiguous', count(*) FILTER (WHERE candidate_count>1),
  'conflicts', count(*) FILTER (WHERE original_workflow_state_id IS NOT NULL
    AND original_workflow_state_id<>target_state_id),
  'rows', COALESCE(jsonb_agg(jsonb_build_object(
    'id', id, 'originalServiceKind', original_service_kind,
    'originalStage', original_stage, 'normalizedStage', normalized_stage,
    'originalServiceOfferingId', original_service_offering_id,
    'targetServiceOfferingId', target_service_offering_id,
    'originalWorkflowStateId', original_workflow_state_id,
    'targetWorkflowStateId', target_state_id, 'workflowId', workflow_id,
    'candidateCount', candidate_count,
    'evidence', 'explicit service-offering binding plus one normalized state code in its active persisted workflow'
  ) ORDER BY id), '[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report', 'pipeline-persisted-registry',
  'workflows', count(DISTINCT workflow.id),
  'states', count(DISTINCT state.id),
  'bindings', count(DISTINCT binding.id),
  'initialDefaults', count(DISTINCT default_state.id),
  'transitions', count(DISTINCT transition.id)
)
FROM workflow_definition workflow
JOIN workflow_state state ON state.workflow_id=workflow.id AND state.active
JOIN pipeline_workflow_binding binding ON binding.workflow_id=workflow.id AND binding.active
LEFT JOIN workflow_default_state default_state ON default_state.workflow_id=workflow.id
  AND default_state.context='initial' AND default_state.active
LEFT JOIN workflow_transition transition ON transition.workflow_id=workflow.id AND transition.active
WHERE workflow.active AND workflow.code LIKE 'pipeline-%';

ROLLBACK;
