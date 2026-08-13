\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

SELECT jsonb_build_object(
  'report', 'social-event-type-schema-readiness',
  'eventTypeTablePresent', to_regclass('public.event_type') IS NOT NULL,
  'eventTypeIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='social_event' AND column_name='event_type_id'
  ),
  'legacyMetadataRows', (
    SELECT count(*) FROM social_event
    WHERE metadata IS NOT NULL AND metadata::jsonb ? 'eventType'
  )
);

WITH resolved AS (
  SELECT event.id,
    event.event_type_id AS original_event_type_id,
    event.metadata AS original_metadata,
    event.metadata::jsonb ->> 'eventType' AS original_event_type,
    match.candidate_count,
    match.target_id
  FROM social_event event
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      (array_agg(item.id ORDER BY item.id))[1] AS target_id
    FROM event_type item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id
      AND catalog.code='event-types' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id
      AND state.workflow_id=catalog.workflow_id
      AND state.code='published' AND state.active
    WHERE item.active AND item.deprecated_at IS NULL
      AND (item.effective_from IS NULL OR item.effective_from<=CURRENT_DATE)
      AND (item.effective_until IS NULL OR item.effective_until>=CURRENT_DATE)
      AND (
        item.id=event.event_type_id
        OR (
          event.metadata IS NOT NULL
          AND NULLIF(btrim(event.metadata::jsonb ->> 'eventType'), '') IS NOT NULL
          AND lower(btrim(event.metadata::jsonb ->> 'eventType')) IN (
            lower(item.code), lower(item.name_es), lower(item.name_en), lower(COALESCE(item.current_slug, ''))
          )
        )
      )
  ) match ON TRUE
  WHERE event.event_type_id IS NULL
     OR event.metadata IS NOT NULL AND event.metadata::jsonb ? 'eventType'
)
SELECT jsonb_build_object(
  'report', 'social-event-type-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (
    WHERE candidate_count=1
      AND (original_event_type_id IS NULL OR original_event_type_id=target_id)
  ),
  'unresolved', count(*) FILTER (WHERE candidate_count=0),
  'ambiguous', count(*) FILTER (WHERE candidate_count>1),
  'conflicts', count(*) FILTER (
    WHERE original_event_type_id IS NOT NULL AND original_event_type_id<>target_id
  ),
  'rows', COALESCE(jsonb_agg(jsonb_build_object(
    'id', id,
    'originalEventType', original_event_type,
    'originalEventTypeId', original_event_type_id,
    'candidateCount', candidate_count,
    'targetEventTypeId', target_id,
    'evidence', 'unique canonical id or normalized code/name/slug match within event-types'
  ) ORDER BY id), '[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report', 'social-event-persisted-types',
  'items', (
    SELECT jsonb_agg(jsonb_build_object(
      'id', item.id, 'code', item.code, 'nameEs', item.name_es, 'nameEn', item.name_en
    ) ORDER BY item.sort_order, item.id)
    FROM event_type item
  ),
  'default', (
    SELECT jsonb_agg(jsonb_build_object(
      'catalogId', catalog_id, 'entityId', entity_id,
      'scopeKind', scope_kind, 'scopeId', scope_id
    ))
    FROM catalog_scoped_default
    WHERE active AND scope_kind='social-event' AND scope_id='global'
  )
);

ROLLBACK;
