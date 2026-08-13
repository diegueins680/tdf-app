\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

SELECT jsonb_build_object(
  'report', 'feedback-catalog-schema-readiness',
  'categoryTablePresent', to_regclass('public.feedback_category') IS NOT NULL,
  'severityTablePresent', to_regclass('public.feedback_severity') IS NOT NULL,
  'categoryIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='feedback' AND column_name='category_id'
  ),
  'severityIdPresent', EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema='public' AND table_name='feedback' AND column_name='severity_id'
  )
);

WITH resolved AS (
  SELECT feedback.id,
    feedback.category AS original_category,
    feedback.severity AS original_severity,
    feedback.category_id AS original_category_id,
    feedback.severity_id AS original_severity_id,
    category_match.candidate_count AS category_candidates,
    category_match.target_id AS target_category_id,
    severity_match.candidate_count AS severity_candidates,
    severity_match.target_id AS target_severity_id
  FROM feedback
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      (array_agg(item.id ORDER BY item.id))[1] AS target_id
    FROM feedback_category item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id
      AND catalog.code='feedback-categories' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id
      AND state.workflow_id=catalog.workflow_id
      AND state.code='published' AND state.active
    WHERE item.active AND item.deprecated_at IS NULL
      AND (
        item.id=feedback.category_id
        OR (feedback.category IS NOT NULL AND lower(btrim(feedback.category)) IN (
          lower(item.code), lower(item.name_es), lower(item.name_en)
        ))
      )
  ) category_match ON TRUE
  LEFT JOIN LATERAL (
    SELECT count(*) AS candidate_count,
      (array_agg(item.id ORDER BY item.id))[1] AS target_id
    FROM feedback_severity item
    JOIN catalog_definition catalog ON catalog.id=item.catalog_id
      AND catalog.code='feedback-severities' AND catalog.active
    JOIN workflow_state state ON state.id=item.workflow_state_id
      AND state.workflow_id=catalog.workflow_id
      AND state.code='published' AND state.active
    WHERE item.active AND item.deprecated_at IS NULL
      AND (
        item.id=feedback.severity_id
        OR (feedback.severity IS NOT NULL AND lower(btrim(feedback.severity)) IN (
          lower(item.code), lower(item.name_es), lower(item.name_en)
        ))
      )
  ) severity_match ON TRUE
)
SELECT jsonb_build_object(
  'report', 'feedback-category-severity-map',
  'sourceRows', count(*),
  'mapped', count(*) FILTER (
    WHERE category_candidates=1 AND severity_candidates=1
      AND (original_category_id IS NULL OR original_category_id=target_category_id)
      AND (original_severity_id IS NULL OR original_severity_id=target_severity_id)
  ),
  'unresolved', count(*) FILTER (WHERE category_candidates=0 OR severity_candidates=0),
  'ambiguous', count(*) FILTER (WHERE category_candidates>1 OR severity_candidates>1),
  'conflicts', count(*) FILTER (
    WHERE (original_category_id IS NOT NULL AND original_category_id<>target_category_id)
       OR (original_severity_id IS NOT NULL AND original_severity_id<>target_severity_id)
  ),
  'rows', COALESCE(jsonb_agg(jsonb_build_object(
    'id', id,
    'originalCategory', original_category,
    'originalSeverity', original_severity,
    'categoryCandidates', category_candidates,
    'severityCandidates', severity_candidates,
    'targetCategoryId', target_category_id,
    'targetSeverityId', target_severity_id,
    'evidence', 'unique normalized code/name match within the typed active published catalog'
  ) ORDER BY id), '[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report', 'feedback-persisted-options',
  'categories', (SELECT jsonb_agg(jsonb_build_object('id', id, 'code', code, 'nameEs', name_es, 'nameEn', name_en) ORDER BY sort_order, id) FROM feedback_category),
  'severities', (SELECT jsonb_agg(jsonb_build_object('id', id, 'code', code, 'nameEs', name_es, 'nameEn', name_en) ORDER BY sort_order, id) FROM feedback_severity),
  'defaults', (SELECT jsonb_agg(jsonb_build_object('catalogId', catalog_id, 'entityId', entity_id, 'scopeKind', scope_kind, 'scopeId', scope_id) ORDER BY scope_kind) FROM catalog_scoped_default WHERE active AND scope_kind IN ('feedback-category','feedback-severity'))
);

ROLLBACK;
