\set ON_ERROR_STOP on

SELECT EXISTS (
  SELECT 1 FROM information_schema.columns
  WHERE table_schema='public' AND table_name='event_moment_reaction' AND column_name='id'
)::int AS has_reaction_id,
EXISTS (
  SELECT 1 FROM information_schema.columns
  WHERE table_schema='public' AND table_name='event_moment_reaction' AND column_name='reaction_type_id'
)::int AS has_reaction_type_id
\gset

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '2s';

SELECT jsonb_build_object(
  'consumerTablePresent', to_regclass('public.event_moment_reaction') IS NOT NULL,
  'catalogTablePresent', to_regclass('public.reaction_type') IS NOT NULL,
  'catalogDefinitionPresent', EXISTS (
    SELECT 1 FROM catalog_definition WHERE code='reaction-types' AND active
  ),
  'idColumnPresent', :has_reaction_id::int=1,
  'reactionTypeIdColumnPresent', :has_reaction_type_id::int=1,
  'totalRows', (SELECT count(*) FROM event_moment_reaction)
) AS schema_and_counts;

\if :has_reaction_type_id
SELECT jsonb_build_object(
  'sourceRows', (SELECT count(*) FROM event_moment_reaction WHERE reaction IS NOT NULL OR reaction_type_id IS NULL),
  'canonicalRows', (SELECT count(*) FROM event_moment_reaction WHERE reaction IS NULL AND reaction_type_id IS NOT NULL)
) AS consumer_counts;
\else
SELECT jsonb_build_object(
  'sourceRows', (SELECT count(*) FROM event_moment_reaction),
  'canonicalRows', 0,
  'note', 'reaction_type_id will be added by the apply migration'
) AS consumer_counts;
\endif

\if :has_reaction_id
\if :has_reaction_type_id
WITH candidate_items AS (
  SELECT item.id, lower(item.code) AS code
  FROM reaction_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
    AND catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id
    AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  WHERE item.active AND to_jsonb(item)->>'deprecated_at' IS NULL
  UNION ALL
  SELECT seed.id, seed.code
  FROM (VALUES
    ('50800000-0000-4000-8000-000000000001'::uuid, 'fire'),
    ('50800000-0000-4000-8000-000000000002'::uuid, 'love'),
    ('50800000-0000-4000-8000-000000000003'::uuid, 'applause')
  ) seed(id, code)
  JOIN catalog_definition catalog ON catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.workflow_id=catalog.workflow_id
    AND state.code='published' AND state.active
  WHERE NOT EXISTS (SELECT 1 FROM reaction_type existing WHERE lower(existing.code)=seed.code)
), source AS (
  SELECT reaction.id, reaction.moment_id, reaction.reactor_party_id,
    reaction.reaction AS original_value,
    lower(btrim(reaction.reaction)) AS normalized_value,
    count(item.id) FILTER (WHERE item.id IS NOT NULL) AS candidate_count,
    (array_agg(item.id ORDER BY item.id) FILTER (WHERE item.id IS NOT NULL))[1] AS target_id
  FROM event_moment_reaction reaction
  LEFT JOIN candidate_items item ON item.code=CASE lower(btrim(reaction.reaction))
    WHEN 'heart' THEN 'love'
    WHEN 'clap' THEN 'applause'
    ELSE lower(btrim(reaction.reaction))
  END
  WHERE reaction.reaction IS NOT NULL OR reaction.reaction_type_id IS NULL
  GROUP BY reaction.id, reaction.moment_id, reaction.reactor_party_id, reaction.reaction
)
SELECT id, moment_id, reactor_party_id, original_value, normalized_value,
  candidate_count, target_id,
  CASE
    WHEN original_value IS NULL THEN 'missing-legacy-value'
    WHEN candidate_count=0 THEN 'unresolved'
    WHEN candidate_count>1 THEN 'ambiguous'
    ELSE 'mapped'
  END AS decision
FROM source
ORDER BY id;
\else
WITH candidate_items AS (
  SELECT item.id, lower(item.code) AS code
  FROM reaction_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  WHERE item.active AND to_jsonb(item)->>'deprecated_at' IS NULL
  UNION ALL
  SELECT seed.id, seed.code FROM (VALUES
    ('50800000-0000-4000-8000-000000000001'::uuid, 'fire'),
    ('50800000-0000-4000-8000-000000000002'::uuid, 'love'),
    ('50800000-0000-4000-8000-000000000003'::uuid, 'applause')
  ) seed(id, code)
  JOIN catalog_definition catalog ON catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  WHERE NOT EXISTS (SELECT 1 FROM reaction_type existing WHERE lower(existing.code)=seed.code)
), source AS (
  SELECT reaction.id, reaction.moment_id, reaction.reactor_party_id,
    reaction.reaction AS original_value,
    lower(btrim(reaction.reaction)) AS normalized_value,
    count(item.id) FILTER (WHERE item.id IS NOT NULL) AS candidate_count,
    (array_agg(item.id ORDER BY item.id) FILTER (WHERE item.id IS NOT NULL))[1] AS target_id
  FROM event_moment_reaction reaction
  LEFT JOIN candidate_items item ON item.code=CASE lower(btrim(reaction.reaction))
    WHEN 'heart' THEN 'love' WHEN 'clap' THEN 'applause' ELSE lower(btrim(reaction.reaction)) END
  GROUP BY reaction.id, reaction.moment_id, reaction.reactor_party_id, reaction.reaction
)
SELECT id, moment_id, reactor_party_id, original_value, normalized_value, candidate_count, target_id,
  CASE WHEN original_value IS NULL THEN 'missing-legacy-value' WHEN candidate_count=0 THEN 'unresolved' WHEN candidate_count>1 THEN 'ambiguous' ELSE 'mapped' END AS decision
FROM source ORDER BY id;
\endif
\else
WITH candidate_items AS (
  SELECT item.id, lower(item.code) AS code
  FROM reaction_type item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id AND catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.id=item.workflow_state_id AND state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  WHERE item.active AND to_jsonb(item)->>'deprecated_at' IS NULL
  UNION ALL
  SELECT seed.id, seed.code FROM (VALUES
    ('50800000-0000-4000-8000-000000000001'::uuid, 'fire'),
    ('50800000-0000-4000-8000-000000000002'::uuid, 'love'),
    ('50800000-0000-4000-8000-000000000003'::uuid, 'applause')
  ) seed(id, code)
  JOIN catalog_definition catalog ON catalog.code='reaction-types' AND catalog.active
  JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published' AND state.active
  WHERE NOT EXISTS (SELECT 1 FROM reaction_type existing WHERE lower(existing.code)=seed.code)
), source AS (
  SELECT reaction.moment_id::text || ':' || reaction.reaction || ':' || reaction.reactor_party_id AS source_identity,
    reaction.moment_id, reaction.reactor_party_id, reaction.reaction AS original_value,
    lower(btrim(reaction.reaction)) AS normalized_value,
    count(item.id) FILTER (WHERE item.id IS NOT NULL) AS candidate_count,
    (array_agg(item.id ORDER BY item.id) FILTER (WHERE item.id IS NOT NULL))[1] AS target_id
  FROM event_moment_reaction reaction
  LEFT JOIN candidate_items item ON item.code=CASE lower(btrim(reaction.reaction))
    WHEN 'heart' THEN 'love' WHEN 'clap' THEN 'applause' ELSE lower(btrim(reaction.reaction)) END
  GROUP BY reaction.moment_id, reaction.reactor_party_id, reaction.reaction
)
SELECT source_identity, moment_id, reactor_party_id, original_value, normalized_value, candidate_count, target_id,
  CASE WHEN original_value IS NULL THEN 'missing-legacy-value' WHEN candidate_count=0 THEN 'unresolved' WHEN candidate_count>1 THEN 'ambiguous' ELSE 'mapped' END AS decision
FROM source ORDER BY source_identity;
\endif

SELECT id, code, emoji, name_es, name_en, sort_order, active, workflow_state_id,
  to_jsonb(reaction_type)->>'deprecated_at' AS deprecated_at,
  to_jsonb(reaction_type)->>'replacement_id' AS replacement_id,
  version
FROM reaction_type
ORDER BY sort_order, code, id;

ROLLBACK;
