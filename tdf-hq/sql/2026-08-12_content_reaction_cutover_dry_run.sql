\set ON_ERROR_STOP on

SELECT (to_regclass('public.content_reaction') IS NOT NULL)::int AS has_legacy_table,
  (to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL)::int AS has_preserved_source,
  (to_regclass('public.content_reaction_type') IS NOT NULL)::int AS has_catalog_table
\gset

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '2s';

SELECT jsonb_build_object(
  'legacyTablePresent', :has_legacy_table::int=1,
  'preservedSourcePresent', :has_preserved_source::int=1,
  'catalogTablePresent', :has_catalog_table::int=1,
  'postTargetTablePresent', to_regclass('public.fan_club_post') IS NOT NULL,
  'memoryTargetTablePresent', to_regclass('public.fan_club_memory') IS NOT NULL
) AS schema_status;

\if :has_legacy_table
WITH candidates(code, id) AS (
  VALUES
    ('fire','50900000-0000-4000-8000-000000000001'::uuid),
    ('heart','50900000-0000-4000-8000-000000000002'::uuid),
    ('clap','50900000-0000-4000-8000-000000000003'::uuid),
    ('mic_drop','50900000-0000-4000-8000-000000000004'::uuid),
    ('skull','50900000-0000-4000-8000-000000000005'::uuid)
), source AS (
  SELECT reaction.target_type, reaction.target_id, reaction.reactor_party_id,
    reaction.reaction AS original_value, lower(btrim(reaction.reaction)) AS normalized_value,
    candidate.id AS target_reaction_type_id,
    CASE reaction.target_type
      WHEN 'post' THEN EXISTS (SELECT 1 FROM fan_club_post target WHERE target.id=reaction.target_id)
      WHEN 'memory' THEN EXISTS (SELECT 1 FROM fan_club_memory target WHERE target.id=reaction.target_id)
      ELSE FALSE
    END AS target_exists
  FROM content_reaction reaction
  LEFT JOIN candidates candidate ON candidate.code=lower(btrim(reaction.reaction))
)
SELECT target_type, target_id, reactor_party_id, original_value, normalized_value,
  target_reaction_type_id,
  CASE
    WHEN target_type NOT IN ('post','memory') THEN 'unsupported-target-type'
    WHEN NOT target_exists THEN 'missing-target'
    WHEN target_reaction_type_id IS NULL THEN 'unresolved-reaction'
    ELSE 'mapped'
  END AS decision
FROM source
ORDER BY target_type, target_id, reactor_party_id;

SELECT jsonb_build_object(
  'sourceRows', count(*),
  'postRows', count(*) FILTER (WHERE target_type='post'),
  'memoryRows', count(*) FILTER (WHERE target_type='memory'),
  'unsupportedTargetRows', count(*) FILTER (WHERE target_type NOT IN ('post','memory')),
  'unresolvedReactionRows', count(*) FILTER (WHERE lower(btrim(reaction)) NOT IN ('fire','heart','clap','mic_drop','skull'))
) AS source_counts
FROM content_reaction;
\else
\if :has_preserved_source
SELECT jsonb_build_object(
  'sourceRows', count(*),
  'postRows', count(*) FILTER (WHERE target_type='post'),
  'memoryRows', count(*) FILTER (WHERE target_type='memory'),
  'note', 'source was already preserved by an earlier apply'
) AS source_counts
FROM catalog_content_reaction_legacy_source;
\else
SELECT jsonb_build_object('sourceRows',0,'note','clean installation without legacy content reactions') AS source_counts;
\endif
\endif

ROLLBACK;
