\set ON_ERROR_STOP on

SELECT jsonb_build_object(
  'creatorBadgeTable',to_regclass('public.creator_badge'),
  'hasLegacyBadgeType',EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type'),
  'hasCanonicalBadgeTypeId',EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type_id'),
  'rowCount',CASE WHEN to_regclass('public.creator_badge') IS NULL THEN 0 ELSE (SELECT count(*) FROM creator_badge) END
) AS creator_badge_schema;

SELECT CASE WHEN EXISTS (
  SELECT 1 FROM information_schema.columns
  WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type'
) THEN $sql$
  SELECT badge.id AS creator_badge_id,badge.party_id,badge.club_id,
    badge.badge_type AS original_value,lower(btrim(badge.badge_type)) AS normalized_value,
    item.id AS target_badge_type_id,item.code AS target_code,
    CASE WHEN item.id IS NULL THEN 'unresolved' ELSE 'deterministic-exact-normalized-code' END AS resolution
  FROM creator_badge badge
  LEFT JOIN (VALUES
    ('50a00000-0000-4000-8000-000000000001'::uuid,'trendsetter'),
    ('50a00000-0000-4000-8000-000000000002'::uuid,'regular'),
    ('50a00000-0000-4000-8000-000000000003'::uuid,'og')
  ) item(id,code) ON item.code=lower(btrim(badge.badge_type))
  ORDER BY badge.id
$sql$ ELSE $sql$
  SELECT badge.id AS creator_badge_id,badge.party_id,badge.club_id,
    item.code AS original_value,item.code AS normalized_value,
    item.id AS target_badge_type_id,item.code AS target_code,
    CASE WHEN item.id IS NULL THEN 'invalid-canonical-reference' ELSE 'already-canonical' END AS resolution
  FROM creator_badge badge
  LEFT JOIN creator_badge_type item ON item.id=badge.badge_type_id
  ORDER BY badge.id
$sql$ END
\gexec

SELECT CASE WHEN EXISTS (
  SELECT 1 FROM information_schema.columns
  WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type'
) THEN $sql$
  SELECT jsonb_build_object(
    'rows',count(*),
    'unresolved',count(*) FILTER (WHERE item.id IS NULL),
    'normalizedVariants',count(*) FILTER (WHERE badge.badge_type<>lower(btrim(badge.badge_type))),
    'duplicateCanonicalAssignments',count(*)-count(DISTINCT (badge.party_id,badge.club_id,item.id))
  ) AS creator_badge_dry_run
  FROM creator_badge badge
  LEFT JOIN (VALUES
    ('50a00000-0000-4000-8000-000000000001'::uuid,'trendsetter'),
    ('50a00000-0000-4000-8000-000000000002'::uuid,'regular'),
    ('50a00000-0000-4000-8000-000000000003'::uuid,'og')
  ) item(id,code) ON item.code=lower(btrim(badge.badge_type))
$sql$ ELSE $sql$
  SELECT jsonb_build_object(
    'rows',count(*),
    'unresolved',count(*) FILTER (WHERE item.id IS NULL),
    'normalizedVariants',0,
    'duplicateCanonicalAssignments',count(*)-count(DISTINCT (badge.party_id,badge.club_id,badge.badge_type_id))
  ) AS creator_badge_dry_run
  FROM creator_badge badge
  LEFT JOIN creator_badge_type item ON item.id=badge.badge_type_id
$sql$ END
\gexec
