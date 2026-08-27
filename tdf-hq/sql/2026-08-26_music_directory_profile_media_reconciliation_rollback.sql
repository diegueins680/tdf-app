-- Remove only the packaged media association created by the forward migration.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

WITH targets AS (
  SELECT profile.id,
    coalesce((
      SELECT jsonb_agg(entry.value ORDER BY entry.ordinality)
      FROM jsonb_array_elements(
        CASE WHEN jsonb_typeof(profile.portfolio) = 'array' THEN profile.portfolio ELSE '[]'::jsonb END
      ) WITH ORDINALITY entry(value, ordinality)
      WHERE NOT (
        entry.value->>'source' = 'packaged-profile-media'
        AND entry.value->>'url' = '/assets/serve/directory/profiles/diego-saa-bajista.webp'
      )
    ), '[]'::jsonb) AS portfolio
  FROM directory_profile profile
  WHERE profile.slug = 'diego-saa-bajista'
    AND EXISTS (
      SELECT 1
      FROM jsonb_array_elements(
        CASE WHEN jsonb_typeof(profile.portfolio) = 'array' THEN profile.portfolio ELSE '[]'::jsonb END
      ) entry(value)
      WHERE entry.value->>'source' = 'packaged-profile-media'
        AND entry.value->>'url' = '/assets/serve/directory/profiles/diego-saa-bajista.webp'
    )
), updated AS (
  UPDATE directory_profile profile
  SET portfolio = target.portfolio,
      updated_at = now(),
      version = profile.version + 1
  FROM targets target
  WHERE profile.id = target.id
  RETURNING profile.id
)
SELECT directory_refresh_profile_search(id) FROM updated;

COMMIT;
