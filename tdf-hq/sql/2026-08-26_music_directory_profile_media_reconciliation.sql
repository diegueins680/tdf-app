-- Reconcile packaged profile media that already has a verified legacy source
-- but was never associated with its canonical directory profile.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

WITH packaged_media(slug, asset_url) AS (
  VALUES
    ('diego-saa-bajista', '/assets/serve/directory/profiles/diego-saa-bajista.webp')
), candidates AS (
  SELECT profile.id, media.asset_url
  FROM directory_profile profile
  JOIN packaged_media media ON media.slug = profile.slug
  WHERE directory_profile_primary_image_url(profile.portfolio) IS NULL
    AND EXISTS (
      SELECT 1
      FROM artist_profile artist
      WHERE artist.artist_party_id = profile.subject_party_id
        AND nullif(btrim(artist.hero_image_url), '') IS NOT NULL
    )
), updated AS (
  UPDATE directory_profile profile
  SET portfolio = coalesce(profile.portfolio, '[]'::jsonb) || jsonb_build_array(
        jsonb_build_object(
          'itemType', 'image',
          'title', 'Foto de perfil',
          'url', candidate.asset_url,
          'source', 'packaged-profile-media'
        )
      ),
      updated_at = now(),
      version = profile.version + 1
  FROM candidates candidate
  WHERE profile.id = candidate.id
  RETURNING profile.id
)
SELECT directory_refresh_profile_search(id) FROM updated;

COMMIT;
