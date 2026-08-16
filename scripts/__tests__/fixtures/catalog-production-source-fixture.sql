-- Synthetic, non-production source rows required to exercise the fail-closed
-- Records migration from the schema-only production baseline.

INSERT INTO public.party (display_name, is_org, created_at)
VALUES
  ('Catalog recovery fixture A', false, now()),
  ('Catalog recovery fixture B', false, now());

INSERT INTO public.user_credential (party_id, username, password_hash, active)
SELECT
  id,
  CASE display_name
    WHEN 'Catalog recovery fixture A' THEN 'catalog-recovery-a'
    ELSE 'catalog-recovery-b'
  END,
  'non-production-fixture-hash',
  true
FROM public.party
WHERE display_name IN ('Catalog recovery fixture A', 'Catalog recovery fixture B');

ALTER TABLE public.party_role DISABLE TRIGGER operations_party_role_scope_sync;

INSERT INTO public.party_role (party_id, role, active)
SELECT id, 'Admin', true
FROM public.party
WHERE display_name IN ('Catalog recovery fixture A', 'Catalog recovery fixture B');

ALTER TABLE public.party_role ENABLE TRIGGER operations_party_role_scope_sync;

INSERT INTO public.supported_currencies
  (currency_code, symbol, decimal_places, decimal_separator, thousands_separator, enabled)
VALUES ('USD', '$', 2, '.', ',', true)
ON CONFLICT (currency_code) DO NOTHING;

WITH fixture_party AS (
  INSERT INTO public.party (display_name, is_org, created_at)
  VALUES ('Catalog migration fixture', false, now())
  RETURNING id
)
INSERT INTO public.user_locale_preferences
  (user_id, locale, currency, timezone, country_code)
SELECT id, 'es', 'USD', 'America/Guayaquil', 'EC'
FROM fixture_party;

INSERT INTO public.cms_content
  (slug, locale, version, status, title, payload, published_at)
VALUES
  (
    'records-releases',
    'es',
    1,
    'published',
    'Lanzamientos de prueba',
    jsonb_build_object(
      'playlistUrl', 'https://open.spotify.com/playlist/fixtureplaylist01',
      'tracks', jsonb_build_array(jsonb_build_object(
        'title', 'Lanzamiento canónico',
        'artist', 'Artista de prueba',
        'spotifyUrl', 'https://open.spotify.com/track/1234567890123456789012',
        'durationMs', '180000',
        'sortOrder', 1
      ))
    ),
    '2026-08-14T00:00:00Z'
  ),
  (
    'records-recordings',
    'es',
    1,
    'published',
    'Grabaciones de prueba',
    jsonb_build_object(
      'channelUrl', 'https://www.youtube.com/@tdf-fixture',
      'videos', jsonb_build_array(jsonb_build_object(
        'title', 'Grabación canónica',
        'artist', 'Artista de prueba',
        'url', 'https://www.youtube.com/watch?v=recording01',
        'duration', '03:00',
        'sortOrder', 1
      ))
    ),
    '2026-08-14T00:00:00Z'
  ),
  (
    'records-sessions',
    'es',
    1,
    'published',
    'Sesiones de prueba',
    jsonb_build_object(
      'playlistUrl', 'https://www.youtube.com/playlist?list=PL_fixture123',
      'videos', jsonb_build_array(jsonb_build_object(
        'title', 'Sesión canónica',
        'guests', 'Invitados de prueba',
        'url', 'https://www.youtube.com/watch?v=session00123',
        'duration', '04:00',
        'sortOrder', 1
      ))
    ),
    '2026-08-14T00:00:00Z'
  );
