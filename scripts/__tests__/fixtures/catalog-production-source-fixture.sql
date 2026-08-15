-- Synthetic, non-production source rows required to exercise the fail-closed
-- Records migration from the schema-only production baseline.

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
