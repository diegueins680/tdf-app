-- Keep saved preferences while excluding confirmed invalid legacy sources.
-- Canonical authorization remains delegated to music_public_asset_accessible.
BEGIN;
SET LOCAL lock_timeout='10s';
CREATE OR REPLACE FUNCTION music_artist_featured_track_eligible(
  target_artist BIGINT, target_recording UUID, target_legacy BIGINT, territory TEXT
) RETURNS BOOLEAN LANGUAGE sql STABLE AS $$
  SELECT CASE
    WHEN target_recording IS NOT NULL AND target_legacy IS NULL THEN EXISTS (
      SELECT 1 FROM music_public_release publication
      JOIN music_release_track track ON track.release_version_id=publication.release_version_id
      JOIN music_asset asset ON asset.release_version_id=publication.release_version_id
        AND asset.recording_id=track.recording_id
      WHERE publication.artist_party_id=target_artist AND track.recording_id=target_recording
        AND asset.asset_role IN ('stream_audio','preview_audio')
        AND music_public_asset_accessible(asset.id,territory)
    )
    WHEN target_legacy IS NOT NULL AND target_recording IS NULL THEN EXISTS (
      SELECT 1 FROM artist_release legacy WHERE legacy.id=target_legacy
        AND legacy.artist_party_id=target_artist
        AND (music_media_eligible('legacy',legacy.id::text||':spotify')
          OR music_media_eligible('legacy',legacy.id::text||':youtube'))
    )
    ELSE FALSE
  END
$$;
COMMIT;
