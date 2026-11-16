BEGIN;

-- A preference is a reference, never a copy of a recording, release or grant.
CREATE TABLE IF NOT EXISTS music_artist_featured_track (
  artist_party_id BIGINT PRIMARY KEY REFERENCES party(id) ON DELETE CASCADE,
  recording_id UUID REFERENCES music_recording(id) ON DELETE CASCADE,
  legacy_release_id BIGINT REFERENCES artist_release(id) ON DELETE CASCADE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT music_artist_featured_track_one_source
    CHECK (num_nonnulls(recording_id, legacy_release_id) = 1)
);

-- Reuse the public asset gate, including preview, territory and withdrawal rules.
-- Legacy references remain subject to the existing direct-media classifier and
-- native playback validation. Selecting a reference grants no media access.
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
        AND (NULLIF(btrim(legacy.spotify_url),'') IS NOT NULL
          OR NULLIF(btrim(legacy.youtube_url),'') IS NOT NULL)
    )
    ELSE FALSE
  END
$$;

CREATE OR REPLACE FUNCTION music_set_artist_featured_track(
  actor BIGINT, target_recording UUID, target_legacy BIGINT, territory TEXT
) RETURNS BOOLEAN LANGUAGE plpgsql AS $$
BEGIN
  IF actor IS NULL OR actor<=0 THEN RETURN FALSE; END IF;
  IF target_recording IS NULL AND target_legacy IS NULL THEN
    DELETE FROM music_artist_featured_track WHERE artist_party_id=actor;
    RETURN TRUE;
  END IF;
  IF NOT music_artist_featured_track_eligible(actor,target_recording,target_legacy,territory) THEN
    RETURN FALSE;
  END IF;
  INSERT INTO music_artist_featured_track(artist_party_id,recording_id,legacy_release_id)
    VALUES(actor,target_recording,target_legacy)
    ON CONFLICT(artist_party_id) DO UPDATE SET
      recording_id=EXCLUDED.recording_id,legacy_release_id=EXCLUDED.legacy_release_id,
      updated_at=clock_timestamp();
  RETURN TRUE;
END
$$;

COMMIT;
