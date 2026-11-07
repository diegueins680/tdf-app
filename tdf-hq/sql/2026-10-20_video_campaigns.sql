-- Draft-only artist campaign planning. No provider activation or background sender.
CREATE TABLE IF NOT EXISTS video_campaign (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT NOT NULL REFERENCES party(id),
  campaign_key TEXT NOT NULL,
  revision BIGINT NOT NULL CHECK (revision > 0),
  document TEXT NOT NULL CHECK (octet_length(document) <= 131072),
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_video_campaign UNIQUE (artist_party_id, campaign_key),
  CONSTRAINT video_campaign_draft_only CHECK (document::jsonb->>'status' IN ('draft', 'ready'))
);
CREATE INDEX IF NOT EXISTS video_campaign_artist_updated
  ON video_campaign (artist_party_id, updated_at DESC);

CREATE TABLE IF NOT EXISTS video_campaign_conversion (
  id BIGSERIAL PRIMARY KEY,
  campaign_id BIGINT NOT NULL REFERENCES video_campaign(id),
  viewer_party_id BIGINT NOT NULL REFERENCES party(id),
  kind TEXT NOT NULL CHECK (kind IN ('registration', 'follow')),
  source TEXT NOT NULL CHECK (source IN ('instagram', 'tiktok')),
  piece TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_video_campaign_conversion UNIQUE (campaign_id, viewer_party_id, kind)
);
