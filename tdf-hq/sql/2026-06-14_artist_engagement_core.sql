-- Artist profile discovery and fan engagement core.

CREATE UNIQUE INDEX IF NOT EXISTS idx_artist_profile_slug
  ON artist_profile(slug)
  WHERE slug IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_artist_profile_genres
  ON artist_profile(genres);

CREATE INDEX IF NOT EXISTS idx_artist_follow_artist
  ON fan_follow(artist_party_id);

CREATE TABLE IF NOT EXISTS engagement_event (
  id BIGSERIAL PRIMARY KEY,
  actor_party_id BIGINT REFERENCES party(id),
  target_artist_id BIGINT REFERENCES party(id),
  entity_type TEXT NOT NULL,
  entity_id BIGINT,
  event_type TEXT NOT NULL,
  metadata TEXT,
  created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_engagement_artist_created
  ON engagement_event(target_artist_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_engagement_actor_created
  ON engagement_event(actor_party_id, created_at DESC);
