-- COMMUNITY-ARTIST-SIMILARITY-001 (formal/system/artist-similarity.md).
-- Storage for the nightly multi-signal artist similarity, and two partial
-- indexes for the per-fan signal reads of a recommendation request.
-- Additive. Nothing reads these tables while
-- FAN_MULTI_SIGNAL_RECOMMENDATIONS_ENABLED is off.
BEGIN;
SET LOCAL lock_timeout = '10s';

-- One row per completed run. A run inserts its generation, norms and pairs in
-- a single transaction, so a visible generation is always complete.
CREATE TABLE IF NOT EXISTS artist_similarity_generation (
  id BIGSERIAL PRIMARY KEY,
  snapshot_at TIMESTAMPTZ NOT NULL,
  completed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  artist_count INTEGER NOT NULL CHECK (artist_count >= 0),
  pair_count INTEGER NOT NULL CHECK (pair_count >= 0),
  CHECK (completed_at >= snapshot_at)
);

-- Squared length of an artist's weighted fan vector.
CREATE TABLE IF NOT EXISTS artist_similarity_norm (
  generation BIGINT NOT NULL REFERENCES artist_similarity_generation(id) ON DELETE CASCADE,
  artist_party_id BIGINT NOT NULL,
  norm2 BIGINT NOT NULL CHECK (norm2 > 0),
  PRIMARY KEY (generation, artist_party_id)
);

-- Shared evidence of two artists, stored once with the smaller id first and
-- only when at least two distinct fans support the pair.
CREATE TABLE IF NOT EXISTS artist_similarity_pair (
  generation BIGINT NOT NULL REFERENCES artist_similarity_generation(id) ON DELETE CASCADE,
  artist_a BIGINT NOT NULL,
  artist_b BIGINT NOT NULL,
  dot BIGINT NOT NULL CHECK (dot > 0),
  support INTEGER NOT NULL CHECK (support >= 2),
  PRIMARY KEY (generation, artist_a, artist_b),
  CHECK (artist_a < artist_b)
);
CREATE INDEX IF NOT EXISTS artist_similarity_pair_by_b
  ON artist_similarity_pair(generation, artist_b);

-- A request reads the caller's own eligible plays and accepted RSVPs; neither
-- table had an index that starts with the party.
CREATE INDEX IF NOT EXISTS idx_music_playback_party_eligible
  ON music_playback_event(party_id, occurred_at)
  WHERE eligible_play AND party_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS ix_event_rsvp_party_accepted
  ON event_rsvp(party_id)
  WHERE status = 'accepted';
COMMIT;
