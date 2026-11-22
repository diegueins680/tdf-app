-- Friend suggestions by personalized PageRank (COMMUNITY-FRIEND-SUGGESTION-001,
-- formal/system/friend-suggestions-pagerank.md). Additive: three derived tables
-- written by the nightly run.
-- Nothing reads them while FRIEND_SUGGESTIONS_PAGERANK_ENABLED is false.
BEGIN;

CREATE TABLE IF NOT EXISTS social_suggestion_run (
  id BIGSERIAL PRIMARY KEY,
  started_at TIMESTAMPTZ NOT NULL,
  completed_at TIMESTAMPTZ NOT NULL,
  damping DOUBLE PRECISION NOT NULL CHECK (damping > 0 AND damping < 1),
  iterations INT NOT NULL CHECK (iterations > 0),
  viewer_count INT NOT NULL CHECK (viewer_count >= 0),
  CHECK (completed_at >= started_at)
);

-- One row per computed viewer. support holds every party that carried score
-- for the viewer, the viewer included.
CREATE TABLE IF NOT EXISTS social_suggestion_viewer (
  viewer_id BIGINT PRIMARY KEY REFERENCES party(id) ON DELETE CASCADE,
  run_id BIGINT NOT NULL REFERENCES social_suggestion_run(id) ON DELETE CASCADE,
  support BIGINT[] NOT NULL CHECK (cardinality(support) >= 1)
);

CREATE INDEX IF NOT EXISTS social_suggestion_viewer_run_idx
  ON social_suggestion_viewer (run_id);

CREATE TABLE IF NOT EXISTS social_suggestion_score (
  viewer_id BIGINT NOT NULL REFERENCES social_suggestion_viewer(viewer_id) ON DELETE CASCADE,
  candidate_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  position INT NOT NULL CHECK (position >= 1),
  score DOUBLE PRECISION NOT NULL CHECK (score > 0 AND score <= 1),
  PRIMARY KEY (viewer_id, candidate_id),
  UNIQUE (viewer_id, position),
  CHECK (viewer_id <> candidate_id)
);

COMMIT;
