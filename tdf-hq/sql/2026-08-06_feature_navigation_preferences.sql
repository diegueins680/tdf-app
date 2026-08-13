-- Forward migration: cross-device feature favorites, pins, and recent visits.
CREATE TABLE IF NOT EXISTS feature_navigation_preferences (
  id BIGSERIAL PRIMARY KEY,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  feature_id TEXT NOT NULL,
  favorite BOOLEAN NOT NULL DEFAULT FALSE,
  pinned BOOLEAN NOT NULL DEFAULT FALSE,
  pin_order INTEGER NULL,
  last_visited_at TIMESTAMPTZ NULL,
  use_count INTEGER NOT NULL DEFAULT 0,
  updated_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT feature_navigation_preferences_party_feature_unique UNIQUE (party_id, feature_id),
  CONSTRAINT feature_navigation_preferences_feature_id_check CHECK (
    length(feature_id) BETWEEN 1 AND 160
    AND feature_id !~ '[[:cntrl:]]'
  ),
  CONSTRAINT feature_navigation_preferences_pin_order_check CHECK (
    (pinned AND pin_order BETWEEN 0 AND 1000)
    OR (NOT pinned AND pin_order IS NULL)
  ),
  CONSTRAINT feature_navigation_preferences_use_count_check CHECK (use_count >= 0)
);

CREATE INDEX IF NOT EXISTS feature_navigation_preferences_pinned_idx
  ON feature_navigation_preferences (party_id, pinned, pin_order);

CREATE INDEX IF NOT EXISTS feature_navigation_preferences_recent_idx
  ON feature_navigation_preferences (party_id, last_visited_at DESC);

-- Rollback (manual, only after exporting affected rows):
-- DROP TABLE IF EXISTS feature_navigation_preferences;
