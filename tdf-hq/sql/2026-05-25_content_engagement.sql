-- Content Engagement System
-- Adds reactions, notifications, creator badges, and content boosting

-- ============================================================================
-- Phase 1: Content Reactions
-- ============================================================================

CREATE TABLE IF NOT EXISTS content_reaction (
    target_type TEXT NOT NULL CHECK (target_type IN ('post', 'memory', 'release')),
    target_id BIGINT NOT NULL,
    reactor_party_id BIGINT NOT NULL REFERENCES party(id),
    reaction TEXT NOT NULL CHECK (reaction IN ('fire', 'heart', 'clap', 'mic_drop', 'skull')),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (target_type, target_id, reactor_party_id)
);

CREATE INDEX idx_content_reaction_target ON content_reaction(target_type, target_id);
CREATE INDEX idx_content_reaction_reactor ON content_reaction(reactor_party_id);
CREATE INDEX idx_content_reaction_created ON content_reaction(created_at);

-- ============================================================================
-- Phase 3: In-App Notifications
-- ============================================================================

CREATE TABLE IF NOT EXISTS notification (
    id BIGSERIAL PRIMARY KEY,
    recipient_party_id BIGINT NOT NULL REFERENCES party(id),
    notif_type TEXT NOT NULL CHECK (notif_type IN ('reaction_received', 'post_trending', 'weekly_top', 'artist_liked')),
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    target_type TEXT,
    target_id BIGINT,
    is_read BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_notification_recipient ON notification(recipient_party_id, is_read, created_at DESC);

-- ============================================================================
-- Phase 4: Creator Badges
-- ============================================================================

CREATE TABLE IF NOT EXISTS creator_badge (
    id BIGSERIAL PRIMARY KEY,
    party_id BIGINT NOT NULL REFERENCES party(id),
    club_id BIGINT NOT NULL REFERENCES fan_club(id) ON DELETE CASCADE,
    badge_type TEXT NOT NULL CHECK (badge_type IN ('trendsetter', 'regular', 'og')),
    awarded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    expires_at TIMESTAMPTZ,
    UNIQUE (party_id, club_id, badge_type)
);

CREATE INDEX idx_creator_badge_club ON creator_badge(club_id, badge_type);

-- ============================================================================
-- Phase 5: Content Boost
-- ============================================================================

CREATE TABLE IF NOT EXISTS boosted_content (
    id BIGSERIAL PRIMARY KEY,
    target_type TEXT NOT NULL,
    target_id BIGINT NOT NULL,
    club_id BIGINT NOT NULL REFERENCES fan_club(id) ON DELETE CASCADE,
    total_reactions INTEGER NOT NULL,
    boosted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    surfaced_to_artist BOOLEAN NOT NULL DEFAULT FALSE,
    UNIQUE (target_type, target_id)
);

CREATE INDEX idx_boosted_content_club ON boosted_content(club_id, boosted_at DESC);
