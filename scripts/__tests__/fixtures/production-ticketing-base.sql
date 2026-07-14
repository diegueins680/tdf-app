-- Minimal representation of the production ticket/event schema immediately
-- before the guarded 2026-07-12 release migrations. This intentionally omits
-- every column/table introduced by the migrations under test.

CREATE TABLE party (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE notification (
    id BIGSERIAL PRIMARY KEY,
    recipient_party_id BIGINT NOT NULL REFERENCES party(id),
    notif_type TEXT NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    target_type TEXT,
    target_id BIGINT,
    is_read BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_notification_recipient
    ON notification (recipient_party_id, is_read, created_at DESC);

CREATE TABLE social_event (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE venue (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE social_artist_profile (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE event_ticket_tier (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    code VARCHAR NOT NULL,
    name VARCHAR NOT NULL,
    description VARCHAR,
    price_cents BIGINT NOT NULL,
    currency VARCHAR NOT NULL,
    quantity_total BIGINT NOT NULL,
    quantity_sold BIGINT NOT NULL,
    sales_start TIMESTAMPTZ,
    sales_end TIMESTAMPTZ,
    is_active BOOLEAN NOT NULL,
    position BIGINT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_event_ticket_tier_code UNIQUE (event_id, code)
);

CREATE TABLE event_ticket_order (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    tier_id BIGINT NOT NULL REFERENCES event_ticket_tier(id),
    buyer_party_id VARCHAR,
    buyer_name VARCHAR,
    buyer_email VARCHAR,
    quantity BIGINT NOT NULL,
    amount_cents BIGINT NOT NULL,
    currency VARCHAR NOT NULL,
    status VARCHAR NOT NULL,
    metadata VARCHAR,
    purchased_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE event_ticket (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    tier_ref_id BIGINT NOT NULL REFERENCES event_ticket_tier(id),
    order_ref_id BIGINT NOT NULL REFERENCES event_ticket_order(id),
    holder_name VARCHAR,
    holder_email VARCHAR,
    code VARCHAR NOT NULL,
    status VARCHAR NOT NULL,
    checked_in_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_event_ticket_code UNIQUE (code)
);
