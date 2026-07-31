-- Consent-gated, operator-activated WhatsApp campaign sequences.
-- This migration is intentionally additive because production keeps
-- RUN_MIGRATIONS=false and applies reviewed runtime schema explicitly.

BEGIN;

CREATE TABLE IF NOT EXISTS campaign_automation (
    id BIGSERIAL PRIMARY KEY,
    campaign_id BIGINT NOT NULL REFERENCES campaign(id),
    template_key TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'draft',
    start_at TIMESTAMPTZ NOT NULL,
    daily_limit INTEGER NOT NULL DEFAULT 20,
    last_run_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_campaign_automation_campaign UNIQUE (campaign_id),
    CONSTRAINT unique_campaign_automation_template UNIQUE (template_key),
    CONSTRAINT campaign_automation_daily_limit_check
        CHECK (daily_limit BETWEEN 1 AND 100),
    CONSTRAINT campaign_automation_status_check
        CHECK (status IN ('draft', 'active', 'paused', 'completed'))
);

CREATE INDEX IF NOT EXISTS index_campaign_automation_status
    ON campaign_automation (status, start_at);

CREATE TABLE IF NOT EXISTS campaign_automation_step (
    id BIGSERIAL PRIMARY KEY,
    automation_id BIGINT NOT NULL REFERENCES campaign_automation(id) ON DELETE CASCADE,
    position INTEGER NOT NULL,
    delay_days INTEGER NOT NULL,
    channel TEXT NOT NULL DEFAULT 'whatsapp',
    provider_template_name TEXT NOT NULL,
    language_code TEXT NOT NULL DEFAULT 'es',
    body TEXT NOT NULL,
    cta_path TEXT NOT NULL,
    active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_campaign_automation_step UNIQUE (automation_id, position),
    CONSTRAINT campaign_automation_step_position_check CHECK (position > 0),
    CONSTRAINT campaign_automation_step_delay_check CHECK (delay_days >= 0),
    CONSTRAINT campaign_automation_step_channel_check CHECK (channel = 'whatsapp')
);

CREATE INDEX IF NOT EXISTS index_campaign_automation_step_active
    ON campaign_automation_step (automation_id, active, position);

CREATE TABLE IF NOT EXISTS campaign_enrollment (
    id BIGSERIAL PRIMARY KEY,
    automation_id BIGINT NOT NULL REFERENCES campaign_automation(id) ON DELETE CASCADE,
    party_id BIGINT NOT NULL REFERENCES party(id),
    status TEXT NOT NULL DEFAULT 'scheduled',
    next_step_position INTEGER NOT NULL DEFAULT 1,
    next_run_at TIMESTAMPTZ NOT NULL,
    last_sent_at TIMESTAMPTZ,
    stopped_at TIMESTAMPTZ,
    stop_reason TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_campaign_enrollment UNIQUE (automation_id, party_id),
    CONSTRAINT campaign_enrollment_next_step_check CHECK (next_step_position > 0),
    CONSTRAINT campaign_enrollment_status_check
        CHECK (status IN ('scheduled', 'completed', 'stopped', 'replied', 'converted'))
);

CREATE INDEX IF NOT EXISTS index_campaign_enrollment_due
    ON campaign_enrollment (automation_id, status, next_run_at);

CREATE INDEX IF NOT EXISTS index_campaign_enrollment_party
    ON campaign_enrollment (party_id, created_at);

CREATE TABLE IF NOT EXISTS campaign_delivery (
    id BIGSERIAL PRIMARY KEY,
    automation_id BIGINT NOT NULL REFERENCES campaign_automation(id) ON DELETE CASCADE,
    enrollment_id BIGINT NOT NULL REFERENCES campaign_enrollment(id) ON DELETE CASCADE,
    step_id BIGINT NOT NULL REFERENCES campaign_automation_step(id),
    party_id BIGINT NOT NULL REFERENCES party(id),
    channel TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'pending',
    scheduled_at TIMESTAMPTZ NOT NULL,
    attempted_at TIMESTAMPTZ,
    sent_at TIMESTAMPTZ,
    provider_message_id TEXT,
    error TEXT,
    body_snapshot TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_campaign_delivery UNIQUE (enrollment_id, step_id),
    CONSTRAINT campaign_delivery_channel_check CHECK (channel = 'whatsapp'),
    CONSTRAINT campaign_delivery_status_check
        CHECK (status IN ('pending', 'sent', 'failed'))
);

CREATE INDEX IF NOT EXISTS index_campaign_delivery_automation
    ON campaign_delivery (automation_id, status, created_at);

COMMIT;
