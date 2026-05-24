-- Ticketing System Enhancements
-- Date: 2026-05-24
-- Description: Add comprehensive ticketing features including promo codes, refunds,
--              transfers, waitlist, Stripe integration, and QR codes

-- =============================================================================
-- PROMO CODES
-- =============================================================================

-- Promo code definitions
CREATE TABLE IF NOT EXISTS promo_code (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT REFERENCES social_event(id) ON DELETE CASCADE,
    code TEXT NOT NULL UNIQUE,
    description TEXT,
    discount_type TEXT NOT NULL CHECK (discount_type IN ('percentage', 'fixed_amount')),
    discount_value INTEGER NOT NULL CHECK (discount_value >= 0),
    currency TEXT NOT NULL DEFAULT 'USD',
    max_redemptions INTEGER CHECK (max_redemptions IS NULL OR max_redemptions > 0),
    current_redemptions INTEGER NOT NULL DEFAULT 0,
    valid_from TIMESTAMP,
    valid_until TIMESTAMP,
    tier_ids TEXT,  -- JSON array of tier IDs this code applies to
    min_purchase_amount_cents INTEGER,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_by_party_id TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_promo_code_event ON promo_code(event_id) WHERE event_id IS NOT NULL;
CREATE INDEX idx_promo_code_active ON promo_code(is_active, valid_until) WHERE is_active = TRUE;
CREATE INDEX idx_promo_code_lookup ON promo_code(code, is_active);

-- Promo code redemptions tracking
CREATE TABLE IF NOT EXISTS promo_code_redemption (
    id BIGSERIAL PRIMARY KEY,
    promo_code_id BIGINT NOT NULL REFERENCES promo_code(id) ON DELETE CASCADE,
    order_id BIGINT NOT NULL REFERENCES event_ticket_order(id) ON DELETE CASCADE,
    discount_amount_cents INTEGER NOT NULL,
    redeemed_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_promo_redemption_code ON promo_code_redemption(promo_code_id);
CREATE INDEX idx_promo_redemption_order ON promo_code_redemption(order_id);

-- =============================================================================
-- REFUNDS
-- =============================================================================

-- Ticket refund requests
CREATE TABLE IF NOT EXISTS ticket_refund_request (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES event_ticket_order(id) ON DELETE CASCADE,
    requested_by_party_id TEXT,
    reason TEXT,
    amount_cents INTEGER NOT NULL,
    status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'approved', 'rejected', 'processed')),
    approved_by_party_id TEXT,
    approved_at TIMESTAMP,
    rejection_reason TEXT,
    stripe_refund_id TEXT,  -- Stripe refund ID for tracking
    processed_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_refund_order ON ticket_refund_request(order_id);
CREATE INDEX idx_refund_status ON ticket_refund_request(status, created_at);
CREATE INDEX idx_refund_stripe ON ticket_refund_request(stripe_refund_id) WHERE stripe_refund_id IS NOT NULL;

-- =============================================================================
-- TICKET TRANSFERS
-- =============================================================================

-- Ticket transfer requests
CREATE TABLE IF NOT EXISTS ticket_transfer (
    id BIGSERIAL PRIMARY KEY,
    ticket_id BIGINT NOT NULL REFERENCES event_ticket(id) ON DELETE CASCADE,
    from_party_id TEXT,
    to_party_id TEXT,
    to_email TEXT,
    to_name TEXT,
    status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'accepted', 'cancelled', 'expired')),
    transfer_code TEXT NOT NULL UNIQUE,
    message TEXT,
    expires_at TIMESTAMP,
    accepted_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_transfer_ticket ON ticket_transfer(ticket_id);
CREATE INDEX idx_transfer_code ON ticket_transfer(transfer_code);
CREATE INDEX idx_transfer_status ON ticket_transfer(status, created_at);
CREATE INDEX idx_transfer_email ON ticket_transfer(to_email) WHERE status = 'pending';

-- =============================================================================
-- WAITLIST
-- =============================================================================

-- Event waitlist entries
CREATE TABLE IF NOT EXISTS event_waitlist (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
    tier_id BIGINT REFERENCES event_ticket_tier(id) ON DELETE CASCADE,
    party_id TEXT,
    email TEXT NOT NULL,
    name TEXT,
    quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity > 0),
    status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'notified', 'converted', 'expired', 'cancelled')),
    priority INTEGER NOT NULL DEFAULT 0,
    notified_at TIMESTAMP,
    expires_at TIMESTAMP,
    converted_order_id BIGINT REFERENCES event_ticket_order(id),
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_waitlist_event ON event_waitlist(event_id, status);
CREATE INDEX idx_waitlist_tier ON event_waitlist(tier_id, status) WHERE tier_id IS NOT NULL;
CREATE INDEX idx_waitlist_email ON event_waitlist(email);
CREATE INDEX idx_waitlist_priority ON event_waitlist(event_id, priority DESC, created_at ASC) WHERE status = 'active';

-- =============================================================================
-- STRIPE INTEGRATION
-- =============================================================================

-- Stripe payment intents
CREATE TABLE IF NOT EXISTS stripe_payment_intent (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES event_ticket_order(id) ON DELETE CASCADE,
    stripe_payment_intent_id TEXT NOT NULL UNIQUE,
    stripe_client_secret TEXT NOT NULL,
    amount_cents INTEGER NOT NULL,
    currency TEXT NOT NULL DEFAULT 'USD',
    status TEXT NOT NULL,
    metadata TEXT,  -- JSON
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_stripe_intent_order ON stripe_payment_intent(order_id);

-- Stripe webhook event log (prevent duplicate processing)
CREATE TABLE IF NOT EXISTS stripe_webhook_event (
    id BIGSERIAL PRIMARY KEY,
    stripe_event_id TEXT NOT NULL UNIQUE,
    event_type TEXT NOT NULL,
    payload TEXT NOT NULL,
    processed_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_stripe_webhook_type ON stripe_webhook_event(event_type, processed_at);

-- =============================================================================
-- QR CODES
-- =============================================================================

-- Ticket QR codes
CREATE TABLE IF NOT EXISTS ticket_qr_code (
    id BIGSERIAL PRIMARY KEY,
    ticket_id BIGINT NOT NULL UNIQUE REFERENCES event_ticket(id) ON DELETE CASCADE,
    qr_data TEXT NOT NULL,
    qr_image_url TEXT,
    generated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- =============================================================================
-- EXTEND EXISTING TABLES
-- =============================================================================

-- Add columns to event_ticket_order
ALTER TABLE event_ticket_order
    ADD COLUMN IF NOT EXISTS stripe_payment_intent_id TEXT,
    ADD COLUMN IF NOT EXISTS promo_code_id BIGINT REFERENCES promo_code(id),
    ADD COLUMN IF NOT EXISTS original_amount_cents INTEGER,
    ADD COLUMN IF NOT EXISTS payment_method TEXT;

CREATE INDEX IF NOT EXISTS idx_ticket_order_stripe_intent ON event_ticket_order(stripe_payment_intent_id) WHERE stripe_payment_intent_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_ticket_order_promo ON event_ticket_order(promo_code_id) WHERE promo_code_id IS NOT NULL;

-- Add columns to event_ticket
ALTER TABLE event_ticket
    ADD COLUMN IF NOT EXISTS current_holder_party_id TEXT,
    ADD COLUMN IF NOT EXISTS current_holder_email TEXT,
    ADD COLUMN IF NOT EXISTS current_holder_name TEXT,
    ADD COLUMN IF NOT EXISTS original_holder_party_id TEXT,
    ADD COLUMN IF NOT EXISTS transfer_history TEXT;  -- JSON array

CREATE INDEX IF NOT EXISTS idx_ticket_current_holder ON event_ticket(current_holder_party_id) WHERE current_holder_party_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_ticket_current_email ON event_ticket(current_holder_email) WHERE current_holder_email IS NOT NULL;

-- Add columns to event_ticket_tier
ALTER TABLE event_ticket_tier
    ADD COLUMN IF NOT EXISTS enable_waitlist BOOLEAN NOT NULL DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS allow_transfers BOOLEAN NOT NULL DEFAULT TRUE,
    ADD COLUMN IF NOT EXISTS refund_policy TEXT NOT NULL DEFAULT 'full',
    ADD COLUMN IF NOT EXISTS refund_deadline TIMESTAMP;

-- =============================================================================
-- DATA MIGRATION: Set current holder from order data
-- =============================================================================

-- Update existing tickets to set current holder from order
UPDATE event_ticket t
SET
    current_holder_party_id = o.buyer_party_id,
    current_holder_email = o.buyer_email,
    current_holder_name = o.buyer_name,
    original_holder_party_id = o.buyer_party_id
FROM event_ticket_order o
WHERE t.order_ref_id = o.id
  AND t.current_holder_party_id IS NULL
  AND o.buyer_email IS NOT NULL;

-- =============================================================================
-- COMMENTS
-- =============================================================================

COMMENT ON TABLE promo_code IS 'Promotional discount codes for event tickets';
COMMENT ON TABLE promo_code_redemption IS 'Track promo code usage per order';
COMMENT ON TABLE ticket_refund_request IS 'Refund requests with approval workflow';
COMMENT ON TABLE ticket_transfer IS 'Ticket transfer requests between users';
COMMENT ON TABLE event_waitlist IS 'Waitlist for sold-out events';
COMMENT ON TABLE stripe_payment_intent IS 'Stripe payment intents for idempotency';
COMMENT ON TABLE stripe_webhook_event IS 'Log of processed Stripe webhooks';
COMMENT ON TABLE ticket_qr_code IS 'QR codes for ticket validation';

COMMENT ON COLUMN promo_code.discount_type IS 'Either "percentage" or "fixed_amount"';
COMMENT ON COLUMN promo_code.discount_value IS 'Basis points for percentage (5000 = 50%), cents for fixed amount';
COMMENT ON COLUMN promo_code.tier_ids IS 'JSON array of tier IDs this code applies to, null = all tiers';
COMMENT ON COLUMN ticket_refund_request.status IS 'pending -> approved/rejected -> processed (via webhook)';
COMMENT ON COLUMN ticket_transfer.transfer_code IS 'Unique code for accepting transfer';
COMMENT ON COLUMN event_waitlist.priority IS 'Higher priority gets notified first';
COMMENT ON COLUMN event_ticket.transfer_history IS 'JSON array of transfer IDs';
COMMENT ON COLUMN event_ticket_tier.refund_policy IS 'full, partial, or none';
