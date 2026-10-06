-- Disposable provider-retry database only. These are the ORM-owned base
-- dependencies from test-marketplace-rental-checkout-runtime-migration.sh,
-- not a production schema. The final minimal ticket projection exists only
-- to exercise canonical capture-ledger posting, not ticket inventory or issuance.
-- The harness applies the real sale/rental migrations afterwards because
-- CheckoutStore's capture ledger queries their runtime even for TDF services.
\set ON_ERROR_STOP on
DO $$ BEGIN
  IF current_database() <> 'tdf_provider_retry_test' THEN
    RAISE EXCEPTION 'Provider retry fixture requires its dedicated disposable database';
  END IF;
END $$;
BEGIN;
-- Only the ORM-owned party identity required by the real manual-evidence
-- migration. These are synthetic reviewer identities, not authenticated users.
CREATE FUNCTION trigger_set_timestamp() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN NEW.updated_at = NOW(); RETURN NEW; END $$;
CREATE TABLE party (id BIGINT PRIMARY KEY);
INSERT INTO party(id) VALUES (1),(2);
CREATE TABLE asset (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), name TEXT NOT NULL,
  category TEXT NOT NULL, condition TEXT NOT NULL, status TEXT NOT NULL,
  owner TEXT NOT NULL, maintenance_policy TEXT NOT NULL
);
CREATE TABLE marketplace_listing (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), asset_id UUID NOT NULL REFERENCES asset(id),
  title TEXT NOT NULL, purpose TEXT NOT NULL DEFAULT 'sale', price_usd_cents BIGINT NOT NULL,
  markup_pct BIGINT NOT NULL DEFAULT 25, currency TEXT NOT NULL DEFAULT 'USD',
  active BOOLEAN NOT NULL DEFAULT TRUE, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE marketplace_cart (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE marketplace_cart_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), cart_id UUID NOT NULL REFERENCES marketplace_cart(id),
  listing_id UUID NOT NULL REFERENCES marketplace_listing(id), quantity BIGINT NOT NULL DEFAULT 1,
  UNIQUE(cart_id, listing_id)
);
CREATE TABLE marketplace_order (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), cart_id UUID REFERENCES marketplace_cart(id),
  buyer_name TEXT NOT NULL, buyer_email TEXT NOT NULL, buyer_phone TEXT,
  total_usd_cents BIGINT NOT NULL, currency TEXT NOT NULL DEFAULT 'USD',
  status TEXT NOT NULL DEFAULT 'pending', payment_provider TEXT,
  paid_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE TABLE marketplace_order_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), order_id UUID NOT NULL REFERENCES marketplace_order(id),
  listing_id UUID NOT NULL REFERENCES marketplace_listing(id), quantity BIGINT NOT NULL,
  unit_price_usd_cents BIGINT NOT NULL, subtotal_usd_cents BIGINT NOT NULL
);
COMMIT;

-- Minimal immutable ticket monetary projection for canonical ledger integration.
-- Full ticket inventory/issuance constraints are covered by the ticket runtime suite.
CREATE TABLE event_ticket_checkout_runtime (
 checkout_id uuid PRIMARY KEY,
 platform_fee_minor bigint NOT NULL CHECK(platform_fee_minor>=0),
 organizer_payable_minor bigint NOT NULL CHECK(organizer_payable_minor>=0),
 tax_minor bigint NOT NULL CHECK(tax_minor>=0)
);

-- Synthetic ticket projection tables for actual refund/admission transactions.
CREATE TABLE social_event (id BIGSERIAL PRIMARY KEY,organizer_party_id VARCHAR NULL,title VARCHAR NOT NULL,description VARCHAR NULL,venue_id INTEGER NULL,event_type_id VARCHAR NULL,workflow_state_id VARCHAR NULL,timezone VARCHAR NULL,start_time TIMESTAMPTZ NOT NULL,end_time TIMESTAMPTZ NULL,price_cents INTEGER NULL,currency_id VARCHAR NULL,capacity INTEGER NULL,metadata VARCHAR NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);
CREATE TABLE event_ticket_tier (id BIGSERIAL PRIMARY KEY,event_id INTEGER NOT NULL,code VARCHAR NOT NULL,name VARCHAR NOT NULL,description VARCHAR NULL,price_cents INTEGER NOT NULL,currency VARCHAR NOT NULL,currency_id VARCHAR NULL,quantity_total INTEGER NOT NULL,quantity_sold INTEGER NOT NULL,sales_start TIMESTAMPTZ NULL,sales_end TIMESTAMPTZ NULL,is_active BOOLEAN NOT NULL,position INTEGER NULL,enable_waitlist BOOLEAN NOT NULL DEFAULT FALSE,allow_transfers BOOLEAN NOT NULL DEFAULT FALSE,refund_policy VARCHAR NOT NULL DEFAULT 'none',refund_deadline TIMESTAMPTZ NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);
CREATE TABLE event_ticket_order (id BIGSERIAL PRIMARY KEY,event_id INTEGER NOT NULL,tier_id INTEGER NOT NULL,buyer_party_id VARCHAR NULL,buyer_name VARCHAR NULL,buyer_email VARCHAR NULL,quantity INTEGER NOT NULL,amount_cents INTEGER NOT NULL,currency VARCHAR NOT NULL,status VARCHAR NOT NULL,metadata VARCHAR NULL,checkout_idempotency_key VARCHAR NULL,purchased_at TIMESTAMPTZ NOT NULL,stripe_payment_intent_id VARCHAR NULL,promo_code_id INTEGER NULL,original_amount_cents INTEGER NULL,payment_method VARCHAR NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);
CREATE TABLE event_ticket (id BIGSERIAL PRIMARY KEY,event_id INTEGER NOT NULL,tier_ref_id INTEGER NOT NULL,order_ref_id INTEGER NOT NULL,holder_name VARCHAR NULL,holder_email VARCHAR NULL,code VARCHAR NOT NULL UNIQUE,status VARCHAR NOT NULL,checked_in_at TIMESTAMPTZ NULL,current_holder_party_id VARCHAR NULL,current_holder_email VARCHAR NULL,current_holder_name VARCHAR NULL,original_holder_party_id VARCHAR NULL,transfer_history VARCHAR NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);
CREATE TABLE ticket_transfer (id BIGSERIAL PRIMARY KEY,ticket_id BIGINT NOT NULL REFERENCES event_ticket(id),from_party_id VARCHAR NULL,to_party_id VARCHAR NULL,to_email VARCHAR NULL,to_name VARCHAR NULL,status VARCHAR NOT NULL DEFAULT 'pending',transfer_code VARCHAR NOT NULL UNIQUE,message VARCHAR NULL,expires_at TIMESTAMPTZ NULL,accepted_at TIMESTAMPTZ NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);
ALTER TABLE event_ticket_checkout_runtime ADD COLUMN order_id BIGINT UNIQUE REFERENCES event_ticket_order(id),
 ADD COLUMN event_id BIGINT, ADD COLUMN currency TEXT, ADD COLUMN checkout_total_minor BIGINT,
 ADD COLUMN payment_status TEXT DEFAULT 'paid';

CREATE TABLE ticket_refund_request (
 id BIGSERIAL PRIMARY KEY, order_id BIGINT NOT NULL REFERENCES event_ticket_order(id),
 requested_by_party_id TEXT, reason TEXT, amount_cents BIGINT NOT NULL,
 status TEXT NOT NULL DEFAULT 'pending', approved_by_party_id TEXT, approved_at TIMESTAMPTZ,
 rejection_reason TEXT, stripe_refund_id TEXT, processed_at TIMESTAMPTZ,
 created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
