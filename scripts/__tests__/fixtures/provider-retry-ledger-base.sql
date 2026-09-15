-- Disposable provider-retry database only. These are the ORM-owned base
-- dependencies from test-marketplace-rental-checkout-runtime-migration.sh,
-- not replacements for any commerce/runtime table or production schema.
-- The harness applies the real sale/rental migrations afterwards because
-- CheckoutStore's capture ledger queries their runtime even for TDF services.
\set ON_ERROR_STOP on
DO $$ BEGIN
  IF current_database() <> 'tdf_provider_retry_test' THEN
    RAISE EXCEPTION 'Provider retry fixture requires its dedicated disposable database';
  END IF;
END $$;
BEGIN;
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
