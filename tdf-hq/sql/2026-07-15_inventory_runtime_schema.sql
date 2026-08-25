-- Bring older production inventory databases up to the current AssetCheckout model.
-- The initial inventory schema predates custody, payment, and evidence fields.

BEGIN;

ALTER TABLE public.asset_checkout
    ADD COLUMN IF NOT EXISTS disposition VARCHAR NOT NULL DEFAULT 'Loan',
    ADD COLUMN IF NOT EXISTS terms_and_conditions VARCHAR,
    ADD COLUMN IF NOT EXISTS holder_email VARCHAR,
    ADD COLUMN IF NOT EXISTS holder_phone VARCHAR,
    ADD COLUMN IF NOT EXISTS payment_type VARCHAR,
    ADD COLUMN IF NOT EXISTS payment_installments BIGINT,
    ADD COLUMN IF NOT EXISTS payment_reference VARCHAR,
    ADD COLUMN IF NOT EXISTS payment_amount_cents BIGINT,
    ADD COLUMN IF NOT EXISTS payment_currency VARCHAR,
    ADD COLUMN IF NOT EXISTS payment_outstanding_cents BIGINT,
    ADD COLUMN IF NOT EXISTS photo_out_url VARCHAR,
    ADD COLUMN IF NOT EXISTS photo_in_url VARCHAR;

CREATE INDEX IF NOT EXISTS idx_asset_checkout_asset
    ON public.asset_checkout(asset_id);

CREATE INDEX IF NOT EXISTS idx_asset_checkout_returned
    ON public.asset_checkout(asset_id, returned_at)
    WHERE returned_at IS NULL;

COMMIT;
