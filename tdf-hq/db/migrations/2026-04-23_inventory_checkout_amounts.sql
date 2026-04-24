ALTER TABLE asset_checkout
  ADD COLUMN IF NOT EXISTS payment_amount_cents INTEGER;

ALTER TABLE asset_checkout
  ADD COLUMN IF NOT EXISTS payment_currency TEXT;

ALTER TABLE asset_checkout
  ADD COLUMN IF NOT EXISTS payment_outstanding_cents INTEGER;
