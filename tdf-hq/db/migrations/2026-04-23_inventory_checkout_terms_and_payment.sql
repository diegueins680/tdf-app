ALTER TABLE asset_checkout
  ADD COLUMN IF NOT EXISTS terms_and_conditions TEXT,
  ADD COLUMN IF NOT EXISTS payment_type TEXT,
  ADD COLUMN IF NOT EXISTS payment_installments INTEGER,
  ADD COLUMN IF NOT EXISTS payment_reference TEXT;
