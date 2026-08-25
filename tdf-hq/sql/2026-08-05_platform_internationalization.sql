BEGIN;

CREATE TABLE IF NOT EXISTS supported_currencies (
  id BIGSERIAL PRIMARY KEY,
  currency_code TEXT NOT NULL UNIQUE,
  symbol TEXT NOT NULL,
  decimal_places INTEGER NOT NULL,
  decimal_separator TEXT NOT NULL,
  thousands_separator TEXT NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT supported_currency_code_iso4217
    CHECK (currency_code ~ '^[A-Z]{3}$'),
  CONSTRAINT supported_currency_decimal_places
    CHECK (decimal_places BETWEEN 0 AND 3)
);

INSERT INTO supported_currencies
  (currency_code, symbol, decimal_places, decimal_separator, thousands_separator, enabled)
VALUES
  ('USD', '$', 2, '.', ',', TRUE),
  ('EUR', '€', 2, ',', '.', TRUE),
  ('GBP', '£', 2, '.', ',', TRUE),
  ('CAD', 'C$', 2, '.', ',', TRUE),
  ('AUD', 'A$', 2, '.', ',', TRUE),
  ('JPY', '¥', 0, '.', ',', TRUE),
  ('BRL', 'R$', 2, ',', '.', TRUE)
ON CONFLICT (currency_code) DO UPDATE SET
  symbol = EXCLUDED.symbol,
  decimal_places = EXCLUDED.decimal_places,
  decimal_separator = EXCLUDED.decimal_separator,
  thousands_separator = EXCLUDED.thousands_separator,
  enabled = EXCLUDED.enabled,
  updated_at = now();

CREATE TABLE IF NOT EXISTS user_locale_preferences (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  locale TEXT NOT NULL,
  currency TEXT NOT NULL REFERENCES supported_currencies(currency_code),
  timezone TEXT NOT NULL,
  country_code TEXT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT user_locale_preferences_user_unique UNIQUE (user_id),
  CONSTRAINT user_locale_preferences_locale_check
    CHECK (locale ~ '^[a-z]{2}(-[A-Z]{2})?$'),
  CONSTRAINT user_locale_preferences_country_check
    CHECK (country_code IS NULL OR country_code ~ '^[A-Z]{2}$')
);

-- Preserve the presentation existing accounts had before the configurable
-- defaults moved to en/USD/UTC. New accounts inherit runtime configuration.
INSERT INTO user_locale_preferences
  (user_id, locale, currency, timezone, country_code)
SELECT id, 'es', 'USD', 'America/Guayaquil', 'EC'
FROM party
ON CONFLICT (user_id) DO NOTHING;

ALTER TABLE IF EXISTS payment
  ADD COLUMN IF NOT EXISTS currency TEXT NOT NULL DEFAULT 'USD';

ALTER TABLE IF EXISTS party
  ADD COLUMN IF NOT EXISTS country_code TEXT;

ALTER TABLE IF EXISTS artist_profile
  ADD COLUMN IF NOT EXISTS country_code TEXT;

ALTER TABLE IF EXISTS social_artist_profile
  ADD COLUMN IF NOT EXISTS country_code TEXT;

ALTER TABLE IF EXISTS venue
  ADD COLUMN IF NOT EXISTS country_code TEXT,
  ADD COLUMN IF NOT EXISTS timezone TEXT;

ALTER TABLE IF EXISTS social_event
  ADD COLUMN IF NOT EXISTS timezone TEXT;

ALTER TABLE IF EXISTS course
  ADD COLUMN IF NOT EXISTS currency TEXT NOT NULL DEFAULT 'USD';

ALTER TABLE IF EXISTS service_catalog
  ADD COLUMN IF NOT EXISTS currency TEXT NOT NULL DEFAULT 'USD';

ALTER TABLE IF EXISTS receipt
  ADD COLUMN IF NOT EXISTS currency TEXT NOT NULL DEFAULT 'USD';

CREATE TABLE IF NOT EXISTS currency_conversion_audit (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  source_currency TEXT NOT NULL,
  target_currency TEXT NOT NULL,
  source_minor_units BIGINT NOT NULL,
  target_minor_units BIGINT NOT NULL,
  exchange_rate NUMERIC(24, 12) NOT NULL,
  rate_source TEXT NOT NULL,
  rate_observed_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT currency_conversion_source_code
    CHECK (source_currency ~ '^[A-Z]{3}$'),
  CONSTRAINT currency_conversion_target_code
    CHECK (target_currency ~ '^[A-Z]{3}$'),
  CONSTRAINT currency_conversion_positive_rate
    CHECK (exchange_rate > 0)
);

CREATE INDEX IF NOT EXISTS currency_conversion_audit_user_created_idx
  ON currency_conversion_audit (user_id, created_at DESC);

COMMIT;
