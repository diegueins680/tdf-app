-- Synthetic identities, real production session getter. Not a deployment migration.
ALTER TABLE party
 ADD COLUMN legal_name TEXT, ADD COLUMN display_name TEXT NOT NULL DEFAULT 'Persona de prueba',
 ADD COLUMN is_org BOOLEAN NOT NULL DEFAULT FALSE, ADD COLUMN tax_id TEXT,
 ADD COLUMN primary_email TEXT, ADD COLUMN primary_phone TEXT, ADD COLUMN whatsapp TEXT,
 ADD COLUMN instagram TEXT, ADD COLUMN emergency_contact TEXT, ADD COLUMN notes TEXT,
 ADD COLUMN stripe_customer_id TEXT, ADD COLUMN country_code TEXT, ADD COLUMN country_id UUID,
 ADD COLUMN created_at TIMESTAMPTZ NOT NULL DEFAULT now();
UPDATE api_token SET label='password-login:browser-party-' || party_id::TEXT
 WHERE label IS NULL;
CREATE TABLE locale_reference (
 id UUID PRIMARY KEY, code TEXT NOT NULL, language_id UUID NOT NULL, country_id UUID,
 name_es TEXT NOT NULL, name_en TEXT NOT NULL, description_es TEXT, description_en TEXT,
 fallback_locale_id UUID, default_for_platform BOOLEAN NOT NULL DEFAULT FALSE,
 source_version TEXT NOT NULL, last_synced_at TIMESTAMPTZ NOT NULL DEFAULT now(),
 deprecated_at TIMESTAMPTZ, replacement_id UUID, active BOOLEAN NOT NULL DEFAULT TRUE,
 sort_order INT NOT NULL DEFAULT 0, version INT NOT NULL DEFAULT 1
);
CREATE TABLE currency_reference (
 id UUID PRIMARY KEY, code TEXT NOT NULL, numeric_code TEXT, name_es TEXT NOT NULL, name_en TEXT NOT NULL,
 description_es TEXT, description_en TEXT, symbol TEXT NOT NULL, minor_units INT NOT NULL,
 standard TEXT NOT NULL DEFAULT 'ISO 4217', source_version TEXT NOT NULL, effective_from DATE,
 effective_until DATE, deprecated_at TIMESTAMPTZ, replacement_id UUID,
 last_synced_at TIMESTAMPTZ NOT NULL DEFAULT now(), active BOOLEAN NOT NULL DEFAULT TRUE,
 sort_order INT NOT NULL DEFAULT 0, version INT NOT NULL DEFAULT 1
);
INSERT INTO locale_reference(id,code,language_id,name_es,name_en,source_version)
 VALUES ('80000000-0000-4000-8000-000000000001','es','80000000-0000-4000-8000-000000000003',
 'Español','Spanish','disposable browser fixture');
INSERT INTO currency_reference(id,code,name_es,name_en,symbol,minor_units,source_version)
 VALUES ('80000000-0000-4000-8000-000000000002','USD','Dólar','Dollar','$',2,'disposable browser fixture');
CREATE TABLE user_locale_preferences (
 id BIGSERIAL PRIMARY KEY, user_id BIGINT NOT NULL UNIQUE REFERENCES party(id),
 locale TEXT, currency TEXT, timezone TEXT NOT NULL, country_code TEXT,
 locale_id UUID REFERENCES locale_reference(id), currency_id UUID REFERENCES currency_reference(id),
 country_id UUID, show_event_rsvps_on_profile BOOLEAN NOT NULL DEFAULT TRUE,
 updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
INSERT INTO user_locale_preferences(user_id,locale,currency,timezone,locale_id,currency_id)
 SELECT id,'es','USD','America/Guayaquil','80000000-0000-4000-8000-000000000001'::UUID,
 '80000000-0000-4000-8000-000000000002'::UUID FROM party;
