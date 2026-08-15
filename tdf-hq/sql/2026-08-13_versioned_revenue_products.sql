-- Authoritative, versioned product/rate configuration shared by domain orders.
-- The Domo client formula is preserved for review but is deliberately inactive.
BEGIN;

CREATE TABLE commerce_product_version (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  domain_type TEXT NOT NULL,
  product_key TEXT NOT NULL,
  version INT NOT NULL CHECK (version > 0),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  description_es TEXT NOT NULL DEFAULT '',
  description_en TEXT NOT NULL DEFAULT '',
  pricing_model TEXT NOT NULL CHECK (pricing_model IN ('flat','per_unit','tiered','duration','quote')),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  base_amount_minor BIGINT CHECK (base_amount_minor IS NULL OR base_amount_minor >= 0),
  pricing_rules JSONB NOT NULL,
  tax_code TEXT,
  deposit_basis_points INT NOT NULL DEFAULT 0 CHECK (deposit_basis_points BETWEEN 0 AND 10000),
  checkout_hold_minutes INT NOT NULL DEFAULT 15 CHECK (checkout_hold_minutes BETWEEN 1 AND 1440),
  policy_snapshot JSONB NOT NULL,
  source TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','pending_approval','active','retired')),
  approved_by BIGINT,
  approved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (domain_type, product_key, version),
  CHECK (status <> 'active' OR (approved_by IS NOT NULL AND approved_at IS NOT NULL))
);

CREATE UNIQUE INDEX uq_commerce_product_active
  ON commerce_product_version(domain_type, product_key)
  WHERE status = 'active';

CREATE TABLE commerce_rate_card_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  domain_type TEXT NOT NULL,
  product_version_id UUID NOT NULL REFERENCES commerce_product_version(id) ON DELETE RESTRICT,
  baseline_source TEXT NOT NULL,
  baseline_snapshot JSONB NOT NULL,
  candidate_snapshot JSONB NOT NULL,
  variance_notes TEXT,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','approved','rejected','superseded')),
  requested_by BIGINT,
  reviewed_by BIGINT,
  reviewed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (product_version_id),
  CHECK (status NOT IN ('approved','rejected') OR (reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL)),
  CHECK (requested_by IS NULL OR reviewed_by IS NULL OR requested_by <> reviewed_by)
);

CREATE OR REPLACE FUNCTION commerce_validate_product_activation()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE activation_requested BOOLEAN := TG_OP = 'INSERT';
BEGIN
  IF TG_OP = 'UPDATE' THEN
    activation_requested := OLD.status <> 'active';
  END IF;
  IF NEW.status = 'active' AND activation_requested AND NOT EXISTS (
    SELECT 1 FROM commerce_rate_card_review review
    WHERE review.product_version_id = NEW.id AND review.status = 'approved'
  ) THEN
    RAISE EXCEPTION 'A product version requires an approved rate-card comparison before activation';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.status IN ('active','retired') AND (
    OLD.domain_type, OLD.product_key, OLD.version, OLD.pricing_model, OLD.currency,
    OLD.base_amount_minor, OLD.pricing_rules, OLD.tax_code, OLD.deposit_basis_points,
    OLD.checkout_hold_minutes, OLD.policy_snapshot
  ) IS DISTINCT FROM (
    NEW.domain_type, NEW.product_key, NEW.version, NEW.pricing_model, NEW.currency,
    NEW.base_amount_minor, NEW.pricing_rules, NEW.tax_code, NEW.deposit_basis_points,
    NEW.checkout_hold_minutes, NEW.policy_snapshot
  ) THEN
    RAISE EXCEPTION 'Approved product economics are immutable; create a new version';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_product_activation
  BEFORE INSERT OR UPDATE ON commerce_product_version
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_product_activation();

INSERT INTO commerce_product_version(
  id,domain_type,product_key,version,name_es,name_en,description_es,description_en,
  pricing_model,currency,base_amount_minor,pricing_rules,tax_code,deposit_basis_points,
  checkout_hold_minutes,policy_snapshot,source,status
) VALUES (
  '81000000-0000-0000-0000-000000000001',
  'domo','legacy_public_formula',1,'Fórmula pública histórica del Domo','Legacy Domo public formula',
  'Valores preservados desde el cliente para comparación; no aprobados para checkout.',
  'Values preserved from the client for comparison; not approved for checkout.',
  'quote','USD',NULL,
  '{
    "event_types": {
      "wedding": {"base_minor":180000,"per_guest_minor":800,"minimum_hours":8,"included_guests":60},
      "corporate": {"base_minor":120000,"per_guest_minor":600,"minimum_hours":6,"included_guests":40},
      "retreat": {"base_minor":95000,"per_guest_minor":500,"minimum_hours":6,"included_guests":25},
      "concert": {"base_minor":150000,"per_guest_minor":700,"minimum_hours":7,"included_guests":80},
      "workshop": {"base_minor":70000,"per_guest_minor":450,"minimum_hours":4,"included_guests":20},
      "photo": {"base_minor":45000,"per_guest_minor":300,"minimum_hours":3,"included_guests":8}
    },
    "hour_minor":18000,
    "setup_hour_minor":7000,
    "catering_minimum_minor":35000,
    "catering_per_guest_minor":650,
    "production_minor":42000,
    "transport_minor":30000,
    "legacy_tax_basis_points":1200
  }'::jsonb,
  'EC-IVA-LEGACY-UNVERIFIED',4000,15,
  '{"availability":"staff_confirmed","cancellation":"pending_legal_review","deposit":"legacy_40_percent_pending_approval"}'::jsonb,
  'client_legacy_snapshot','pending_approval'
)
ON CONFLICT (domain_type, product_key, version) DO NOTHING;

INSERT INTO commerce_rate_card_review(
  id,domain_type,product_version_id,baseline_source,baseline_snapshot,candidate_snapshot,variance_notes,status
)
SELECT
  '82000000-0000-0000-0000-000000000001','domo',id,'DomoVenuePage client constants',pricing_rules,pricing_rules,
  'Candidate intentionally mirrors the historical client formula. Finance, operations, tax, and legal review are required before activation.',
  'pending'
FROM commerce_product_version
WHERE domain_type='domo' AND product_key='legacy_public_formula' AND version=1
ON CONFLICT (product_version_id) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('domo.authoritative_quotes', FALSE, 'production', 'Requires an approved active rate card, availability holds, versioned terms, and quote APIs'),
  ('domo.checkout', FALSE, 'production', 'Requires authoritative quote conversion, verified provider checkout, and operational approval')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
