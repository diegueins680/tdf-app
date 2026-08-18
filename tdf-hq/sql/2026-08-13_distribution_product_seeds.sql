-- Bilingual, editable distribution-product benchmarks. Nothing is activated by this migration.
-- Requires 2026-08-13_distribution_accounting_core.
BEGIN;

ALTER TABLE distribution_product_version
  DROP CONSTRAINT distribution_product_version_product_key_version_key;

ALTER TABLE distribution_product_version
  ADD CONSTRAINT distribution_product_version_product_key_version_locale_key
  UNIQUE (product_key, version, locale);

CREATE TABLE distribution_product_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  product_key TEXT NOT NULL,
  product_version INT NOT NULL CHECK (product_version > 0),
  benchmark_source TEXT NOT NULL,
  benchmark_snapshot JSONB NOT NULL,
  recommendation TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','approved','rejected','superseded')),
  requested_by BIGINT,
  reviewed_by BIGINT,
  reviewed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (product_key, product_version),
  CHECK (status NOT IN ('approved','rejected') OR (reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL)),
  CHECK (requested_by IS NULL OR reviewed_by IS NULL OR requested_by <> reviewed_by)
);

CREATE OR REPLACE FUNCTION distribution_validate_product_activation()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE activation_requested BOOLEAN := TG_OP = 'INSERT';
BEGIN
  IF TG_OP = 'UPDATE' THEN
    activation_requested := OLD.status <> 'active';
  END IF;

  IF NEW.status = 'active' AND activation_requested AND NOT EXISTS (
    SELECT 1 FROM distribution_product_review review
    WHERE review.product_key = NEW.product_key
      AND review.product_version = NEW.version
      AND review.status = 'approved'
  ) THEN
    RAISE EXCEPTION 'A distribution product requires an approved market and margin review before activation';
  END IF;

  IF TG_OP = 'UPDATE' AND OLD.status IN ('active','retired') AND (
    OLD.product_key, OLD.version, OLD.product_kind, OLD.locale, OLD.name, OLD.currency,
    OLD.price_minor, OLD.royalty_share_bps, OLD.terms_version, OLD.included_services
  ) IS DISTINCT FROM (
    NEW.product_key, NEW.version, NEW.product_kind, NEW.locale, NEW.name, NEW.currency,
    NEW.price_minor, NEW.royalty_share_bps, NEW.terms_version, NEW.included_services
  ) THEN
    RAISE EXCEPTION 'Approved distribution economics are immutable; create a new version';
  END IF;

  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_product_activation
  BEFORE INSERT OR UPDATE ON distribution_product_version
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_product_activation();

INSERT INTO distribution_product_version(
  id,product_key,version,product_kind,locale,name,currency,price_minor,
  royalty_share_bps,terms_version,included_services,status
) VALUES
  ('83000000-0000-0000-0000-000000000001','distribution.single.standard',1,'single','es','Distribución de sencillo asistida','USD',5000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"single"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000002','distribution.single.standard',1,'single','en','Assisted single distribution','USD',5000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"single"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000003','distribution.ep.standard',1,'ep','es','Distribución de EP asistida','USD',10000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"ep"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000004','distribution.ep.standard',1,'ep','en','Assisted EP distribution','USD',10000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"ep"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000005','distribution.album.standard',1,'album','es','Distribución de álbum asistida','USD',15000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"album"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000006','distribution.album.standard',1,'album','en','Assisted album distribution','USD',15000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","service":"curated_release_delivery","format":"album"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000007','distribution.catalog.annual',1,'catalog_management','es','Gestión anual de catálogo','USD',6000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","billing_period":"annual","automatic_renewal":false}','pending_approval'),
  ('83000000-0000-0000-0000-000000000008','distribution.catalog.annual',1,'catalog_management','en','Annual catalog management','USD',6000,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","billing_period":"annual","automatic_renewal":false}','pending_approval'),
  ('83000000-0000-0000-0000-000000000009','distribution.artist.monthly',1,'subscription','es','Plan mensual de artista','USD',2500,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","billing_period":"monthly","automatic_renewal":false}','pending_approval'),
  ('83000000-0000-0000-0000-000000000010','distribution.artist.monthly',1,'subscription','en','Monthly artist plan','USD',2500,1000,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","billing_period":"monthly","automatic_renewal":false}','pending_approval'),
  ('83000000-0000-0000-0000-000000000011','distribution.addon.priority_review',1,'add_on','es','Revisión prioritaria','USD',3000,0,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","capability":"priority_review"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000012','distribution.addon.priority_review',1,'add_on','en','Priority review','USD',3000,0,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","capability":"priority_review"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000013','distribution.addon.metadata_qc',1,'add_on','es','Asistencia de metadatos y QC','USD',5000,0,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","capability":"metadata_qc"}','pending_approval'),
  ('83000000-0000-0000-0000-000000000014','distribution.addon.metadata_qc',1,'add_on','en','Metadata and QC assistance','USD',5000,0,'distribution-pricing-review-2026-08-13','{"seed_version":"2026-08-13-market-benchmark-v1","capability":"metadata_qc"}','pending_approval');

INSERT INTO distribution_product_review(
  id,product_key,product_version,benchmark_source,benchmark_snapshot,recommendation,status
) VALUES
  ('84000000-0000-0000-0000-000000000001','distribution.single.standard',1,'official_public_pricing_2026-08-13','{"cdbaby_single_usd":9.99,"tunecore_rising_annual_usd":24.99,"distrokid_musician_annual_usd":24.99}','$50 is a provisional high-touch service price, not a commodity-delivery comparison. Validate labor, tax, partner and support margin.','pending'),
  ('84000000-0000-0000-0000-000000000002','distribution.ep.standard',1,'official_public_pricing_2026-08-13','{"cdbaby_album_usd":14.99,"tunecore_rising_annual_usd":24.99,"distrokid_musician_annual_usd":24.99}','$100 is defensible only with material intake, metadata, rights and support work.','pending'),
  ('84000000-0000-0000-0000-000000000003','distribution.album.standard',1,'official_public_pricing_2026-08-13','{"cdbaby_album_usd":14.99,"tunecore_rising_annual_usd":24.99,"distrokid_musician_annual_usd":24.99}','$150 is defensible only as a curated label-service offer with a documented scope and margin.','pending'),
  ('84000000-0000-0000-0000-000000000004','distribution.catalog.annual',1,'official_public_pricing_2026-08-13','{"tunecore_rising_annual_usd":24.99,"distrokid_musician_annual_usd":24.99}','Confirm catalog limits, renewal behavior and non-takedown grace terms before approval.','pending'),
  ('84000000-0000-0000-0000-000000000005','distribution.artist.monthly',1,'official_public_pricing_2026-08-13','{"tunecore_rising_annual_usd":24.99,"distrokid_musician_annual_usd":24.99}','Keep automatic renewal disabled until a merchant subscription capability is verified.','pending'),
  ('84000000-0000-0000-0000-000000000006','distribution.addon.priority_review',1,'internal_cost_review_required','{}','Approve only after staffing, SLA and refund terms are costed.','pending'),
  ('84000000-0000-0000-0000-000000000007','distribution.addon.metadata_qc',1,'internal_cost_review_required','{}','Approve only after scope, revision limits and staff cost are defined.','pending');

COMMIT;
