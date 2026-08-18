-- Removes only untouched, unapproved benchmark seeds and restores the prior locale constraint.
BEGIN;

DO $$
BEGIN
  IF (SELECT count(*) FROM distribution_product_version WHERE id::text LIKE '83000000-0000-0000-0000-0000000000%') <> 14
     OR EXISTS (
       SELECT 1 FROM distribution_product_version
       WHERE id::text LIKE '83000000-0000-0000-0000-0000000000%'
         AND (
           status <> 'pending_approval'
           OR terms_version <> 'distribution-pricing-review-2026-08-13'
           OR included_services->>'seed_version' <> '2026-08-13-market-benchmark-v1'
           OR price_minor <> CASE product_key
             WHEN 'distribution.single.standard' THEN 5000
             WHEN 'distribution.ep.standard' THEN 10000
             WHEN 'distribution.album.standard' THEN 15000
             WHEN 'distribution.catalog.annual' THEN 6000
             WHEN 'distribution.artist.monthly' THEN 2500
             WHEN 'distribution.addon.priority_review' THEN 3000
             WHEN 'distribution.addon.metadata_qc' THEN 5000
             ELSE -1
           END
         )
     )
     OR EXISTS (SELECT 1 FROM distribution_product_review WHERE status <> 'pending') THEN
    RAISE EXCEPTION 'Refusing distribution-product rollback: seeded products or reviews were changed';
  END IF;
END $$;

DELETE FROM distribution_product_version
WHERE id::text LIKE '83000000-0000-0000-0000-0000000000%';

DROP TRIGGER trg_distribution_product_activation ON distribution_product_version;
DROP FUNCTION distribution_validate_product_activation();
DROP TABLE distribution_product_review;

ALTER TABLE distribution_product_version
  DROP CONSTRAINT distribution_product_version_product_key_version_locale_key;

ALTER TABLE distribution_product_version
  ADD CONSTRAINT distribution_product_version_product_key_version_key
  UNIQUE (product_key, version);

COMMIT;
