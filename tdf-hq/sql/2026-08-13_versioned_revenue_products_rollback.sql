BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_product_version
    WHERE source <> 'client_legacy_snapshot' OR status <> 'pending_approval'
  ) OR EXISTS (
    SELECT 1 FROM commerce_rate_card_review WHERE status <> 'pending'
  ) THEN
    RAISE EXCEPTION 'Refusing product-catalog rollback: reviewed or runtime product records exist';
  END IF;
END $$;

DELETE FROM revenue_feature_flag WHERE flag_key IN ('domo.authoritative_quotes','domo.checkout');
DELETE FROM commerce_rate_card_review WHERE status = 'pending';
DELETE FROM commerce_product_version WHERE source = 'client_legacy_snapshot';
DROP TABLE commerce_rate_card_review;
DROP TABLE commerce_product_version;
DROP FUNCTION commerce_validate_product_activation();

COMMIT;
