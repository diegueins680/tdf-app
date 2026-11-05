-- Rollback of 2026-11-04_domo_quote_deposit_policy_validator: restores the
-- basis-point-only insert validator of 2026-08-18_domo_quote_checkout_runtime.
-- Existing rows are unaffected (the trigger fires on INSERT); quotes the policy
-- adjusts can no longer be created after rollback.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE OR REPLACE FUNCTION domo_validate_quote_runtime_insert()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  quote commerce_quote%ROWTYPE;
  product commerce_product_version%ROWTYPE;
  line_total BIGINT;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO quote FROM commerce_quote WHERE id = NEW.quote_id;
  SELECT * INTO product FROM commerce_product_version WHERE id = NEW.product_version_id;
  SELECT COALESCE(sum(subtotal_minor), 0) INTO line_total
    FROM commerce_quote_line WHERE quote_id = NEW.quote_id;

  IF checkout.id IS NULL OR quote.id IS NULL OR product.id IS NULL THEN
    RAISE EXCEPTION 'Domo quote runtime references missing canonical records';
  END IF;
  IF checkout.domain_type <> 'domo_event_quote'
     OR checkout.domain_order_id <> 'domo-quote:' || NEW.id::text
     OR checkout.quote_id <> NEW.quote_id
     OR checkout.status <> 'holding'
     OR checkout.total_minor <> NEW.deposit_minor
     OR checkout.currency <> NEW.currency THEN
    RAISE EXCEPTION 'Domo quote runtime does not match the immutable deposit checkout';
  END IF;
  IF quote.domain_type <> 'domo_event_quote'
     OR quote.domain_subject_id <> NEW.id::text
     OR quote.version <> 1
     OR quote.status <> 'sent'
     OR quote.currency <> NEW.currency
     OR quote.subtotal_minor <> NEW.subtotal_minor
     OR quote.tax_minor <> NEW.tax_minor
     OR quote.total_minor <> NEW.total_minor
     OR quote.deposit_minor <> NEW.deposit_minor
     OR quote.expires_at <> NEW.hold_expires_at
     OR line_total <> NEW.subtotal_minor THEN
    RAISE EXCEPTION 'Domo quote runtime does not match the immutable full quote and lines';
  END IF;
  IF product.domain_type <> 'domo'
     OR product.status <> 'active'
     OR product.approved_by IS NULL
     OR product.approved_at IS NULL
     OR product.currency <> NEW.currency
     OR product.deposit_basis_points <> NEW.deposit_basis_points
     OR (product.pricing_rules->>'legacy_tax_basis_points')::int <> NEW.tax_basis_points
     OR product.product_key || '-v' || product.version::text <> NEW.rate_card_version
     OR encode(digest(convert_to(product.pricing_rules::text, 'UTF8'), 'sha256'), 'hex')
          <> NEW.rate_card_rules_sha256
     OR (product.policy_snapshot->>'timezone') IS DISTINCT FROM NEW.timezone
     OR (product.policy_snapshot->>'terms_version') IS DISTINCT FROM NEW.terms_version
     OR NOT EXISTS (
       SELECT 1 FROM commerce_rate_card_review review
       WHERE review.product_version_id = product.id
         AND review.domain_type = product.domain_type
         AND review.status = 'approved'
         AND review.reviewed_by IS NOT NULL
         AND review.reviewed_at IS NOT NULL
     ) THEN
    RAISE EXCEPTION 'Domo quote runtime requires the exact approved active rate card';
  END IF;
  IF NEW.tax_minor <> (NEW.subtotal_minor * NEW.tax_basis_points + 5000) / 10000
     OR NEW.deposit_minor <> (NEW.total_minor * NEW.deposit_basis_points + 5000) / 10000 THEN
    RAISE EXCEPTION 'Domo quote tax or deposit does not match the approved basis points';
  END IF;
  RETURN NEW;
END $$;

COMMIT;
