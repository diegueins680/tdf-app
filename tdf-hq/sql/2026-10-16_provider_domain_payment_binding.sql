-- Extend verified checkout admission without weakening legacy payment evidence.
-- Provider callbacks remain query triggers: authenticated server-to-server
-- verification in ProviderReconciliation is still required before success.
\set ON_ERROR_STOP on
BEGIN;
LOCK TABLE commerce_checkout_session IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION commerce_checkout_has_verified_payment(
  checkout commerce_checkout_session
) RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1
    FROM commerce_payment_attempt attempt
    JOIN commerce_provider_binding binding
      ON binding.payment_attempt_id = attempt.id
     AND binding.provider = attempt.provider
     AND binding.environment = attempt.environment
     AND binding.merchant_account_ref = attempt.merchant_account_ref
     AND binding.amount_minor = checkout.total_minor
     AND binding.currency = checkout.currency
    WHERE attempt.checkout_id = checkout.id
      AND attempt.status = 'succeeded'
      AND attempt.environment = checkout.environment
      AND attempt.amount_minor = checkout.total_minor
      AND attempt.currency = checkout.currency
      AND (
        (
          binding.merchant_reference = checkout.domain_order_id
          AND (
            (attempt.provider = 'datafast' AND binding.resource_type = 'checkout')
            OR (attempt.provider = 'paypal' AND binding.resource_type = 'capture')
            OR (attempt.provider = 'stripe' AND binding.resource_type = 'payment')
            OR (
              attempt.provider IN ('bank_transfer','cash','pos')
              AND binding.resource_type = 'manual_evidence'
              AND EXISTS (
                SELECT 1 FROM commerce_manual_payment_evidence evidence
                WHERE evidence.checkout_id = checkout.id
                  AND evidence.payment_attempt_id = attempt.id
                  AND evidence.status = 'approved'
                  AND evidence.submitted_by IS NOT NULL
                  AND evidence.reviewed_by IS NOT NULL
                  AND evidence.submitted_by <> evidence.reviewed_by
              )
            )
          )
        )
        OR (
          ((attempt.provider = 'placetopay' AND binding.resource_type = 'session')
            OR (attempt.provider = 'payphone' AND binding.resource_type = 'sale'))
          AND attempt.operation = 'create'
          AND EXISTS (
            SELECT 1
            FROM commerce_provider_operation operation
            JOIN commerce_payment_intent intent ON intent.id = attempt.payment_intent_id
            WHERE operation.payment_attempt_id = attempt.id
              AND operation.operation = 'create'
              AND operation.provider = attempt.provider
              AND operation.environment = attempt.environment
              AND operation.merchant_account_ref = attempt.merchant_account_ref
              AND operation.provider_reference = binding.merchant_reference
              AND operation.provider_resource_id = binding.provider_resource_id
              AND intent.checkout_id = checkout.id
              AND intent.provider = attempt.provider
              AND intent.amount_minor = checkout.total_minor
              AND intent.currency = checkout.currency
          )
        )
      )
  );
$$;

CREATE OR REPLACE FUNCTION service_booking_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'service_booking'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT commerce_checkout_has_verified_payment(NEW) THEN
    RAISE EXCEPTION 'Service booking checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION event_ticket_checkout_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'event_ticket_order'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT commerce_checkout_has_verified_payment(NEW) THEN
    RAISE EXCEPTION 'Event ticket checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION course_checkout_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'course_registration'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT commerce_checkout_has_verified_payment(NEW) THEN
    RAISE EXCEPTION 'Course checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION domo_quote_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'domo_event_quote'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT EXISTS (
       SELECT 1 FROM domo_event_quote_runtime runtime
       WHERE runtime.checkout_id = NEW.id
         AND runtime.quote_status = 'deposit_due'
         AND runtime.fulfillment_status = 'date_held'
         AND runtime.hold_expires_at > COALESCE(NEW.paid_at, NOW())
     ) THEN
    RAISE EXCEPTION 'Domo deposit checkout is not attached to an accepted active date hold';
  END IF;
  IF NEW.domain_type = 'domo_event_quote'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT commerce_checkout_has_verified_payment(NEW) THEN
    RAISE EXCEPTION 'Domo deposit checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

-- Retain this admission guard across application rollback; no evidence is rewritten.
COMMIT;
