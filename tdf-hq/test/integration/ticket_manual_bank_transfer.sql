-- Uses only the disposable checkout migration fixture. No provider transaction.
-- Event 2 opts into manual bank transfer: 24h hold, selections close in 2 days.
-- Event 3 has the same terms but its selection cutoff already passed.
INSERT INTO party(id) VALUES (901), (902);
INSERT INTO social_event(id, title, start_time, end_time) VALUES
  (2, 'Manual transfer pilot', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days'),
  (3, 'Closed transfer window', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days');
INSERT INTO event_ticket_tier(id, event_id, code, name, price_cents, currency,
  quantity_total, quantity_sold, is_active, allow_transfers) VALUES
  (2, 2, 'general', 'General', 2000, 'USD', 20, 0, TRUE, TRUE),
  (3, 3, 'general', 'General', 2000, 'USD', 20, 0, TRUE, TRUE);
INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,
  buyer_fee_bps, organizer_fee_bps, tax_bps, hold_minutes, terms_version,
  terms_summary, refund_policy, max_tickets_per_order,
  manual_transfer_hold_minutes, manual_transfer_cutoff_at) VALUES
  (2, 'manual-v1', 'USD', 0, 0, 0, 10, 'manual-terms-v1', 'Manual pilot terms.',
   'Manual pilot refund policy.', 4, 1440, NOW() + INTERVAL '2 days'),
  (3, 'manual-closed-v1', 'USD', 0, 0, 0, 10, 'manual-terms-v1', 'Closed pilot terms.',
   'Closed pilot refund policy.', 4, 1440, NOW() - INTERVAL '1 minute');
UPDATE event_ticket_checkout_policy
  SET approval_status = 'approved', active = TRUE, approved_at = NOW(), approved_by = 'manual-test'
  WHERE event_id IN (2, 3);

-- Hold and terms bounds are enforced by the database, not by the handler.
DO $$
DECLARE
  rejected BOOLEAN;
BEGIN
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET manual_transfer_hold_minutes = 60 WHERE event_id = 2;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Approved manual transfer terms are immutable; create a new policy version' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Approved manual transfer terms were mutable'; END IF;

  rejected := FALSE;
  BEGIN
    INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,
      terms_version, terms_summary, refund_policy, manual_transfer_hold_minutes)
    VALUES (2, 'manual-unpaired', 'USD', 'manual-terms-v1', 'x', 'x', 1440);
  EXCEPTION WHEN check_violation THEN
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Manual transfer hold without a cutoff was accepted'; END IF;
END $$;

CREATE TEMP TABLE manual_case(order_id BIGINT, tier_id BIGINT, event_id BIGINT,
  checkout_id UUID, hold INTERVAL);
INSERT INTO manual_case VALUES
  (11, 2, 2, 'e6100000-0000-4000-8000-000000000011', INTERVAL '10 minutes'),
  (12, 2, 2, 'e6100000-0000-4000-8000-000000000012', INTERVAL '2 seconds'),
  (13, 3, 3, 'e6100000-0000-4000-8000-000000000013', INTERVAL '10 minutes');

UPDATE event_ticket_tier SET quantity_sold = quantity_sold + 2 WHERE id = 2;
UPDATE event_ticket_tier SET quantity_sold = quantity_sold + 1 WHERE id = 3;
INSERT INTO event_ticket_order(id, event_id, tier_id, buyer_name, buyer_email, quantity,
  amount_cents, currency, status, original_amount_cents, payment_method)
SELECT order_id, event_id, tier_id, 'Transfer buyer', 'buyer' || order_id || '@example.invalid',
  1, 2000, 'USD', 'pending', 2000, 'bank_transfer'
FROM manual_case;
INSERT INTO commerce_checkout_session(id, domain_type, domain_order_id, status, environment,
  currency, subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash,
  idempotency_key, expires_at)
SELECT checkout_id, 'event_ticket_order', order_id::text, 'awaiting_payment', 'sandbox',
  'USD', 2000, 0, 2000, 'buyer' || order_id || '@example.invalid',
  md5('lookup' || order_id) || md5('lookup2' || order_id),
  'manual-checkout-idempotency-' || order_id, NOW() + hold
FROM manual_case;
INSERT INTO event_ticket_checkout_runtime(order_id, event_id, tier_id, checkout_id, policy_id,
  policy_version, lookup_token_hash, create_idempotency_key, create_request_sha256,
  quantity, currency, unit_price_minor, gross_face_value_minor, discount_minor,
  net_face_value_minor, buyer_fee_bps, buyer_fee_minor, organizer_fee_bps,
  organizer_fee_minor, tax_bps, tax_minor, checkout_total_minor, organizer_payable_minor,
  platform_fee_minor, terms_version, terms_accepted_at, hold_expires_at)
SELECT c.order_id, c.event_id, c.tier_id, c.checkout_id, p.id, p.policy_version,
  md5('runtime' || c.order_id) || md5('runtime2' || c.order_id),
  'manual-runtime-idempotency-' || c.order_id,
  md5('request' || c.order_id) || md5('request2' || c.order_id),
  1, 'USD', 2000, 2000, 0, 2000, 0, 0, 0, 0, 0, 0, 2000, 2000, 0,
  'manual-terms-v1', NOW(), NOW() + c.hold
FROM manual_case c JOIN event_ticket_checkout_policy p ON p.event_id = c.event_id;

DO $$
DECLARE
  rejected BOOLEAN;
BEGIN
  -- Without a selected bank transfer attempt the hold cannot grow.
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_runtime
      SET manual_hold_expires_at = NOW() + INTERVAL '1 hour' WHERE order_id = 11;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Manual transfer hold extension is outside the approved policy' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Hold grew without a bank transfer selection'; END IF;
END $$;

INSERT INTO commerce_payment_attempt(id, checkout_id, provider, environment, operation,
  status, amount_minor, currency, merchant_account_ref, idempotency_key)
SELECT ('e6200000-0000-4000-8000-0000000000' || order_id)::uuid, checkout_id,
  'bank_transfer', 'sandbox', 'manual_verify', 'requires_review', 2000, 'USD',
  'tdf-manual-settlement', 'manual-attempt-idempotency-' || order_id
FROM manual_case;

-- Order 12 is extended before its original two-second hold passes.
UPDATE event_ticket_checkout_runtime
  SET manual_hold_expires_at = NOW() + INTERVAL '1 hour', manual_submitter_party_id = 901
  WHERE order_id = 12;

DO $$
DECLARE
  rejected BOOLEAN;
BEGIN
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_runtime
      SET manual_hold_expires_at = NOW() + INTERVAL '1441 minutes' WHERE order_id = 11;
  EXCEPTION WHEN raise_exception THEN
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Hold grew beyond the approved transfer window'; END IF;

  UPDATE event_ticket_checkout_runtime
    SET manual_hold_expires_at = NOW() + INTERVAL '23 hours' WHERE order_id = 11;

  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_runtime
      SET manual_hold_expires_at = NOW() + INTERVAL '22 hours' WHERE order_id = 11;
  EXCEPTION WHEN raise_exception THEN
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'An extended hold was shortened'; END IF;

  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_runtime
      SET manual_hold_expires_at = NOW() + INTERVAL '1 hour' WHERE order_id = 13;
  EXCEPTION WHEN raise_exception THEN
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'A first transfer selection started after the cutoff'; END IF;

  UPDATE event_ticket_checkout_runtime SET manual_submitter_party_id = 901 WHERE order_id = 11;
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_runtime SET manual_submitter_party_id = 902 WHERE order_id = 11;
  EXCEPTION WHEN raise_exception THEN
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'The recorded transfer submitter was replaced'; END IF;

  -- Extended holds survive the original expiry; unextended ones still expire.
  PERFORM event_ticket_checkout_expire_holds(NOW() + INTERVAL '11 minutes', NULL, 2);
  IF (SELECT status FROM commerce_checkout_session
      WHERE id = 'e6100000-0000-4000-8000-000000000011') <> 'awaiting_payment' THEN
    RAISE EXCEPTION 'An extended transfer hold expired at the original deadline';
  END IF;
  PERFORM event_ticket_checkout_expire_holds(NOW() + INTERVAL '11 minutes', NULL, 3);
  IF (SELECT status FROM commerce_checkout_session
      WHERE id = 'e6100000-0000-4000-8000-000000000013') <> 'expired' THEN
    RAISE EXCEPTION 'An unextended hold survived its deadline';
  END IF;
  IF (SELECT quantity_sold FROM event_ticket_tier WHERE id = 3) <> 0 THEN
    RAISE EXCEPTION 'Expired hold did not release inventory exactly once';
  END IF;
END $$;

-- Order 12: extend, let the original two-second hold pass, then settle it by
-- independently reviewed evidence. The paid transition must accept the
-- extended hold and the issuance must follow canonical payment.
SELECT pg_sleep(3);
INSERT INTO commerce_manual_payment_evidence(checkout_id, payment_attempt_id, status)
VALUES ('e6100000-0000-4000-8000-000000000012', 'e6200000-0000-4000-8000-000000000012',
  'awaiting_evidence');
UPDATE commerce_manual_payment_evidence
  SET status = 'submitted', customer_reference = 'BANK-REF-12', submitted_amount_minor = 2000,
      currency = 'USD', submitted_at = NOW(), submitted_by = 901
  WHERE payment_attempt_id = 'e6200000-0000-4000-8000-000000000012';
UPDATE commerce_manual_payment_evidence
  SET status = 'under_review', reviewed_by = 902, review_notes = 'Checking bank statement'
  WHERE payment_attempt_id = 'e6200000-0000-4000-8000-000000000012';
UPDATE commerce_manual_payment_evidence
  SET status = 'approved', reviewed_at = NOW(), review_notes = 'Deposit matched'
  WHERE payment_attempt_id = 'e6200000-0000-4000-8000-000000000012';
INSERT INTO commerce_provider_binding(payment_attempt_id, provider, environment,
  merchant_account_ref, resource_type, provider_resource_id, merchant_reference,
  amount_minor, currency)
SELECT 'e6200000-0000-4000-8000-000000000012', 'bank_transfer', 'sandbox',
  'tdf-manual-settlement', 'manual_evidence', id::text, '12', 2000, 'USD'
FROM commerce_manual_payment_evidence
WHERE payment_attempt_id = 'e6200000-0000-4000-8000-000000000012';
UPDATE commerce_payment_attempt SET status = 'succeeded'
  WHERE id = 'e6200000-0000-4000-8000-000000000012';
UPDATE commerce_checkout_session SET status = 'paid', paid_minor = 2000, paid_at = NOW()
  WHERE id = 'e6100000-0000-4000-8000-000000000012';
UPDATE event_ticket_checkout_runtime SET fulfillment_status = 'issued', issued_at = NOW()
  WHERE order_id = 12;

DO $$
BEGIN
  IF (SELECT payment_status || ':' || fulfillment_status
      FROM event_ticket_checkout_runtime WHERE order_id = 12) <> 'paid:issued' THEN
    RAISE EXCEPTION 'Reviewed transfer did not settle the extended hold';
  END IF;
END $$;
