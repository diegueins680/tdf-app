-- Uses only the disposable checkout migration fixture. No provider transaction.
-- Event 4 requires electronic invoices; orders 21 and 22 are paid by reviewed
-- bank transfer evidence, which is the simplest canonical paid path here.
INSERT INTO party(id) VALUES (911), (912);
INSERT INTO social_event(id, title, start_time, end_time) VALUES
  (4, 'Invoiced pilot', NOW() + INTERVAL '30 days', NOW() + INTERVAL '31 days');
INSERT INTO event_ticket_tier(id, event_id, code, name, price_cents, currency,
  quantity_total, quantity_sold, is_active, allow_transfers) VALUES
  (4, 4, 'general', 'General', 2000, 'USD', 20, 2, TRUE, TRUE);
INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,
  buyer_fee_bps, organizer_fee_bps, tax_bps, hold_minutes, terms_version,
  terms_summary, refund_policy, max_tickets_per_order, tax_invoice_required) VALUES
  (4, 'invoiced-v1', 'USD', 0, 0, 0, 10, 'invoiced-terms-v1', 'Invoiced terms.',
   'Invoiced refund policy.', 4, TRUE);
UPDATE event_ticket_checkout_policy
  SET approval_status = 'approved', active = TRUE, approved_at = NOW(), approved_by = 'invoice-test'
  WHERE event_id = 4;

DO $$
DECLARE rejected BOOLEAN := FALSE;
BEGIN
  BEGIN
    UPDATE event_ticket_checkout_policy SET tax_invoice_required = FALSE WHERE event_id = 4;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM NOT IN ('Approved invoicing requirement is immutable; create a new policy version',
                       'Published ticket policy is immutable; create a new version') THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Approved invoicing requirement was mutable'; END IF;
END $$;

CREATE TEMP TABLE invoice_case(order_id BIGINT, checkout_id UUID, attempt_id UUID);
INSERT INTO invoice_case VALUES
  (21, 'e7100000-0000-4000-8000-000000000021', 'e7200000-0000-4000-8000-000000000021'),
  (22, 'e7100000-0000-4000-8000-000000000022', 'e7200000-0000-4000-8000-000000000022');
INSERT INTO event_ticket_order(id, event_id, tier_id, buyer_name, buyer_email, quantity,
  amount_cents, currency, status, original_amount_cents, payment_method)
SELECT order_id, 4, 4, 'Invoice buyer', 'invoice' || order_id || '@example.invalid',
  1, 2000, 'USD', 'pending', 2000, 'bank_transfer' FROM invoice_case;
INSERT INTO commerce_checkout_session(id, domain_type, domain_order_id, status, environment,
  currency, subtotal_minor, tax_minor, total_minor, customer_email, lookup_token_hash,
  idempotency_key, expires_at)
SELECT checkout_id, 'event_ticket_order', order_id::text, 'awaiting_payment', 'sandbox',
  'USD', 2000, 0, 2000, 'invoice' || order_id || '@example.invalid',
  md5('ilookup' || order_id) || md5('ilookup2' || order_id),
  'invoice-checkout-idempotency-' || order_id, NOW() + INTERVAL '1 hour'
FROM invoice_case;
INSERT INTO event_ticket_checkout_runtime(order_id, event_id, tier_id, checkout_id, policy_id,
  policy_version, lookup_token_hash, create_idempotency_key, create_request_sha256,
  quantity, currency, unit_price_minor, gross_face_value_minor, discount_minor,
  net_face_value_minor, buyer_fee_bps, buyer_fee_minor, organizer_fee_bps,
  organizer_fee_minor, tax_bps, tax_minor, checkout_total_minor, organizer_payable_minor,
  platform_fee_minor, terms_version, terms_accepted_at, hold_expires_at)
SELECT c.order_id, 4, 4, c.checkout_id, p.id, p.policy_version,
  md5('iruntime' || c.order_id) || md5('iruntime2' || c.order_id),
  'invoice-runtime-idempotency-' || c.order_id,
  md5('irequest' || c.order_id) || md5('irequest2' || c.order_id),
  1, 'USD', 2000, 2000, 0, 2000, 0, 0, 0, 0, 0, 0, 2000, 2000, 0,
  'invoiced-terms-v1', NOW(), NOW() + INTERVAL '1 hour'
FROM invoice_case c JOIN event_ticket_checkout_policy p ON p.event_id = 4;
INSERT INTO event_ticket_billing_identity(order_id, id_type, id_number, legal_name)
VALUES (21, 'consumidor_final', NULL, NULL), (22, 'cedula', '1716535511', 'Ana Perez');

DO $$
DECLARE rejected BOOLEAN := FALSE;
BEGIN
  BEGIN
    UPDATE event_ticket_billing_identity SET legal_name = 'Other' WHERE order_id = 22;
  EXCEPTION WHEN raise_exception THEN rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Billing identity was mutable'; END IF;
  rejected := FALSE;
  BEGIN
    INSERT INTO event_ticket_billing_identity(order_id, id_type, id_number, legal_name)
    VALUES (21, 'cedula', NULL, 'Missing number');
  EXCEPTION WHEN check_violation OR unique_violation THEN rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Incomplete billing identity was accepted'; END IF;
END $$;

INSERT INTO commerce_payment_attempt(id, checkout_id, provider, environment, operation,
  status, amount_minor, currency, merchant_account_ref, idempotency_key)
SELECT attempt_id, checkout_id, 'bank_transfer', 'sandbox', 'manual_verify', 'requires_review',
  2000, 'USD', 'tdf-manual-settlement', 'invoice-attempt-' || order_id FROM invoice_case;
INSERT INTO commerce_manual_payment_evidence(checkout_id, payment_attempt_id, status)
SELECT checkout_id, attempt_id, 'awaiting_evidence' FROM invoice_case;
UPDATE commerce_manual_payment_evidence evidence
  SET status = 'submitted', customer_reference = 'REF-' || c.order_id, submitted_amount_minor = 2000,
      currency = 'USD', submitted_at = NOW(), submitted_by = 911
  FROM invoice_case c WHERE evidence.payment_attempt_id = c.attempt_id;
UPDATE commerce_manual_payment_evidence evidence
  SET status = 'under_review', reviewed_by = 912, review_notes = 'Checking statement'
  FROM invoice_case c WHERE evidence.payment_attempt_id = c.attempt_id;
UPDATE commerce_manual_payment_evidence evidence
  SET status = 'approved', reviewed_at = NOW(), review_notes = 'Deposit matched'
  FROM invoice_case c WHERE evidence.payment_attempt_id = c.attempt_id;
INSERT INTO commerce_provider_binding(payment_attempt_id, provider, environment,
  merchant_account_ref, resource_type, provider_resource_id, merchant_reference,
  amount_minor, currency)
SELECT c.attempt_id, 'bank_transfer', 'sandbox', 'tdf-manual-settlement', 'manual_evidence',
  evidence.id::text, c.order_id::text, 2000, 'USD'
FROM invoice_case c JOIN commerce_manual_payment_evidence evidence ON evidence.payment_attempt_id = c.attempt_id;
UPDATE commerce_payment_attempt SET status = 'succeeded'
  WHERE id IN (SELECT attempt_id FROM invoice_case);

-- Without an enabled issuer point, a policy that requires invoices cannot become paid.
DO $$
DECLARE rejected BOOLEAN := FALSE;
BEGIN
  BEGIN
    UPDATE commerce_checkout_session SET status = 'paid', paid_minor = 2000, paid_at = NOW()
      WHERE id = 'e7100000-0000-4000-8000-000000000021';
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Invoicing is required for this ticket policy but no issuer point is enabled' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Paid without an invoice issuer point'; END IF;
END $$;

INSERT INTO commerce_tax_issuer_point(environment, provider, establishment, emission_point,
  next_sequential, enabled) VALUES ('sandbox', 'datil', '001', '002', 41, TRUE);
UPDATE commerce_checkout_session SET status = 'paid', paid_minor = 2000, paid_at = NOW()
  WHERE id IN (SELECT checkout_id FROM invoice_case);

DO $$
DECLARE
  rejected BOOLEAN := FALSE;
  document_count INTEGER;
BEGIN
  SELECT count(*) INTO document_count FROM commerce_tax_document;
  IF document_count <> 2 THEN RAISE EXCEPTION 'Expected one invoice per paid order, found %', document_count; END IF;
  IF (SELECT array_agg(sequential ORDER BY sequential) FROM commerce_tax_document) <> ARRAY[41, 42]::bigint[] THEN
    RAISE EXCEPTION 'Invoice sequence was not allocated contiguously';
  END IF;
  IF (SELECT next_sequential FROM commerce_tax_issuer_point WHERE environment = 'sandbox') <> 43 THEN
    RAISE EXCEPTION 'Issuer point sequence did not advance';
  END IF;

  -- A repeated paid transition cannot enqueue a second invoice.
  UPDATE event_ticket_checkout_runtime SET payment_status = 'paid' WHERE order_id = 21;
  IF (SELECT count(*) FROM commerce_tax_document) <> 2 THEN RAISE EXCEPTION 'Duplicate invoice enqueued'; END IF;

  BEGIN
    UPDATE commerce_tax_document SET sequential = 99 WHERE domain_order_id = '21';
  EXCEPTION WHEN raise_exception THEN rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Invoice number was mutable'; END IF;

  UPDATE commerce_tax_document
    SET access_key = '2410202601179321509200110010020000000411234567811', issued_on = CURRENT_DATE,
        status = 'authorized', authorization_number = '2410202601179321509200110010020000000411234567811',
        authorized_at = NOW()
    WHERE domain_order_id = '21';
  rejected := FALSE;
  BEGIN
    UPDATE commerce_tax_document SET status = 'failed' WHERE domain_order_id = '21';
  EXCEPTION WHEN raise_exception THEN rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Authorized invoice was reopened'; END IF;
  rejected := FALSE;
  BEGIN
    UPDATE commerce_tax_document SET access_key = '1111111111111111111111111111111111111111111111111'
      WHERE domain_order_id = '21';
  EXCEPTION WHEN raise_exception THEN rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Bound access key was mutable'; END IF;
END $$;
