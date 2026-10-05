-- Uses only the disposable checkout migration fixture. No provider transaction.
BEGIN;
DO $$
DECLARE
  rejected BOOLEAN;
  quantity_before INTEGER;
BEGIN
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET max_tickets_per_order=5 WHERE event_id=1;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Approved ticket quantity limit is immutable; create a new policy version' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Approved limit was mutable'; END IF;

  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET approval_status='draft',active=FALSE WHERE event_id=1;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Published ticket policies cannot return to an earlier approval state' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Approved policy could be demoted before changing its limit'; END IF;

  UPDATE event_ticket_checkout_policy SET approval_status='retired',active=FALSE WHERE event_id=1;
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET approval_status='approved' WHERE event_id=1;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Published ticket policies cannot return to an earlier approval state' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Retired policy could be revived'; END IF;

  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET tax_bps=100 WHERE event_id=1;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Published ticket policy is immutable; create a new version' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Retired commercial terms could be edited'; END IF;

  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_checkout_policy SET max_tickets_per_order=5 WHERE event_id=1;
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Approved ticket quantity limit is immutable; create a new policy version' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Retired limit was mutable'; END IF;

  SELECT quantity_sold INTO quantity_before FROM event_ticket_tier WHERE id=1;
  rejected := FALSE;
  BEGIN
    UPDATE event_ticket_tier SET quantity_sold=quantity_sold+1 WHERE id=1;
    INSERT INTO event_ticket_checkout_runtime
      SELECT (jsonb_populate_record(NULL::event_ticket_checkout_runtime,
        to_jsonb(r) || jsonb_build_object('quantity',5))).*
      FROM event_ticket_checkout_runtime r WHERE order_id=1;
  EXCEPTION WHEN check_violation THEN
    IF SQLERRM <> 'Ticket quantity exceeds the purchased policy limit' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected THEN RAISE EXCEPTION 'Oversized order was accepted'; END IF;
  IF (SELECT quantity_sold FROM event_ticket_tier WHERE id=1) <> quantity_before THEN
    RAISE EXCEPTION 'Rejected oversized order left an inventory mutation';
  END IF;
END $$;
ROLLBACK;

-- Exact upper boundary with complete canonical snapshots. The checkout remains
-- unpaid, and this transaction is rolled back after checking the insert.
BEGIN;
INSERT INTO event_ticket_order(id,event_id,tier_id,buyer_name,buyer_email,
  quantity,amount_cents,currency,status,original_amount_cents,payment_method)
VALUES (3,1,1,'Limit boundary','limit@example.invalid',4,10608,'USD','pending',10400,'paypal');
INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,
  environment,currency,subtotal_minor,tax_minor,total_minor,customer_email,
  lookup_token_hash,idempotency_key,expires_at)
VALUES ('e5100000-0000-4000-8000-000000000003','event_ticket_order','3',
  'awaiting_payment','sandbox','USD',10608,0,10608,'limit@example.invalid',
  repeat('7',64),'ticket-checkout-idempotency-0003',NOW()+INTERVAL '10 minutes');
INSERT INTO event_ticket_checkout_runtime
  SELECT (jsonb_populate_record(NULL::event_ticket_checkout_runtime,
    to_jsonb(r) || jsonb_build_object(
      'order_id',3,'checkout_id','e5100000-0000-4000-8000-000000000003',
      'lookup_token_hash',repeat('8',64),'create_idempotency_key','ticket-runtime-idempotency-0003',
      'create_request_sha256',repeat('9',64),'quantity',4,'unit_price_minor',2600,
      'gross_face_value_minor',10400,'net_face_value_minor',10400,
      'buyer_fee_minor',208,'organizer_fee_minor',208,'checkout_total_minor',10608,
      'organizer_payable_minor',10192,'platform_fee_minor',416,
      'payment_status','awaiting_payment','fulfillment_status','seat_held','issued_at',NULL,
      'created_at',NOW(),'terms_accepted_at',NOW(),'hold_expires_at',NOW()+INTERVAL '10 minutes'
    ))).*
  FROM event_ticket_checkout_runtime r WHERE order_id=1;
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM event_ticket_checkout_runtime WHERE order_id=3 AND quantity=4) THEN
    RAISE EXCEPTION 'Exact quantity limit was rejected';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM event_ticket_checkout_policy_history
      WHERE event_id=1 AND snapshot->>'max_tickets_per_order'='4') THEN
    RAISE EXCEPTION 'Approved limit is absent from policy history';
  END IF;
END $$;
ROLLBACK;
