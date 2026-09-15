-- Test-only executable refinement of docs/event-operations/merch-expiry-test-contract.md.
-- Run after the owning migration fixture's competing reservation transactions finish.
BEGIN;
CREATE FUNCTION pg_temp.merch_expiry_snapshot() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'checkouts',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_checkout_session t),
    'orders',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM merch_order t),
    'variants',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM merch_product_variant t),
    'reservations',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM merch_inventory_reservation t),
    'holds',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_reservation_hold t),
    'payments',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM commerce_payment_attempt t),
    'fulfillment',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM merch_fulfillment_event t),
    'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM merch_audit_event t))
$$;
DO $$
DECLARE
  deadline TIMESTAMPTZ;
  before_tick JSONB;
  after_tick JSONB;
  paid_before JSONB;
  rejected BOOLEAN := FALSE;
  expired_checkouts INTEGER;
BEGIN
  IF (SELECT count(*) FROM commerce_checkout_session) <> 3
     OR (SELECT count(*) FROM commerce_checkout_session WHERE status='holding') <> 2
     OR (SELECT count(DISTINCT expires_at) FROM commerce_checkout_session WHERE status='holding') <> 1
     OR (SELECT count(*) FROM merch_inventory_reservation WHERE status='active' AND quantity=1) <> 1
     OR (SELECT count(*) FROM merch_inventory_reservation WHERE status='consumed' AND quantity=4) <> 1
     OR (SELECT count(*) FROM merch_inventory_reservation) <> 2 THEN
    RAISE EXCEPTION 'MX fixture requires two pending checkouts, one active unit and four consumed units';
  END IF;
  SELECT max(expires_at) INTO deadline FROM commerce_checkout_session WHERE status='holding';
  SELECT jsonb_build_object('checkout',to_jsonb(c),'order',to_jsonb(o),
    'reservation',to_jsonb(r),'hold',to_jsonb(h)) INTO paid_before
  FROM commerce_checkout_session c
  JOIN merch_order o ON o.checkout_id=c.id
  JOIN merch_inventory_reservation r ON r.checkout_id=c.id
  JOIN commerce_reservation_hold h ON h.id=r.commerce_hold_id
  WHERE c.id='9a000000-0000-4000-8000-000000000001' AND c.status='paid'
    AND o.payment_status='paid' AND r.status='consumed' AND h.status='consumed';
  IF paid_before IS NULL THEN RAISE EXCEPTION 'MX fixture requires paid/consumed canonical evidence'; END IF;

  before_tick := pg_temp.merch_expiry_snapshot();
  expired_checkouts := merch_release_expired_reservations(deadline - interval '1 microsecond');
  IF expired_checkouts IS DISTINCT FROM 0
     OR pg_temp.merch_expiry_snapshot() IS DISTINCT FROM before_tick THEN
    RAISE EXCEPTION 'MX-01: early expiry must be a complete no-op';
  END IF;

  expired_checkouts := merch_release_expired_reservations(deadline);
  IF expired_checkouts IS DISTINCT FROM 2 THEN
    RAISE EXCEPTION 'MX-02: expected 2 expired checkouts (not 1 reserved unit), got %', expired_checkouts;
  END IF;
  IF (SELECT count(*) FROM commerce_checkout_session WHERE status='expired' AND updated_at=deadline) <> 2
     OR (SELECT count(*) FROM merch_order WHERE payment_status='failed') <> 2
     OR (SELECT count(*) FROM merch_inventory_reservation r
         JOIN commerce_reservation_hold h ON h.id=r.commerce_hold_id
         WHERE r.status='expired' AND h.status='expired' AND r.quantity=1 AND h.quantity=1) <> 1
     OR EXISTS (SELECT 1 FROM merch_inventory_reservation WHERE status='active')
     OR EXISTS (SELECT 1 FROM commerce_reservation_hold WHERE status='active') THEN
    RAISE EXCEPTION 'MX-02: checkout/order/inventory/canonical-hold expiry projections diverged';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM merch_product_variant
      WHERE id='96000000-0000-4000-8000-000000000001' AND stock_reserved=0 AND stock_sold=4)
     OR paid_before IS DISTINCT FROM (
       SELECT jsonb_build_object('checkout',to_jsonb(c),'order',to_jsonb(o),
         'reservation',to_jsonb(r),'hold',to_jsonb(h))
       FROM commerce_checkout_session c
       JOIN merch_order o ON o.checkout_id=c.id
       JOIN merch_inventory_reservation r ON r.checkout_id=c.id
       JOIN commerce_reservation_hold h ON h.id=r.commerce_hold_id
       WHERE c.id='9a000000-0000-4000-8000-000000000001') THEN
    RAISE EXCEPTION 'MX-03: expiry altered sold stock or paid/consumed evidence';
  END IF;

  after_tick := pg_temp.merch_expiry_snapshot();
  expired_checkouts := merch_release_expired_reservations(deadline);
  IF expired_checkouts IS DISTINCT FROM 0
     OR pg_temp.merch_expiry_snapshot() IS DISTINCT FROM after_tick THEN
    RAISE EXCEPTION 'MX-04: same-instant expiry retry changed durable state';
  END IF;
  expired_checkouts := merch_release_expired_reservations(deadline + interval '1 hour');
  IF expired_checkouts IS DISTINCT FROM 0
     OR pg_temp.merch_expiry_snapshot() IS DISTINCT FROM after_tick THEN
    RAISE EXCEPTION 'MX-04: later expiry retry changed durable state';
  END IF;

  BEGIN
    PERFORM * FROM merch_reserve_stock(
      '98000000-0000-4000-8000-000000000002','9a000000-0000-4000-8000-000000000002',
      '[{"variantId":"96000000-0000-4000-8000-000000000001","quantity":1}]',deadline);
  EXCEPTION WHEN raise_exception THEN
    IF SQLERRM <> 'Canonical merch checkout is not eligible for stock reservation' THEN RAISE; END IF;
    rejected := TRUE;
  END;
  IF NOT rejected OR pg_temp.merch_expiry_snapshot() IS DISTINCT FROM after_tick THEN
    RAISE EXCEPTION 'MX-05: expired checkout allowed a new reservation or changed durable state';
  END IF;
END $$;
COMMIT;
