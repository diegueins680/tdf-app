-- Additive repair: retain historical migration meaning. A conflict aborts this
-- migration; never pick a winner or silently cancel an existing booking.
BEGIN;
LOCK TABLE booking, booking_resource, service_booking_resource_allocation
  IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION service_booking_projected_booking_status(fulfillment TEXT)
RETURNS TEXT LANGUAGE sql IMMUTABLE AS $$
  SELECT CASE
    WHEN fulfillment = 'on_hold' THEN 'Tentative'
    WHEN fulfillment IN ('cancelled','expired') THEN 'Cancelled'
    WHEN fulfillment = 'completed' THEN 'Completed'
    WHEN fulfillment = 'no_show' THEN 'NoShow'
    WHEN fulfillment IN ('in_progress','balance_due','overtime_review','disputed') THEN 'InProgress'
    ELSE 'Confirmed' END;
$$;

CREATE OR REPLACE FUNCTION service_booking_sync_legacy_booking_allocation()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime service_booking_checkout_runtime%ROWTYPE;
DECLARE next_status TEXT;
BEGIN
  IF OLD.starts_at IS NOT DISTINCT FROM NEW.starts_at
     AND OLD.ends_at IS NOT DISTINCT FROM NEW.ends_at
     AND OLD.status IS NOT DISTINCT FROM NEW.status
     AND OLD.service_offering_id IS NOT DISTINCT FROM NEW.service_offering_id THEN
    RETURN NEW;
  END IF;
  SELECT * INTO runtime FROM service_booking_checkout_runtime WHERE booking_id = NEW.id;
  IF runtime.booking_id IS NOT NULL THEN
    IF NEW.starts_at IS DISTINCT FROM runtime.starts_at
       OR NEW.ends_at IS DISTINCT FROM runtime.ends_at
       OR NEW.service_offering_id IS DISTINCT FROM runtime.service_offering_id THEN
      RAISE EXCEPTION 'Booking cannot diverge from its checkout snapshot'
        USING ERRCODE = '23514', CONSTRAINT = 'booking_checkout_snapshot_correspondence';
    END IF;
    IF OLD.status IS DISTINCT FROM NEW.status AND NEW.status::text IS DISTINCT FROM
      service_booking_projected_booking_status(runtime.fulfillment_status) THEN
      RAISE EXCEPTION 'Booking lifecycle must follow its checkout fulfillment'
        USING ERRCODE = '23514', CONSTRAINT = 'booking_checkout_lifecycle_correspondence';
    END IF;
    next_status := CASE
      WHEN runtime.fulfillment_status IN ('cancelled','expired') THEN 'released'
      WHEN runtime.fulfillment_status = 'completed' THEN 'completed'
      WHEN runtime.fulfillment_status = 'on_hold' THEN 'holding'
      ELSE 'reserved' END;
  ELSE
    next_status := CASE
      WHEN NEW.status::text IN ('Cancelled','NoShow') THEN 'released'
      WHEN NEW.status::text = 'Completed' OR NEW.ends_at <= NOW() THEN 'completed'
      ELSE 'reserved' END;
  END IF;
  INSERT INTO service_booking_resource_allocation
    (booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at)
  SELECT NEW.id, relation.resource_id, NEW.starts_at, NEW.ends_at, next_status,
         COALESCE(runtime.hold_expires_at, NEW.ends_at)
    FROM booking_resource relation WHERE relation.booking_id = NEW.id
    GROUP BY relation.resource_id
  ON CONFLICT (booking_id, resource_id) DO UPDATE SET
    starts_at = EXCLUDED.starts_at, ends_at = EXCLUDED.ends_at,
    allocation_status = EXCLUDED.allocation_status,
    hold_expires_at = EXCLUDED.hold_expires_at, updated_at = NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_sync_legacy_allocation ON booking;
CREATE TRIGGER trg_service_booking_sync_legacy_allocation
  AFTER UPDATE OF starts_at, ends_at, status, service_offering_id ON booking
  FOR EACH ROW EXECUTE FUNCTION service_booking_sync_legacy_booking_allocation();

-- New resource relations use the same status interpretation. Lock the parent
-- before reading its interval so resource insertion cannot project a stale edit.
CREATE OR REPLACE FUNCTION service_booking_allocate_resource()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE booked booking%ROWTYPE;
DECLARE runtime service_booking_checkout_runtime%ROWTYPE;
DECLARE next_status TEXT;
BEGIN
  SELECT * INTO booked FROM booking WHERE id = NEW.booking_id FOR UPDATE;
  SELECT * INTO runtime FROM service_booking_checkout_runtime WHERE booking_id = NEW.booking_id;
  next_status := CASE
    WHEN runtime.booking_id IS NOT NULL THEN CASE
      WHEN runtime.fulfillment_status IN ('cancelled','expired') THEN 'released'
      WHEN runtime.fulfillment_status = 'completed' THEN 'completed'
      WHEN runtime.fulfillment_status = 'on_hold' THEN 'holding' ELSE 'reserved' END
    WHEN booked.status::text IN ('Cancelled','NoShow') THEN 'released'
    WHEN booked.status::text = 'Completed' OR booked.ends_at <= NOW() THEN 'completed'
    ELSE 'reserved' END;
  INSERT INTO service_booking_resource_allocation
    (booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at)
  VALUES (NEW.booking_id, NEW.resource_id, booked.starts_at, booked.ends_at,
          next_status, COALESCE(runtime.hold_expires_at, booked.ends_at))
  ON CONFLICT (booking_id, resource_id) DO UPDATE SET
    starts_at = EXCLUDED.starts_at, ends_at = EXCLUDED.ends_at,
    allocation_status = EXCLUDED.allocation_status,
    hold_expires_at = EXCLUDED.hold_expires_at, updated_at = NOW();
  RETURN NEW;
END $$;

-- Runtime fulfillment is authoritative for checkout-bound bookings. Its existing
-- transition/payment guards run first; project that accepted state into booking.
CREATE OR REPLACE FUNCTION service_booking_sync_domain_booking_status()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.fulfillment_status IS DISTINCT FROM NEW.fulfillment_status THEN
    UPDATE booking SET status = service_booking_projected_booking_status(NEW.fulfillment_status)
      WHERE id = NEW.booking_id;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_service_booking_sync_domain_booking_status ON service_booking_checkout_runtime;
CREATE TRIGGER trg_service_booking_sync_domain_booking_status
  AFTER UPDATE OF fulfillment_status ON service_booking_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION service_booking_sync_domain_booking_status();

-- Existing legacy rows may already have drifted. Repair their projection only
-- under exclusion enforcement. Past legacy rows remain completed projections,
-- matching the original migration; moving them into the future reacquires the
-- resource. Bound checkout evidence is never rewritten.
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM booking b JOIN service_booking_checkout_runtime r ON r.booking_id = b.id
    WHERE b.starts_at IS DISTINCT FROM r.starts_at OR b.ends_at IS DISTINCT FROM r.ends_at
       OR b.service_offering_id IS DISTINCT FROM r.service_offering_id
       OR b.status::text IS DISTINCT FROM service_booking_projected_booking_status(r.fulfillment_status)) THEN
    RAISE EXCEPTION 'Existing booking checkout snapshot or lifecycle divergence requires reconciliation'
      USING ERRCODE = '23514';
  END IF;
END $$;
INSERT INTO service_booking_resource_allocation
  (booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at)
SELECT b.id, relation.resource_id, b.starts_at, b.ends_at,
       CASE WHEN b.status::text IN ('Cancelled','NoShow') THEN 'released'
            WHEN b.status::text = 'Completed' OR b.ends_at <= NOW() THEN 'completed' ELSE 'reserved' END,
       b.ends_at
FROM booking b JOIN booking_resource relation ON relation.booking_id = b.id
WHERE NOT EXISTS (SELECT 1 FROM service_booking_checkout_runtime r WHERE r.booking_id = b.id)
GROUP BY b.id, relation.resource_id
ON CONFLICT (booking_id, resource_id) DO UPDATE SET
  starts_at = EXCLUDED.starts_at, ends_at = EXCLUDED.ends_at,
  allocation_status = EXCLUDED.allocation_status,
  hold_expires_at = EXCLUDED.hold_expires_at, updated_at = NOW();
COMMIT;
