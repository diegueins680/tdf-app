-- Verified public reviews for events, marketplace listings, and services.
--
-- Orders and bookings are private eligibility evidence. Public review rows keep
-- only the target and evidence kind/id, and the API never exposes source_id.
\set ON_ERROR_STOP on

BEGIN;

-- The directory owns the shared abuse-control table. Keep its closed set of
-- scopes explicit while allowing experience reviews to use the same durable
-- daily counter as profile reviews.
ALTER TABLE directory_rate_limit
  DROP CONSTRAINT IF EXISTS directory_rate_limit_scope_check;
ALTER TABLE directory_rate_limit
  ADD CONSTRAINT directory_rate_limit_scope_check
  CHECK (scope IN (
    'search','profile_create','classified_publish','application','invitation',
    'contact','report','review','experience-review'
  ));

CREATE TABLE IF NOT EXISTS experience_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  target_kind TEXT NOT NULL CHECK (target_kind IN (
    'event','marketplace_listing','service_offering','service_package'
  )),
  target_id TEXT NOT NULL CHECK (length(btrim(target_id)) BETWEEN 1 AND 80),
  source_kind TEXT NOT NULL CHECK (source_kind IN (
    'event_ticket_order','marketplace_order','service_booking','service_storefront_order'
  )),
  source_id TEXT NOT NULL CHECK (length(btrim(source_id)) BETWEEN 1 AND 80),
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  rating SMALLINT NOT NULL CHECK (rating BETWEEN 1 AND 5),
  body TEXT CHECK (
    body IS NULL OR (
      length(btrim(body)) BETWEEN 10 AND 2000
      AND body !~ '[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]'
    )
  ),
  status TEXT NOT NULL DEFAULT 'published'
    CHECK (status IN ('published','hidden','removed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (source_kind, source_id, target_kind, target_id, author_party_id)
);

CREATE INDEX IF NOT EXISTS experience_review_target_public_idx
  ON experience_review(target_kind, target_id, created_at DESC, id DESC)
  WHERE status = 'published';

CREATE INDEX IF NOT EXISTS experience_review_author_idx
  ON experience_review(author_party_id, created_at DESC, id DESC);

CREATE OR REPLACE FUNCTION experience_review_source_is_eligible(
  requested_target_kind TEXT,
  requested_target_id TEXT,
  requested_source_kind TEXT,
  requested_source_id TEXT,
  requested_author_party_id BIGINT
) RETURNS BOOLEAN
LANGUAGE plpgsql
STABLE
AS $$
BEGIN
  CASE requested_target_kind
    WHEN 'event' THEN
      RETURN requested_source_kind = 'event_ticket_order' AND EXISTS (
        SELECT 1
        FROM event_ticket_order orders
        JOIN social_event event ON event.id = orders.event_id
        LEFT JOIN event_ticket_checkout_runtime runtime ON runtime.order_id = orders.id
        WHERE orders.id::text = requested_source_id
          AND orders.event_id::text = requested_target_id
          AND orders.buyer_party_id = requested_author_party_id
          AND COALESCE(event.end_time, event.start_time) <= NOW()
          AND (
            (runtime.payment_status IN ('paid','partially_refunded')
              AND runtime.fulfillment_status IN ('issued','transferred','checked_in'))
            OR (runtime.order_id IS NULL AND lower(orders.status) IN ('paid','completed','fulfilled'))
          )
      );

    WHEN 'marketplace_listing' THEN
      RETURN requested_source_kind = 'marketplace_order' AND EXISTS (
        SELECT 1
        FROM marketplace_order orders
        JOIN marketplace_order_item item ON item.order_id = orders.id
        JOIN party author ON author.id = requested_author_party_id
        LEFT JOIN marketplace_sale_order_runtime sale ON sale.order_id = orders.id
        LEFT JOIN marketplace_rental_order_runtime rental ON rental.order_id = orders.id
        WHERE orders.id::text = requested_source_id
          AND item.listing_id::text = requested_target_id
          AND author.primary_email IS NOT NULL
          AND lower(btrim(orders.buyer_email)) = lower(btrim(author.primary_email))
          AND (
            sale.fulfillment_status IN ('delivered','closed')
            OR rental.rental_status = 'closed'
          )
      );

    WHEN 'service_offering' THEN
      RETURN requested_source_kind = 'service_booking' AND EXISTS (
        SELECT 1
        FROM booking booking
        LEFT JOIN service_booking_checkout_runtime runtime
          ON runtime.booking_id = booking.id
        WHERE booking.id::text = requested_source_id
          AND booking.party_id = requested_author_party_id
          AND booking.service_offering_id::text = requested_target_id
          AND (
            runtime.fulfillment_status = 'completed'
            OR (runtime.booking_id IS NULL AND lower(booking.status::text) = 'completed')
          )
      );

    WHEN 'service_package' THEN
      RETURN requested_source_kind = 'service_storefront_order' AND EXISTS (
        SELECT 1
        FROM service_storefront_order orders
        JOIN party author ON author.id = requested_author_party_id
        WHERE orders.id::text = requested_source_id
          AND orders.package_id::text = requested_target_id
          AND orders.status = 'completed'
          AND author.primary_email IS NOT NULL
          AND lower(btrim(orders.buyer_email)) = lower(btrim(author.primary_email))
      );

    ELSE
      RETURN FALSE;
  END CASE;
END $$;

CREATE OR REPLACE FUNCTION experience_review_validate_write()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'UPDATE' THEN
    IF ROW(
      NEW.target_kind, NEW.target_id, NEW.source_kind, NEW.source_id,
      NEW.author_party_id, NEW.rating, NEW.body, NEW.created_at
    ) IS DISTINCT FROM ROW(
      OLD.target_kind, OLD.target_id, OLD.source_kind, OLD.source_id,
      OLD.author_party_id, OLD.rating, OLD.body, OLD.created_at
    ) THEN
      RAISE EXCEPTION 'Published review evidence and content are immutable';
    END IF;
    NEW.updated_at := NOW();
    RETURN NEW;
  END IF;

  IF NOT experience_review_source_is_eligible(
    NEW.target_kind, NEW.target_id, NEW.source_kind, NEW.source_id, NEW.author_party_id
  ) THEN
    RAISE EXCEPTION 'Review requires an eligible completed interaction';
  END IF;

  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_experience_review_validate_write ON experience_review;
CREATE TRIGGER trg_experience_review_validate_write
  BEFORE INSERT OR UPDATE ON experience_review
  FOR EACH ROW EXECUTE FUNCTION experience_review_validate_write();

COMMENT ON TABLE experience_review IS
  'Public reviews backed by a private completed ticket order, marketplace order, service booking, or storefront order.';
COMMENT ON COLUMN experience_review.source_id IS
  'Private eligibility evidence; never include in public API responses.';

COMMIT;
