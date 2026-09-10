-- Repair event-ticket review eligibility after buyer_party_id became textual.
--
-- The public ticket runtime stores an authenticated Party ID as canonical text.
-- Cast the BIGINT function argument to text so PostgreSQL never attempts an
-- undefined varchar = bigint comparison for eligible ticket buyers.
\set ON_ERROR_STOP on

BEGIN;

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
          AND orders.buyer_party_id = requested_author_party_id::text
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

COMMIT;
