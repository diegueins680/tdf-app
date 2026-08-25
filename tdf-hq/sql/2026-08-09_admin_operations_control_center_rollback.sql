BEGIN;

-- Safe rollback: stop visibility, capture, and provider delivery while retaining
-- all events, work, approvals, delivery attempts, and immutable audit evidence.
UPDATE operations_organization SET operations_enabled = FALSE, updated_at = now()
WHERE operations_enabled IS DISTINCT FROM FALSE;
UPDATE operations_provider_config SET enabled = FALSE, updated_at = now()
WHERE enabled IS DISTINCT FROM FALSE;

DROP TRIGGER IF EXISTS operations_course_registration_capture ON course_registration;
DROP TRIGGER IF EXISTS operations_booking_capture ON booking;
DROP TRIGGER IF EXISTS operations_invoice_capture ON invoice;
DROP TRIGGER IF EXISTS operations_payment_capture ON payment;
DROP TRIGGER IF EXISTS operations_registration_receipt_capture ON course_registration_receipt;
DROP TRIGGER IF EXISTS operations_marketplace_order_capture ON marketplace_order;
DROP TRIGGER IF EXISTS operations_maintenance_capture ON maintenance_ticket;
DROP TRIGGER IF EXISTS operations_service_order_capture ON service_order;
DROP TRIGGER IF EXISTS operations_package_purchase_capture ON package_purchase;
DROP TRIGGER IF EXISTS operations_lead_interest_capture ON lead_interest;
DROP TRIGGER IF EXISTS operations_trial_request_capture ON trial_request;
DROP TRIGGER IF EXISTS operations_artist_profile_capture ON artist_profile;
DROP TRIGGER IF EXISTS operations_intern_task_capture ON intern_task;
DROP TRIGGER IF EXISTS operations_integration_failure_capture ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_whatsapp_inbound_capture ON whats_app_message;
DROP TRIGGER IF EXISTS operations_instagram_inbound_capture ON instagram_message;
DROP TRIGGER IF EXISTS operations_facebook_inbound_capture ON facebook_message;
DROP TRIGGER IF EXISTS operations_feature_access_request_capture ON feature_access_requests;
DROP TRIGGER IF EXISTS operations_proposal_capture ON proposal;
DROP TRIGGER IF EXISTS operations_stock_item_capture ON stock_item;
DROP TRIGGER IF EXISTS operations_intern_project_capture ON intern_project;
DROP TRIGGER IF EXISTS operations_social_event_capture ON social_event;

COMMIT;
