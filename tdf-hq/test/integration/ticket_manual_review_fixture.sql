-- Fixture for test/TicketManualReviewMain.hs. Load only into the disposable
-- tdf_ticket_manual_review_test database after the production schema snapshot
-- and the production migration batch. No provider or bank is contacted.
BEGIN;
-- Staff: organizer of event 7001, an unrelated strict administrator, a strict
-- administrator who owns the buyer email of every order, and other staff.
INSERT INTO party(id, display_name, is_org, primary_email, created_at) VALUES
  (7101, 'Organizadora', FALSE, 'organizadora@example.invalid', NOW()),
  (7102, 'Administrador', FALSE, 'admin@example.invalid', NOW()),
  (7103, 'Admin comprador', FALSE, ' Comprador@Example.invalid ', NOW()),
  (7104, 'Recepción', FALSE, 'recepcion@example.invalid', NOW());

-- Any active published event type satisfies catalog_validate_social_event_type.
-- The event is on sale in the social-event-lifecycle workflow, like event 141.
INSERT INTO social_event(id, organizer_party_id, title, start_time, end_time,
  event_type_id, workflow_state_id)
SELECT 7001, '7101', 'Transferencias verificadas', NOW() + INTERVAL '30 days',
  NOW() + INTERVAL '31 days', item.id,
  (SELECT lifecycle.id FROM workflow_state lifecycle
   JOIN workflow_definition workflow ON workflow.id = lifecycle.workflow_id
   WHERE workflow.code = 'social-event-lifecycle' AND workflow.active
     AND lifecycle.active AND lifecycle.code = 'on_sale')
FROM event_type item
JOIN catalog_definition catalog ON catalog.id = item.catalog_id
  AND catalog.code = 'event-types' AND catalog.active
JOIN workflow_state state ON state.id = item.workflow_state_id
  AND state.workflow_id = catalog.workflow_id AND state.code = 'published' AND state.active
WHERE item.active AND item.deprecated_at IS NULL
ORDER BY item.id LIMIT 1;
INSERT INTO event_ticket_tier(id, event_id, code, name, price_cents, currency,
  quantity_total, quantity_sold, is_active, allow_transfers) VALUES
  (7001, 7001, 'general', 'General', 2000, 'USD', 50, 0, TRUE, TRUE);
INSERT INTO event_ticket_checkout_policy(event_id, policy_version, currency,
  buyer_fee_bps, organizer_fee_bps, tax_bps, hold_minutes, terms_version,
  terms_summary, refund_policy, max_tickets_per_order,
  manual_transfer_hold_minutes, manual_transfer_cutoff_at) VALUES
  (7001, 'manual-review-v1', 'USD', 0, 0, 0, 10, 'manual-review-terms-v1',
   'Términos de prueba.', 'Reembolsos de prueba.', 4, 1440, NOW() + INTERVAL '2 days');
UPDATE event_ticket_checkout_policy
  SET approval_status = 'approved', active = TRUE, approved_at = NOW(),
      approved_by = 'manual-review-fixture'
  WHERE event_id = 7001;

-- The operator configuration for the manual rail (docs/events/
-- ticket-manual-bank-transfer.md), applied to the sandbox environment only.
UPDATE commerce_provider_account
  SET status = 'ready', enabled = TRUE, credential_status = 'validated',
      contract_status = 'approved', merchant_account_ref = 'tdf-manual-settlement',
      verified_at = NOW(), verified_by = 7102, disabled_reason = NULL
  WHERE provider = 'bank_transfer' AND environment = 'sandbox';
UPDATE commerce_provider_capability capability
  SET verification_status = 'sandbox_verified', verified_at = NOW()
  FROM commerce_provider_account account
  WHERE capability.provider_account_id = account.id
    AND account.provider = 'bank_transfer' AND account.environment = 'sandbox';
COMMIT;
