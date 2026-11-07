BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM commerce_tax_document)
     OR EXISTS (SELECT 1 FROM event_ticket_billing_identity)
     OR EXISTS (SELECT 1 FROM event_ticket_checkout_policy WHERE tax_invoice_required) THEN
    RAISE EXCEPTION 'Retain invoicing evidence and buyer identification; use a forward repair';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_enqueue_tax_invoice ON event_ticket_checkout_runtime;
DROP FUNCTION IF EXISTS event_ticket_enqueue_tax_invoice();
DROP TABLE IF EXISTS commerce_tax_document;
DROP FUNCTION IF EXISTS commerce_tax_document_guard();
DROP TABLE IF EXISTS commerce_tax_issuer_point;
DROP TABLE IF EXISTS event_ticket_billing_identity;
DROP FUNCTION IF EXISTS event_ticket_billing_identity_immutable();
DROP TRIGGER IF EXISTS trg_event_ticket_tax_invoice_policy_immutable ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_tax_invoice_policy_immutable();
ALTER TABLE event_ticket_checkout_policy DROP COLUMN tax_invoice_required;
COMMIT;
