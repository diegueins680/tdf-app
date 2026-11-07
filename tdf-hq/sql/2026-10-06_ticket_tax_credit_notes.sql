-- SRI credit notes for verified refunds of invoiced ticket orders. The internal
-- credit-note receipt written by the canonical refund store enqueues exactly one
-- provider credit note per refund, numbered from its own series and bound to the
-- order's invoice. A document records when a provider submission started, so an
-- interrupted submission is never mistaken for one that was never sent.
BEGIN;
ALTER TABLE commerce_tax_issuer_point
  ADD COLUMN IF NOT EXISTS next_credit_note_sequential BIGINT NOT NULL DEFAULT 1
    CHECK (next_credit_note_sequential BETWEEN 1 AND 999999999);

ALTER TABLE commerce_tax_document
  ADD COLUMN IF NOT EXISTS refund_id UUID REFERENCES commerce_refund(id) ON DELETE RESTRICT,
  ADD COLUMN IF NOT EXISTS related_document_id UUID REFERENCES commerce_tax_document(id) ON DELETE RESTRICT,
  ADD COLUMN IF NOT EXISTS submitted_at TIMESTAMPTZ;

ALTER TABLE commerce_tax_document DROP CONSTRAINT IF EXISTS commerce_tax_document_kind_check;
ALTER TABLE commerce_tax_document ADD CONSTRAINT commerce_tax_document_kind_check
  CHECK (kind IN ('invoice','credit_note'));
ALTER TABLE commerce_tax_document DROP CONSTRAINT IF EXISTS commerce_tax_document_kind_domain_type_domain_order_id_key;
ALTER TABLE commerce_tax_document DROP CONSTRAINT IF EXISTS commerce_tax_document_credit_note_binding;
ALTER TABLE commerce_tax_document ADD CONSTRAINT commerce_tax_document_credit_note_binding
  CHECK ((kind = 'credit_note') = (refund_id IS NOT NULL AND related_document_id IS NOT NULL));
CREATE UNIQUE INDEX IF NOT EXISTS commerce_tax_document_one_invoice_per_order
  ON commerce_tax_document(domain_type, domain_order_id) WHERE kind = 'invoice';
CREATE UNIQUE INDEX IF NOT EXISTS commerce_tax_document_one_credit_note_per_refund
  ON commerce_tax_document(refund_id) WHERE kind = 'credit_note';

CREATE OR REPLACE FUNCTION commerce_tax_document_guard()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF ROW(NEW.kind, NEW.domain_type, NEW.domain_order_id, NEW.checkout_id, NEW.environment,
         NEW.provider, NEW.establishment, NEW.emission_point, NEW.sequential,
         NEW.amount_minor, NEW.currency, NEW.created_at, NEW.refund_id, NEW.related_document_id)
     IS DISTINCT FROM
     ROW(OLD.kind, OLD.domain_type, OLD.domain_order_id, OLD.checkout_id, OLD.environment,
         OLD.provider, OLD.establishment, OLD.emission_point, OLD.sequential,
         OLD.amount_minor, OLD.currency, OLD.created_at, OLD.refund_id, OLD.related_document_id)
     OR (OLD.access_key IS NOT NULL AND NEW.access_key IS DISTINCT FROM OLD.access_key)
     OR (OLD.issued_on IS NOT NULL AND NEW.issued_on IS DISTINCT FROM OLD.issued_on)
     OR (OLD.submitted_at IS NOT NULL AND NEW.submitted_at IS DISTINCT FROM OLD.submitted_at
         AND NOT (OLD.status = 'failed' AND NEW.status = 'pending' AND NEW.submitted_at IS NULL)) THEN
    RAISE EXCEPTION 'Tax document identity is immutable';
  END IF;
  IF OLD.status = 'authorized' AND NEW.status <> 'authorized' THEN
    RAISE EXCEPTION 'Authorized tax documents are final';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION commerce_enqueue_tax_credit_note()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  invoice commerce_tax_document%ROWTYPE;
  point commerce_tax_issuer_point%ROWTYPE;
BEGIN
  IF NEW.kind <> 'credit_note' OR NEW.refund_id IS NULL THEN
    RETURN NEW;
  END IF;
  SELECT * INTO invoice FROM commerce_tax_document
    WHERE kind = 'invoice' AND checkout_id = NEW.checkout_id;
  IF invoice.id IS NULL THEN
    RETURN NEW;
  END IF;
  IF EXISTS (SELECT 1 FROM commerce_tax_document WHERE kind = 'credit_note' AND refund_id = NEW.refund_id) THEN
    RETURN NEW;
  END IF;
  SELECT * INTO point FROM commerce_tax_issuer_point
    WHERE environment = invoice.environment FOR UPDATE;
  IF point.environment IS NULL THEN
    RAISE EXCEPTION 'A credit note is required but the invoice issuer point is missing';
  END IF;
  INSERT INTO commerce_tax_document(kind, domain_type, domain_order_id, checkout_id, environment,
    provider, establishment, emission_point, sequential, amount_minor, currency, refund_id,
    related_document_id)
  VALUES ('credit_note', invoice.domain_type, invoice.domain_order_id, invoice.checkout_id,
    invoice.environment, invoice.provider, point.establishment, point.emission_point,
    point.next_credit_note_sequential, NEW.amount_minor, NEW.currency, NEW.refund_id, invoice.id);
  UPDATE commerce_tax_issuer_point
    SET next_credit_note_sequential = next_credit_note_sequential + 1, updated_at = NOW()
    WHERE environment = point.environment;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_enqueue_tax_credit_note ON commerce_receipt;
CREATE TRIGGER trg_commerce_enqueue_tax_credit_note
  AFTER INSERT ON commerce_receipt
  FOR EACH ROW EXECUTE FUNCTION commerce_enqueue_tax_credit_note();
COMMIT;
