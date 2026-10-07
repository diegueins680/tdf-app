BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM commerce_tax_document WHERE kind = 'credit_note' OR submitted_at IS NOT NULL) THEN
    RAISE EXCEPTION 'Retain credit notes and submission evidence; use a forward repair';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_commerce_enqueue_tax_credit_note ON commerce_receipt;
DROP FUNCTION IF EXISTS commerce_enqueue_tax_credit_note();
DROP INDEX IF EXISTS commerce_tax_document_one_credit_note_per_refund;
DROP INDEX IF EXISTS commerce_tax_document_one_invoice_per_order;
ALTER TABLE commerce_tax_document DROP CONSTRAINT IF EXISTS commerce_tax_document_credit_note_binding;
ALTER TABLE commerce_tax_document DROP CONSTRAINT IF EXISTS commerce_tax_document_kind_check;
ALTER TABLE commerce_tax_document ADD CONSTRAINT commerce_tax_document_kind_check CHECK (kind IN ('invoice'));
ALTER TABLE commerce_tax_document
  ADD CONSTRAINT commerce_tax_document_kind_domain_type_domain_order_id_key UNIQUE (kind, domain_type, domain_order_id);
CREATE OR REPLACE FUNCTION commerce_tax_document_guard()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF ROW(NEW.kind, NEW.domain_type, NEW.domain_order_id, NEW.checkout_id, NEW.environment,
         NEW.provider, NEW.establishment, NEW.emission_point, NEW.sequential,
         NEW.amount_minor, NEW.currency, NEW.created_at)
     IS DISTINCT FROM
     ROW(OLD.kind, OLD.domain_type, OLD.domain_order_id, OLD.checkout_id, OLD.environment,
         OLD.provider, OLD.establishment, OLD.emission_point, OLD.sequential,
         OLD.amount_minor, OLD.currency, OLD.created_at)
     OR (OLD.access_key IS NOT NULL AND NEW.access_key IS DISTINCT FROM OLD.access_key)
     OR (OLD.issued_on IS NOT NULL AND NEW.issued_on IS DISTINCT FROM OLD.issued_on) THEN
    RAISE EXCEPTION 'Tax document identity is immutable';
  END IF;
  IF OLD.status = 'authorized' AND NEW.status <> 'authorized' THEN
    RAISE EXCEPTION 'Authorized tax documents are final';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;
ALTER TABLE commerce_tax_document
  DROP COLUMN submitted_at, DROP COLUMN related_document_id, DROP COLUMN refund_id;
ALTER TABLE commerce_tax_issuer_point DROP COLUMN next_credit_note_sequential;
COMMIT;
