-- Ecuador electronic invoices for paid public ticket orders, issued through an
-- authorized provider. A policy that requires invoices cannot sell unless an
-- issuer point is configured; every paid order of such a policy enqueues one
-- invoice exactly once, with a sequential number allocated under a row lock.
BEGIN;
ALTER TABLE event_ticket_checkout_policy
  ADD COLUMN IF NOT EXISTS tax_invoice_required BOOLEAN NOT NULL DEFAULT FALSE;

CREATE OR REPLACE FUNCTION event_ticket_tax_invoice_policy_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.approval_status IN ('approved','retired')
     AND NEW.tax_invoice_required IS DISTINCT FROM OLD.tax_invoice_required THEN
    RAISE EXCEPTION 'Approved invoicing requirement is immutable; create a new policy version';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_tax_invoice_policy_immutable
  ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_tax_invoice_policy_immutable
  BEFORE UPDATE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_tax_invoice_policy_immutable();

-- Buyer identification captured once at checkout; consumidor final carries no number.
CREATE TABLE IF NOT EXISTS event_ticket_billing_identity (
  order_id BIGINT PRIMARY KEY REFERENCES event_ticket_order(id) ON DELETE RESTRICT,
  id_type TEXT NOT NULL CHECK (id_type IN ('consumidor_final','cedula','ruc','pasaporte')),
  id_number TEXT CHECK (id_number IS NULL OR id_number ~ '^[A-Za-z0-9]{3,20}$'),
  legal_name TEXT CHECK (legal_name IS NULL OR length(btrim(legal_name)) BETWEEN 2 AND 300),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((id_type = 'consumidor_final') = (id_number IS NULL)),
  CHECK (id_type = 'consumidor_final' OR legal_name IS NOT NULL)
);

CREATE OR REPLACE FUNCTION event_ticket_billing_identity_immutable()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'Ticket billing identity is immutable once recorded';
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_billing_identity_immutable
  ON event_ticket_billing_identity;
CREATE TRIGGER trg_event_ticket_billing_identity_immutable
  BEFORE UPDATE OR DELETE ON event_ticket_billing_identity
  FOR EACH ROW EXECUTE FUNCTION event_ticket_billing_identity_immutable();

-- One configured emission point per checkout environment. Issuer identity and
-- provider credentials stay in server configuration, never in this table.
CREATE TABLE IF NOT EXISTS commerce_tax_issuer_point (
  environment TEXT PRIMARY KEY CHECK (environment IN ('sandbox','production')),
  provider TEXT NOT NULL CHECK (provider IN ('datil')),
  establishment TEXT NOT NULL CHECK (establishment ~ '^[0-9]{3}$'),
  emission_point TEXT NOT NULL CHECK (emission_point ~ '^[0-9]{3}$'),
  next_sequential BIGINT NOT NULL DEFAULT 1 CHECK (next_sequential BETWEEN 1 AND 999999999),
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS commerce_tax_document (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  kind TEXT NOT NULL CHECK (kind IN ('invoice')),
  domain_type TEXT NOT NULL CHECK (domain_type IN ('event_ticket_order')),
  domain_order_id TEXT NOT NULL,
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  provider TEXT NOT NULL CHECK (provider IN ('datil')),
  establishment TEXT NOT NULL CHECK (establishment ~ '^[0-9]{3}$'),
  emission_point TEXT NOT NULL CHECK (emission_point ~ '^[0-9]{3}$'),
  sequential BIGINT NOT NULL CHECK (sequential BETWEEN 1 AND 999999999),
  access_key TEXT CHECK (access_key IS NULL OR access_key ~ '^[0-9]{49}$'),
  issued_on DATE,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency = 'USD'),
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN (
    'pending','submitted','authorized','rejected','uncertain','failed')),
  attempts INTEGER NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  lease_token UUID,
  lease_expires_at TIMESTAMPTZ,
  provider_document_id TEXT CHECK (provider_document_id IS NULL OR provider_document_id ~ '^[A-Za-z0-9_-]{1,80}$'),
  authorization_number TEXT CHECK (authorization_number IS NULL OR authorization_number ~ '^[0-9]{10,49}$'),
  authorized_at TIMESTAMPTZ,
  last_error TEXT CHECK (last_error IS NULL OR length(last_error) <= 500),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (kind, domain_type, domain_order_id),
  UNIQUE (environment, establishment, emission_point, kind, sequential),
  CHECK (status <> 'authorized' OR (access_key IS NOT NULL AND authorization_number IS NOT NULL
    AND authorized_at IS NOT NULL)),
  CHECK ((lease_token IS NULL) = (lease_expires_at IS NULL))
);
CREATE INDEX IF NOT EXISTS commerce_tax_document_due_idx
  ON commerce_tax_document(next_attempt_at)
  WHERE status IN ('pending','submitted');

-- Identity, amount, number and the bound access key never change after enqueue.
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

DROP TRIGGER IF EXISTS trg_commerce_tax_document_guard ON commerce_tax_document;
CREATE TRIGGER trg_commerce_tax_document_guard
  BEFORE UPDATE ON commerce_tax_document
  FOR EACH ROW EXECUTE FUNCTION commerce_tax_document_guard();

-- Enqueue exactly once when a ticket order whose policy requires invoices
-- becomes paid. A missing issuer point at that moment aborts the payment
-- transition rather than silently skipping a legally required invoice.
CREATE OR REPLACE FUNCTION event_ticket_enqueue_tax_invoice()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  required BOOLEAN;
  checkout_environment TEXT;
  point commerce_tax_issuer_point%ROWTYPE;
BEGIN
  IF NEW.payment_status <> 'paid' OR OLD.payment_status = 'paid' THEN
    RETURN NEW;
  END IF;
  SELECT tax_invoice_required INTO required
    FROM event_ticket_checkout_policy WHERE id = NEW.policy_id;
  IF NOT COALESCE(required, FALSE) THEN
    RETURN NEW;
  END IF;
  IF EXISTS (SELECT 1 FROM commerce_tax_document
             WHERE kind = 'invoice' AND domain_type = 'event_ticket_order'
               AND domain_order_id = NEW.order_id::text) THEN
    RETURN NEW;
  END IF;
  SELECT environment INTO checkout_environment
    FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO point FROM commerce_tax_issuer_point
    WHERE environment = checkout_environment AND enabled FOR UPDATE;
  IF point.environment IS NULL THEN
    RAISE EXCEPTION 'Invoicing is required for this ticket policy but no issuer point is enabled';
  END IF;
  INSERT INTO commerce_tax_document(kind, domain_type, domain_order_id, checkout_id,
    environment, provider, establishment, emission_point, sequential, amount_minor, currency)
  VALUES ('invoice', 'event_ticket_order', NEW.order_id::text, NEW.checkout_id,
    point.environment, point.provider, point.establishment, point.emission_point,
    point.next_sequential, NEW.checkout_total_minor, NEW.currency);
  UPDATE commerce_tax_issuer_point
    SET next_sequential = next_sequential + 1, updated_at = NOW()
    WHERE environment = point.environment;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_enqueue_tax_invoice ON event_ticket_checkout_runtime;
CREATE TRIGGER trg_event_ticket_enqueue_tax_invoice
  AFTER UPDATE OF payment_status ON event_ticket_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION event_ticket_enqueue_tax_invoice();
COMMIT;
