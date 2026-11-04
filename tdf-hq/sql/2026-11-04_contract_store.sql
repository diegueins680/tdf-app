-- CONTRACT-STORE-001 / CONTRACT-SEND-001: contracts were JSON files under a
-- relative contracts/store directory, outside the deployment's persistent
-- volume, and "send" answered "sent" without sending anything. Contracts now
-- live in the contract table; each send request writes a contract_delivery row
-- before SMTP is contacted and records whether SMTP accepted or refused it.
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE TABLE IF NOT EXISTS contract (
  id                   UUID PRIMARY KEY,
  kind                 TEXT NOT NULL,
  payload              TEXT NOT NULL,
  created_by_party_id  BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  created_at           TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT contract_id_not_nil CHECK (id <> '00000000-0000-0000-0000-000000000000'::uuid),
  CONSTRAINT contract_kind_shape CHECK (char_length(kind) BETWEEN 1 AND 80 AND kind ~ '^[a-z0-9_-]+$'),
  CONSTRAINT contract_payload_object CHECK (jsonb_typeof(payload::jsonb) = 'object')
);

CREATE TABLE IF NOT EXISTS contract_delivery (
  id                     BIGSERIAL PRIMARY KEY,
  contract_id            UUID NOT NULL REFERENCES contract(id) ON DELETE CASCADE,
  recipient_email        TEXT NOT NULL,
  requested_by_party_id  BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  requested_at           TIMESTAMPTZ NOT NULL DEFAULT now(),
  accepted_at            TIMESTAMPTZ NULL,
  failed_at              TIMESTAMPTZ NULL,
  failure                TEXT NULL,
  CONSTRAINT contract_delivery_single_outcome CHECK (accepted_at IS NULL OR failed_at IS NULL),
  CONSTRAINT contract_delivery_failure_reason CHECK ((failed_at IS NULL) = (failure IS NULL)),
  CONSTRAINT contract_delivery_recipient_shape CHECK (char_length(recipient_email) BETWEEN 3 AND 254 AND position('@' IN recipient_email) > 1)
);

CREATE INDEX IF NOT EXISTS contract_delivery_contract_idx ON contract_delivery(contract_id, requested_at DESC);

COMMIT;
