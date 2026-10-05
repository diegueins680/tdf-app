-- Independent status queries for known immutable payment resources. No charge
-- creation or synthetic webhook evidence. Both environment flags default off.
\set ON_ERROR_STOP on
BEGIN;
SET LOCAL lock_timeout = '10s';

CREATE TABLE IF NOT EXISTS commerce_provider_query_job (
  operation_id UUID PRIMARY KEY REFERENCES commerce_provider_operation(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'pending'
    CHECK (status IN ('pending','processing','retry','completed','dead_letter')),
  attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count BETWEEN 0 AND 24),
  lease_token UUID,
  lease_expires_at TIMESTAMPTZ,
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT (clock_timestamp() + INTERVAL '30 seconds'),
  last_attempt_at TIMESTAMPTZ,
  last_error_code TEXT CHECK (last_error_code ~ '^[a-z_]{1,80}$'),
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  CHECK ((status = 'processing') = (lease_token IS NOT NULL AND lease_expires_at IS NOT NULL)),
  CHECK ((lease_token IS NULL) = (lease_expires_at IS NULL)),
  CHECK (status <> 'processing' OR (attempt_count > 0 AND last_attempt_at IS NOT NULL)),
  CHECK ((status IN ('completed','dead_letter')) = (completed_at IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS idx_commerce_provider_query_job_due
  ON commerce_provider_query_job(next_attempt_at, operation_id)
  WHERE status IN ('pending','retry');
CREATE INDEX IF NOT EXISTS idx_commerce_provider_query_job_lease
  ON commerce_provider_query_job(lease_expires_at, operation_id)
  WHERE status = 'processing';

CREATE OR REPLACE FUNCTION commerce_protect_provider_query_job()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Provider query job history must be preserved';
  END IF;
  IF (NEW.operation_id, NEW.created_at) IS DISTINCT FROM (OLD.operation_id, OLD.created_at)
     OR NEW.attempt_count < OLD.attempt_count THEN
    RAISE EXCEPTION 'Provider query job identity and attempt history are immutable';
  END IF;
  IF OLD.status IN ('completed','dead_letter') AND NEW IS DISTINCT FROM OLD THEN
    RAISE EXCEPTION 'Terminal provider query jobs cannot be rewritten';
  END IF;
  IF OLD.status <> NEW.status AND NOT (
    (OLD.status IN ('pending','retry') AND NEW.status = 'processing')
    OR (OLD.status = 'processing' AND NEW.status IN ('retry','completed','dead_letter'))
  ) THEN
    RAISE EXCEPTION 'Invalid provider query job transition';
  END IF;
  IF OLD.status = 'processing' AND NEW.status = 'processing'
     AND OLD.lease_token IS DISTINCT FROM NEW.lease_token
     AND OLD.lease_expires_at > clock_timestamp() THEN
    RAISE EXCEPTION 'A live provider query lease cannot be replaced';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_commerce_provider_query_job_immutable ON commerce_provider_query_job;
CREATE TRIGGER trg_commerce_provider_query_job_immutable
  BEFORE UPDATE OR DELETE ON commerce_provider_query_job
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_provider_query_job();

-- One account exists per provider/environment in the canonical registry.
-- Share this budget between callback and scheduled queries, across replicas.
-- Credential rotation must not reset the quota by changing a merchant alias.
CREATE TABLE IF NOT EXISTS commerce_provider_query_budget (
  provider TEXT NOT NULL,
  environment TEXT NOT NULL,
  next_query_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (provider, environment),
  FOREIGN KEY (provider, environment)
    REFERENCES commerce_provider_account(provider, environment) ON DELETE RESTRICT
);

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.provider_query_recovery', FALSE, 'sandbox',
   'Requires qualified account, shared query budget, lease fencing and sandbox verification'),
  ('checkout.provider_query_recovery', FALSE, 'production',
   'Requires qualified account, shared query budget, lease fencing and sandbox verification')
ON CONFLICT (flag_key, environment) DO NOTHING;
COMMIT;
