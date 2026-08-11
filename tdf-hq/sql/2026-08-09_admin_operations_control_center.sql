BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS operations_organization (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  slug TEXT NOT NULL UNIQUE,
  display_name TEXT NOT NULL,
  default_timezone TEXT NOT NULL DEFAULT 'America/Guayaquil',
  default_currency CHAR(3) NOT NULL DEFAULT 'USD',
  operations_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (slug ~ '^[a-z0-9][a-z0-9-]{1,62}$'),
  CHECK (default_currency ~ '^[A-Z]{3}$')
);

CREATE TABLE IF NOT EXISTS operations_branch (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  slug TEXT NOT NULL,
  display_name TEXT NOT NULL,
  timezone TEXT NOT NULL DEFAULT 'America/Guayaquil',
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (organization_id, slug),
  UNIQUE (id, organization_id)
);

CREATE TABLE IF NOT EXISTS operations_scope_member (
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  party_id BIGINT NOT NULL REFERENCES party(id),
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (organization_id, branch_id, party_id),
  CHECK (branch_id IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS operations_scope_member_party_idx
  ON operations_scope_member (party_id, organization_id, branch_id)
  WHERE active;

CREATE TABLE IF NOT EXISTS operations_business_hours (
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  iso_weekday SMALLINT NOT NULL CHECK (iso_weekday BETWEEN 1 AND 7),
  opens_at TIME NOT NULL,
  closes_at TIME NOT NULL,
  PRIMARY KEY (organization_id, branch_id, iso_weekday),
  CHECK (opens_at < closes_at)
);

CREATE TABLE IF NOT EXISTS operations_holiday (
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  holiday_date DATE NOT NULL,
  label TEXT NOT NULL,
  PRIMARY KEY (organization_id, branch_id, holiday_date)
);

CREATE TABLE IF NOT EXISTS operations_domain_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  event_type TEXT NOT NULL,
  aggregate_type TEXT NOT NULL,
  aggregate_id TEXT NOT NULL,
  source_system TEXT NOT NULL,
  source_channel TEXT NOT NULL,
  correlation_key TEXT NOT NULL,
  deduplication_key TEXT NOT NULL,
  provider_event_id TEXT,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  continuous_sla BOOLEAN NOT NULL DEFAULT FALSE,
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (organization_id, deduplication_key),
  CHECK (jsonb_typeof(payload) = 'object'),
  CHECK (length(event_type) BETWEEN 1 AND 160),
  CHECK (length(correlation_key) BETWEEN 1 AND 320)
);

CREATE UNIQUE INDEX IF NOT EXISTS operations_domain_event_provider_uidx
  ON operations_domain_event (organization_id, source_system, provider_event_id)
  WHERE provider_event_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS operations_domain_event_aggregate_idx
  ON operations_domain_event (organization_id, aggregate_type, aggregate_id, occurred_at, id);
CREATE INDEX IF NOT EXISTS operations_domain_event_retention_brin_idx
  ON operations_domain_event USING BRIN (recorded_at) WITH (pages_per_range = 64);

CREATE TABLE IF NOT EXISTS operations_outbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  event_id UUID NOT NULL UNIQUE REFERENCES operations_domain_event(id),
  aggregate_type TEXT NOT NULL,
  aggregate_id TEXT NOT NULL,
  aggregate_sequence BIGINT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  attempt_count INTEGER NOT NULL DEFAULT 0,
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  locked_at TIMESTAMPTZ,
  locked_by TEXT,
  processed_at TIMESTAMPTZ,
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (status IN ('pending', 'processing', 'processed', 'dead_letter')),
  CHECK (attempt_count >= 0),
  UNIQUE (organization_id, aggregate_type, aggregate_id, aggregate_sequence)
);

CREATE INDEX IF NOT EXISTS operations_outbox_claim_idx
  ON operations_outbox (next_attempt_at, created_at, id)
  WHERE status IN ('pending', 'processing');

CREATE TABLE IF NOT EXISTS operations_aggregate_sequence (
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  aggregate_type TEXT NOT NULL,
  aggregate_id TEXT NOT NULL,
  last_sequence BIGINT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (organization_id, aggregate_type, aggregate_id),
  CHECK (last_sequence > 0)
);

-- Upgrade-safe initialization when the outbox already contains records.
INSERT INTO operations_aggregate_sequence (
  organization_id, aggregate_type, aggregate_id, last_sequence
)
SELECT organization_id, aggregate_type, aggregate_id, max(aggregate_sequence)
FROM operations_outbox
GROUP BY organization_id, aggregate_type, aggregate_id
ON CONFLICT (organization_id, aggregate_type, aggregate_id) DO UPDATE
SET last_sequence = GREATEST(
      operations_aggregate_sequence.last_sequence,
      EXCLUDED.last_sequence
    ),
    updated_at = now();

CREATE TABLE IF NOT EXISTS operations_work_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  source_system TEXT NOT NULL,
  source_channel TEXT NOT NULL,
  entity_type TEXT NOT NULL,
  entity_id TEXT,
  uncorrelated BOOLEAN NOT NULL DEFAULT FALSE,
  correlation_key TEXT NOT NULL,
  external_provider_event_id TEXT,
  title_es TEXT NOT NULL,
  title_en TEXT NOT NULL,
  description_es TEXT NOT NULL,
  description_en TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'new',
  priority TEXT NOT NULL DEFAULT 'normal',
  recommended_priority TEXT NOT NULL DEFAULT 'normal',
  priority_override_reason TEXT,
  severity TEXT NOT NULL DEFAULT 'info',
  first_seen_by BIGINT REFERENCES party(id),
  first_seen_at TIMESTAMPTZ,
  assignee_party_id BIGINT REFERENCES party(id),
  responsible_team TEXT,
  customer_party_id BIGINT REFERENCES party(id),
  service_key TEXT,
  amount_minor BIGINT,
  currency CHAR(3),
  payment_state TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  due_at TIMESTAMPTZ,
  snoozed_until TIMESTAMPTZ,
  waiting_started_at TIMESTAMPTZ,
  waiting_reason TEXT,
  waiting_external_dependency BOOLEAN NOT NULL DEFAULT FALSE,
  resume_at TIMESTAMPTZ,
  resolved_at TIMESTAMPTZ,
  archived_at TIMESTAMPTZ,
  sla_breached_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (organization_id, correlation_key),
  CHECK (status IN ('new', 'seen', 'assigned', 'in_progress', 'waiting', 'resolved', 'archived')),
  CHECK (priority IN ('urgent', 'high', 'normal', 'low')),
  CHECK (recommended_priority IN ('urgent', 'high', 'normal', 'low')),
  CHECK (severity IN ('critical', 'error', 'warning', 'info')),
  CHECK ((uncorrelated AND entity_id IS NULL) OR (NOT uncorrelated AND entity_id IS NOT NULL)),
  CHECK ((status = 'waiting' AND waiting_reason IS NOT NULL) OR status <> 'waiting'),
  CHECK ((first_seen_at IS NULL AND first_seen_by IS NULL) OR (first_seen_at IS NOT NULL AND first_seen_by IS NOT NULL)),
  CHECK (currency IS NULL OR currency ~ '^[A-Z]{3}$'),
  CHECK (jsonb_typeof(metadata) = 'object')
);

CREATE INDEX IF NOT EXISTS operations_work_item_inbox_idx
  ON operations_work_item (organization_id, status, priority, updated_at DESC, id DESC);
CREATE INDEX IF NOT EXISTS operations_work_item_assignee_idx
  ON operations_work_item (organization_id, assignee_party_id, status, updated_at DESC);
CREATE INDEX IF NOT EXISTS operations_work_item_branch_idx
  ON operations_work_item (organization_id, branch_id, status, updated_at DESC);
CREATE INDEX IF NOT EXISTS operations_work_item_sla_idx
  ON operations_work_item (organization_id, due_at, status)
  WHERE status NOT IN ('resolved', 'archived');
CREATE INDEX IF NOT EXISTS operations_work_item_search_idx
  ON operations_work_item USING GIN (
    to_tsvector('simple', coalesce(title_es, '') || ' ' || coalesce(title_en, '') || ' ' ||
      coalesce(description_es, '') || ' ' || coalesce(description_en, '') || ' ' ||
      coalesce(entity_id, '') || ' ' || coalesce(correlation_key, ''))
  );

CREATE TABLE IF NOT EXISTS operations_work_item_event (
  id BIGSERIAL PRIMARY KEY,
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  work_item_id UUID NOT NULL REFERENCES operations_work_item(id),
  domain_event_id UUID REFERENCES operations_domain_event(id),
  event_type TEXT NOT NULL,
  actor_party_id BIGINT REFERENCES party(id),
  actor_role TEXT,
  body_es TEXT NOT NULL,
  body_en TEXT NOT NULL,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  occurred_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (domain_event_id),
  CHECK (jsonb_typeof(metadata) = 'object')
);

CREATE INDEX IF NOT EXISTS operations_work_item_event_thread_idx
  ON operations_work_item_event (organization_id, work_item_id, occurred_at, id);
CREATE INDEX IF NOT EXISTS operations_work_item_event_retention_brin_idx
  ON operations_work_item_event USING BRIN (occurred_at) WITH (pages_per_range = 64);

CREATE TABLE IF NOT EXISTS operations_note (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  work_item_id UUID NOT NULL REFERENCES operations_work_item(id),
  author_party_id BIGINT NOT NULL REFERENCES party(id),
  body TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  edited_at TIMESTAMPTZ,
  CHECK (length(btrim(body)) BETWEEN 1 AND 5000)
);

CREATE TABLE IF NOT EXISTS operations_mention (
  note_id UUID NOT NULL REFERENCES operations_note(id),
  mentioned_party_id BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (note_id, mentioned_party_id)
);

CREATE TABLE IF NOT EXISTS operations_sla_timer (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  work_item_id UUID NOT NULL REFERENCES operations_work_item(id),
  phase TEXT NOT NULL,
  starts_at TIMESTAMPTZ NOT NULL,
  due_at TIMESTAMPTZ NOT NULL,
  continuous_elapsed BOOLEAN NOT NULL DEFAULT FALSE,
  paused_at TIMESTAMPTZ,
  paused_seconds BIGINT NOT NULL DEFAULT 0,
  completed_at TIMESTAMPTZ,
  breached_at TIMESTAMPTZ,
  UNIQUE (work_item_id, phase),
  CHECK (phase IN ('acknowledge', 'mitigate', 'resolve')),
  CHECK (paused_seconds >= 0)
);

CREATE TABLE IF NOT EXISTS operations_sla_reminder (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  work_item_id UUID NOT NULL REFERENCES operations_work_item(id),
  timer_id UUID NOT NULL REFERENCES operations_sla_timer(id),
  threshold_percent SMALLINT NOT NULL,
  target_role TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  acknowledged_at TIMESTAMPTZ,
  UNIQUE (timer_id, threshold_percent, target_role),
  CHECK (threshold_percent IN (50, 80, 100, 150))
);

CREATE TABLE IF NOT EXISTS operations_outbound_delivery (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  work_item_id UUID REFERENCES operations_work_item(id),
  channel TEXT NOT NULL,
  provider TEXT NOT NULL,
  template_key TEXT,
  recipient_ref TEXT NOT NULL,
  consent_basis TEXT,
  idempotency_key TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  attempt_count INTEGER NOT NULL DEFAULT 0,
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  provider_message_id TEXT,
  last_error_code TEXT,
  last_error_redacted TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  delivered_at TIMESTAMPTZ,
  UNIQUE (organization_id, idempotency_key),
  CHECK (channel IN ('email', 'push', 'sms', 'whatsapp')),
  CHECK (status IN ('pending', 'sending', 'delivered', 'failed', 'dead_letter')),
  CHECK (attempt_count >= 0)
);

CREATE INDEX IF NOT EXISTS operations_outbound_claim_idx
  ON operations_outbound_delivery (next_attempt_at, created_at)
  WHERE status IN ('pending', 'failed');

CREATE TABLE IF NOT EXISTS operations_integration_failure (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  provider TEXT NOT NULL,
  direction TEXT NOT NULL,
  source_record_type TEXT NOT NULL,
  source_record_id TEXT NOT NULL,
  failure_code TEXT NOT NULL,
  redacted_summary TEXT NOT NULL,
  retryable BOOLEAN NOT NULL,
  status TEXT NOT NULL DEFAULT 'open',
  attempt_count INTEGER NOT NULL DEFAULT 0,
  last_attempt_at TIMESTAMPTZ,
  next_attempt_at TIMESTAMPTZ,
  resolved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (direction IN ('inbound', 'outbound', 'internal')),
  CHECK (status IN ('open', 'retrying', 'resolved', 'dead_letter'))
);

CREATE INDEX IF NOT EXISTS operations_integration_failure_queue_idx
  ON operations_integration_failure (organization_id, status, created_at DESC);

CREATE TABLE IF NOT EXISTS operations_provider_config (
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  provider TEXT NOT NULL,
  country_code CHAR(2),
  currency CHAR(3),
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  sandbox BOOLEAN NOT NULL DEFAULT TRUE,
  configuration JSONB NOT NULL DEFAULT '{}'::jsonb,
  updated_by BIGINT REFERENCES party(id),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (organization_id, provider, country_code, currency),
  CHECK (configuration ?& ARRAY[]::text[]),
  CHECK (NOT (configuration ?| ARRAY['secret', 'token', 'password', 'privateKey', 'certificate']))
);

CREATE TABLE IF NOT EXISTS operations_inbound_receipt (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  provider TEXT NOT NULL,
  provider_event_id TEXT NOT NULL,
  signature_verified BOOLEAN NOT NULL,
  received_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  occurred_at TIMESTAMPTZ,
  payload_digest TEXT NOT NULL,
  replay_window_valid BOOLEAN NOT NULL,
  correlation_status TEXT NOT NULL,
  party_id BIGINT REFERENCES party(id),
  entity_type TEXT,
  entity_id TEXT,
  redacted_metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (organization_id, provider, provider_event_id),
  CHECK (correlation_status IN ('correlated', 'uncertain', 'uncorrelated', 'rejected'))
);

CREATE TABLE IF NOT EXISTS operations_approval_request (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  work_item_id UUID REFERENCES operations_work_item(id),
  action_type TEXT NOT NULL,
  target_entity_type TEXT NOT NULL,
  target_entity_id TEXT NOT NULL,
  amount_minor BIGINT,
  currency CHAR(3),
  requester_party_id BIGINT NOT NULL REFERENCES party(id),
  requester_role TEXT NOT NULL,
  request_reason TEXT NOT NULL,
  requested_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  approver_party_id BIGINT REFERENCES party(id),
  approver_role TEXT,
  decision TEXT NOT NULL DEFAULT 'pending',
  decision_reason TEXT,
  decided_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  execution_status TEXT NOT NULL DEFAULT 'not_started',
  idempotency_key TEXT NOT NULL,
  UNIQUE (organization_id, idempotency_key),
  CHECK (decision IN ('pending', 'approved', 'rejected', 'expired', 'cancelled')),
  CHECK (execution_status IN ('not_started', 'pending', 'completed', 'failed')),
  CHECK (approver_party_id IS NULL OR approver_party_id <> requester_party_id),
  CHECK ((decision = 'pending' AND approver_party_id IS NULL AND decided_at IS NULL) OR decision <> 'pending')
);

CREATE TABLE IF NOT EXISTS operations_saved_view (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  owner_party_id BIGINT REFERENCES party(id),
  name TEXT NOT NULL,
  shared BOOLEAN NOT NULL DEFAULT FALSE,
  filters JSONB NOT NULL DEFAULT '{}'::jsonb,
  columns JSONB NOT NULL DEFAULT '[]'::jsonb,
  widgets JSONB NOT NULL DEFAULT '[]'::jsonb,
  subscribed_event_types JSONB NOT NULL DEFAULT '[]'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (organization_id, owner_party_id, name)
);

CREATE TABLE IF NOT EXISTS operations_push_subscription (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  party_id BIGINT NOT NULL REFERENCES party(id),
  platform TEXT NOT NULL,
  device_token_digest TEXT NOT NULL,
  encrypted_device_token BYTEA NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (organization_id, party_id, device_token_digest),
  CHECK (platform IN ('ios', 'android', 'web'))
);

CREATE TABLE IF NOT EXISTS operations_stream_event (
  id BIGSERIAL PRIMARY KEY,
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  event_type TEXT NOT NULL,
  work_item_id UUID REFERENCES operations_work_item(id),
  visible_to_party_id BIGINT REFERENCES party(id),
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS operations_stream_resume_idx
  ON operations_stream_event (organization_id, id);

CREATE TABLE IF NOT EXISTS operations_admin_audit (
  id BIGSERIAL PRIMARY KEY,
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  branch_id UUID REFERENCES operations_branch(id),
  actor_party_id BIGINT REFERENCES party(id),
  acting_role TEXT NOT NULL,
  source_client TEXT NOT NULL,
  action TEXT NOT NULL,
  target_entity_type TEXT NOT NULL,
  target_entity_id TEXT NOT NULL,
  previous_value JSONB,
  new_value JSONB,
  request_id TEXT NOT NULL,
  correlation_id TEXT NOT NULL,
  approval_request_id UUID REFERENCES operations_approval_request(id),
  reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS operations_admin_audit_target_idx
  ON operations_admin_audit (organization_id, target_entity_type, target_entity_id, created_at DESC);
CREATE INDEX IF NOT EXISTS operations_admin_audit_retention_brin_idx
  ON operations_admin_audit USING BRIN (created_at) WITH (pages_per_range = 64);

CREATE TABLE IF NOT EXISTS operations_backfill_run (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES operations_organization(id),
  source_name TEXT NOT NULL,
  run_key TEXT NOT NULL DEFAULT 'default',
  status TEXT NOT NULL DEFAULT 'running',
  dry_run BOOLEAN NOT NULL DEFAULT TRUE,
  cursor_value TEXT,
  scanned_count BIGINT NOT NULL DEFAULT 0,
  eligible_count BIGINT NOT NULL DEFAULT 0,
  inserted_count BIGINT NOT NULL DEFAULT 0,
  skipped_count BIGINT NOT NULL DEFAULT 0,
  error_count BIGINT NOT NULL DEFAULT 0,
  started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  heartbeat_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  finished_at TIMESTAMPTZ,
  UNIQUE (organization_id, source_name, dry_run, started_at),
  CHECK (status IN ('running', 'completed', 'failed', 'cancelled'))
);

ALTER TABLE operations_backfill_run ADD COLUMN IF NOT EXISTS run_key TEXT NOT NULL DEFAULT 'default';
CREATE UNIQUE INDEX IF NOT EXISTS operations_backfill_run_key_uidx
  ON operations_backfill_run (organization_id, source_name, run_key, dry_run);

CREATE OR REPLACE FUNCTION operations_reject_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% is append-only', TG_TABLE_NAME USING ERRCODE = '55000';
END;
$$;

DROP TRIGGER IF EXISTS operations_domain_event_immutable ON operations_domain_event;
CREATE TRIGGER operations_domain_event_immutable
  BEFORE UPDATE OR DELETE ON operations_domain_event
  FOR EACH ROW EXECUTE FUNCTION operations_reject_mutation();

DROP TRIGGER IF EXISTS operations_admin_audit_immutable ON operations_admin_audit;
CREATE TRIGGER operations_admin_audit_immutable
  BEFORE UPDATE OR DELETE ON operations_admin_audit
  FOR EACH ROW EXECUTE FUNCTION operations_reject_mutation();

CREATE OR REPLACE FUNCTION operations_enqueue_domain_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  next_sequence BIGINT;
BEGIN
  INSERT INTO operations_aggregate_sequence (
    organization_id, aggregate_type, aggregate_id, last_sequence
  ) VALUES (
    NEW.organization_id, NEW.aggregate_type, NEW.aggregate_id, 1
  )
  ON CONFLICT (organization_id, aggregate_type, aggregate_id) DO UPDATE
  SET last_sequence = operations_aggregate_sequence.last_sequence + 1,
      updated_at = now()
  RETURNING last_sequence INTO next_sequence;

  INSERT INTO operations_outbox (
    organization_id, event_id, aggregate_type, aggregate_id, aggregate_sequence
  ) VALUES (
    NEW.organization_id, NEW.id, NEW.aggregate_type, NEW.aggregate_id, next_sequence
  ) ON CONFLICT (event_id) DO NOTHING;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_domain_event_outbox ON operations_domain_event;
CREATE TRIGGER operations_domain_event_outbox
  AFTER INSERT ON operations_domain_event
  FOR EACH ROW EXECUTE FUNCTION operations_enqueue_domain_event();

CREATE OR REPLACE FUNCTION operations_business_deadline(
  p_organization_id UUID,
  p_branch_id UUID,
  p_started_at TIMESTAMPTZ,
  p_business_minutes INTEGER
) RETURNS TIMESTAMPTZ LANGUAGE plpgsql STABLE AS $$
DECLARE
  tz TEXT;
  cursor_at TIMESTAMPTZ := p_started_at;
  local_day DATE;
  day_open TIME;
  day_close TIME;
  open_at TIMESTAMPTZ;
  close_at TIMESTAMPTZ;
  available_minutes INTEGER;
  remaining_minutes INTEGER := GREATEST(p_business_minutes, 0);
  guard_days INTEGER := 0;
BEGIN
  SELECT COALESCE(b.timezone, o.default_timezone) INTO tz
  FROM operations_organization o
  LEFT JOIN operations_branch b ON b.id = p_branch_id AND b.organization_id = o.id
  WHERE o.id = p_organization_id;
  tz := COALESCE(tz, 'America/Guayaquil');

  WHILE remaining_minutes > 0 LOOP
    guard_days := guard_days + 1;
    IF guard_days > 740 THEN
      RAISE EXCEPTION 'business calendar has no available hours';
    END IF;
    local_day := (cursor_at AT TIME ZONE tz)::date;

    SELECT h.opens_at, h.closes_at INTO day_open, day_close
    FROM operations_business_hours h
    WHERE h.organization_id = p_organization_id
      AND (h.branch_id = p_branch_id OR (p_branch_id IS NULL AND h.branch_id IS NULL))
      AND h.iso_weekday = EXTRACT(ISODOW FROM local_day)::smallint
      AND NOT EXISTS (
        SELECT 1 FROM operations_holiday holiday
        WHERE holiday.organization_id = p_organization_id
          AND (holiday.branch_id = p_branch_id OR holiday.branch_id IS NULL)
          AND holiday.holiday_date = local_day
      )
    ORDER BY (h.branch_id IS NOT NULL) DESC
    LIMIT 1;

    IF day_open IS NULL THEN
      cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
      CONTINUE;
    END IF;

    open_at := ((local_day + day_open)::timestamp AT TIME ZONE tz);
    close_at := ((local_day + day_close)::timestamp AT TIME ZONE tz);
    cursor_at := GREATEST(cursor_at, open_at);
    IF cursor_at >= close_at THEN
      cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
      CONTINUE;
    END IF;

    available_minutes := floor(EXTRACT(EPOCH FROM (close_at - cursor_at)) / 60)::integer;
    IF remaining_minutes <= available_minutes THEN
      RETURN cursor_at + make_interval(mins => remaining_minutes);
    END IF;
    remaining_minutes := remaining_minutes - available_minutes;
    cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
  END LOOP;
  RETURN cursor_at;
END;
$$;

CREATE OR REPLACE FUNCTION operations_record_event(
  p_event_type TEXT,
  p_aggregate_type TEXT,
  p_aggregate_id TEXT,
  p_correlation_key TEXT,
  p_source_channel TEXT,
  p_priority TEXT,
  p_title_es TEXT,
  p_title_en TEXT,
  p_description_es TEXT,
  p_description_en TEXT,
  p_metadata JSONB DEFAULT '{}'::jsonb,
  p_occurred_at TIMESTAMPTZ DEFAULT now(),
  p_provider_event_id TEXT DEFAULT NULL,
  p_continuous_sla BOOLEAN DEFAULT FALSE
) RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE
  org_id UUID := '00000000-0000-4000-8000-000000000001'::uuid;
  branch_id UUID := '00000000-0000-4000-8000-000000000002'::uuid;
  event_id UUID;
  dedup_key TEXT;
  event_source_system TEXT;
BEGIN
  event_source_system := CASE
    WHEN p_provider_event_id IS NULL THEN 'tdf-hq'
    ELSE p_source_channel
  END;
  dedup_key := encode(digest(
    concat_ws('|', p_event_type, p_aggregate_type, p_aggregate_id, p_correlation_key,
      p_occurred_at::text, COALESCE(p_provider_event_id, ''), COALESCE(p_metadata, '{}'::jsonb)::text),
    'sha256'), 'hex');

  INSERT INTO operations_domain_event (
    organization_id, branch_id, event_type, aggregate_type, aggregate_id,
    source_system, source_channel, correlation_key, deduplication_key,
    provider_event_id, occurred_at, continuous_sla, payload
  ) VALUES (
    org_id, branch_id, p_event_type, p_aggregate_type, p_aggregate_id,
    event_source_system, p_source_channel, p_correlation_key, dedup_key,
    p_provider_event_id, p_occurred_at, p_continuous_sla,
    jsonb_strip_nulls(jsonb_build_object(
      'priority', p_priority,
      'titleEs', p_title_es,
      'titleEn', p_title_en,
      'descriptionEs', p_description_es,
      'descriptionEn', p_description_en,
      'metadata', COALESCE(p_metadata, '{}'::jsonb)
    ))
  )
  ON CONFLICT DO NOTHING
  RETURNING id INTO event_id;
  IF event_id IS NULL THEN
    SELECT id INTO event_id
    FROM operations_domain_event AS existing_event
    WHERE existing_event.organization_id = org_id
      AND (
        existing_event.deduplication_key = dedup_key
        OR (p_provider_event_id IS NOT NULL
          AND existing_event.source_system = event_source_system
          AND existing_event.provider_event_id = p_provider_event_id)
      )
    ORDER BY recorded_at
    LIMIT 1;
  END IF;
  IF event_id IS NULL THEN
    RAISE EXCEPTION 'operations event conflict could not be resolved'
      USING ERRCODE = '40001';
  END IF;
  RETURN event_id;
END;
$$;

CREATE OR REPLACE FUNCTION operations_process_outbox_batch(
  p_limit INTEGER DEFAULT 100,
  p_worker TEXT DEFAULT 'operations-worker'
) RETURNS TABLE(processed INTEGER, failed INTEGER, dead_lettered INTEGER)
LANGUAGE plpgsql AS $$
DECLARE
  queued RECORD;
  work_id UUID;
  priority_value TEXT;
  ack_minutes INTEGER;
  mitigation_minutes INTEGER;
  resolution_minutes INTEGER;
  ack_due TIMESTAMPTZ;
  mitigation_due TIMESTAMPTZ;
  resolution_due TIMESTAMPTZ;
  terminal_event BOOLEAN;
  processed_count INTEGER := 0;
  failed_count INTEGER := 0;
  dead_count INTEGER := 0;
BEGIN
  FOR queued IN
    SELECT o.*, e.event_type, e.branch_id, e.source_system, e.source_channel,
      e.correlation_key, e.provider_event_id, e.occurred_at, e.continuous_sla, e.payload
    FROM operations_outbox o
    JOIN operations_domain_event e ON e.id = o.event_id
    WHERE o.status IN ('pending', 'processing')
      AND o.next_attempt_at <= now()
      AND (o.locked_at IS NULL OR o.locked_at < now() - interval '5 minutes')
      AND NOT EXISTS (
        SELECT 1 FROM operations_outbox earlier
        WHERE earlier.organization_id = o.organization_id
          AND earlier.aggregate_type = o.aggregate_type
          AND earlier.aggregate_id = o.aggregate_id
          AND earlier.aggregate_sequence < o.aggregate_sequence
          AND earlier.status <> 'processed'
      )
    ORDER BY o.created_at, o.id
    FOR UPDATE OF o SKIP LOCKED
    LIMIT LEAST(GREATEST(p_limit, 1), 500)
  LOOP
    BEGIN
      UPDATE operations_outbox
      SET status = 'processing', locked_at = now(), locked_by = p_worker
      WHERE id = queued.id;

      priority_value := CASE queued.payload->>'priority'
        WHEN 'urgent' THEN 'urgent'
        WHEN 'high' THEN 'high'
        WHEN 'low' THEN 'low'
        ELSE 'normal'
      END;
      ack_minutes := CASE priority_value WHEN 'urgent' THEN 15 WHEN 'high' THEN 60 WHEN 'normal' THEN 240 ELSE 480 END;
      mitigation_minutes := CASE priority_value WHEN 'urgent' THEN 60 ELSE ack_minutes END;
      resolution_minutes := CASE priority_value WHEN 'urgent' THEN 240 WHEN 'high' THEN 480 WHEN 'normal' THEN 1440 ELSE 2400 END;
      terminal_event := COALESCE((queued.payload->'metadata'->>'terminal')::boolean, FALSE);

      IF queued.continuous_sla OR priority_value = 'urgent' THEN
        ack_due := queued.occurred_at + make_interval(mins => ack_minutes);
        mitigation_due := queued.occurred_at + make_interval(mins => mitigation_minutes);
        resolution_due := queued.occurred_at + make_interval(mins => resolution_minutes);
      ELSE
        ack_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, ack_minutes);
        mitigation_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, mitigation_minutes);
        resolution_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, resolution_minutes);
      END IF;

      INSERT INTO operations_work_item (
        organization_id, branch_id, source_system, source_channel, entity_type, entity_id,
        uncorrelated, correlation_key, external_provider_event_id,
        title_es, title_en, description_es, description_en,
        status, priority, recommended_priority, severity,
        created_at, updated_at, due_at, resolved_at, metadata
      ) VALUES (
        queued.organization_id, queued.branch_id, queued.source_system, queued.source_channel,
        queued.aggregate_type,
        CASE WHEN queued.aggregate_type = 'uncorrelated_inbound' THEN NULL ELSE queued.aggregate_id END,
        queued.aggregate_type = 'uncorrelated_inbound', queued.correlation_key, queued.provider_event_id,
        COALESCE(queued.payload->>'titleEs', queued.event_type),
        COALESCE(queued.payload->>'titleEn', queued.event_type),
        COALESCE(queued.payload->>'descriptionEs', queued.event_type),
        COALESCE(queued.payload->>'descriptionEn', queued.event_type),
        CASE WHEN terminal_event THEN 'resolved' ELSE 'new' END, priority_value, priority_value,
        CASE priority_value WHEN 'urgent' THEN 'error' WHEN 'high' THEN 'warning' ELSE 'info' END,
        queued.occurred_at, now(), resolution_due,
        CASE WHEN terminal_event THEN queued.occurred_at ELSE NULL END,
        COALESCE(queued.payload->'metadata', '{}'::jsonb)
      )
      ON CONFLICT (organization_id, correlation_key) DO UPDATE SET
        title_es = EXCLUDED.title_es,
        title_en = EXCLUDED.title_en,
        description_es = EXCLUDED.description_es,
        description_en = EXCLUDED.description_en,
        source_channel = EXCLUDED.source_channel,
        external_provider_event_id = COALESCE(EXCLUDED.external_provider_event_id, operations_work_item.external_provider_event_id),
        recommended_priority = EXCLUDED.recommended_priority,
        priority = CASE
          WHEN operations_work_item.priority_override_reason IS NOT NULL THEN operations_work_item.priority
          WHEN array_position(ARRAY['urgent','high','normal','low'], EXCLUDED.priority) <
               array_position(ARRAY['urgent','high','normal','low'], operations_work_item.priority)
            THEN EXCLUDED.priority
          ELSE operations_work_item.priority
        END,
        status = CASE
          WHEN terminal_event THEN 'resolved'
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN 'new'
          ELSE operations_work_item.status END,
        resolved_at = CASE
          WHEN terminal_event THEN queued.occurred_at
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN NULL
          ELSE operations_work_item.resolved_at END,
        archived_at = CASE
          WHEN terminal_event THEN NULL
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN NULL
          ELSE operations_work_item.archived_at END,
        due_at = CASE WHEN operations_work_item.status IN ('resolved', 'archived') THEN EXCLUDED.due_at ELSE operations_work_item.due_at END,
        metadata = operations_work_item.metadata || EXCLUDED.metadata,
        updated_at = now(),
        version = operations_work_item.version + 1
      RETURNING id INTO work_id;

      INSERT INTO operations_work_item_event (
        organization_id, work_item_id, domain_event_id, event_type,
        body_es, body_en, metadata, occurred_at
      ) VALUES (
        queued.organization_id, work_id, queued.event_id, queued.event_type,
        COALESCE(queued.payload->>'descriptionEs', queued.event_type),
        COALESCE(queued.payload->>'descriptionEn', queued.event_type),
        COALESCE(queued.payload->'metadata', '{}'::jsonb), queued.occurred_at
      ) ON CONFLICT (domain_event_id) DO NOTHING;

      INSERT INTO operations_sla_timer (
        organization_id, work_item_id, phase, starts_at, due_at, continuous_elapsed
      ) VALUES
        (queued.organization_id, work_id, 'acknowledge', queued.occurred_at, ack_due, queued.continuous_sla OR priority_value = 'urgent'),
        (queued.organization_id, work_id, 'mitigate', queued.occurred_at, mitigation_due, queued.continuous_sla OR priority_value = 'urgent'),
        (queued.organization_id, work_id, 'resolve', queued.occurred_at, resolution_due, queued.continuous_sla OR priority_value = 'urgent')
      ON CONFLICT (work_item_id, phase) DO NOTHING;

      INSERT INTO operations_stream_event (
        organization_id, branch_id, event_type, work_item_id, payload
      ) VALUES (
        queued.organization_id, queued.branch_id, 'work_item.updated', work_id,
        jsonb_build_object('workItemId', work_id, 'domainEventId', queued.event_id)
      );

      INSERT INTO operations_admin_audit (
        organization_id, branch_id, acting_role, source_client, action,
        target_entity_type, target_entity_id, new_value, request_id, correlation_id
      ) VALUES (
        queued.organization_id, queued.branch_id, 'system', p_worker, 'project_domain_event',
        'operations_work_item', work_id::text,
        jsonb_build_object('domainEventId', queued.event_id), queued.id::text, queued.correlation_key
      );

      UPDATE operations_outbox
      SET status = 'processed', processed_at = now(), locked_at = NULL, locked_by = NULL,
          last_error = NULL
      WHERE id = queued.id;
      processed_count := processed_count + 1;
    EXCEPTION WHEN OTHERS THEN
      failed_count := failed_count + 1;
      UPDATE operations_outbox
      SET attempt_count = attempt_count + 1,
          status = CASE WHEN attempt_count + 1 >= 8 THEN 'dead_letter' ELSE 'pending' END,
          next_attempt_at = now() +
            make_interval(secs => LEAST(3600, (2 ^ LEAST(attempt_count + 1, 10))::integer)) +
            make_interval(secs => floor(random() * 15)::integer),
          last_error = left(SQLSTATE || ': ' || SQLERRM, 1000),
          locked_at = NULL,
          locked_by = NULL
      WHERE id = queued.id;

      IF (SELECT status = 'dead_letter' FROM operations_outbox WHERE id = queued.id) THEN
        dead_count := dead_count + 1;
        INSERT INTO operations_integration_failure (
          organization_id, branch_id, provider, direction, source_record_type,
          source_record_id, failure_code, redacted_summary, retryable, status,
          attempt_count, last_attempt_at
        ) VALUES (
          queued.organization_id, queued.branch_id, 'internal_outbox', 'internal',
          queued.aggregate_type, queued.aggregate_id, SQLSTATE, left(SQLERRM, 500),
          TRUE, 'dead_letter', 8, now()
        );
      END IF;
    END;
  END LOOP;
  RETURN QUERY SELECT processed_count, failed_count, dead_count;
END;
$$;

CREATE OR REPLACE FUNCTION operations_tick_sla(p_now TIMESTAMPTZ DEFAULT now())
RETURNS TABLE(reminders_created INTEGER, breached_created INTEGER)
LANGUAGE plpgsql AS $$
DECLARE
  reminder_count INTEGER := 0;
  breach_count INTEGER := 0;
BEGIN
  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, threshold, 'responsible'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  CROSS JOIN (VALUES (50), (80)) AS thresholds(threshold)
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL
    AND timer.paused_at IS NULL
    AND p_now >= timer.starts_at +
      ((timer.due_at - timer.starts_at) * (thresholds.threshold::numeric / 100))
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;
  GET DIAGNOSTICS reminder_count = ROW_COUNT;

  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, 100, 'manager'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL AND timer.paused_at IS NULL
    AND p_now >= timer.due_at
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;

  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, 150, 'admin'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL AND timer.paused_at IS NULL
    AND p_now >= timer.starts_at + ((timer.due_at - timer.starts_at) * 1.5)
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;
  GET DIAGNOSTICS breach_count = ROW_COUNT;

  UPDATE operations_work_item item
  SET sla_breached_at = COALESCE(item.sla_breached_at, p_now),
      updated_at = p_now,
      version = version + 1
  WHERE item.sla_breached_at IS NULL
    AND EXISTS (
      SELECT 1 FROM operations_sla_reminder reminder
      WHERE reminder.work_item_id = item.id AND reminder.threshold_percent = 150
    );

  UPDATE operations_sla_timer timer
  SET breached_at = COALESCE(timer.breached_at, p_now)
  WHERE timer.breached_at IS NULL
    AND EXISTS (
      SELECT 1 FROM operations_sla_reminder reminder
      WHERE reminder.timer_id = timer.id AND reminder.threshold_percent = 150
    );

  RETURN QUERY SELECT reminder_count, breach_count;
END;
$$;

INSERT INTO operations_organization (
  id, slug, display_name, default_timezone, default_currency, operations_enabled
) VALUES (
  '00000000-0000-4000-8000-000000000001', 'tdf-default', 'TDF',
  'America/Guayaquil', 'USD', FALSE
) ON CONFLICT (id) DO NOTHING;

INSERT INTO operations_branch (
  id, organization_id, slug, display_name, timezone, active
) VALUES (
  '00000000-0000-4000-8000-000000000002',
  '00000000-0000-4000-8000-000000000001',
  'principal', 'Principal', 'America/Guayaquil', TRUE
) ON CONFLICT (id) DO NOTHING;

INSERT INTO operations_business_hours (
  organization_id, branch_id, iso_weekday, opens_at, closes_at
)
SELECT
  '00000000-0000-4000-8000-000000000001'::uuid,
  '00000000-0000-4000-8000-000000000002'::uuid,
  weekday, TIME '09:00', TIME '18:00'
FROM generate_series(1, 5) AS weekday
ON CONFLICT (organization_id, branch_id, iso_weekday) DO NOTHING;

-- Adapter registry is explicit and safe-by-default. Activation requires a
-- verified credential/configuration checklist; no provider silently falls
-- back to mock delivery.
INSERT INTO operations_provider_config (
  organization_id, provider, country_code, currency, enabled, sandbox, configuration
)
SELECT
  '00000000-0000-4000-8000-000000000001'::uuid,
  provider, 'EC', 'USD', FALSE, TRUE,
  jsonb_build_object('adapterVersion', 'operations-v1', 'activationState', 'credentials_required')
FROM unnest(ARRAY[
  'email', 'mobile_push', 'sms', 'whatsapp', 'instagram', 'google_calendar',
  'stripe', 'paypal', 'datafast', 'payphone', 'bank_transfer', 'crypto', 'sri'
]) AS provider
ON CONFLICT (organization_id, provider, country_code, currency) DO NOTHING;

INSERT INTO operations_scope_member (organization_id, branch_id, party_id)
SELECT DISTINCT
  '00000000-0000-4000-8000-000000000001'::uuid,
  '00000000-0000-4000-8000-000000000002'::uuid,
  role.party_id
FROM party_role role
WHERE role.active = TRUE
  AND role.role::text IN (
    'Admin', 'Manager', 'StudioManager', 'Accounting', 'Reception', 'Teacher',
    'Engineer', 'LiveSessionsProducer', 'Producer', 'AandR', 'Maintenance', 'ReadOnly'
  )
ON CONFLICT (organization_id, branch_id, party_id) DO UPDATE SET
  active = TRUE, updated_at = now();

CREATE OR REPLACE FUNCTION operations_sync_scope_member_from_role()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.active = TRUE AND NEW.role::text IN (
    'Admin', 'Manager', 'StudioManager', 'Accounting', 'Reception', 'Teacher',
    'Engineer', 'LiveSessionsProducer', 'Producer', 'AandR', 'Maintenance', 'ReadOnly'
  ) THEN
    INSERT INTO operations_scope_member (organization_id, branch_id, party_id)
    VALUES (
      '00000000-0000-4000-8000-000000000001',
      '00000000-0000-4000-8000-000000000002',
      NEW.party_id
    ) ON CONFLICT (organization_id, branch_id, party_id) DO UPDATE SET
      active = TRUE, updated_at = now();
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_party_role_scope_sync ON party_role;
CREATE TRIGGER operations_party_role_scope_sync
  AFTER INSERT OR UPDATE OF role, active ON party_role
  FOR EACH ROW EXECUTE FUNCTION operations_sync_scope_member_from_role();

CREATE OR REPLACE FUNCTION operations_course_registration_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN
    RETURN NEW;
  END IF;
  event_name := CASE WHEN TG_OP = 'INSERT' THEN 'course_registration.created'
    ELSE 'course_registration.' || lower(NEW.status) END;
  priority_name := CASE WHEN lower(NEW.status) IN ('pending', 'new', 'awaiting_confirmation') THEN 'high' ELSE 'normal' END;
  PERFORM operations_record_event(
    event_name, 'course_registration', NEW.id::text,
    'course_registration:' || NEW.id::text, NEW.source, priority_name,
    'Inscripción de curso requiere atención', 'Course registration needs attention',
    'Revise la inscripción y ejecute la acción empresarial correspondiente.',
    'Review the registration and perform the corresponding business action.',
    jsonb_build_object('courseSlug', NEW.course_slug, 'registrationStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('cancelled', 'rejected')),
    COALESCE(NEW.updated_at, NEW.created_at), NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_course_registration_capture ON course_registration;
CREATE TRIGGER operations_course_registration_capture
  AFTER INSERT OR UPDATE OF status ON course_registration
  FOR EACH ROW EXECUTE FUNCTION operations_course_registration_event();

CREATE OR REPLACE FUNCTION operations_booking_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.starts_at IS NOT DISTINCT FROM OLD.starts_at
    AND NEW.ends_at IS NOT DISTINCT FROM OLD.ends_at THEN
    RETURN NEW;
  END IF;
  event_name := CASE WHEN TG_OP = 'INSERT' THEN 'booking.created' ELSE 'booking.modified' END;
  priority_name := CASE WHEN NEW.starts_at <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END;
  PERFORM operations_record_event(
    event_name, 'booking', NEW.id::text, 'booking:' || NEW.id::text,
    'web', priority_name,
    'Reserva requiere revisión', 'Reservation needs review',
    'Revise horario, recursos y conflictos antes de confirmar.',
    'Review schedule, resources, and conflicts before confirming.',
    jsonb_build_object('bookingStatus', NEW.status::text, 'startsAt', NEW.starts_at, 'endsAt', NEW.ends_at,
      'terminal', NEW.status::text IN ('Completed', 'Cancelled', 'NoShow')),
    COALESCE(NEW.created_at, now()), NULL, NEW.starts_at <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_booking_capture ON booking;
CREATE TRIGGER operations_booking_capture
  AFTER INSERT OR UPDATE OF status, starts_at, ends_at ON booking
  FOR EACH ROW EXECUTE FUNCTION operations_booking_event();

CREATE OR REPLACE FUNCTION operations_invoice_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.due_date IS NOT DISTINCT FROM OLD.due_date THEN
    RETURN NEW;
  END IF;
  event_name := CASE
    WHEN NEW.status::text = 'Draft' THEN 'invoice.created'
    WHEN NEW.due_date < current_date AND NEW.status::text NOT IN ('Paid', 'CancelledI') THEN 'invoice.overdue'
    ELSE 'invoice.' || lower(NEW.status::text)
  END;
  priority_name := CASE WHEN event_name = 'invoice.overdue' THEN 'high' ELSE 'normal' END;
  PERFORM operations_record_event(
    event_name, 'invoice', NEW.id::text, 'invoice:' || NEW.id::text,
    'web', priority_name,
    CASE WHEN event_name = 'invoice.overdue' THEN 'Factura vencida' ELSE 'Factura requiere seguimiento' END,
    CASE WHEN event_name = 'invoice.overdue' THEN 'Overdue invoice' ELSE 'Invoice needs follow-up' END,
    'Revise el estado legal, de entrega y de pago sin reescribir documentos emitidos.',
    'Review legal, delivery, and payment status without rewriting issued documents.',
    jsonb_build_object('invoiceStatus', NEW.status::text, 'amountMinor', NEW.total_cents, 'currency', NEW.currency, 'dueDate', NEW.due_date,
      'terminal', NEW.status::text IN ('Paid', 'CancelledI')),
    COALESCE(NEW.created_at, now()), NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_invoice_capture ON invoice;
CREATE TRIGGER operations_invoice_capture
  AFTER INSERT OR UPDATE OF status, due_date ON invoice
  FOR EACH ROW EXECUTE FUNCTION operations_invoice_event();

CREATE OR REPLACE FUNCTION operations_payment_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'payment.recorded', 'payment', NEW.id::text, 'payment:' || NEW.id::text,
    'web', CASE WHEN NEW.method::text = 'BankTransferM' THEN 'high' ELSE 'normal' END,
    CASE WHEN NEW.method::text = 'BankTransferM' THEN 'Transferencia requiere verificación' ELSE 'Pago registrado' END,
    CASE WHEN NEW.method::text = 'BankTransferM' THEN 'Transfer requires verification' ELSE 'Payment recorded' END,
    'Verifique y concilie el pago con la factura o pedido correspondiente.',
    'Verify and reconcile the payment with the corresponding invoice or order.',
    jsonb_build_object('paymentMethod', NEW.method::text, 'amountMinor', NEW.amount_cents, 'currency', NEW.currency, 'invoiceId', NEW.invoice_id),
    COALESCE(NEW.created_at, NEW.received_at), NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_payment_capture ON payment;
CREATE TRIGGER operations_payment_capture
  AFTER INSERT ON payment
  FOR EACH ROW EXECUTE FUNCTION operations_payment_event();

CREATE OR REPLACE FUNCTION operations_registration_receipt_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'bank_transfer_receipt.uploaded', 'course_registration', NEW.registration_id::text,
    'course_registration:' || NEW.registration_id::text, 'web', 'high',
    'Comprobante de transferencia cargado', 'Bank-transfer receipt uploaded',
    'Verifique el comprobante antes de aplicar el pago.',
    'Verify the receipt before applying the payment.',
    jsonb_build_object('receiptId', NEW.id, 'mimeType', NEW.mime_type),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_registration_receipt_capture ON course_registration_receipt;
CREATE TRIGGER operations_registration_receipt_capture
  AFTER INSERT ON course_registration_receipt
  FOR EACH ROW EXECUTE FUNCTION operations_registration_receipt_event();

CREATE OR REPLACE FUNCTION operations_marketplace_order_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'marketplace_order.created' ELSE 'marketplace_order.' || lower(NEW.status) END,
    'marketplace_order', NEW.id::text, 'marketplace_order:' || NEW.id::text,
    'marketplace', CASE WHEN lower(NEW.status) IN ('payment_failed', 'disputed') THEN 'urgent' ELSE 'high' END,
    'Pedido de marketplace requiere atención', 'Marketplace order needs attention',
    'Revise pago, proveedor y cumplimiento del pedido.', 'Review payment, provider, and order fulfillment.',
    jsonb_build_object('orderStatus', NEW.status, 'amountMinor', NEW.total_usd_cents, 'currency', NEW.currency,
      'terminal', lower(NEW.status) IN ('fulfilled', 'completed', 'cancelled', 'refunded')),
    COALESCE(NEW.updated_at, NEW.created_at), NULL, lower(NEW.status) IN ('payment_failed', 'disputed')
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_marketplace_order_capture ON marketplace_order;
CREATE TRIGGER operations_marketplace_order_capture
  AFTER INSERT OR UPDATE OF status ON marketplace_order
  FOR EACH ROW EXECUTE FUNCTION operations_marketplace_order_event();

CREATE OR REPLACE FUNCTION operations_maintenance_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'maintenance.opened' ELSE 'maintenance.' || lower(NEW.status) END,
    'maintenance_ticket', NEW.id::text, 'maintenance_ticket:' || NEW.id::text,
    'internal', CASE WHEN lower(NEW.status) IN ('blocked', 'unsafe') THEN 'urgent' ELSE 'high' END,
    'Mantenimiento requiere atención', 'Maintenance needs attention',
    NEW.summary, NEW.summary,
    jsonb_build_object('assetId', NEW.asset_id, 'maintenanceStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('closed', 'completed', 'resolved')),
    NEW.opened_at, NULL, lower(NEW.status) IN ('blocked', 'unsafe')
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_maintenance_capture ON maintenance_ticket;
CREATE TRIGGER operations_maintenance_capture
  AFTER INSERT OR UPDATE OF status ON maintenance_ticket
  FOR EACH ROW EXECUTE FUNCTION operations_maintenance_event();

CREATE OR REPLACE FUNCTION operations_service_order_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.scheduled_start IS NOT DISTINCT FROM OLD.scheduled_start
    AND NEW.scheduled_end IS NOT DISTINCT FROM OLD.scheduled_end THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'service_inquiry.created' ELSE 'service_order.' || lower(NEW.status) END,
    'service_order', NEW.id::text, 'service_order:' || NEW.id::text,
    'web', CASE WHEN NEW.scheduled_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
    'Solicitud de servicio requiere seguimiento', 'Service request needs follow-up',
    COALESCE(NEW.description, NEW.title, 'Revise alcance, cotización, agenda y pago.'),
    COALESCE(NEW.description, NEW.title, 'Review scope, quote, schedule, and payment.'),
    jsonb_build_object('serviceKind', NEW.service_kind::text, 'orderStatus', NEW.status,
      'amountMinor', NEW.price_quoted_cents, 'startsAt', NEW.scheduled_start,
      'terminal', lower(NEW.status) IN ('completed', 'cancelled', 'rejected')),
    NEW.created_at, NULL, NEW.scheduled_start <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_service_order_capture ON service_order;
CREATE TRIGGER operations_service_order_capture
  AFTER INSERT OR UPDATE OF status, scheduled_start, scheduled_end ON service_order
  FOR EACH ROW EXECUTE FUNCTION operations_service_order_event();

CREATE OR REPLACE FUNCTION operations_package_purchase_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.remaining_units IS NOT DISTINCT FROM OLD.remaining_units
    AND NEW.expires_at IS NOT DISTINCT FROM OLD.expires_at THEN RETURN NEW; END IF;
  IF lower(NEW.status) = 'active' AND
      (NEW.remaining_units <= 2 OR (NEW.expires_at IS NOT NULL AND NEW.expires_at <= now() + interval '30 days')) THEN
    PERFORM operations_record_event(
      'package.depletion_or_expiry_warning', 'package_purchase', NEW.id::text,
      'package_purchase:' || NEW.id::text, 'internal',
      CASE WHEN NEW.remaining_units <= 0 OR NEW.expires_at <= now() + interval '7 days' THEN 'high' ELSE 'normal' END,
      'Paquete próximo a agotarse o vencer', 'Package nearing depletion or expiry',
      'Contacte al cliente y defina renovación o uso pendiente.',
      'Contact the customer and arrange renewal or remaining use.',
      jsonb_build_object('remainingUnits', NEW.remaining_units, 'expiresAt', NEW.expires_at,
        'buyerPartyId', NEW.buyer_id, 'terminal', false),
      NEW.purchased_at, NULL, FALSE
    );
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_package_purchase_capture ON package_purchase;
CREATE TRIGGER operations_package_purchase_capture
  AFTER INSERT OR UPDATE OF status, remaining_units, expires_at ON package_purchase
  FOR EACH ROW EXECUTE FUNCTION operations_package_purchase_event();

CREATE OR REPLACE FUNCTION operations_lead_interest_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'lead.created' ELSE 'lead.' || lower(NEW.status) END,
    'lead_interest', NEW.id::text, 'lead_interest:' || NEW.id::text, NEW.source,
    CASE WHEN lower(NEW.status) IN ('open', 'new') THEN 'high' ELSE 'normal' END,
    'Lead requiere seguimiento', 'Lead needs follow-up',
    COALESCE(NEW.details, 'Contacte al lead y registre el siguiente paso.'),
    COALESCE(NEW.details, 'Contact the lead and record the next step.'),
    jsonb_build_object('partyId', NEW.party_id, 'interestType', NEW.interest_type,
      'leadStatus', NEW.status, 'terminal', lower(NEW.status) IN ('won', 'lost', 'closed', 'cancelled')),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_lead_interest_capture ON lead_interest;
CREATE TRIGGER operations_lead_interest_capture
  AFTER INSERT OR UPDATE OF status ON lead_interest
  FOR EACH ROW EXECUTE FUNCTION operations_lead_interest_event();

CREATE OR REPLACE FUNCTION operations_trial_request_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.assigned_teacher_id IS NOT DISTINCT FROM OLD.assigned_teacher_id THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'trial_request.created' ELSE 'trial_request.' || lower(NEW.status) END,
    'trial_request', NEW.id::text, 'trial_request:' || NEW.id::text, 'web',
    CASE WHEN NEW.pref1_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
    'Clase de prueba requiere coordinación', 'Trial lesson needs coordination',
    'Asigne profesor, horario y sala antes de confirmar.',
    'Assign a teacher, schedule, and room before confirming.',
    jsonb_build_object('partyId', NEW.party_id, 'subjectId', NEW.subject_id,
      'startsAt', NEW.pref1_start, 'trialStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('completed', 'cancelled', 'rejected')),
    NEW.created_at, NULL, NEW.pref1_start <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_trial_request_capture ON trial_request;
CREATE TRIGGER operations_trial_request_capture
  AFTER INSERT OR UPDATE OF status, assigned_teacher_id ON trial_request
  FOR EACH ROW EXECUTE FUNCTION operations_trial_request_event();

CREATE OR REPLACE FUNCTION operations_artist_profile_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'artist.registration_requires_review', 'artist_profile', NEW.id::text,
    'artist_profile:' || NEW.id::text, 'web', 'normal',
    'Perfil de artista requiere revisión', 'Artist profile needs review',
    'Revise identidad, permisos y publicación del perfil.',
    'Review identity, permissions, and profile publication.',
    jsonb_build_object('artistPartyId', NEW.artist_party_id, 'terminal', false),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_artist_profile_capture ON artist_profile;
CREATE TRIGGER operations_artist_profile_capture
  AFTER INSERT ON artist_profile
  FOR EACH ROW EXECUTE FUNCTION operations_artist_profile_event();

CREATE OR REPLACE FUNCTION operations_intern_task_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.assigned_to IS NOT DISTINCT FROM OLD.assigned_to
    AND NEW.due_at IS NOT DISTINCT FROM OLD.due_at THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'project_task.created' ELSE 'project_task.' || lower(NEW.status) END,
    'intern_task', NEW.id::text, 'intern_task:' || NEW.id::text, 'internal',
    CASE WHEN NEW.due_at IS NOT NULL AND NEW.due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
    'Tarea de proyecto requiere acción', 'Project task needs action',
    COALESCE(NEW.description, NEW.title), COALESCE(NEW.description, NEW.title),
    jsonb_build_object('projectId', NEW.project_id, 'assignedPartyId', NEW.assigned_to,
      'dueAt', NEW.due_at, 'taskStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('done', 'completed', 'cancelled')),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_intern_task_capture ON intern_task;
CREATE TRIGGER operations_intern_task_capture
  AFTER INSERT OR UPDATE OF status, assigned_to, due_at ON intern_task
  FOR EACH ROW EXECUTE FUNCTION operations_intern_task_event();

CREATE OR REPLACE FUNCTION operations_integration_failure_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM operations_record_event(
    'integration.failure', 'integration_failure', NEW.id::text,
    'integration_failure:' || NEW.id::text, 'internal',
    CASE WHEN NEW.status = 'dead_letter' THEN 'urgent' ELSE 'high' END,
    'Fallo de integración requiere atención', 'Integration failure needs attention',
    NEW.redacted_summary, NEW.redacted_summary,
    jsonb_build_object('provider', NEW.provider, 'failureCode', NEW.failure_code,
      'retryable', NEW.retryable, 'terminal', false),
    NEW.created_at, NULL, NEW.status = 'dead_letter'
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_integration_failure_capture ON operations_integration_failure;
CREATE TRIGGER operations_integration_failure_capture
  AFTER INSERT ON operations_integration_failure
  FOR EACH ROW EXECUTE FUNCTION operations_integration_failure_event();

CREATE OR REPLACE FUNCTION operations_whatsapp_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'communication.whatsapp.received',
    CASE WHEN NEW.party_id IS NULL THEN 'uncorrelated_inbound' ELSE 'party' END,
    COALESCE(NEW.party_id::text, NEW.sender_id),
    'whatsapp:' || NEW.sender_id, 'whatsapp', 'high',
    'Mensaje de WhatsApp requiere respuesta', 'WhatsApp message needs a response',
    'Revise la conversación y responda por un canal autorizado.',
    'Review the conversation and respond through an approved channel.',
    jsonb_build_object('partyId', NEW.party_id, 'replyStatus', NEW.reply_status,
      'uncorrelatedIdentity', NEW.party_id IS NULL, 'terminal', false),
    NEW.created_at, NEW.external_id, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_whatsapp_inbound_capture ON whats_app_message;
CREATE TRIGGER operations_whatsapp_inbound_capture
  AFTER INSERT ON whats_app_message
  FOR EACH ROW EXECUTE FUNCTION operations_whatsapp_inbound_event();

CREATE OR REPLACE FUNCTION operations_social_inbound_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE provider_name TEXT := CASE WHEN TG_TABLE_NAME = 'instagram_message' THEN 'instagram' ELSE 'facebook' END;
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'communication.' || provider_name || '.received', 'uncorrelated_inbound', NEW.sender_id,
    provider_name || ':' || NEW.sender_id, provider_name, 'high',
    'Mensaje social requiere respuesta', 'Social message needs a response',
    'Correlacione la identidad si es posible y responda por el canal autorizado.',
    'Correlate the identity when possible and respond through the approved channel.',
    jsonb_build_object('provider', provider_name, 'replyStatus', NEW.reply_status,
      'uncorrelatedIdentity', true, 'terminal', false),
    NEW.created_at, NEW.external_id, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_instagram_inbound_capture ON instagram_message;
CREATE TRIGGER operations_instagram_inbound_capture
  AFTER INSERT ON instagram_message
  FOR EACH ROW EXECUTE FUNCTION operations_social_inbound_event();
DROP TRIGGER IF EXISTS operations_facebook_inbound_capture ON facebook_message;
CREATE TRIGGER operations_facebook_inbound_capture
  AFTER INSERT ON facebook_message
  FOR EACH ROW EXECUTE FUNCTION operations_social_inbound_event();

CREATE OR REPLACE FUNCTION operations_feature_access_request_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) NOT IN ('pending', 'open');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'user.access_request.closed' ELSE 'user.access_request.review_required' END,
    'feature_access_request', NEW.id::text, 'feature_access_request:' || NEW.id::text,
    'web', CASE WHEN NEW.expires_at IS NOT NULL AND NEW.expires_at <= now() + interval '1 day' THEN 'high' ELSE 'normal' END,
    'Solicitud de acceso requiere revisión', 'Access request needs review',
    'Revise el alcance y aplique mínimo privilegio.', 'Review scope and apply least privilege.',
    jsonb_build_object('requesterPartyId', NEW.requester_party_id, 'featureId', NEW.feature_id,
      'requestedAction', NEW.action, 'requestStatus', NEW.status, 'terminal', terminal),
    COALESCE(NEW.updated_at, NEW.requested_at), NULL, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_feature_access_request_capture ON feature_access_requests;
CREATE TRIGGER operations_feature_access_request_capture
  AFTER INSERT OR UPDATE OF status, reviewer_party_id ON feature_access_requests
  FOR EACH ROW EXECUTE FUNCTION operations_feature_access_request_event();

CREATE OR REPLACE FUNCTION operations_proposal_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) IN ('accepted', 'rejected', 'expired', 'cancelled');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'proposal.closed' ELSE 'proposal.review_required' END,
    'proposal', NEW.id::text, 'proposal:' || NEW.id::text, 'web',
    CASE WHEN lower(NEW.status) = 'sent' THEN 'high' ELSE 'normal' END,
    'Cotización requiere seguimiento', 'Quote needs follow-up',
    'Revise la cotización, el cliente y el siguiente paso.', 'Review the quote, customer, and next step.',
    jsonb_build_object('clientPartyId', NEW.client_party_id, 'proposalStatus', NEW.status,
      'serviceKind', NEW.service_kind, 'terminal', terminal),
    NEW.updated_at, NULL, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_proposal_capture ON proposal;
CREATE TRIGGER operations_proposal_capture
  AFTER INSERT OR UPDATE OF status, client_party_id ON proposal
  FOR EACH ROW EXECUTE FUNCTION operations_proposal_event();

CREATE OR REPLACE FUNCTION operations_stock_item_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE warning BOOLEAN := NEW.reorder_point IS NOT NULL AND NEW.on_hand <= NEW.reorder_point;
BEGIN
  PERFORM operations_record_event(
    CASE WHEN warning THEN 'inventory.reorder_required' ELSE 'inventory.stock_restored' END,
    'stock_item', NEW.id::text, 'stock_item:' || NEW.id::text, 'internal',
    CASE WHEN warning AND NEW.on_hand <= 0 THEN 'high' ELSE 'normal' END,
    CASE WHEN warning THEN 'Inventario requiere reposición' ELSE 'Nivel de inventario restablecido' END,
    CASE WHEN warning THEN 'Inventory requires replenishment' ELSE 'Inventory level restored' END,
    'Revise existencias y necesidades operativas.', 'Review stock and operational requirements.',
    jsonb_build_object('onHand', NEW.on_hand, 'reorderPoint', NEW.reorder_point,
      'terminal', NOT warning), now(), NULL, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_stock_item_capture ON stock_item;
CREATE TRIGGER operations_stock_item_capture
  AFTER INSERT OR UPDATE OF on_hand, reorder_point ON stock_item
  FOR EACH ROW WHEN (NEW.reorder_point IS NOT NULL) EXECUTE FUNCTION operations_stock_item_event();

CREATE OR REPLACE FUNCTION operations_intern_project_event()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) IN ('completed', 'cancelled', 'archived');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'project.closed' ELSE 'project.action_required' END,
    'intern_project', NEW.id::text, 'intern_project:' || NEW.id::text, 'internal',
    CASE WHEN NEW.due_at IS NOT NULL AND NEW.due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
    'Proyecto requiere seguimiento', 'Project needs follow-up',
    'Revise estado, responsables y fecha objetivo.', 'Review status, owners, and target date.',
    jsonb_build_object('projectStatus', NEW.status, 'dueAt', NEW.due_at, 'terminal', terminal),
    NEW.updated_at, NULL, false
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_intern_project_capture ON intern_project;
CREATE TRIGGER operations_intern_project_capture
  AFTER INSERT OR UPDATE OF status, due_at ON intern_project
  FOR EACH ROW EXECUTE FUNCTION operations_intern_project_event();

CREATE OR REPLACE FUNCTION operations_social_event_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.end_time < now() THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'event.production_review_required', 'social_event', NEW.id::text,
    'social_event:' || NEW.id::text, 'web',
    CASE WHEN NEW.start_time <= now() + interval '24 hours' THEN 'urgent' ELSE 'normal' END,
    'Evento requiere coordinación de producción', 'Event needs production coordination',
    'Revise venue, capacidad, tareas y responsables.', 'Review venue, capacity, tasks, and owners.',
    jsonb_build_object('startsAt', NEW.start_time, 'endsAt', NEW.end_time,
      'venueId', NEW.venue_id, 'terminal', false), NEW.updated_at, NULL,
    NEW.start_time <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_social_event_capture ON social_event;
CREATE TRIGGER operations_social_event_capture
  AFTER INSERT OR UPDATE OF start_time, end_time, venue_id ON social_event
  FOR EACH ROW EXECUTE FUNCTION operations_social_event_event();

CREATE OR REPLACE FUNCTION operations_validate_entity_reference()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  exists_value BOOLEAN;
BEGIN
  IF NEW.uncorrelated THEN RETURN NEW; END IF;
  CASE NEW.entity_type
    WHEN 'course_registration' THEN SELECT EXISTS(SELECT 1 FROM course_registration WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'booking' THEN SELECT EXISTS(SELECT 1 FROM booking WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'invoice' THEN SELECT EXISTS(SELECT 1 FROM invoice WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'payment' THEN SELECT EXISTS(SELECT 1 FROM payment WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'package_purchase' THEN SELECT EXISTS(SELECT 1 FROM package_purchase WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'party' THEN SELECT EXISTS(SELECT 1 FROM party WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'marketplace_order' THEN SELECT EXISTS(SELECT 1 FROM marketplace_order WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'maintenance_ticket' THEN SELECT EXISTS(SELECT 1 FROM maintenance_ticket WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'service_order' THEN SELECT EXISTS(SELECT 1 FROM service_order WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'lead_interest' THEN SELECT EXISTS(SELECT 1 FROM lead_interest WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'trial_request' THEN SELECT EXISTS(SELECT 1 FROM trial_request WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'artist_profile' THEN SELECT EXISTS(SELECT 1 FROM artist_profile WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'intern_task' THEN SELECT EXISTS(SELECT 1 FROM intern_task WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'integration_failure' THEN SELECT EXISTS(SELECT 1 FROM operations_integration_failure WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'feature_access_request' THEN SELECT EXISTS(SELECT 1 FROM feature_access_requests WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'proposal' THEN SELECT EXISTS(SELECT 1 FROM proposal WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'stock_item' THEN SELECT EXISTS(SELECT 1 FROM stock_item WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'intern_project' THEN SELECT EXISTS(SELECT 1 FROM intern_project WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'social_event' THEN SELECT EXISTS(SELECT 1 FROM social_event WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'manual' THEN
      SELECT EXISTS(
        SELECT 1 FROM operations_domain_event
        WHERE aggregate_type = 'manual' AND aggregate_id = NEW.entity_id
      ) INTO exists_value;
    ELSE RAISE EXCEPTION 'unsupported operations entity_type %', NEW.entity_type USING ERRCODE = '23514';
  END CASE;
  IF NOT exists_value THEN
    RAISE EXCEPTION 'operations work item references missing %.%', NEW.entity_type, NEW.entity_id USING ERRCODE = '23503';
  END IF;
  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS operations_work_item_entity_reference ON operations_work_item;
CREATE CONSTRAINT TRIGGER operations_work_item_entity_reference
  AFTER INSERT OR UPDATE OF entity_type, entity_id, uncorrelated ON operations_work_item
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION operations_validate_entity_reference();

-- Idempotent, resumable backfill. Progress is implicit in durable domain events:
-- each invocation selects only eligible sources without an existing backfill event.
CREATE OR REPLACE FUNCTION operations_backfill_batch(
  p_run_key TEXT,
  p_batch_size INTEGER DEFAULT 500,
  p_dry_run BOOLEAN DEFAULT TRUE
) RETURNS TABLE(
  run_id UUID,
  eligible BIGINT,
  inserted BIGINT,
  remaining BIGINT,
  run_status TEXT
) LANGUAGE plpgsql AS $$
DECLARE
  org_id UUID := '00000000-0000-4000-8000-000000000001'::uuid;
  current_run_id UUID;
  v_eligible_count BIGINT := 0;
  v_inserted_count BIGINT := 0;
  v_remaining_count BIGINT := 0;
  source RECORD;
BEGIN
  IF btrim(COALESCE(p_run_key, '')) = '' THEN
    RAISE EXCEPTION 'run key is required' USING ERRCODE = '22023';
  END IF;
  IF p_batch_size < 1 OR p_batch_size > 5000 THEN
    RAISE EXCEPTION 'batch size must be between 1 and 5000' USING ERRCODE = '22023';
  END IF;

  INSERT INTO operations_backfill_run (
    organization_id, source_name, run_key, status, dry_run, heartbeat_at
  ) VALUES (org_id, 'operations-v1', p_run_key, 'running', p_dry_run, now())
  ON CONFLICT (organization_id, source_name, run_key, dry_run) DO UPDATE SET
    status = 'running', heartbeat_at = now(), finished_at = NULL
  RETURNING id INTO current_run_id;

  SELECT count(*) INTO v_eligible_count FROM (
    SELECT 'course_registration:' || id::text AS correlation_key FROM course_registration
      WHERE lower(status) IN ('new', 'pending', 'pending_payment', 'awaiting_confirmation', 'waitlisted')
    UNION ALL
    SELECT 'booking:' || id::text FROM booking
      WHERE status::text = 'Tentative' AND ends_at >= now() - interval '1 day'
    UNION ALL
    SELECT 'invoice:' || id::text FROM invoice
      WHERE status::text IN ('Sent', 'PartiallyPaid')
    UNION ALL
    SELECT 'package_purchase:' || id::text FROM package_purchase
      WHERE lower(status) = 'active' AND
        (remaining_units <= 2 OR (expires_at IS NOT NULL AND expires_at <= now() + interval '30 days'))
    UNION ALL
    SELECT 'marketplace_order:' || id::text FROM marketplace_order
      WHERE lower(status) IN ('pending', 'stripe_pending', 'paypal_pending', 'datafast_init', 'payment_failed', 'disputed')
    UNION ALL
    SELECT 'maintenance_ticket:' || id::text FROM maintenance_ticket
      WHERE lower(status) NOT IN ('closed', 'completed', 'resolved')
    UNION ALL
    SELECT 'service_order:' || id::text FROM service_order
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
    UNION ALL
    SELECT 'lead_interest:' || id::text FROM lead_interest
      WHERE lower(status) IN ('open', 'new', 'contacted', 'qualified')
    UNION ALL
    SELECT 'trial_request:' || id::text FROM trial_request
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
    UNION ALL
    SELECT 'proposal:' || id::text FROM proposal
      WHERE lower(status) NOT IN ('accepted', 'rejected', 'expired', 'cancelled')
    UNION ALL
    SELECT 'stock_item:' || id::text FROM stock_item
      WHERE reorder_point IS NOT NULL AND on_hand <= reorder_point
    UNION ALL
    SELECT 'feature_access_request:' || id::text FROM feature_access_requests
      WHERE lower(status) IN ('pending', 'open')
    UNION ALL
    SELECT 'intern_project:' || id::text FROM intern_project
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'archived')
    UNION ALL
    SELECT 'social_event:' || id::text FROM social_event
      WHERE end_time >= now()
  ) candidates
  WHERE NOT EXISTS (
    SELECT 1 FROM operations_domain_event event
    WHERE event.organization_id = org_id
      AND event.correlation_key = candidates.correlation_key
      AND event.payload->'metadata'->>'backfillVersion' = 'operations-v1'
  );

  IF NOT p_dry_run THEN
    FOR source IN
      SELECT * FROM (
        SELECT 'course_registration'::text AS entity_type, id::text AS entity_id,
          'course_registration:' || id::text AS correlation_key, created_at AS occurred_at,
          'high'::text AS priority, 'Inscripción existente requiere atención'::text AS title_es,
          'Existing registration needs attention'::text AS title_en,
          jsonb_build_object('courseSlug', course_slug, 'registrationStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false) AS metadata
        FROM course_registration
        WHERE lower(status) IN ('new', 'pending', 'pending_payment', 'awaiting_confirmation', 'waitlisted')
        UNION ALL
        SELECT 'booking', id::text, 'booking:' || id::text, created_at,
          CASE WHEN starts_at <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Reserva existente requiere revisión', 'Existing reservation needs review',
          jsonb_build_object('bookingStatus', status::text, 'startsAt', starts_at, 'endsAt', ends_at,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM booking WHERE status::text = 'Tentative' AND ends_at >= now() - interval '1 day'
        UNION ALL
        SELECT 'invoice', id::text, 'invoice:' || id::text, created_at,
          CASE WHEN due_date < current_date THEN 'high' ELSE 'normal' END,
          CASE WHEN due_date < current_date THEN 'Factura vencida existente' ELSE 'Factura emitida requiere seguimiento' END,
          CASE WHEN due_date < current_date THEN 'Existing overdue invoice' ELSE 'Issued invoice needs follow-up' END,
          jsonb_build_object('invoiceStatus', status::text, 'amountMinor', total_cents, 'currency', currency,
            'dueDate', due_date, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM invoice WHERE status::text IN ('Sent', 'PartiallyPaid')
        UNION ALL
        SELECT 'package_purchase', id::text, 'package_purchase:' || id::text, purchased_at,
          CASE WHEN remaining_units <= 0 OR expires_at <= now() + interval '7 days' THEN 'high' ELSE 'normal' END,
          'Paquete existente próximo a agotarse o vencer', 'Existing package nearing depletion or expiry',
          jsonb_build_object('remainingUnits', remaining_units, 'expiresAt', expires_at,
            'buyerPartyId', buyer_id, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM package_purchase WHERE lower(status) = 'active' AND
          (remaining_units <= 2 OR (expires_at IS NOT NULL AND expires_at <= now() + interval '30 days'))
        UNION ALL
        SELECT 'marketplace_order', id::text, 'marketplace_order:' || id::text, created_at,
          CASE WHEN lower(status) IN ('payment_failed', 'disputed') THEN 'urgent' ELSE 'high' END,
          'Pedido existente requiere atención', 'Existing marketplace order needs attention',
          jsonb_build_object('orderStatus', status, 'amountMinor', total_usd_cents, 'currency', currency,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM marketplace_order WHERE lower(status) IN
          ('pending', 'stripe_pending', 'paypal_pending', 'datafast_init', 'payment_failed', 'disputed')
        UNION ALL
        SELECT 'maintenance_ticket', id::text, 'maintenance_ticket:' || id::text, opened_at,
          CASE WHEN lower(status) IN ('blocked', 'unsafe') THEN 'urgent' ELSE 'high' END,
          'Mantenimiento existente requiere atención', 'Existing maintenance needs attention',
          jsonb_build_object('assetId', asset_id, 'maintenanceStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM maintenance_ticket WHERE lower(status) NOT IN ('closed', 'completed', 'resolved')
        UNION ALL
        SELECT 'service_order', id::text, 'service_order:' || id::text, created_at,
          CASE WHEN scheduled_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Solicitud de servicio existente requiere seguimiento', 'Existing service request needs follow-up',
          jsonb_build_object('serviceKind', service_kind::text, 'orderStatus', status,
            'amountMinor', price_quoted_cents, 'startsAt', scheduled_start,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM service_order WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
        UNION ALL
        SELECT 'lead_interest', id::text, 'lead_interest:' || id::text, created_at,
          'high', 'Lead existente requiere seguimiento', 'Existing lead needs follow-up',
          jsonb_build_object('partyId', party_id, 'interestType', interest_type, 'leadStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM lead_interest WHERE lower(status) IN ('open', 'new', 'contacted', 'qualified')
        UNION ALL
        SELECT 'trial_request', id::text, 'trial_request:' || id::text, created_at,
          CASE WHEN pref1_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Clase de prueba existente requiere coordinación', 'Existing trial lesson needs coordination',
          jsonb_build_object('partyId', party_id, 'subjectId', subject_id, 'startsAt', pref1_start,
            'trialStatus', status, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM trial_request WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
        UNION ALL
        SELECT 'proposal', id::text, 'proposal:' || id::text, created_at,
          CASE WHEN lower(status) = 'sent' THEN 'high' ELSE 'normal' END,
          'Cotización existente requiere seguimiento', 'Existing quote needs follow-up',
          jsonb_build_object('clientPartyId', client_party_id, 'proposalStatus', status,
            'serviceKind', service_kind, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM proposal WHERE lower(status) NOT IN ('accepted', 'rejected', 'expired', 'cancelled')
        UNION ALL
        SELECT 'stock_item', id::text, 'stock_item:' || id::text, now(),
          CASE WHEN on_hand <= 0 THEN 'high' ELSE 'normal' END,
          'Inventario existente requiere reposición', 'Existing inventory needs replenishment',
          jsonb_build_object('onHand', on_hand, 'reorderPoint', reorder_point,
            'timestampBasis', 'backfill_run', 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM stock_item WHERE reorder_point IS NOT NULL AND on_hand <= reorder_point
        UNION ALL
        SELECT 'feature_access_request', id::text, 'feature_access_request:' || id::text, requested_at,
          'normal', 'Solicitud de acceso existente requiere revisión', 'Existing access request needs review',
          jsonb_build_object('requesterPartyId', requester_party_id, 'featureId', feature_id,
            'requestedAction', action, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM feature_access_requests WHERE lower(status) IN ('pending', 'open')
        UNION ALL
        SELECT 'intern_project', id::text, 'intern_project:' || id::text, created_at,
          CASE WHEN due_at IS NOT NULL AND due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
          'Proyecto existente requiere seguimiento', 'Existing project needs follow-up',
          jsonb_build_object('projectStatus', status, 'dueAt', due_at,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM intern_project WHERE lower(status) NOT IN ('completed', 'cancelled', 'archived')
        UNION ALL
        SELECT 'social_event', id::text, 'social_event:' || id::text, created_at,
          CASE WHEN start_time <= now() + interval '24 hours' THEN 'urgent' ELSE 'normal' END,
          'Evento existente requiere coordinación', 'Existing event needs coordination',
          jsonb_build_object('startsAt', start_time, 'endsAt', end_time, 'venueId', venue_id,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM social_event WHERE end_time >= now()
      ) eligible_source
      WHERE NOT EXISTS (
        SELECT 1 FROM operations_domain_event event
        WHERE event.organization_id = org_id
          AND event.correlation_key = eligible_source.correlation_key
          AND event.payload->'metadata'->>'backfillVersion' = 'operations-v1'
      )
      ORDER BY occurred_at, entity_type, entity_id
      LIMIT p_batch_size
    LOOP
      PERFORM operations_record_event(
        'backfill.' || source.entity_type || '.attention_required', source.entity_type,
        source.entity_id, source.correlation_key, 'backfill', source.priority,
        source.title_es, source.title_en,
        'Registro operativo pendiente detectado por backfill; revise el registro fuente.',
        'Pending operational record detected by backfill; review the source record.',
        source.metadata, source.occurred_at, NULL, source.priority = 'urgent'
      );
      v_inserted_count := v_inserted_count + 1;
    END LOOP;
  END IF;

  v_remaining_count := CASE WHEN p_dry_run THEN v_eligible_count ELSE GREATEST(v_eligible_count - v_inserted_count, 0) END;
  UPDATE operations_backfill_run SET
    scanned_count = operations_backfill_run.scanned_count + v_eligible_count,
    eligible_count = operations_backfill_run.eligible_count + v_eligible_count,
    inserted_count = operations_backfill_run.inserted_count + v_inserted_count,
    skipped_count = operations_backfill_run.skipped_count + GREATEST(v_eligible_count - v_inserted_count, 0),
    cursor_value = jsonb_build_object('remaining', v_remaining_count, 'batchSize', p_batch_size)::text,
    heartbeat_at = now(),
    status = CASE WHEN p_dry_run OR v_remaining_count = 0 THEN 'completed' ELSE 'running' END,
    finished_at = CASE WHEN p_dry_run OR v_remaining_count = 0 THEN now() ELSE NULL END
  WHERE id = current_run_id;

  RETURN QUERY SELECT current_run_id, v_eligible_count, v_inserted_count, v_remaining_count,
    CASE WHEN p_dry_run OR v_remaining_count = 0 THEN 'completed'::text ELSE 'running'::text END;
END;
$$;

COMMIT;
