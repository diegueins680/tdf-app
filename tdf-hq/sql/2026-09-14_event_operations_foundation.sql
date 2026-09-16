BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS event_operation_lifecycle_transition_policy (
  from_state TEXT NOT NULL,
  to_state TEXT NOT NULL,
  required_authority TEXT NOT NULL,
  public_after BOOLEAN NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (from_state, to_state),
  CONSTRAINT event_operation_transition_state_check CHECK (
    from_state IN (
      'draft', 'planning', 'pending_approval', 'approved', 'published',
      'staffing', 'ready', 'in_progress', 'completed', 'settlement_pending',
      'settled', 'archived', 'reprogrammed', 'cancelled'
    ) AND to_state IN (
      'draft', 'planning', 'pending_approval', 'approved', 'published',
      'staffing', 'ready', 'in_progress', 'completed', 'settlement_pending',
      'settled', 'archived', 'reprogrammed', 'cancelled'
    )
  ),
  CONSTRAINT event_operation_transition_authority_check CHECK (
    required_authority IN ('owner', 'event_approver', 'finance_approver', 'records_manager')
  ),
  CONSTRAINT event_operation_transition_not_self CHECK (from_state <> to_state)
);

INSERT INTO event_operation_lifecycle_transition_policy (
  from_state, to_state, required_authority, public_after
) VALUES
  ('draft', 'planning', 'owner', FALSE),
  ('draft', 'cancelled', 'owner', FALSE),
  ('planning', 'pending_approval', 'owner', FALSE),
  ('planning', 'cancelled', 'owner', FALSE),
  ('pending_approval', 'planning', 'owner', FALSE),
  ('pending_approval', 'approved', 'event_approver', FALSE),
  ('pending_approval', 'cancelled', 'owner', FALSE),
  ('approved', 'planning', 'owner', FALSE),
  ('approved', 'published', 'owner', TRUE),
  ('approved', 'cancelled', 'owner', FALSE),
  ('published', 'planning', 'owner', FALSE),
  ('published', 'staffing', 'owner', TRUE),
  ('published', 'reprogrammed', 'owner', FALSE),
  ('published', 'cancelled', 'owner', FALSE),
  ('staffing', 'ready', 'owner', TRUE),
  ('staffing', 'reprogrammed', 'owner', FALSE),
  ('staffing', 'cancelled', 'owner', FALSE),
  ('ready', 'in_progress', 'owner', TRUE),
  ('ready', 'reprogrammed', 'owner', FALSE),
  ('ready', 'cancelled', 'owner', FALSE),
  ('in_progress', 'completed', 'owner', TRUE),
  ('in_progress', 'cancelled', 'owner', FALSE),
  ('completed', 'settlement_pending', 'owner', FALSE),
  ('settlement_pending', 'settled', 'finance_approver', FALSE),
  ('settled', 'archived', 'records_manager', FALSE),
  ('reprogrammed', 'planning', 'owner', FALSE),
  ('reprogrammed', 'cancelled', 'owner', FALSE),
  ('cancelled', 'archived', 'records_manager', FALSE)
ON CONFLICT (from_state, to_state) DO UPDATE SET
  required_authority = EXCLUDED.required_authority,
  public_after = EXCLUDED.public_after,
  active = TRUE;

CREATE TABLE IF NOT EXISTS event_operation_event_state (
  event_id BIGINT PRIMARY KEY REFERENCES social_event(id) ON DELETE CASCADE,
  canonical_state TEXT NOT NULL,
  version BIGINT NOT NULL DEFAULT 1,
  legacy_state_code TEXT NULL,
  migration_evidence TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_event_state_code_check CHECK (
    canonical_state IN (
      'draft', 'planning', 'pending_approval', 'approved', 'published',
      'staffing', 'ready', 'in_progress', 'completed', 'settlement_pending',
      'settled', 'archived', 'reprogrammed', 'cancelled'
    )
  ),
  CONSTRAINT event_operation_event_state_version_check CHECK (version > 0)
);

INSERT INTO event_operation_event_state (
  event_id, canonical_state, legacy_state_code, migration_evidence
)
SELECT event.id,
  CASE state.code
    WHEN 'planning' THEN 'planning'
    WHEN 'announced' THEN 'published'
    WHEN 'on_sale' THEN 'staffing'
    WHEN 'live' THEN 'in_progress'
    WHEN 'postponed' THEN 'reprogrammed'
    WHEN 'completed' THEN 'completed'
    WHEN 'cancelled' THEN 'cancelled'
    WHEN 'unavailable' THEN 'cancelled'
    WHEN 'out_of_scope' THEN 'cancelled'
    ELSE 'draft'
  END,
  state.code,
  CASE
    WHEN state.code IS NULL THEN 'no active legacy workflow state; review required before activation'
    WHEN state.code IN ('unavailable', 'out_of_scope') THEN
      'legacy non-operational state mapped to cancelled; review required before activation'
    ELSE 'deterministic compatibility map from social-event-lifecycle'
  END
FROM social_event event
LEFT JOIN workflow_state state ON state.id = event.workflow_state_id
ON CONFLICT (event_id) DO NOTHING;

CREATE TABLE IF NOT EXISTS event_operation_migration_issue (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
  issue_code TEXT NOT NULL,
  source_value TEXT NULL,
  detail TEXT NOT NULL,
  resolved_at TIMESTAMPTZ NULL,
  resolved_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX IF NOT EXISTS event_operation_migration_issue_unique
  ON event_operation_migration_issue (event_id, issue_code, COALESCE(source_value, ''));

CREATE TABLE IF NOT EXISTS event_operation_relationship (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  relationship_kind TEXT NOT NULL,
  valid_from TIMESTAMPTZ NOT NULL DEFAULT now(),
  valid_until TIMESTAMPTZ NULL,
  revoked_at TIMESTAMPTZ NULL,
  created_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  revoked_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  provenance JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_relationship_kind_check CHECK (
    relationship_kind IN ('primary_owner', 'co_owner', 'coproducer')
  ),
  CONSTRAINT event_operation_relationship_window_check CHECK (
    valid_until IS NULL OR valid_until > valid_from
  ),
  CONSTRAINT event_operation_relationship_revocation_check CHECK (
    revoked_at IS NULL OR revoked_at >= valid_from
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS event_operation_relationship_active_unique
  ON event_operation_relationship (event_id, party_id, relationship_kind)
  WHERE revoked_at IS NULL;
CREATE INDEX IF NOT EXISTS event_operation_relationship_event_active_idx
  ON event_operation_relationship (event_id, relationship_kind, valid_until)
  WHERE revoked_at IS NULL;

INSERT INTO event_operation_relationship (
  event_id, party_id, relationship_kind, provenance
)
SELECT event.id, party.id, 'primary_owner',
  jsonb_build_object('source', 'social_event.organizer_party_id', 'value', event.organizer_party_id)
FROM social_event event
JOIN party ON party.id::text = btrim(event.organizer_party_id)
WHERE event.organizer_party_id IS NOT NULL
  AND btrim(event.organizer_party_id) ~ '^[1-9][0-9]*$'
ON CONFLICT DO NOTHING;

INSERT INTO event_operation_migration_issue (
  event_id, issue_code, source_value, detail
)
SELECT event.id,
  CASE
    WHEN NULLIF(btrim(event.organizer_party_id), '') IS NULL THEN 'owner_missing'
    ELSE 'owner_reference_unresolved'
  END,
  NULLIF(btrim(event.organizer_party_id), ''),
  'Canonical owner was not inferred; event operations must remain disabled until reviewed.'
FROM social_event event
LEFT JOIN event_operation_relationship relationship
  ON relationship.event_id = event.id
  AND relationship.relationship_kind IN ('primary_owner', 'co_owner')
  AND relationship.revoked_at IS NULL
WHERE relationship.id IS NULL
ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS event_operation_grant (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
  grantee_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  scope_code TEXT NOT NULL,
  resource_kind TEXT NOT NULL DEFAULT 'event',
  resource_id TEXT NULL,
  valid_from TIMESTAMPTZ NOT NULL DEFAULT now(),
  valid_until TIMESTAMPTZ NULL,
  issued_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  invitation_id BIGINT NULL REFERENCES event_invitation(id) ON DELETE SET NULL,
  revoked_at TIMESTAMPTZ NULL,
  revoked_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  revocation_reason TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_grant_scope_check CHECK (
    scope_code IN (
      'event.read', 'event.manage', 'event.publish', 'event.approve',
      'task.read', 'task.manage', 'booking.manage', 'contract.manage',
      'finance.read', 'finance.approve', 'audit.read'
    )
  ),
  CONSTRAINT event_operation_grant_resource_check CHECK (
    resource_kind IN ('event', 'session', 'task', 'booking', 'contract', 'document')
  ),
  CONSTRAINT event_operation_grant_resource_identity_check CHECK (
    (resource_kind = 'event' AND resource_id IS NULL)
    OR (resource_kind <> 'event' AND NULLIF(btrim(resource_id), '') IS NOT NULL)
  ),
  CONSTRAINT event_operation_grant_window_check CHECK (
    valid_until IS NULL OR valid_until > valid_from
  ),
  CONSTRAINT event_operation_grant_revocation_check CHECK (
    (revoked_at IS NULL AND revoked_by_party_id IS NULL AND revocation_reason IS NULL)
    OR (revoked_at IS NOT NULL AND revoked_by_party_id IS NOT NULL
        AND NULLIF(btrim(revocation_reason), '') IS NOT NULL)
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS event_operation_grant_active_unique
  ON event_operation_grant (
    event_id, grantee_party_id, scope_code, resource_kind, COALESCE(resource_id, '')
  ) WHERE revoked_at IS NULL;
CREATE INDEX IF NOT EXISTS event_operation_grant_lookup_idx
  ON event_operation_grant (event_id, grantee_party_id, scope_code, valid_until)
  WHERE revoked_at IS NULL;

CREATE TABLE IF NOT EXISTS event_operation_revision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  version BIGINT NOT NULL,
  snapshot JSONB NOT NULL,
  content_sha256 BYTEA NOT NULL,
  authored_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (event_id, version),
  CONSTRAINT event_operation_revision_version_check CHECK (version > 0),
  CONSTRAINT event_operation_revision_hash_check CHECK (octet_length(content_sha256) = 32),
  CONSTRAINT event_operation_revision_snapshot_check CHECK (jsonb_typeof(snapshot) = 'object')
);

CREATE TABLE IF NOT EXISTS event_operation_command_receipt (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  operation_code TEXT NOT NULL,
  command_id UUID NOT NULL,
  request_sha256 BYTEA NOT NULL,
  expected_version BIGINT NULL,
  result_version BIGINT NULL,
  outcome TEXT NOT NULL,
  response JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (event_id, actor_party_id, operation_code, command_id),
  CONSTRAINT event_operation_command_hash_check CHECK (octet_length(request_sha256) = 32),
  CONSTRAINT event_operation_command_outcome_check CHECK (
    outcome IN ('accepted', 'rejected', 'conflict')
  ),
  CONSTRAINT event_operation_command_response_check CHECK (jsonb_typeof(response) = 'object')
);

CREATE TABLE IF NOT EXISTS event_operation_audit_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  actor_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  actor_reference TEXT NOT NULL,
  operation_code TEXT NOT NULL,
  command_id UUID NULL,
  resource_kind TEXT NOT NULL,
  resource_id TEXT NOT NULL,
  outcome TEXT NOT NULL,
  reason TEXT NULL,
  before_state JSONB NULL,
  after_state JSONB NULL,
  correlation_id TEXT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_audit_outcome_check CHECK (
    outcome IN ('accepted', 'rejected', 'conflict', 'override')
  ),
  CONSTRAINT event_operation_audit_override_reason_check CHECK (
    outcome <> 'override' OR NULLIF(btrim(reason), '') IS NOT NULL
  )
);

CREATE INDEX IF NOT EXISTS event_operation_audit_event_timeline_idx
  ON event_operation_audit_event (event_id, occurred_at DESC, id);
CREATE INDEX IF NOT EXISTS event_operation_audit_event_command_idx
  ON event_operation_audit_event (command_id) WHERE command_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS event_operation_transition (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  command_id UUID NOT NULL,
  actor_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  from_state TEXT NOT NULL,
  to_state TEXT NOT NULL,
  expected_version BIGINT NOT NULL,
  result_version BIGINT NOT NULL,
  authority_code TEXT NOT NULL,
  reason TEXT NULL,
  effects JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (event_id, command_id),
  FOREIGN KEY (from_state, to_state)
    REFERENCES event_operation_lifecycle_transition_policy (from_state, to_state),
  CONSTRAINT event_operation_transition_version_check CHECK (
    expected_version > 0 AND result_version = expected_version + 1
  ),
  CONSTRAINT event_operation_transition_effects_check CHECK (jsonb_typeof(effects) = 'object')
);

CREATE TABLE IF NOT EXISTS event_operation_session (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
  revision_id UUID NULL REFERENCES event_operation_revision(id) ON DELETE RESTRICT,
  name TEXT NOT NULL,
  attendance_mode TEXT NOT NULL,
  timezone TEXT NOT NULL DEFAULT 'America/Guayaquil',
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  visibility TEXT NOT NULL DEFAULT 'internal',
  version BIGINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_session_name_check CHECK (NULLIF(btrim(name), '') IS NOT NULL),
  CONSTRAINT event_operation_session_mode_check CHECK (
    attendance_mode IN ('physical', 'virtual', 'hybrid')
  ),
  CONSTRAINT event_operation_session_window_check CHECK (ends_at > starts_at),
  CONSTRAINT event_operation_session_visibility_check CHECK (
    visibility IN ('public', 'unlisted', 'private', 'internal', 'role_restricted')
  ),
  CONSTRAINT event_operation_session_version_check CHECK (version > 0)
);

CREATE INDEX IF NOT EXISTS event_operation_session_event_time_idx
  ON event_operation_session (event_id, starts_at, ends_at);

CREATE OR REPLACE FUNCTION event_operation_validate_timezone()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_timezone_names WHERE name = NEW.timezone) THEN
    RAISE EXCEPTION 'unknown IANA timezone: %', NEW.timezone USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS event_operation_session_timezone_guard ON event_operation_session;
CREATE TRIGGER event_operation_session_timezone_guard
  BEFORE INSERT OR UPDATE OF timezone ON event_operation_session
  FOR EACH ROW EXECUTE FUNCTION event_operation_validate_timezone();

CREATE TABLE IF NOT EXISTS event_invitation_security (
  event_invitation_id BIGINT PRIMARY KEY REFERENCES event_invitation(id) ON DELETE CASCADE,
  token_digest BYTEA NOT NULL UNIQUE,
  purpose TEXT NOT NULL DEFAULT 'event_collaboration',
  invited_scopes TEXT[] NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  consumed_at TIMESTAMPTZ NULL,
  revoked_at TIMESTAMPTZ NULL,
  status_version BIGINT NOT NULL DEFAULT 1,
  created_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_invitation_security_hash_check CHECK (octet_length(token_digest) = 32),
  CONSTRAINT event_invitation_security_purpose_check CHECK (purpose = 'event_collaboration'),
  CONSTRAINT event_invitation_security_scopes_check CHECK (cardinality(invited_scopes) > 0),
  CONSTRAINT event_invitation_security_expiry_check CHECK (expires_at > created_at),
  CONSTRAINT event_invitation_security_terminal_check CHECK (
    consumed_at IS NULL OR revoked_at IS NULL OR revoked_at >= consumed_at
  ),
  CONSTRAINT event_invitation_security_version_check CHECK (status_version > 0)
);

-- Acceptance history survives a later revocation. Upgrade installations whose
-- earlier mutually-exclusive terminal check rejected this documented lifecycle.
ALTER TABLE event_invitation_security
  DROP CONSTRAINT IF EXISTS event_invitation_security_terminal_check;
ALTER TABLE event_invitation_security
  ADD CONSTRAINT event_invitation_security_terminal_check CHECK (
    consumed_at IS NULL OR revoked_at IS NULL OR revoked_at >= consumed_at
  );

CREATE TABLE IF NOT EXISTS event_operation_task_policy (
  activity_id BIGINT PRIMARY KEY REFERENCES event_logistics_activity(id) ON DELETE CASCADE,
  requires_accountability BOOLEAN NOT NULL DEFAULT TRUE,
  dependencies_gate_completion BOOLEAN NOT NULL DEFAULT TRUE,
  version BIGINT NOT NULL DEFAULT 1,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_task_policy_version_check CHECK (version > 0)
);

CREATE TABLE IF NOT EXISTS event_operation_raci_assignment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  activity_id BIGINT NOT NULL REFERENCES event_logistics_activity(id) ON DELETE CASCADE,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  raci_role TEXT NOT NULL,
  assigned_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  valid_from TIMESTAMPTZ NOT NULL DEFAULT now(),
  valid_until TIMESTAMPTZ NULL,
  revoked_at TIMESTAMPTZ NULL,
  revoked_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  revocation_reason TEXT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT event_operation_raci_role_check CHECK (
    raci_role IN ('responsible', 'accountable', 'consulted', 'informed')
  ),
  CONSTRAINT event_operation_raci_window_check CHECK (
    valid_until IS NULL OR valid_until > valid_from
  ),
  CONSTRAINT event_operation_raci_revocation_check CHECK (
    (revoked_at IS NULL AND revoked_by_party_id IS NULL AND revocation_reason IS NULL)
    OR (revoked_at IS NOT NULL AND revoked_by_party_id IS NOT NULL
        AND NULLIF(btrim(revocation_reason), '') IS NOT NULL)
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS event_operation_raci_active_party_role_unique
  ON event_operation_raci_assignment (activity_id, party_id, raci_role)
  WHERE revoked_at IS NULL;
CREATE UNIQUE INDEX IF NOT EXISTS event_operation_raci_one_accountable
  ON event_operation_raci_assignment (activity_id)
  WHERE raci_role = 'accountable' AND revoked_at IS NULL;

CREATE TABLE IF NOT EXISTS event_operation_task_override (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  activity_id BIGINT NOT NULL REFERENCES event_logistics_activity(id) ON DELETE RESTRICT,
  activity_version INTEGER NOT NULL,
  override_kind TEXT NOT NULL,
  reason TEXT NOT NULL,
  policy_reference TEXT NOT NULL,
  authorized_by_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (activity_id, activity_version, override_kind),
  CONSTRAINT event_operation_task_override_kind_check CHECK (
    override_kind IN ('blocked_completion', 'responsibility_exception')
  ),
  CONSTRAINT event_operation_task_override_reason_check CHECK (
    NULLIF(btrim(reason), '') IS NOT NULL AND NULLIF(btrim(policy_reference), '') IS NOT NULL
  )
);

CREATE OR REPLACE FUNCTION event_operation_reject_history_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% is append-only', TG_TABLE_NAME USING ERRCODE = '55000';
END
$$;

DROP TRIGGER IF EXISTS event_operation_revision_immutable ON event_operation_revision;
CREATE TRIGGER event_operation_revision_immutable
  BEFORE UPDATE OR DELETE ON event_operation_revision
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();
DROP TRIGGER IF EXISTS event_operation_command_receipt_immutable ON event_operation_command_receipt;
CREATE TRIGGER event_operation_command_receipt_immutable
  BEFORE UPDATE OR DELETE ON event_operation_command_receipt
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();
DROP TRIGGER IF EXISTS event_operation_audit_immutable ON event_operation_audit_event;
CREATE TRIGGER event_operation_audit_immutable
  BEFORE UPDATE OR DELETE ON event_operation_audit_event
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();
DROP TRIGGER IF EXISTS event_operation_transition_immutable ON event_operation_transition;
CREATE TRIGGER event_operation_transition_immutable
  BEFORE UPDATE OR DELETE ON event_operation_transition
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();
DROP TRIGGER IF EXISTS event_operation_task_override_immutable ON event_operation_task_override;
CREATE TRIGGER event_operation_task_override_immutable
  BEFORE UPDATE OR DELETE ON event_operation_task_override
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();

-- Row triggers do not fire for TRUNCATE, including on empty history tables.
DO $$
DECLARE history_table TEXT;
BEGIN
  FOREACH history_table IN ARRAY ARRAY[
    'event_operation_revision', 'event_operation_command_receipt',
    'event_operation_audit_event', 'event_operation_transition',
    'event_operation_task_override'
  ] LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_history_no_truncate ON %I', history_table);
    EXECUTE format('CREATE TRIGGER event_operation_history_no_truncate BEFORE TRUNCATE ON %I FOR EACH STATEMENT EXECUTE FUNCTION event_operation_reject_history_mutation()', history_table);
  END LOOP;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_validate_raci()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_activity_id BIGINT := COALESCE(NEW.activity_id, OLD.activity_id);
  accountable_count INTEGER;
  responsible_count INTEGER;
  checked_at TIMESTAMPTZ := clock_timestamp();
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM event_operation_task_policy policy
    WHERE policy.activity_id = target_activity_id AND policy.requires_accountability
  ) THEN
    RETURN NULL;
  END IF;

  SELECT
    count(*) FILTER (WHERE assignment.raci_role = 'accountable' AND assignment.revoked_at IS NULL
      AND assignment.valid_from <= checked_at
      AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)),
    count(*) FILTER (WHERE assignment.raci_role = 'responsible' AND assignment.revoked_at IS NULL
      AND assignment.valid_from <= checked_at
      AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until))
  INTO accountable_count, responsible_count
  FROM event_operation_raci_assignment assignment
  WHERE assignment.activity_id = target_activity_id;

  IF accountable_count <> 1 THEN
    RAISE EXCEPTION 'actionable activity % requires exactly one Accountable party', target_activity_id
      USING ERRCODE = '23514';
  END IF;
  IF responsible_count < 1 THEN
    RAISE EXCEPTION 'actionable activity % requires at least one Responsible party', target_activity_id
      USING ERRCODE = '23514';
  END IF;
  RETURN NULL;
END
$$;

DROP TRIGGER IF EXISTS event_operation_raci_assignment_guard ON event_operation_raci_assignment;
CREATE CONSTRAINT TRIGGER event_operation_raci_assignment_guard
  AFTER INSERT OR UPDATE OR DELETE ON event_operation_raci_assignment
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION event_operation_validate_raci();
DROP TRIGGER IF EXISTS event_operation_task_policy_raci_guard ON event_operation_task_policy;
CREATE CONSTRAINT TRIGGER event_operation_task_policy_raci_guard
  AFTER INSERT OR UPDATE ON event_operation_task_policy
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION event_operation_validate_raci();

CREATE OR REPLACE FUNCTION event_operation_reject_dependency_cycle()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  activity_event_id BIGINT;
  dependency_event_id BIGINT;
BEGIN
  SELECT event_id INTO activity_event_id
  FROM event_logistics_activity WHERE id = NEW.activity_id;
  SELECT event_id INTO dependency_event_id
  FROM event_logistics_activity WHERE id = NEW.depends_on_activity_id;

  IF activity_event_id IS NULL OR dependency_event_id IS NULL
     OR activity_event_id <> dependency_event_id THEN
    RAISE EXCEPTION 'event logistics dependencies must remain within one event'
      USING ERRCODE = '23514';
  END IF;

  PERFORM pg_advisory_xact_lock(hashtextextended('event-operation-task-dag:' || activity_event_id::text, 0));

  IF NEW.activity_id = NEW.depends_on_activity_id OR EXISTS (
    WITH RECURSIVE reachable(id) AS (
      SELECT NEW.depends_on_activity_id
      UNION
      SELECT dependency.depends_on_activity_id
      FROM event_logistics_dependency dependency
      JOIN reachable ON dependency.activity_id = reachable.id
    )
    SELECT 1 FROM reachable WHERE id = NEW.activity_id
  ) THEN
    RAISE EXCEPTION 'event logistics dependency would create a cycle'
      USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS event_operation_dependency_cycle_guard ON event_logistics_dependency;
CREATE TRIGGER event_operation_dependency_cycle_guard
  BEFORE INSERT OR UPDATE OF activity_id, depends_on_activity_id
  ON event_logistics_dependency
  FOR EACH ROW EXECUTE FUNCTION event_operation_reject_dependency_cycle();

CREATE OR REPLACE FUNCTION event_operation_validate_raci_for_activity(target_activity_id BIGINT)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  accountable_count INTEGER;
  responsible_count INTEGER;
  checked_at TIMESTAMPTZ := clock_timestamp();
BEGIN
  SELECT
    count(*) FILTER (WHERE assignment.raci_role = 'accountable' AND assignment.revoked_at IS NULL
      AND assignment.valid_from <= checked_at
      AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)),
    count(*) FILTER (WHERE assignment.raci_role = 'responsible' AND assignment.revoked_at IS NULL
      AND assignment.valid_from <= checked_at
      AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until))
  INTO accountable_count, responsible_count
  FROM event_operation_raci_assignment assignment
  WHERE assignment.activity_id = target_activity_id;
  IF accountable_count <> 1 OR responsible_count < 1 THEN
    RAISE EXCEPTION 'activity % requires exactly one Accountable and at least one Responsible party', target_activity_id
      USING ERRCODE = '23514';
  END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_guard_task_completion()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  policy_row event_operation_task_policy%ROWTYPE;
BEGIN
  IF NEW.status <> 'completed' OR OLD.status = 'completed' THEN
    RETURN NEW;
  END IF;

  SELECT * INTO policy_row FROM event_operation_task_policy WHERE activity_id = NEW.id;
  IF NOT FOUND THEN
    RETURN NEW;
  END IF;

  IF policy_row.requires_accountability THEN
    PERFORM event_operation_validate_raci_for_activity(NEW.id);
  END IF;

  IF policy_row.dependencies_gate_completion AND EXISTS (
    SELECT 1
    FROM event_logistics_dependency dependency
    JOIN event_logistics_activity prerequisite
      ON prerequisite.id = dependency.depends_on_activity_id
    WHERE dependency.activity_id = NEW.id
      AND prerequisite.status <> 'completed'
  ) AND NOT EXISTS (
    SELECT 1 FROM event_operation_task_override override_record
    WHERE override_record.activity_id = NEW.id
      AND override_record.activity_version = OLD.version
      AND override_record.override_kind = 'blocked_completion'
  ) THEN
    RAISE EXCEPTION 'activity % has incomplete dependencies', NEW.id USING ERRCODE = '23514';
  END IF;

  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
CREATE TRIGGER event_operation_task_completion_guard
  BEFORE UPDATE OF status ON event_logistics_activity
  FOR EACH ROW EXECUTE FUNCTION event_operation_guard_task_completion();


-- Reuse the event-scoped write fence and deferred checks first implemented in
-- PR #339, commit a6cd9d1892ddafda06eab73a4e71389e802a5f7a, by
-- continuous-improvement-loop[bot]. Foundation-only installations need these
-- guards too: inserting relations after a completed task must not bypass them.
LOCK TABLE event_logistics_activity, event_logistics_dependency,
  event_operation_task_policy, event_operation_raci_assignment IN SHARE ROW EXCLUSIVE MODE;

-- Row triggers cannot validate pre-existing legacy edges. Inspect the entire
-- locked graph, including tasks which have not opted into completion policies.
-- UNION deduplicates reachable pairs so corrupt cycles terminate the scan.
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM event_logistics_dependency dependency
    LEFT JOIN event_logistics_activity activity ON activity.id = dependency.activity_id
    LEFT JOIN event_logistics_activity prerequisite ON prerequisite.id = dependency.depends_on_activity_id
    WHERE activity.event_id IS NULL OR prerequisite.event_id IS NULL
      OR activity.event_id <> prerequisite.event_id
  ) THEN
    RAISE EXCEPTION 'existing event logistics dependencies must remain within one event'
      USING ERRCODE = '23514';
  END IF;
  IF EXISTS (
    WITH RECURSIVE reachable(activity_id, prerequisite_id) AS (
      SELECT activity_id, depends_on_activity_id FROM event_logistics_dependency
      UNION
      SELECT reachable.activity_id, dependency.depends_on_activity_id
      FROM reachable
      JOIN event_logistics_dependency dependency ON dependency.activity_id = reachable.prerequisite_id
    )
    SELECT 1 FROM reachable WHERE activity_id = prerequisite_id
  ) THEN
    RAISE EXCEPTION 'existing event logistics dependency graph contains a cycle'
      USING ERRCODE = '23514';
  END IF;
END
$$;

-- A write, not just an advisory lock: stale RR/SERIALIZABLE snapshots must abort.
CREATE TABLE IF NOT EXISTS event_operation_task_write_fence (
  event_id BIGINT PRIMARY KEY REFERENCES social_event(id) ON DELETE CASCADE,
  revision BIGINT NOT NULL DEFAULT 1 CHECK (revision > 0)
);
-- The fence is synchronization metadata, not retained audit history. Upgrade
-- earlier opt-in installations too; existing event/task audit restrictions remain.
ALTER TABLE event_operation_task_write_fence
  DROP CONSTRAINT IF EXISTS event_operation_task_write_fence_event_id_fkey;
ALTER TABLE event_operation_task_write_fence
  ADD CONSTRAINT event_operation_task_write_fence_event_id_fkey
  FOREIGN KEY (event_id) REFERENCES social_event(id) ON DELETE CASCADE;

-- Shared by the legacy HTTP delete transaction and the direct row-delete guard.
-- Acquire the same write fence as policy/edge mutations before inspecting them.
CREATE OR REPLACE FUNCTION event_operation_assert_task_deletable(target_activity_id BIGINT)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE target_event_id BIGINT;
BEGIN
  SELECT event_id INTO target_event_id FROM event_logistics_activity WHERE id = target_activity_id;
  IF target_event_id IS NOT NULL THEN
    INSERT INTO event_operation_task_write_fence(event_id) VALUES (target_event_id)
      ON CONFLICT (event_id) DO UPDATE
        SET revision = event_operation_task_write_fence.revision + 1;
  END IF;
  IF EXISTS (SELECT 1 FROM event_operation_task_policy WHERE activity_id = target_activity_id)
    OR EXISTS (
      SELECT 1 FROM event_logistics_dependency dependency
      JOIN event_operation_task_policy policy ON policy.activity_id = dependency.activity_id
      WHERE dependency.depends_on_activity_id = target_activity_id
    ) THEN
    RAISE EXCEPTION 'protected tasks and their prerequisites require an audited archival workflow'
      USING ERRCODE = '23514';
  END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_task_write_lock()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_event_id BIGINT;
  target_activity_id BIGINT;
BEGIN
  IF TG_TABLE_NAME = 'event_logistics_activity' THEN
    IF TG_OP = 'UPDATE' AND (NEW.id <> OLD.id OR NEW.event_id <> OLD.event_id) THEN
      RAISE EXCEPTION 'task identity and event are immutable' USING ERRCODE = '23514';
    END IF;
    IF TG_OP = 'DELETE' THEN
      PERFORM event_operation_assert_task_deletable(OLD.id);
    END IF;
    target_event_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  ELSE
    IF TG_OP = 'UPDATE' AND NEW.activity_id <> OLD.activity_id THEN
      RAISE EXCEPTION 'task relation identity is immutable; replace explicitly' USING ERRCODE = '23514';
    END IF;
    IF TG_TABLE_NAME = 'event_operation_task_policy' THEN
      IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'task policy removal requires an audited workflow' USING ERRCODE = '23514';
      ELSIF TG_OP = 'UPDATE' THEN
        IF (OLD.requires_accountability AND NOT NEW.requires_accountability)
          OR (OLD.dependencies_gate_completion AND NOT NEW.dependencies_gate_completion) THEN
          RAISE EXCEPTION 'task policy weakening requires an audited workflow' USING ERRCODE = '23514';
        END IF;
      END IF;
    END IF;
    target_activity_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.activity_id ELSE NEW.activity_id END;
    SELECT event_id INTO target_event_id FROM event_logistics_activity WHERE id = target_activity_id;
  END IF;
  IF target_event_id IS NOT NULL THEN
    INSERT INTO event_operation_task_write_fence(event_id) VALUES (target_event_id)
      ON CONFLICT (event_id) DO UPDATE
        SET revision = event_operation_task_write_fence.revision + 1;
  END IF;
  IF TG_OP = 'DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_validate_task_event(target_event_id BIGINT)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  invalid_activity_id BIGINT;
  checked_at TIMESTAMPTZ := clock_timestamp();
BEGIN
  SELECT activity.id INTO invalid_activity_id
  FROM event_logistics_activity activity
  JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
  WHERE activity.event_id = target_event_id AND policy.requires_accountability
    AND (
      (SELECT count(*) FROM event_operation_raci_assignment assignment
       WHERE assignment.activity_id = activity.id AND assignment.revoked_at IS NULL
         AND assignment.valid_from <= checked_at
         AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)
         AND assignment.raci_role = 'accountable') <> 1
      OR NOT EXISTS (
        SELECT 1 FROM event_operation_raci_assignment assignment
        WHERE assignment.activity_id = activity.id AND assignment.revoked_at IS NULL
         AND assignment.valid_from <= checked_at
         AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)
          AND assignment.raci_role = 'responsible'
      )
    ) LIMIT 1;
  IF FOUND THEN
    RAISE EXCEPTION 'task % requires one Accountable and at least one Responsible', invalid_activity_id
      USING ERRCODE = '23514';
  END IF;

  SELECT activity.id INTO invalid_activity_id
  FROM event_logistics_activity activity
  JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
  WHERE activity.event_id = target_event_id AND activity.status = 'completed'
    AND policy.dependencies_gate_completion
    AND EXISTS (
      SELECT 1 FROM event_logistics_dependency dependency
      JOIN event_logistics_activity prerequisite ON prerequisite.id = dependency.depends_on_activity_id
      WHERE dependency.activity_id = activity.id AND prerequisite.status <> 'completed'
    )
    AND NOT EXISTS (
      SELECT 1 FROM event_operation_task_override override_record
      WHERE override_record.activity_id = activity.id
        AND override_record.activity_version = activity.version - 1
        AND override_record.override_kind = 'blocked_completion'
    ) LIMIT 1;
  IF FOUND THEN
    RAISE EXCEPTION 'completed task % has incomplete dependencies', invalid_activity_id
      USING ERRCODE = '23514';
  END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_task_commit_check()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_event_id BIGINT;
  target_activity_id BIGINT;
BEGIN
  IF TG_TABLE_NAME = 'event_logistics_activity' THEN
    target_event_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  ELSE
    target_activity_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.activity_id ELSE NEW.activity_id END;
    SELECT event_id INTO target_event_id FROM event_logistics_activity WHERE id = target_activity_id;
  END IF;
  -- An old completion exception covers its existing graph, not new blocked
  -- edges. Check the final state so valid same-transaction prerequisite completion
  -- still works, and ignore deleted edges or unchanged relation updates.
  IF TG_TABLE_NAME = 'event_logistics_dependency' AND TG_OP <> 'DELETE' THEN
    IF TG_OP = 'INSERT' OR NEW.depends_on_activity_id <> OLD.depends_on_activity_id THEN
      IF EXISTS (
        SELECT 1 FROM event_logistics_dependency dependency
        JOIN event_logistics_activity activity ON activity.id = dependency.activity_id
        JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
        JOIN event_logistics_activity prerequisite
          ON prerequisite.id = dependency.depends_on_activity_id
        WHERE dependency.id = NEW.id AND dependency.activity_id = NEW.activity_id
          AND dependency.depends_on_activity_id = NEW.depends_on_activity_id
          AND activity.status = 'completed' AND policy.dependencies_gate_completion
          AND prerequisite.status <> 'completed'
      ) THEN
        RAISE EXCEPTION 'completed task % cannot acquire an incomplete prerequisite', NEW.activity_id
          USING ERRCODE = '23514';
      END IF;
    END IF;
  END IF;
  PERFORM event_operation_validate_task_event(target_event_id);
  RETURN NULL;
END
$$;

DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
DO $$
DECLARE target_table TEXT;
BEGIN
  FOREACH target_table IN ARRAY ARRAY[
    'event_logistics_activity', 'event_logistics_dependency',
    'event_operation_task_policy', 'event_operation_raci_assignment'
  ] LOOP
    -- Names sort before the existing DAG guard. Constraint checks fire AFTER actual changes,
    -- never on the fence row, which would be too early with SET CONSTRAINTS ALL IMMEDIATE.
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_00_task_lock ON %I', target_table);
    EXECUTE format('CREATE TRIGGER event_operation_00_task_lock BEFORE INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION event_operation_task_write_lock()', target_table);
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON %I', target_table);
    EXECUTE format('CREATE CONSTRAINT TRIGGER event_operation_task_commit_guard
      AFTER INSERT OR UPDATE OR DELETE ON %I DEFERRABLE INITIALLY DEFERRED
      FOR EACH ROW EXECUTE FUNCTION event_operation_task_commit_check()', target_table);
  END LOOP;
  -- Refuse incompatible opt-in data. No automatic repairs or silent policy downgrades.
  PERFORM event_operation_validate_task_event(activity.event_id)
    FROM event_logistics_activity activity
    JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
    GROUP BY activity.event_id;
END
$$;

-- Expiration does not fabricate a revocation actor. An authorized caller must
-- explicitly retire expired rows and insert replacements in one transaction.
-- Keep both the old validity interval and attributed revocation evidence.
CREATE OR REPLACE FUNCTION event_operation_retire_expired_raci(
  target_activity_id BIGINT, actor_party_id BIGINT, retirement_reason TEXT
) RETURNS INTEGER LANGUAGE plpgsql SECURITY INVOKER AS $$
DECLARE retired_count INTEGER;
BEGIN
  IF actor_party_id IS NULL OR NULLIF(btrim(retirement_reason), '') IS NULL THEN
    RAISE EXCEPTION 'RACI retirement requires an actor and reason' USING ERRCODE = '23514';
  END IF;
  UPDATE event_operation_raci_assignment
  SET revoked_at = clock_timestamp(), revoked_by_party_id = actor_party_id,
      revocation_reason = retirement_reason
  WHERE activity_id = target_activity_id AND revoked_at IS NULL
    AND valid_until IS NOT NULL AND valid_until <= clock_timestamp();
  GET DIAGNOSTICS retired_count = ROW_COUNT;
  RETURN retired_count;
END
$$;
REVOKE ALL ON FUNCTION event_operation_retire_expired_raci(BIGINT, BIGINT, TEXT) FROM PUBLIC;

COMMIT;
