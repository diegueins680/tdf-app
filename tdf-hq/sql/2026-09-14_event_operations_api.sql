BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- This unmerged migration is not in the production manifest. Keep authorization
-- changes on the command's existing row-lock boundary, without changing business version.
ALTER TABLE event_operation_event_state
  ADD COLUMN IF NOT EXISTS authorization_version BIGINT NOT NULL DEFAULT 0
    CHECK (authorization_version >= 0);

CREATE OR REPLACE FUNCTION event_operation_fence_authorization_change()
RETURNS trigger
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE target_event_id BIGINT;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    IF NEW.id <> OLD.id OR NEW.event_id <> OLD.event_id THEN
      RAISE EXCEPTION 'authorization identity/event is immutable; revoke and issue explicitly'
        USING ERRCODE = '23514';
    END IF;
    IF TG_TABLE_NAME = 'event_operation_grant' THEN
      IF NEW.grantee_party_id <> OLD.grantee_party_id THEN
        RAISE EXCEPTION 'grant recipient is immutable; revoke and issue explicitly' USING ERRCODE = '23514';
      END IF;
    ELSIF NEW.party_id <> OLD.party_id THEN
      RAISE EXCEPTION 'relationship party is immutable; revoke and issue explicitly' USING ERRCODE = '23514';
    END IF;
  END IF;
  target_event_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  UPDATE event_operation_event_state
    SET authorization_version = authorization_version + 1
    WHERE event_id = target_event_id;
  IF TG_OP = 'DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
END
$$;

DROP TRIGGER IF EXISTS event_operation_authorization_fence ON event_operation_grant;
CREATE TRIGGER event_operation_authorization_fence
BEFORE INSERT OR UPDATE OR DELETE ON event_operation_grant
FOR EACH ROW EXECUTE FUNCTION event_operation_fence_authorization_change();
DROP TRIGGER IF EXISTS event_operation_authorization_fence ON event_operation_relationship;
CREATE TRIGGER event_operation_authorization_fence
BEFORE INSERT OR UPDATE OR DELETE ON event_operation_relationship
FOR EACH ROW EXECUTE FUNCTION event_operation_fence_authorization_change();

CREATE TABLE IF NOT EXISTS event_operation_feature_flag (
  feature_code TEXT PRIMARY KEY,
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  change_reason TEXT NULL,
  CONSTRAINT event_operation_feature_flag_code_check CHECK (
    feature_code = 'event.operations.api'
  ),
  CONSTRAINT event_operation_feature_flag_reason_check CHECK (
    enabled = FALSE OR NULLIF(btrim(change_reason), '') IS NOT NULL
  ),
  CONSTRAINT event_operation_feature_flag_actor_check CHECK (
    enabled = FALSE OR updated_by_party_id IS NOT NULL
  )
);

INSERT INTO event_operation_feature_flag(feature_code, enabled)
VALUES ('event.operations.api', FALSE)
ON CONFLICT (feature_code) DO NOTHING;

CREATE TABLE IF NOT EXISTS event_operation_feature_flag_history (
  id BIGSERIAL PRIMARY KEY,
  feature_code TEXT NOT NULL,
  enabled BOOLEAN NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  changed_by_party_id BIGINT NULL REFERENCES party(id) ON DELETE SET NULL,
  change_reason TEXT NULL,
  CONSTRAINT event_operation_feature_flag_history_code_check CHECK (
    feature_code = 'event.operations.api'
  ),
  CONSTRAINT event_operation_feature_flag_history_reason_check CHECK (
    enabled = FALSE OR NULLIF(btrim(change_reason), '') IS NOT NULL
  ),
  CONSTRAINT event_operation_feature_flag_history_actor_check CHECK (
    enabled = FALSE OR changed_by_party_id IS NOT NULL
  )
);

CREATE OR REPLACE FUNCTION event_operation_capture_feature_flag_history()
RETURNS trigger
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
BEGIN
  INSERT INTO event_operation_feature_flag_history(
    feature_code, enabled, changed_at, changed_by_party_id, change_reason
  ) VALUES (
    NEW.feature_code, NEW.enabled, NEW.updated_at,
    NEW.updated_by_party_id, NEW.change_reason
  );
  RETURN NEW;
END
$$;

DROP TRIGGER IF EXISTS event_operation_feature_flag_history_capture
  ON event_operation_feature_flag;
CREATE TRIGGER event_operation_feature_flag_history_capture
AFTER INSERT OR UPDATE OF enabled, updated_at, updated_by_party_id, change_reason
ON event_operation_feature_flag
FOR EACH ROW EXECUTE FUNCTION event_operation_capture_feature_flag_history();

DROP TRIGGER IF EXISTS event_operation_feature_flag_history_immutable
  ON event_operation_feature_flag_history;
CREATE TRIGGER event_operation_feature_flag_history_immutable
BEFORE UPDATE OR DELETE ON event_operation_feature_flag_history
FOR EACH ROW EXECUTE FUNCTION event_operation_reject_history_mutation();

INSERT INTO event_operation_feature_flag_history(
  feature_code, enabled, changed_at, changed_by_party_id, change_reason
)
SELECT
  flag.feature_code, flag.enabled, flag.updated_at,
  flag.updated_by_party_id, flag.change_reason
FROM event_operation_feature_flag flag
WHERE NOT EXISTS (
  SELECT 1 FROM event_operation_feature_flag_history history
  WHERE history.feature_code = flag.feature_code
);

CREATE TABLE IF NOT EXISTS event_operation_transition_capability (
  from_state TEXT NOT NULL,
  to_state TEXT NOT NULL,
  write_enabled BOOLEAN NOT NULL DEFAULT FALSE,
  rationale TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (from_state, to_state),
  FOREIGN KEY (from_state, to_state)
    REFERENCES event_operation_lifecycle_transition_policy(from_state, to_state),
  CONSTRAINT event_operation_transition_capability_rationale_check CHECK (
    NULLIF(btrim(rationale), '') IS NOT NULL
  )
);

INSERT INTO event_operation_transition_capability(
  from_state, to_state, write_enabled, rationale
)
SELECT
  policy.from_state,
  policy.to_state,
  (policy.from_state, policy.to_state) IN (
    ('draft', 'planning'),
    ('planning', 'pending_approval'),
    ('pending_approval', 'planning'),
    ('pending_approval', 'approved'),
    ('approved', 'planning')
  ),
  CASE
    WHEN (policy.from_state, policy.to_state) IN (
      ('draft', 'planning'),
      ('planning', 'pending_approval'),
      ('pending_approval', 'planning'),
      ('pending_approval', 'approved'),
      ('approved', 'planning')
    ) THEN 'phase-3 planning and independent approval transition has no public, booking, ticket, contract, or financial side effect'
    ELSE 'disabled until its synchronous guards and compensating/outbox effects are implemented and tested'
  END
FROM event_operation_lifecycle_transition_policy policy
ON CONFLICT (from_state, to_state) DO NOTHING;

CREATE UNIQUE INDEX IF NOT EXISTS event_operation_command_receipt_global_key
  ON event_operation_command_receipt(event_id, operation_code, command_id);

CREATE OR REPLACE FUNCTION event_operation_actor_capabilities(
  target_event_id BIGINT,
  target_actor_party_id BIGINT,
  evaluated_at TIMESTAMPTZ DEFAULT now()
)
RETURNS TABLE(scope_code TEXT)
LANGUAGE sql
STABLE
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
  SELECT owner_scope.scope_code
  FROM event_operation_relationship relationship
  CROSS JOIN LATERAL unnest(ARRAY[
    'event.read', 'event.manage', 'event.publish',
    'task.read', 'task.manage', 'booking.manage', 'contract.manage',
    'finance.read', 'audit.read'
  ]::TEXT[]) AS owner_scope(scope_code)
  WHERE relationship.event_id = target_event_id
    AND relationship.party_id = target_actor_party_id
    AND relationship.relationship_kind IN ('primary_owner', 'co_owner')
    AND relationship.revoked_at IS NULL
    AND relationship.valid_from <= evaluated_at
    AND (relationship.valid_until IS NULL OR relationship.valid_until > evaluated_at)
  UNION
  SELECT grant_record.scope_code
  FROM event_operation_grant grant_record
  WHERE grant_record.event_id = target_event_id
    AND grant_record.grantee_party_id = target_actor_party_id
    AND grant_record.resource_kind = 'event'
    AND grant_record.resource_id IS NULL
    AND grant_record.revoked_at IS NULL
    AND grant_record.valid_from <= evaluated_at
    AND (grant_record.valid_until IS NULL OR grant_record.valid_until > evaluated_at)
$$;

CREATE OR REPLACE FUNCTION event_operation_actor_can_read(
  target_event_id BIGINT,
  target_actor_party_id BIGINT,
  evaluated_at TIMESTAMPTZ DEFAULT now()
)
RETURNS BOOLEAN
LANGUAGE sql
STABLE
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
  SELECT EXISTS (
    SELECT 1
    FROM event_operation_actor_capabilities(
      target_event_id, target_actor_party_id, evaluated_at
    ) capability
    WHERE capability.scope_code IN (
      'event.read', 'event.manage', 'event.publish', 'event.approve',
      'task.read', 'task.manage', 'booking.manage', 'contract.manage',
      'finance.read', 'finance.approve', 'audit.read'
    )
  )
$$;

CREATE OR REPLACE FUNCTION event_operation_actor_has_authority(
  target_event_id BIGINT,
  target_actor_party_id BIGINT,
  required_authority_code TEXT,
  evaluated_at TIMESTAMPTZ DEFAULT now()
)
RETURNS BOOLEAN
LANGUAGE sql
STABLE
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
  SELECT CASE required_authority_code
    WHEN 'owner' THEN EXISTS (
      SELECT 1 FROM event_operation_actor_capabilities(
        target_event_id, target_actor_party_id, evaluated_at
      ) capability WHERE capability.scope_code = 'event.manage'
    )
    WHEN 'event_approver' THEN EXISTS (
      SELECT 1 FROM event_operation_actor_capabilities(
        target_event_id, target_actor_party_id, evaluated_at
      ) capability WHERE capability.scope_code = 'event.approve'
    )
    WHEN 'finance_approver' THEN EXISTS (
      SELECT 1 FROM event_operation_actor_capabilities(
        target_event_id, target_actor_party_id, evaluated_at
      ) capability WHERE capability.scope_code = 'finance.approve'
    )
    WHEN 'records_manager' THEN
      EXISTS (
        SELECT 1 FROM event_operation_actor_capabilities(
          target_event_id, target_actor_party_id, evaluated_at
        ) capability WHERE capability.scope_code = 'event.manage'
      )
      AND EXISTS (
        SELECT 1 FROM event_operation_actor_capabilities(
          target_event_id, target_actor_party_id, evaluated_at
        ) capability WHERE capability.scope_code = 'audit.read'
      )
    ELSE FALSE
  END
$$;

CREATE OR REPLACE FUNCTION event_operation_record_transition_rejection(
  target_event_id BIGINT,
  target_actor_party_id BIGINT,
  target_command_id UUID,
  request_hash BYTEA,
  requested_expected_version BIGINT,
  current_version BIGINT,
  current_state TEXT,
  error_code TEXT,
  error_outcome TEXT,
  rejection_reason TEXT,
  correlation_reference TEXT
)
RETURNS JSONB
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  response_payload JSONB;
BEGIN
  response_payload := jsonb_strip_nulls(jsonb_build_object(
    'error', error_code,
    'eventId', target_event_id,
    'canonicalState', current_state,
    'currentVersion', current_version,
    'commandId', target_command_id,
    'replayed', FALSE
  ));

  INSERT INTO event_operation_command_receipt(
    event_id, actor_party_id, operation_code, command_id, request_sha256,
    expected_version, result_version, outcome, response
  ) VALUES (
    target_event_id, target_actor_party_id, 'event.lifecycle.transition',
    target_command_id, request_hash, requested_expected_version, current_version,
    error_outcome, response_payload
  );

  INSERT INTO event_operation_audit_event(
    event_id, actor_party_id, actor_reference, operation_code, command_id,
    resource_kind, resource_id, outcome, reason, before_state, correlation_id
  ) VALUES (
    target_event_id, target_actor_party_id,
    'party:' || target_actor_party_id::TEXT,
    'event.lifecycle.transition', target_command_id,
    'event', target_event_id::TEXT, error_outcome, rejection_reason,
    CASE WHEN current_state IS NULL THEN NULL ELSE jsonb_build_object(
      'canonicalState', current_state, 'version', current_version
    ) END,
    correlation_reference
  );

  RETURN response_payload;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_apply_transition(
  target_event_id BIGINT,
  target_actor_party_id BIGINT,
  target_command_id UUID,
  requested_expected_version BIGINT,
  requested_target_state TEXT,
  requested_reason TEXT,
  correlation_reference TEXT,
  request_sha256_hex TEXT
)
RETURNS JSONB
LANGUAGE plpgsql
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  state_record event_operation_event_state%ROWTYPE;
  policy_record event_operation_lifecycle_transition_policy%ROWTYPE;
  prior_receipt event_operation_command_receipt%ROWTYPE;
  has_prior_receipt BOOLEAN;
  request_hash BYTEA;
  response_payload JSONB;
  review_requester BIGINT;
  effective_reason TEXT := NULLIF(btrim(requested_reason), '');
  effective_correlation TEXT := COALESCE(
    NULLIF(btrim(correlation_reference), ''),
    'command:' || target_command_id::TEXT
  );
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM event_operation_feature_flag flag
    WHERE flag.feature_code = 'event.operations.api' AND flag.enabled
  ) THEN
    RETURN jsonb_build_object('error', 'feature_disabled');
  END IF;

  IF request_sha256_hex !~ '^[0-9a-f]{64}$' THEN
    RETURN jsonb_build_object('error', 'invalid_request');
  END IF;
  request_hash := decode(request_sha256_hex, 'hex');

  SELECT * INTO state_record
  FROM event_operation_event_state
  WHERE event_id = target_event_id
  FOR UPDATE;
  IF NOT FOUND THEN
    RETURN jsonb_build_object('error', 'not_found');
  END IF;

  -- A lock wait must not retain a previously enabled feature decision.
  IF NOT EXISTS (
    SELECT 1 FROM event_operation_feature_flag flag
    WHERE flag.feature_code = 'event.operations.api' AND flag.enabled
  ) THEN
    RETURN jsonb_build_object('error', 'feature_disabled');
  END IF;

  SELECT * INTO prior_receipt
  FROM event_operation_command_receipt
  WHERE event_id = target_event_id
    AND operation_code = 'event.lifecycle.transition'
    AND command_id = target_command_id;
  has_prior_receipt := FOUND;

  -- Reauthorize before replay OR conflicting-key responses. Historical receipts
  -- confer no permission, and unreadable callers must not learn whether a key exists.
  -- Use the absent-target envelope for every unreadable path, including retries.
  -- Preserve private denial diagnostics without exposing event existence to callers.
  -- Wall clock is sampled after the event/authorization fence, never transaction now().
  IF NOT event_operation_actor_can_read(target_event_id, target_actor_party_id, clock_timestamp()) THEN
    IF has_prior_receipt THEN
      INSERT INTO event_operation_audit_event(
        event_id, actor_party_id, actor_reference, operation_code, command_id,
        resource_kind, resource_id, outcome, reason, correlation_id
      ) VALUES (
        target_event_id, target_actor_party_id, 'party:' || target_actor_party_id::TEXT,
        'event.lifecycle.transition', target_command_id, 'event', target_event_id::TEXT,
        'rejected', 'receipt access denied: no current event read authority', effective_correlation
      );
      RETURN jsonb_build_object('error', 'not_found');
    END IF;
    PERFORM event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, NULL, NULL, 'forbidden', 'rejected',
      'actor has no active event relationship or scoped grant', effective_correlation
    );
    RETURN jsonb_build_object('error', 'not_found');
  END IF;

  IF has_prior_receipt THEN
    IF prior_receipt.actor_party_id = target_actor_party_id
       AND prior_receipt.request_sha256 = request_hash THEN
      RETURN prior_receipt.response || jsonb_build_object('replayed', TRUE);
    END IF;
    INSERT INTO event_operation_audit_event(
      event_id, actor_party_id, actor_reference, operation_code, command_id,
      resource_kind, resource_id, outcome, reason, before_state, correlation_id
    ) VALUES (
      target_event_id, target_actor_party_id,
      'party:' || target_actor_party_id::TEXT,
      'event.lifecycle.transition', target_command_id,
      'event', target_event_id::TEXT, 'conflict',
      'command identifier was reused by a different actor or with different content',
      jsonb_build_object(
        'canonicalState', state_record.canonical_state,
        'version', state_record.version
      ),
      effective_correlation
    );
    RETURN jsonb_build_object(
      'error', 'idempotency_conflict',
      'eventId', target_event_id,
      'commandId', target_command_id
    );
  END IF;

  IF requested_expected_version < 1
     OR NULLIF(btrim(correlation_reference), '') IS NULL
     OR length(correlation_reference) > 200
     OR length(requested_target_state) > 64
     OR length(requested_reason) > 2000 THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'invalid_request', 'rejected', 'transition command fields are invalid',
      effective_correlation
    );
  END IF;

  IF requested_expected_version <> state_record.version THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'version_conflict', 'conflict', 'expected version does not match current version',
      effective_correlation
    );
  END IF;

  SELECT * INTO policy_record
  FROM event_operation_lifecycle_transition_policy
  WHERE from_state = state_record.canonical_state
    AND to_state = requested_target_state
    AND active;
  IF NOT FOUND THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'transition_invalid', 'rejected', 'transition is not in the canonical policy',
      effective_correlation
    );
  END IF;

  IF NOT event_operation_actor_has_authority(
    target_event_id, target_actor_party_id, policy_record.required_authority, clock_timestamp()
  ) THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'forbidden', 'rejected',
      'actor lacks the active scope required by the transition policy',
      effective_correlation
    );
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM event_operation_transition_capability capability
    WHERE capability.from_state = policy_record.from_state
      AND capability.to_state = policy_record.to_state
      AND capability.write_enabled
  ) THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'transition_effects_not_ready', 'rejected',
      'required cross-domain effects are not implemented and enabled',
      effective_correlation
    );
  END IF;

  IF requested_target_state IN ('planning')
     AND state_record.canonical_state IN ('pending_approval', 'approved')
     AND effective_reason IS NULL THEN
    RETURN event_operation_record_transition_rejection(
      target_event_id, target_actor_party_id, target_command_id, request_hash,
      requested_expected_version, state_record.version, state_record.canonical_state,
      'reason_required', 'rejected', 'rollback to planning requires a reason',
      effective_correlation
    );
  END IF;

  IF requested_target_state = 'approved' THEN
    SELECT transition.actor_party_id INTO review_requester
    FROM event_operation_transition transition
    WHERE transition.event_id = target_event_id
      AND transition.to_state = 'pending_approval'
    ORDER BY transition.created_at DESC, transition.id DESC
    LIMIT 1;
    IF review_requester IS NULL OR review_requester = target_actor_party_id THEN
      RETURN event_operation_record_transition_rejection(
        target_event_id, target_actor_party_id, target_command_id, request_hash,
        requested_expected_version, state_record.version, state_record.canonical_state,
        'separation_of_duties', 'rejected',
        'approval requires a different actor from the review requester',
        effective_correlation
      );
    END IF;
  END IF;

  UPDATE event_operation_event_state
  SET canonical_state = requested_target_state,
      version = version + 1,
      updated_at = now()
  WHERE event_id = target_event_id;

  response_payload := jsonb_build_object(
    'eventId', target_event_id,
    'canonicalState', requested_target_state,
    'version', state_record.version + 1,
    'commandId', target_command_id,
    'authorityCode', policy_record.required_authority,
    'replayed', FALSE
  );

  INSERT INTO event_operation_transition(
    event_id, command_id, actor_party_id, from_state, to_state,
    expected_version, result_version, authority_code, reason, effects
  ) VALUES (
    target_event_id, target_command_id, target_actor_party_id,
    state_record.canonical_state, requested_target_state,
    requested_expected_version, state_record.version + 1,
    policy_record.required_authority, effective_reason,
    jsonb_build_object(
      'publicVisibilityChanged', FALSE,
      'externalSideEffects', jsonb_build_array()
    )
  );

  INSERT INTO event_operation_audit_event(
    event_id, actor_party_id, actor_reference, operation_code, command_id,
    resource_kind, resource_id, outcome, reason, before_state, after_state,
    correlation_id
  ) VALUES (
    target_event_id, target_actor_party_id,
    'party:' || target_actor_party_id::TEXT,
    'event.lifecycle.transition', target_command_id,
    'event', target_event_id::TEXT, 'accepted', effective_reason,
    jsonb_build_object(
      'canonicalState', state_record.canonical_state, 'version', state_record.version
    ),
    jsonb_build_object(
      'canonicalState', requested_target_state, 'version', state_record.version + 1
    ),
    effective_correlation
  );

  INSERT INTO event_operation_command_receipt(
    event_id, actor_party_id, operation_code, command_id, request_sha256,
    expected_version, result_version, outcome, response
  ) VALUES (
    target_event_id, target_actor_party_id, 'event.lifecycle.transition',
    target_command_id, request_hash, requested_expected_version,
    state_record.version + 1, 'accepted', response_payload
  );

  RETURN response_payload;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_read_snapshot(
  target_event_id BIGINT,
  target_actor_party_id BIGINT
)
RETURNS JSONB
LANGUAGE plpgsql
VOLATILE
SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  state_record event_operation_event_state%ROWTYPE;
  checked_at TIMESTAMPTZ;
  snapshot JSONB;
BEGIN
  -- Shared readers coexist. A flag disable must serialize with snapshot decisions.
  PERFORM 1 FROM event_operation_feature_flag
    WHERE feature_code = 'event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;

  -- Do not evaluate permission in the locking SELECT's pre-wait snapshot.
  SELECT * INTO state_record FROM event_operation_event_state
    WHERE event_id = target_event_id FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;

  checked_at := clock_timestamp();
  IF NOT event_operation_actor_can_read(target_event_id, target_actor_party_id, checked_at) THEN
    RETURN NULL;
  END IF;

  SELECT jsonb_strip_nulls(jsonb_build_object(
    'eventId', target_event_id,
    'canonicalState', state_record.canonical_state,
    'version', state_record.version,
    'legacyStateCode', state_record.legacy_state_code,
    'capabilities', COALESCE((
      SELECT jsonb_agg(capability.scope_code ORDER BY capability.scope_code)
      FROM event_operation_actor_capabilities(target_event_id, target_actor_party_id, checked_at) capability
    ), '[]'::jsonb),
    'availableTransitions', COALESCE((
      SELECT jsonb_agg(policy.to_state ORDER BY policy.to_state)
      FROM event_operation_lifecycle_transition_policy policy
      JOIN event_operation_transition_capability capability
        ON capability.from_state = policy.from_state AND capability.to_state = policy.to_state
        AND capability.write_enabled
      WHERE policy.from_state = state_record.canonical_state AND policy.active
        AND event_operation_actor_has_authority(target_event_id, target_actor_party_id,
                                               policy.required_authority, checked_at)
        AND (policy.to_state <> 'approved' OR EXISTS (
          SELECT 1 FROM (
            SELECT transition.actor_party_id FROM event_operation_transition transition
            WHERE transition.event_id = target_event_id AND transition.to_state = 'pending_approval'
            ORDER BY transition.created_at DESC, transition.id DESC LIMIT 1
          ) requester WHERE requester.actor_party_id <> target_actor_party_id
        ))
    ), '[]'::jsonb)
  )) INTO snapshot;
  RETURN snapshot;
END
$$;

COMMIT;
