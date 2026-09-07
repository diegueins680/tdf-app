-- Staging-only contextual-reputation aggregation infrastructure.
--
-- The worker deliberately writes simulation candidates rather than the public
-- projection table. Product/Legal, moderation, consent, age-assurance and
-- public-read gates remain separate rollout requirements.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_worker_control (
  environment TEXT PRIMARY KEY
    CHECK (environment IN ('test', 'staging', 'production')),
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  simulation_only BOOLEAN NOT NULL DEFAULT TRUE CHECK (simulation_only),
  max_attempts SMALLINT NOT NULL DEFAULT 8 CHECK (max_attempts BETWEEN 1 AND 20),
  lease_seconds INTEGER NOT NULL DEFAULT 120 CHECK (lease_seconds BETWEEN 30 AND 900),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  CHECK (environment <> 'production' OR NOT enabled)
);

INSERT INTO reputation_worker_control(environment, enabled, simulation_only)
VALUES
  ('test', FALSE, TRUE),
  ('staging', FALSE, TRUE),
  ('production', FALSE, TRUE)
ON CONFLICT (environment) DO NOTHING;

CREATE UNIQUE INDEX IF NOT EXISTS reputation_worker_control_single_enabled_idx
  ON reputation_worker_control((enabled))
  WHERE enabled;

UPDATE reputation_formula_version
SET activated_at = COALESCE(activated_at, created_at)
WHERE status = 'active' AND activated_at IS NULL;

CREATE UNIQUE INDEX IF NOT EXISTS reputation_formula_version_single_active_idx
  ON reputation_formula_version((status))
  WHERE status = 'active';

CREATE OR REPLACE FUNCTION reputation_formula_version_guard()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF OLD.id IS DISTINCT FROM NEW.id
     OR OLD.created_at IS DISTINCT FROM NEW.created_at
     OR OLD.created_by_party_id IS DISTINCT FROM NEW.created_by_party_id THEN
    RAISE EXCEPTION 'Reputation formula identity and authorship are immutable';
  END IF;
  IF OLD.status <> 'draft' AND (
    OLD.public_parameters IS DISTINCT FROM NEW.public_parameters
    OR OLD.preference_parameters IS DISTINCT FROM NEW.preference_parameters
    OR OLD.activated_at IS DISTINCT FROM NEW.activated_at
  ) THEN
    RAISE EXCEPTION 'Activated reputation formula versions are immutable';
  END IF;
  IF OLD.status = 'active' AND NEW.status = 'draft' THEN
    RAISE EXCEPTION 'Activated reputation formula versions cannot return to draft';
  END IF;
  IF OLD.status = 'retired' AND NEW.status <> 'retired' THEN
    RAISE EXCEPTION 'Retired reputation formula versions cannot be reactivated';
  END IF;
  IF NEW.status = 'active' AND NEW.activated_at IS NULL THEN
    NEW.activated_at := now();
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_formula_version_guard
  ON reputation_formula_version;
CREATE TRIGGER trg_reputation_formula_version_guard
  BEFORE UPDATE ON reputation_formula_version
  FOR EACH ROW EXECUTE FUNCTION reputation_formula_version_guard();

CREATE TABLE IF NOT EXISTS reputation_aggregation_run (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  environment TEXT NOT NULL CHECK (environment IN ('test', 'staging')),
  run_kind TEXT NOT NULL CHECK (run_kind IN ('simulation', 'backfill', 'recalculation')),
  formula_version_id TEXT NOT NULL
    REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'planned'
    CHECK (status IN ('planned', 'running', 'succeeded', 'failed', 'cancelled')),
  high_water_mark TIMESTAMPTZ NOT NULL,
  requested_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  started_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK ((status = 'planned') = (started_at IS NULL)),
  CHECK ((status IN ('succeeded', 'failed', 'cancelled')) = (completed_at IS NOT NULL)),
  CHECK (completed_at IS NULL OR completed_at >= started_at)
);

CREATE OR REPLACE FUNCTION reputation_aggregation_run_guard()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF OLD.id IS DISTINCT FROM NEW.id
     OR OLD.environment IS DISTINCT FROM NEW.environment
     OR OLD.run_kind IS DISTINCT FROM NEW.run_kind
     OR OLD.formula_version_id IS DISTINCT FROM NEW.formula_version_id
     OR OLD.high_water_mark IS DISTINCT FROM NEW.high_water_mark
     OR OLD.requested_by_party_id IS DISTINCT FROM NEW.requested_by_party_id
     OR OLD.created_at IS DISTINCT FROM NEW.created_at THEN
    RAISE EXCEPTION 'Reputation aggregation run identity and high-water mark are immutable';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_aggregation_run_guard
  ON reputation_aggregation_run;
CREATE TRIGGER trg_reputation_aggregation_run_guard
  BEFORE UPDATE ON reputation_aggregation_run
  FOR EACH ROW EXECUTE FUNCTION reputation_aggregation_run_guard();

CREATE TABLE IF NOT EXISTS reputation_aggregation_outbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  parent_event_id UUID REFERENCES reputation_aggregation_outbox(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL CHECK (event_type IN (
    'evaluation.submitted',
    'evaluation.edited',
    'evaluation.invalidated',
    'evaluation.erased_or_anonymized',
    'signal.moderated',
    'appeal.provisional_opened',
    'appeal.resolved',
    'interaction.invalidated',
    'interaction.restored',
    'category.applicability_changed',
    'public_consent.changed',
    'pilot_consent.changed',
    'age_assurance.changed',
    'recalculation.requested'
  )),
  subject_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  context_key TEXT NOT NULL CHECK (
    length(context_key) BETWEEN 3 AND 256
    AND context_key LIKE '%:%'
    AND context_key !~ '[[:cntrl:]]'
  ),
  category_id UUID REFERENCES reputation_category(id) ON DELETE RESTRICT,
  source_version BIGINT NOT NULL DEFAULT 0 CHECK (source_version >= 0),
  algorithm_version TEXT NOT NULL REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  correlation_id UUID NOT NULL,
  run_id UUID REFERENCES reputation_aggregation_run(id) ON DELETE RESTRICT,
  occurred_at TIMESTAMPTZ NOT NULL,
  processing_status TEXT NOT NULL DEFAULT 'pending'
    CHECK (processing_status IN ('pending', 'processing', 'retry', 'processed', 'dead_letter')),
  attempt_count SMALLINT NOT NULL DEFAULT 0 CHECK (attempt_count BETWEEN 0 AND 20),
  available_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  lease_token UUID,
  lease_owner_hash TEXT CHECK (lease_owner_hash IS NULL OR lease_owner_hash ~ '^[0-9a-f]{64}$'),
  lease_expires_at TIMESTAMPTZ,
  processed_at TIMESTAMPTZ,
  last_error_code TEXT CHECK (
    last_error_code IS NULL OR last_error_code ~ '^[a-z0-9][a-z0-9_.-]{0,63}$'
  ),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(parent_event_id, subject_party_id, context_key, category_id, algorithm_version),
  CHECK (
    (processing_status = 'processing'
      AND lease_token IS NOT NULL
      AND lease_owner_hash IS NOT NULL
      AND lease_expires_at IS NOT NULL)
    OR
    (processing_status <> 'processing'
      AND lease_token IS NULL
      AND lease_owner_hash IS NULL
      AND lease_expires_at IS NULL)
  ),
  CHECK ((processing_status = 'processed') = (processed_at IS NOT NULL)),
  CHECK (processing_status <> 'dead_letter' OR last_error_code IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS reputation_aggregation_outbox_due_idx
  ON reputation_aggregation_outbox(available_at, occurred_at, id)
  WHERE processing_status IN ('pending', 'retry');
CREATE INDEX IF NOT EXISTS reputation_aggregation_outbox_expired_lease_idx
  ON reputation_aggregation_outbox(lease_expires_at, id)
  WHERE processing_status = 'processing';
CREATE INDEX IF NOT EXISTS reputation_aggregation_outbox_dlq_idx
  ON reputation_aggregation_outbox(updated_at, id)
  WHERE processing_status = 'dead_letter';
CREATE UNIQUE INDEX IF NOT EXISTS reputation_aggregation_outbox_run_source_idx
  ON reputation_aggregation_outbox(
    run_id, event_type, subject_party_id, context_key,
    COALESCE(category_id, '00000000-0000-0000-0000-000000000000'::uuid),
    source_version, algorithm_version
  )
  WHERE run_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS reputation_aggregate_candidate (
  subject_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  category_id UUID NOT NULL REFERENCES reputation_category(id) ON DELETE RESTRICT,
  context_key TEXT NOT NULL,
  formula_version_id TEXT NOT NULL REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  score NUMERIC(7,4) NOT NULL CHECK (score BETWEEN 0 AND 100),
  lower_bound NUMERIC(7,4) NOT NULL CHECK (lower_bound BETWEEN 0 AND 100),
  upper_bound NUMERIC(7,4) NOT NULL CHECK (upper_bound BETWEEN 0 AND 100),
  verified_interaction_count INTEGER NOT NULL CHECK (verified_interaction_count >= 0),
  distinct_evaluator_count INTEGER NOT NULL CHECK (distinct_evaluator_count >= 0),
  observation_count INTEGER NOT NULL CHECK (observation_count >= 0),
  confidence TEXT NOT NULL CHECK (confidence IN ('forming', 'low', 'moderate', 'high')),
  publication_state TEXT NOT NULL DEFAULT 'simulation'
    CHECK (publication_state = 'simulation'),
  source_event_id UUID NOT NULL REFERENCES reputation_aggregation_outbox(id) ON DELETE RESTRICT,
  calculated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY(subject_party_id, category_id, context_key, formula_version_id),
  CHECK (lower_bound <= score AND score <= upper_bound)
);

CREATE TABLE IF NOT EXISTS reputation_aggregation_event_action (
  id BIGSERIAL PRIMARY KEY,
  event_id UUID NOT NULL REFERENCES reputation_aggregation_outbox(id) ON DELETE RESTRICT,
  action TEXT NOT NULL CHECK (action IN (
    'claimed', 'processed', 'fan_out', 'retry_scheduled', 'dead_lettered', 'requeued'
  )),
  attempt_count SMALLINT NOT NULL CHECK (attempt_count BETWEEN 0 AND 20),
  worker_hash TEXT CHECK (worker_hash IS NULL OR worker_hash ~ '^[0-9a-f]{64}$'),
  actor_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  reason TEXT CHECK (
    reason IS NULL OR (
      length(btrim(reason)) BETWEEN 8 AND 500
      AND reason !~ '[[:cntrl:]]'
    )
  ),
  error_code TEXT CHECK (
    error_code IS NULL OR error_code ~ '^[a-z0-9][a-z0-9_.-]{0,63}$'
  ),
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb CHECK (jsonb_typeof(metadata) = 'object'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS reputation_aggregation_event_action_event_idx
  ON reputation_aggregation_event_action(event_id, created_at, id);

CREATE OR REPLACE FUNCTION reputation_reject_event_action_mutation()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  RAISE EXCEPTION 'Reputation aggregation event actions are immutable';
END $$;

DROP TRIGGER IF EXISTS trg_reputation_event_action_immutable
  ON reputation_aggregation_event_action;
CREATE TRIGGER trg_reputation_event_action_immutable
  BEFORE UPDATE OR DELETE ON reputation_aggregation_event_action
  FOR EACH ROW EXECUTE FUNCTION reputation_reject_event_action_mutation();

CREATE OR REPLACE FUNCTION reputation_validate_outbox_transition()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF OLD.id IS DISTINCT FROM NEW.id
     OR OLD.parent_event_id IS DISTINCT FROM NEW.parent_event_id
     OR OLD.event_type IS DISTINCT FROM NEW.event_type
     OR OLD.subject_party_id IS DISTINCT FROM NEW.subject_party_id
     OR OLD.context_key IS DISTINCT FROM NEW.context_key
     OR OLD.category_id IS DISTINCT FROM NEW.category_id
     OR OLD.source_version IS DISTINCT FROM NEW.source_version
     OR OLD.algorithm_version IS DISTINCT FROM NEW.algorithm_version
     OR OLD.correlation_id IS DISTINCT FROM NEW.correlation_id
     OR OLD.run_id IS DISTINCT FROM NEW.run_id
     OR OLD.occurred_at IS DISTINCT FROM NEW.occurred_at
     OR OLD.created_at IS DISTINCT FROM NEW.created_at THEN
    RAISE EXCEPTION 'Reputation event evidence is immutable';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.processing_status <> NEW.processing_status THEN
    IF NOT (
      (OLD.processing_status IN ('pending', 'retry', 'processing') AND NEW.processing_status = 'processing')
      OR (OLD.processing_status = 'processing' AND NEW.processing_status IN ('processed', 'retry', 'dead_letter'))
      OR (OLD.processing_status = 'dead_letter' AND NEW.processing_status = 'retry')
    ) THEN
      RAISE EXCEPTION 'Invalid reputation event transition % -> %',
        OLD.processing_status, NEW.processing_status;
    END IF;
  END IF;
  NEW.updated_at := now();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_outbox_transition
  ON reputation_aggregation_outbox;
CREATE TRIGGER trg_reputation_outbox_transition
  BEFORE UPDATE ON reputation_aggregation_outbox
  FOR EACH ROW EXECUTE FUNCTION reputation_validate_outbox_transition();

CREATE OR REPLACE FUNCTION reputation_enqueue_aggregation_event(
  p_event_id UUID,
  p_event_type TEXT,
  p_subject_party_id BIGINT,
  p_context_key TEXT,
  p_category_id UUID,
  p_source_version BIGINT,
  p_algorithm_version TEXT,
  p_correlation_id UUID,
  p_run_id UUID,
  p_occurred_at TIMESTAMPTZ
)
RETURNS UUID
LANGUAGE plpgsql
AS $$
DECLARE
  existing reputation_aggregation_outbox%ROWTYPE;
BEGIN
  INSERT INTO reputation_aggregation_outbox(
    id, event_type, subject_party_id, context_key, category_id,
    source_version, algorithm_version, correlation_id, run_id, occurred_at,
    available_at
  ) VALUES (
    p_event_id, p_event_type, p_subject_party_id, btrim(p_context_key), p_category_id,
    p_source_version, p_algorithm_version, p_correlation_id, p_run_id, p_occurred_at,
    p_occurred_at
  )
  ON CONFLICT DO NOTHING;

  SELECT * INTO existing
  FROM reputation_aggregation_outbox
  WHERE id = p_event_id
     OR (
       p_run_id IS NOT NULL
       AND run_id = p_run_id
       AND event_type = p_event_type
       AND subject_party_id = p_subject_party_id
       AND context_key = btrim(p_context_key)
       AND category_id IS NOT DISTINCT FROM p_category_id
       AND source_version = p_source_version
       AND algorithm_version = p_algorithm_version
     )
  ORDER BY (id = p_event_id) DESC
  LIMIT 1;

  IF existing.event_type IS DISTINCT FROM p_event_type
     OR existing.subject_party_id IS DISTINCT FROM p_subject_party_id
     OR existing.context_key IS DISTINCT FROM btrim(p_context_key)
     OR existing.category_id IS DISTINCT FROM p_category_id
     OR existing.source_version IS DISTINCT FROM p_source_version
     OR existing.algorithm_version IS DISTINCT FROM p_algorithm_version
     OR existing.correlation_id IS DISTINCT FROM p_correlation_id
     OR existing.run_id IS DISTINCT FROM p_run_id
     OR existing.occurred_at IS DISTINCT FROM p_occurred_at THEN
    RAISE EXCEPTION 'Reputation event ID conflicts with different immutable evidence';
  END IF;

  RETURN existing.id;
END $$;

CREATE OR REPLACE FUNCTION reputation_schedule_decay_recalculations(
  p_environment TEXT,
  p_batch_size INTEGER,
  p_now TIMESTAMPTZ
)
RETURNS INTEGER
LANGUAGE plpgsql
AS $$
DECLARE
  schedule_bucket TIMESTAMPTZ;
  schedule_bucket_key TEXT;
  scheduled_count INTEGER := 0;
  target RECORD;
BEGIN
  IF p_environment NOT IN ('test', 'staging') THEN
    RAISE EXCEPTION 'Reputation aggregation worker is staging-only';
  END IF;
  IF p_batch_size NOT BETWEEN 1 AND 100 THEN
    RAISE EXCEPTION 'Reputation decay schedule batch size is invalid';
  END IF;
  IF NOT EXISTS (
    SELECT 1
    FROM reputation_worker_control control
    WHERE control.environment = p_environment
      AND control.enabled
      AND control.simulation_only
  ) THEN
    RETURN 0;
  END IF;

  schedule_bucket := date_trunc('day', p_now AT TIME ZONE 'UTC') AT TIME ZONE 'UTC';
  schedule_bucket_key := to_char(
    schedule_bucket AT TIME ZONE 'UTC',
    'YYYY-MM-DD"T"HH24:MI:SS"Z"'
  );
  FOR target IN
    SELECT candidate.subject_party_id, candidate.category_id,
           candidate.context_key, candidate.formula_version_id
    FROM reputation_aggregate_candidate candidate
    WHERE candidate.publication_state = 'simulation'
      AND candidate.calculated_at < schedule_bucket
      AND NOT EXISTS (
        SELECT 1
        FROM reputation_aggregation_outbox existing
        WHERE existing.id = md5(
          'reputation-decay:' || p_environment || ':' ||
          candidate.subject_party_id::text || ':' || candidate.category_id::text || ':' ||
          candidate.context_key || ':' || candidate.formula_version_id || ':' ||
          schedule_bucket_key
        )::uuid
      )
    ORDER BY candidate.calculated_at, candidate.subject_party_id,
             candidate.category_id, candidate.context_key,
             candidate.formula_version_id
    LIMIT p_batch_size
  LOOP
    PERFORM reputation_enqueue_aggregation_event(
      md5(
        'reputation-decay:' || p_environment || ':' ||
        target.subject_party_id::text || ':' || target.category_id::text || ':' ||
        target.context_key || ':' || target.formula_version_id || ':' ||
        schedule_bucket_key
      )::uuid,
      'recalculation.requested',
      target.subject_party_id,
      target.context_key,
      target.category_id,
      extract(epoch FROM schedule_bucket)::bigint,
      target.formula_version_id,
      md5(
        'reputation-decay-correlation:' || p_environment || ':' || schedule_bucket_key
      )::uuid,
      NULL,
      schedule_bucket
    );
    scheduled_count := scheduled_count + 1;
  END LOOP;
  RETURN scheduled_count;
END $$;

CREATE OR REPLACE FUNCTION reputation_emit_evaluation_events(
  p_evaluation_id UUID,
  p_event_type TEXT,
  p_occurred_at TIMESTAMPTZ
)
RETURNS INTEGER
LANGUAGE plpgsql
AS $$
DECLARE
  evaluation_row RECORD;
  correlation UUID := gen_random_uuid();
  emitted INTEGER := 0;
  target RECORD;
BEGIN
  SELECT evaluation.id, evaluation.subject_party_id, evaluation.revision,
         evaluation.formula_version_id, interaction.context_kind, interaction.context_id
    INTO evaluation_row
  FROM reputation_evaluation evaluation
  JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
  WHERE evaluation.id = p_evaluation_id;

  IF NOT FOUND THEN
    RETURN 0;
  END IF;

  FOR target IN
    WITH subjects AS (
      SELECT evaluation_row.subject_party_id AS subject_party_id
      UNION
      SELECT rank.compared_party_id
      FROM reputation_evaluation_rank rank
      WHERE rank.evaluation_id = p_evaluation_id
    ), categories AS (
      SELECT item.category_id
      FROM reputation_evaluation_category item
      WHERE item.evaluation_id = p_evaluation_id
      UNION
      SELECT rank.category_id
      FROM reputation_evaluation_rank rank
      WHERE rank.evaluation_id = p_evaluation_id
    )
    SELECT subjects.subject_party_id, categories.category_id
    FROM subjects CROSS JOIN categories
    ORDER BY subjects.subject_party_id, categories.category_id
  LOOP
    PERFORM reputation_enqueue_aggregation_event(
      gen_random_uuid(), p_event_type, target.subject_party_id,
      lower(evaluation_row.context_kind) || ':' || evaluation_row.context_id,
      target.category_id, evaluation_row.revision,
      evaluation_row.formula_version_id, correlation, NULL, p_occurred_at
    );
    emitted := emitted + 1;
  END LOOP;

  RETURN emitted;
END $$;

CREATE OR REPLACE FUNCTION reputation_evaluation_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF TG_OP = 'INSERT' AND NEW.status = 'submitted' THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.submitted', now());
  ELSIF TG_OP = 'UPDATE'
     AND NEW.status = 'submitted'
     AND OLD.status = 'draft' THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.submitted', now());
  ELSIF TG_OP = 'UPDATE'
     AND NEW.status = 'submitted'
     AND OLD.status NOT IN ('draft', 'submitted') THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.edited', now());
  ELSIF TG_OP = 'UPDATE'
     AND OLD.status = 'submitted'
     AND NEW.status IN ('under_review', 'void') THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.invalidated', now());
  ELSIF TG_OP = 'UPDATE'
     AND OLD.status = 'submitted'
     AND NEW.status = 'submitted'
     AND OLD.revision IS DISTINCT FROM NEW.revision THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.edited', now());
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION reputation_evaluation_delete_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  PERFORM reputation_emit_evaluation_events(
    OLD.id, 'evaluation.erased_or_anonymized', now()
  );
  RETURN OLD;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_evaluation_delete_outbox
  ON reputation_evaluation;
CREATE TRIGGER trg_reputation_evaluation_delete_outbox
  BEFORE DELETE ON reputation_evaluation
  FOR EACH ROW EXECUTE FUNCTION reputation_evaluation_delete_outbox_trigger();

DROP TRIGGER IF EXISTS trg_reputation_evaluation_outbox ON reputation_evaluation;
CREATE TRIGGER trg_reputation_evaluation_outbox
  AFTER INSERT OR UPDATE ON reputation_evaluation
  FOR EACH ROW EXECUTE FUNCTION reputation_evaluation_outbox_trigger();

CREATE OR REPLACE FUNCTION reputation_rank_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  target_evaluation_id UUID;
  evaluation_row RECORD;
BEGIN
  IF TG_OP = 'DELETE' THEN
    target_evaluation_id := OLD.evaluation_id;
  ELSE
    target_evaluation_id := NEW.evaluation_id;
  END IF;

  IF EXISTS (
    SELECT 1 FROM reputation_evaluation
    WHERE id = target_evaluation_id AND status = 'submitted'
  ) THEN
    PERFORM reputation_emit_evaluation_events(
      target_evaluation_id, 'evaluation.edited', now()
    );

    IF TG_OP IN ('UPDATE', 'DELETE') THEN
      SELECT evaluation.revision, evaluation.formula_version_id,
             interaction.context_kind, interaction.context_id
        INTO evaluation_row
      FROM reputation_evaluation evaluation
      JOIN reputation_interaction interaction
        ON interaction.id = evaluation.interaction_id
      WHERE evaluation.id = target_evaluation_id;

      PERFORM reputation_enqueue_aggregation_event(
        gen_random_uuid(), 'evaluation.edited', OLD.compared_party_id,
        lower(evaluation_row.context_kind) || ':' || evaluation_row.context_id,
        OLD.category_id, evaluation_row.revision, evaluation_row.formula_version_id,
        gen_random_uuid(), NULL, now()
      );
    END IF;
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_rank_outbox ON reputation_evaluation_rank;
CREATE TRIGGER trg_reputation_rank_outbox
  AFTER INSERT OR UPDATE OR DELETE ON reputation_evaluation_rank
  FOR EACH ROW EXECUTE FUNCTION reputation_rank_outbox_trigger();

CREATE OR REPLACE FUNCTION reputation_interaction_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  evaluation_id UUID;
BEGIN
  IF OLD.status = 'eligible' AND NEW.status IN ('disputed', 'void', 'expired') THEN
    FOR evaluation_id IN
      SELECT evaluation.id
      FROM reputation_evaluation evaluation
      WHERE evaluation.interaction_id = NEW.id
        AND evaluation.status IN ('submitted', 'under_review', 'void')
      ORDER BY evaluation.id
    LOOP
      PERFORM reputation_emit_evaluation_events(
        evaluation_id, 'interaction.invalidated', now()
      );
    END LOOP;
  ELSIF OLD.status <> 'eligible' AND NEW.status = 'eligible' THEN
    FOR evaluation_id IN
      SELECT evaluation.id
      FROM reputation_evaluation evaluation
      WHERE evaluation.interaction_id = NEW.id
        AND evaluation.status = 'submitted'
      ORDER BY evaluation.id
    LOOP
      PERFORM reputation_emit_evaluation_events(
        evaluation_id, 'interaction.restored', now()
      );
    END LOOP;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_interaction_outbox ON reputation_interaction;
CREATE TRIGGER trg_reputation_interaction_outbox
  AFTER UPDATE OF status ON reputation_interaction
  FOR EACH ROW EXECUTE FUNCTION reputation_interaction_outbox_trigger();

CREATE OR REPLACE FUNCTION reputation_category_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  target RECORD;
  correlation UUID := gen_random_uuid();
BEGIN
  IF OLD.status IS NOT DISTINCT FROM NEW.status
     AND OLD.merged_into_id IS NOT DISTINCT FROM NEW.merged_into_id
     AND OLD.applicable_roles IS NOT DISTINCT FROM NEW.applicable_roles
     AND OLD.applicable_contexts IS NOT DISTINCT FROM NEW.applicable_contexts
     AND OLD.version IS NOT DISTINCT FROM NEW.version THEN
    RETURN NEW;
  END IF;

  FOR target IN
    SELECT DISTINCT candidate.subject_party_id, candidate.context_key,
           candidate.formula_version_id
    FROM reputation_aggregate_candidate candidate
    WHERE candidate.category_id = NEW.id
    UNION
    SELECT DISTINCT rank.compared_party_id,
           lower(interaction.context_kind) || ':' || interaction.context_id,
           evaluation.formula_version_id
    FROM reputation_evaluation_rank rank
    JOIN reputation_evaluation evaluation ON evaluation.id = rank.evaluation_id
    JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
    WHERE rank.category_id = NEW.id
  LOOP
    PERFORM reputation_enqueue_aggregation_event(
      gen_random_uuid(), 'category.applicability_changed', target.subject_party_id,
      target.context_key, NEW.id, NEW.version, target.formula_version_id,
      correlation, NULL, now()
    );
  END LOOP;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_category_outbox ON reputation_category;
CREATE TRIGGER trg_reputation_category_outbox
  AFTER UPDATE OF status, merged_into_id, applicable_roles, applicable_contexts, version
  ON reputation_category
  FOR EACH ROW EXECUTE FUNCTION reputation_category_outbox_trigger();

CREATE OR REPLACE FUNCTION reputation_claim_aggregation_events(
  p_environment TEXT,
  p_worker_id TEXT,
  p_batch_size INTEGER,
  p_now TIMESTAMPTZ
)
RETURNS TABLE(event_id TEXT, claim_token TEXT, claimed_attempt INTEGER)
LANGUAGE plpgsql
AS $$
DECLARE
  control reputation_worker_control%ROWTYPE;
  queued RECORD;
  token UUID;
  worker_hash TEXT;
BEGIN
  IF p_environment NOT IN ('test', 'staging') THEN
    RAISE EXCEPTION 'Reputation aggregation worker is staging-only';
  END IF;
  IF length(btrim(p_worker_id)) NOT BETWEEN 8 AND 200
     OR p_worker_id ~ '[[:cntrl:]]' THEN
    RAISE EXCEPTION 'Reputation worker ID is invalid';
  END IF;
  IF p_batch_size NOT BETWEEN 1 AND 100 THEN
    RAISE EXCEPTION 'Reputation worker batch size is invalid';
  END IF;

  SELECT * INTO control
  FROM reputation_worker_control
  WHERE environment = p_environment
  FOR SHARE;
  IF NOT FOUND OR NOT control.enabled OR NOT control.simulation_only THEN
    RETURN;
  END IF;

  worker_hash := encode(digest(btrim(p_worker_id), 'sha256'), 'hex');
  FOR queued IN
    SELECT event.id, event.processing_status, event.attempt_count,
           event.lease_owner_hash
    FROM reputation_aggregation_outbox event
    WHERE (
      (
        event.processing_status IN ('pending', 'retry')
        AND event.available_at <= p_now
      ) OR (
        event.processing_status = 'processing'
        AND event.lease_expires_at <= p_now
      )
    )
      AND (
        event.run_id IS NULL
        OR EXISTS (
          SELECT 1
          FROM reputation_aggregation_run run
          WHERE run.id = event.run_id
            AND run.environment = p_environment
        )
      )
    ORDER BY event.available_at, event.occurred_at, event.id
    FOR UPDATE SKIP LOCKED
    LIMIT p_batch_size
  LOOP
    IF queued.processing_status = 'processing'
       AND queued.attempt_count >= control.max_attempts THEN
      UPDATE reputation_aggregation_outbox
      SET processing_status = 'dead_letter',
          lease_token = NULL, lease_owner_hash = NULL, lease_expires_at = NULL,
          processed_at = NULL, last_error_code = 'lease_expired_at_attempt_limit'
      WHERE id = queued.id;
      INSERT INTO reputation_aggregation_event_action(
        event_id, action, attempt_count, worker_hash, error_code, created_at
      ) VALUES (
        queued.id, 'dead_lettered', queued.attempt_count,
        queued.lease_owner_hash, 'lease_expired_at_attempt_limit', p_now
      );
      CONTINUE;
    END IF;

    token := gen_random_uuid();
    UPDATE reputation_aggregation_outbox
    SET processing_status = 'processing',
        attempt_count = attempt_count + 1,
        lease_token = token,
        lease_owner_hash = worker_hash,
        lease_expires_at = p_now + make_interval(secs => control.lease_seconds),
        processed_at = NULL,
        last_error_code = NULL
    WHERE id = queued.id;

    INSERT INTO reputation_aggregation_event_action(
      event_id, action, attempt_count, worker_hash, created_at
    )
    SELECT id, 'claimed', attempt_count, worker_hash, p_now
    FROM reputation_aggregation_outbox
    WHERE id = queued.id;

    event_id := queued.id::text;
    claim_token := token::text;
    SELECT attempt_count INTO claimed_attempt
    FROM reputation_aggregation_outbox
    WHERE id = queued.id;
    RETURN NEXT;
  END LOOP;
END $$;

CREATE OR REPLACE FUNCTION reputation_complete_aggregation_event(
  p_event_id UUID,
  p_claim_token UUID,
  p_now TIMESTAMPTZ
)
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  event_row reputation_aggregation_outbox%ROWTYPE;
  formula_row reputation_formula_version%ROWTYPE;
  prior_strength NUMERIC;
  prior_mean NUMERIC;
  half_life_days NUMERIC;
  evaluator_cap NUMERIC;
  prior_probability NUMERIC;
  run_high_water_mark TIMESTAMPTZ;
  evidence_reference_time TIMESTAMPTZ;
  iteration_index INTEGER;
  cap_iteration_index INTEGER;
  evaluator_total_count INTEGER := 0;
  effective_evidence_weight NUMERIC := 0;
  max_evaluator_share NUMERIC := 0;
  component_count INTEGER := 0;
  interaction_count INTEGER := 0;
  evaluator_count INTEGER := 0;
  observation_count INTEGER := 0;
  confidence_value TEXT;
  fanout_count INTEGER := 0;
BEGIN
  SELECT * INTO event_row
  FROM reputation_aggregation_outbox
  WHERE id = p_event_id
  FOR UPDATE;

  IF NOT FOUND
     OR event_row.processing_status <> 'processing'
     OR event_row.lease_token IS DISTINCT FROM p_claim_token
     OR event_row.lease_expires_at < p_now THEN
    RAISE EXCEPTION 'Reputation event lease is unavailable';
  END IF;

  IF event_row.run_id IS NOT NULL THEN
    SELECT run.high_water_mark
      INTO run_high_water_mark
    FROM reputation_aggregation_run run
    WHERE run.id = event_row.run_id
      AND run.formula_version_id = event_row.algorithm_version
    FOR SHARE;
    IF NOT FOUND THEN
      RAISE EXCEPTION 'Reputation aggregation run is unavailable or uses another formula';
    END IF;
  END IF;
  evidence_reference_time := COALESCE(run_high_water_mark, p_now);

  IF run_high_water_mark IS NOT NULL AND EXISTS (
    SELECT 1
    FROM reputation_aggregation_outbox mutation
    WHERE mutation.run_id IS NULL
      AND mutation.algorithm_version = event_row.algorithm_version
      AND mutation.context_key = event_row.context_key
      AND mutation.occurred_at > run_high_water_mark
      AND (
        event_row.category_id IS NULL
        OR mutation.category_id IS NULL
        OR mutation.category_id = event_row.category_id
      )
      AND mutation.event_type IN (
        'evaluation.edited',
        'evaluation.invalidated',
        'evaluation.erased_or_anonymized',
        'signal.moderated',
        'appeal.provisional_opened',
        'appeal.resolved',
        'interaction.invalidated',
        'interaction.restored',
        'category.applicability_changed'
      )
  ) THEN
    RAISE EXCEPTION 'Reputation aggregation run source changed after its high-water mark';
  END IF;

  IF event_row.category_id IS NULL THEN
    INSERT INTO reputation_aggregation_outbox(
      parent_event_id, event_type, subject_party_id, context_key, category_id,
      source_version, algorithm_version, correlation_id, run_id, occurred_at
    )
    SELECT event_row.id, event_row.event_type, event_row.subject_party_id,
           event_row.context_key, categories.category_id, event_row.source_version,
           event_row.algorithm_version, event_row.correlation_id,
           event_row.run_id, event_row.occurred_at
    FROM (
      SELECT candidate.category_id
      FROM reputation_aggregate_candidate candidate
      WHERE candidate.subject_party_id = event_row.subject_party_id
        AND candidate.context_key = event_row.context_key
        AND candidate.formula_version_id = event_row.algorithm_version
      UNION
      SELECT rank.category_id
      FROM reputation_evaluation_rank rank
      JOIN reputation_evaluation evaluation ON evaluation.id = rank.evaluation_id
      JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
      WHERE rank.compared_party_id = event_row.subject_party_id
        AND lower(interaction.context_kind) || ':' || interaction.context_id = event_row.context_key
    ) categories
    ON CONFLICT (parent_event_id, subject_party_id, context_key, category_id, algorithm_version)
    DO NOTHING;
    GET DIAGNOSTICS fanout_count = ROW_COUNT;

    UPDATE reputation_aggregation_outbox
    SET processing_status = 'processed', processed_at = p_now,
        lease_token = NULL, lease_owner_hash = NULL, lease_expires_at = NULL,
        last_error_code = NULL
    WHERE id = event_row.id;
    INSERT INTO reputation_aggregation_event_action(
      event_id, action, attempt_count, worker_hash, metadata, created_at
    ) VALUES (
      event_row.id, 'fan_out', event_row.attempt_count,
      event_row.lease_owner_hash, jsonb_build_object('childCount', fanout_count), p_now
    );
    RETURN 'fan_out';
  END IF;

  PERFORM pg_advisory_xact_lock(hashtextextended(
    event_row.context_key || ':' || event_row.category_id::text || ':' ||
      event_row.algorithm_version,
    0
  ));

  SELECT * INTO formula_row
  FROM reputation_formula_version
  WHERE id = event_row.algorithm_version;
  IF NOT FOUND OR formula_row.status NOT IN ('active', 'draft') THEN
    RAISE EXCEPTION 'Reputation formula version is unavailable';
  END IF;

  prior_strength := COALESCE((formula_row.public_parameters->>'priorStrength')::numeric, 8);
  prior_mean := COALESCE((formula_row.public_parameters->>'priorMean')::numeric, 50);
  half_life_days := COALESCE((formula_row.public_parameters->>'halfLifeDays')::numeric, 365);
  evaluator_cap := COALESCE((formula_row.public_parameters->>'perEvaluatorCap')::numeric, 0.25);
  IF prior_strength <= 0 OR prior_mean NOT BETWEEN 0 AND 100
     OR half_life_days <= 0 OR evaluator_cap <= 0 OR evaluator_cap > 1 THEN
    RAISE EXCEPTION 'Reputation formula parameters are invalid';
  END IF;

  prior_probability := LEAST(0.999999, GREATEST(0.000001, prior_mean / 100));

  CREATE TEMP TABLE IF NOT EXISTS reputation_bt_component_work (
    subject_party_id BIGINT PRIMARY KEY
  ) ON COMMIT DROP;
  CREATE TEMP TABLE IF NOT EXISTS reputation_bt_observation_work (
    evaluator_party_id BIGINT NOT NULL,
    interaction_id UUID NOT NULL,
    left_party_id BIGINT NOT NULL,
    right_party_id BIGINT,
    left_outcome NUMERIC NOT NULL CHECK (left_outcome BETWEEN 0 AND 1),
    raw_weight NUMERIC NOT NULL CHECK (raw_weight > 0),
    adjusted_weight NUMERIC NOT NULL DEFAULT 0 CHECK (adjusted_weight >= 0)
  ) ON COMMIT DROP;
  CREATE TEMP TABLE IF NOT EXISTS reputation_bt_state_work (
    subject_party_id BIGINT PRIMARY KEY,
    ability NUMERIC NOT NULL
  ) ON COMMIT DROP;
  CREATE TEMP TABLE IF NOT EXISTS reputation_bt_evaluator_weight_work (
    evaluator_party_id BIGINT PRIMARY KEY,
    raw_weight NUMERIC NOT NULL CHECK (raw_weight > 0),
    adjusted_weight NUMERIC NOT NULL CHECK (adjusted_weight >= 0)
  ) ON COMMIT DROP;

  TRUNCATE pg_temp.reputation_bt_component_work,
           pg_temp.reputation_bt_observation_work,
           pg_temp.reputation_bt_state_work,
           pg_temp.reputation_bt_evaluator_weight_work;

  WITH RECURSIVE eligible_rank AS (
    SELECT rank.evaluation_id, rank.compared_party_id, rank.position_group
    FROM reputation_evaluation_rank rank
    JOIN reputation_evaluation evaluation ON evaluation.id = rank.evaluation_id
    JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
    JOIN reputation_category category ON category.id = rank.category_id
    WHERE rank.category_id = event_row.category_id
      AND rank.position_group IS NOT NULL
      AND rank.excluded_reason IS NULL
      AND evaluation.formula_version_id = event_row.algorithm_version
      AND evaluation.status = 'submitted'
      AND evaluation.submitted_at IS NOT NULL
      AND (
        run_high_water_mark IS NULL
        OR evaluation.submitted_at <= run_high_water_mark
      )
      AND interaction.status = 'eligible'
      AND category.status = 'active'
      AND (
        cardinality(category.applicable_contexts) = 0
        OR lower(interaction.context_kind) = ANY(category.applicable_contexts)
      )
      AND lower(interaction.context_kind) || ':' || interaction.context_id = event_row.context_key
  ), pair_edge AS (
    SELECT rank_left.compared_party_id AS left_party_id,
           rank_right.compared_party_id AS right_party_id
    FROM eligible_rank rank_left
    JOIN eligible_rank rank_right
      ON rank_right.evaluation_id = rank_left.evaluation_id
     AND rank_right.compared_party_id > rank_left.compared_party_id
  ), connected(subject_party_id) AS (
    SELECT event_row.subject_party_id
    UNION
    SELECT CASE
      WHEN edge.left_party_id = connected.subject_party_id THEN edge.right_party_id
      ELSE edge.left_party_id
    END
    FROM connected
    JOIN pair_edge edge
      ON connected.subject_party_id IN (edge.left_party_id, edge.right_party_id)
  )
  INSERT INTO pg_temp.reputation_bt_component_work(subject_party_id)
  SELECT subject_party_id FROM connected;

  SELECT count(*) INTO component_count
  FROM pg_temp.reputation_bt_component_work;
  IF component_count > 500 THEN
    RAISE EXCEPTION 'Reputation comparison component exceeds the staging limit';
  END IF;

  -- An absolute 0--100 score is a fractional result against a fixed neutral
  -- opponent. This lets absolute and ordinal evidence share one likelihood.
  INSERT INTO pg_temp.reputation_bt_observation_work(
    evaluator_party_id, interaction_id, left_party_id, right_party_id,
    left_outcome, raw_weight
  )
  SELECT evaluation.evaluator_party_id,
         evaluation.interaction_id,
         rank.compared_party_id,
         NULL,
         rank.absolute_score::numeric / 100,
         power(
           0.5::numeric,
           GREATEST(
             0,
             extract(epoch FROM (evidence_reference_time - evaluation.submitted_at)) / 86400
           )
             / half_life_days
         )
  FROM reputation_evaluation_rank rank
  JOIN reputation_evaluation evaluation ON evaluation.id = rank.evaluation_id
  JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
  JOIN reputation_category category ON category.id = rank.category_id
  JOIN pg_temp.reputation_bt_component_work component
    ON component.subject_party_id = rank.compared_party_id
  WHERE rank.category_id = event_row.category_id
    AND rank.absolute_score IS NOT NULL
    AND rank.excluded_reason IS NULL
    AND evaluation.formula_version_id = event_row.algorithm_version
    AND evaluation.status = 'submitted'
    AND evaluation.submitted_at IS NOT NULL
    AND (
      run_high_water_mark IS NULL
      OR evaluation.submitted_at <= run_high_water_mark
    )
    AND interaction.status = 'eligible'
    AND category.status = 'active'
    AND (
      cardinality(category.applicable_contexts) = 0
      OR lower(interaction.context_kind) = ANY(category.applicable_contexts)
    )
    AND lower(interaction.context_kind) || ':' || interaction.context_id = event_row.context_key;

  -- Every ordinal evaluation contributes pairwise wins, losses, or ties; no
  -- position is converted to a synthetic star/absolute score.
  INSERT INTO pg_temp.reputation_bt_observation_work(
    evaluator_party_id, interaction_id, left_party_id, right_party_id,
    left_outcome, raw_weight
  )
  SELECT evaluation.evaluator_party_id,
         evaluation.interaction_id,
         rank_left.compared_party_id,
         rank_right.compared_party_id,
         CASE
           WHEN rank_left.position_group < rank_right.position_group THEN 1
           WHEN rank_left.position_group = rank_right.position_group THEN 0.5
           ELSE 0
         END,
         power(
           0.5::numeric,
           GREATEST(
             0,
             extract(epoch FROM (evidence_reference_time - evaluation.submitted_at)) / 86400
           )
             / half_life_days
         )
  FROM reputation_evaluation_rank rank_left
  JOIN reputation_evaluation_rank rank_right
    ON rank_right.evaluation_id = rank_left.evaluation_id
   AND rank_right.category_id = rank_left.category_id
   AND rank_right.compared_party_id > rank_left.compared_party_id
  JOIN reputation_evaluation evaluation ON evaluation.id = rank_left.evaluation_id
  JOIN reputation_interaction interaction ON interaction.id = evaluation.interaction_id
  JOIN reputation_category category ON category.id = rank_left.category_id
  JOIN pg_temp.reputation_bt_component_work left_component
    ON left_component.subject_party_id = rank_left.compared_party_id
  JOIN pg_temp.reputation_bt_component_work right_component
    ON right_component.subject_party_id = rank_right.compared_party_id
  WHERE rank_left.category_id = event_row.category_id
    AND rank_left.position_group IS NOT NULL
    AND rank_right.position_group IS NOT NULL
    AND rank_left.excluded_reason IS NULL
    AND rank_right.excluded_reason IS NULL
    AND evaluation.formula_version_id = event_row.algorithm_version
    AND evaluation.status = 'submitted'
    AND evaluation.submitted_at IS NOT NULL
    AND (
      run_high_water_mark IS NULL
      OR evaluation.submitted_at <= run_high_water_mark
    )
    AND interaction.status = 'eligible'
    AND category.status = 'active'
    AND (
      cardinality(category.applicable_contexts) = 0
      OR lower(interaction.context_kind) = ANY(category.applicable_contexts)
    )
    AND lower(interaction.context_kind) || ':' || interaction.context_id = event_row.context_key;

  IF (SELECT count(*) FROM pg_temp.reputation_bt_observation_work) > 25000 THEN
    RAISE EXCEPTION 'Reputation comparison evidence exceeds the staging limit';
  END IF;

  INSERT INTO pg_temp.reputation_bt_evaluator_weight_work(
    evaluator_party_id, raw_weight, adjusted_weight
  )
  SELECT evaluator_party_id, sum(raw_weight), sum(raw_weight)
  FROM pg_temp.reputation_bt_observation_work
  GROUP BY evaluator_party_id;

  SELECT count(*) INTO evaluator_total_count
  FROM pg_temp.reputation_bt_evaluator_weight_work;

  IF evaluator_total_count > 0
     AND evaluator_total_count * evaluator_cap >= 1 THEN
    -- Water-fill against the effective total. Repeatedly capping against the
    -- previous adjusted total converges to sum(min(raw_i, cap * total)).
    FOR cap_iteration_index IN 1..64 LOOP
      SELECT COALESCE(sum(adjusted_weight), 0)
        INTO effective_evidence_weight
      FROM pg_temp.reputation_bt_evaluator_weight_work;
      UPDATE pg_temp.reputation_bt_evaluator_weight_work evaluator
      SET adjusted_weight = LEAST(
        evaluator.raw_weight,
        effective_evidence_weight * evaluator_cap
      );
    END LOOP;
  ELSE
    -- With fewer evaluators than 1/cap, a positive evidence total cannot
    -- satisfy the fractional cap. Retain the conservative one-pass shrinkage
    -- for this mathematically infeasible sparse-evidence case.
    SELECT COALESCE(sum(raw_weight), 0)
      INTO effective_evidence_weight
    FROM pg_temp.reputation_bt_evaluator_weight_work;
    UPDATE pg_temp.reputation_bt_evaluator_weight_work evaluator
    SET adjusted_weight = LEAST(
      evaluator.raw_weight,
      effective_evidence_weight * evaluator_cap
    );
  END IF;

  UPDATE pg_temp.reputation_bt_observation_work observation
  SET adjusted_weight = observation.raw_weight *
    evaluator.adjusted_weight / evaluator.raw_weight
  FROM pg_temp.reputation_bt_evaluator_weight_work evaluator
  WHERE evaluator.evaluator_party_id = observation.evaluator_party_id;

  SELECT COALESCE(
           max(adjusted_weight) / NULLIF(sum(adjusted_weight), 0),
           0
         )
    INTO max_evaluator_share
  FROM pg_temp.reputation_bt_evaluator_weight_work;

  INSERT INTO pg_temp.reputation_bt_state_work(subject_party_id, ability)
  SELECT subject_party_id, ln(prior_probability / (1 - prior_probability))
  FROM pg_temp.reputation_bt_component_work;

  -- Fixed-count diagonal Newton updates keep staging runs deterministic. The
  -- Bayesian prior makes the objective strictly regularized per subject.
  FOR iteration_index IN 1..32 LOOP
    WITH directional AS (
      SELECT observation.left_party_id AS subject_party_id,
             observation.right_party_id AS opponent_party_id,
             observation.left_outcome AS outcome,
             observation.adjusted_weight AS weight
      FROM pg_temp.reputation_bt_observation_work observation
      UNION ALL
      SELECT observation.right_party_id,
             observation.left_party_id,
             1 - observation.left_outcome,
             observation.adjusted_weight
      FROM pg_temp.reputation_bt_observation_work observation
      WHERE observation.right_party_id IS NOT NULL
    ), probability AS (
      SELECT directional.subject_party_id,
             directional.outcome,
             directional.weight,
             1 / (1 + exp(-(
               subject_state.ability - COALESCE(opponent_state.ability, 0)
             ))) AS expected
      FROM directional
      JOIN pg_temp.reputation_bt_state_work subject_state
        ON subject_state.subject_party_id = directional.subject_party_id
      LEFT JOIN pg_temp.reputation_bt_state_work opponent_state
        ON opponent_state.subject_party_id = directional.opponent_party_id
    ), contribution AS (
      SELECT subject_party_id,
             sum(weight * (outcome - expected)) AS gradient,
             sum(weight * expected * (1 - expected)) AS curvature
      FROM probability
      GROUP BY subject_party_id
    ), next_state AS (
      SELECT state.subject_party_id,
             GREATEST(-10, LEAST(10,
               state.ability + (
                 prior_strength * (
                   prior_probability - 1 / (1 + exp(-state.ability))
                 ) + COALESCE(contribution.gradient, 0)
               ) / NULLIF(
                 prior_strength * (1 / (1 + exp(-state.ability))) *
                   (1 - 1 / (1 + exp(-state.ability))) +
                   COALESCE(contribution.curvature, 0),
                 0
               )
             )) AS ability
      FROM pg_temp.reputation_bt_state_work state
      LEFT JOIN contribution
        ON contribution.subject_party_id = state.subject_party_id
    )
    UPDATE pg_temp.reputation_bt_state_work state
    SET ability = next_state.ability
    FROM next_state
    WHERE next_state.subject_party_id = state.subject_party_id;
  END LOOP;

  WITH directional AS (
    SELECT observation.left_party_id AS subject_party_id,
           observation.evaluator_party_id,
           observation.interaction_id,
           observation.adjusted_weight
    FROM pg_temp.reputation_bt_observation_work observation
    UNION ALL
    SELECT observation.right_party_id,
           observation.evaluator_party_id,
           observation.interaction_id,
           observation.adjusted_weight
    FROM pg_temp.reputation_bt_observation_work observation
    WHERE observation.right_party_id IS NOT NULL
  ), metrics AS (
    SELECT component.subject_party_id,
           count(DISTINCT directional.interaction_id)::integer AS interaction_count,
           count(DISTINCT directional.evaluator_party_id)::integer AS evaluator_count,
           count(directional.interaction_id)::integer AS observation_count,
           COALESCE(sum(directional.adjusted_weight), 0) AS effective_weight
    FROM pg_temp.reputation_bt_component_work component
    LEFT JOIN directional
      ON directional.subject_party_id = component.subject_party_id
    GROUP BY component.subject_party_id
  ), candidate AS (
    SELECT state.subject_party_id,
           100 / (1 + exp(-state.ability)) AS score,
           LEAST(50, 50 / sqrt(prior_strength + metrics.effective_weight)) AS margin,
           metrics.interaction_count,
           metrics.evaluator_count,
           metrics.observation_count,
           CASE
             WHEN LEAST(metrics.interaction_count, metrics.evaluator_count) < 3 THEN 'forming'
             WHEN LEAST(metrics.interaction_count, metrics.evaluator_count) < 8 THEN 'low'
             WHEN LEAST(metrics.interaction_count, metrics.evaluator_count) < 25 THEN 'moderate'
             ELSE 'high'
           END AS confidence
    FROM pg_temp.reputation_bt_state_work state
    JOIN metrics ON metrics.subject_party_id = state.subject_party_id
  )
  INSERT INTO reputation_aggregate_candidate(
    subject_party_id, category_id, context_key, formula_version_id,
    score, lower_bound, upper_bound, verified_interaction_count,
    distinct_evaluator_count, observation_count, confidence,
    publication_state, source_event_id, calculated_at
  )
  SELECT candidate.subject_party_id,
         event_row.category_id,
         event_row.context_key,
         event_row.algorithm_version,
         round(candidate.score, 4),
         round(GREATEST(0, candidate.score - candidate.margin), 4),
         round(LEAST(100, candidate.score + candidate.margin), 4),
         candidate.interaction_count,
         candidate.evaluator_count,
         candidate.observation_count,
         candidate.confidence,
         'simulation',
         event_row.id,
         p_now
  FROM candidate
  ON CONFLICT (subject_party_id, category_id, context_key, formula_version_id)
  DO UPDATE SET
    score = EXCLUDED.score,
    lower_bound = EXCLUDED.lower_bound,
    upper_bound = EXCLUDED.upper_bound,
    verified_interaction_count = EXCLUDED.verified_interaction_count,
    distinct_evaluator_count = EXCLUDED.distinct_evaluator_count,
    observation_count = EXCLUDED.observation_count,
    confidence = EXCLUDED.confidence,
    publication_state = 'simulation',
    source_event_id = EXCLUDED.source_event_id,
    calculated_at = EXCLUDED.calculated_at;

  SELECT candidate.verified_interaction_count,
         candidate.distinct_evaluator_count,
         candidate.observation_count,
         candidate.confidence
    INTO interaction_count, evaluator_count, observation_count, confidence_value
  FROM reputation_aggregate_candidate candidate
  WHERE candidate.subject_party_id = event_row.subject_party_id
    AND candidate.category_id = event_row.category_id
    AND candidate.context_key = event_row.context_key
    AND candidate.formula_version_id = event_row.algorithm_version;

  UPDATE reputation_aggregation_outbox
  SET processing_status = 'processed', processed_at = p_now,
      lease_token = NULL, lease_owner_hash = NULL, lease_expires_at = NULL,
      last_error_code = NULL
  WHERE id = event_row.id;
  INSERT INTO reputation_aggregation_event_action(
    event_id, action, attempt_count, worker_hash, metadata, created_at
  ) VALUES (
    event_row.id, 'processed', event_row.attempt_count,
    event_row.lease_owner_hash,
    jsonb_build_object(
      'componentSubjectCount', component_count,
      'confidence', confidence_value,
      'distinctEvaluatorCount', evaluator_count,
      'maxEvaluatorShare', round(max_evaluator_share, 6),
      'observationCount', observation_count,
      'runHighWaterMark', run_high_water_mark,
      'verifiedInteractionCount', interaction_count
    ),
    p_now
  );
  RETURN 'processed';
END $$;

CREATE OR REPLACE FUNCTION reputation_fail_aggregation_event(
  p_environment TEXT,
  p_event_id UUID,
  p_claim_token UUID,
  p_error_code TEXT,
  p_now TIMESTAMPTZ
)
RETURNS TEXT
LANGUAGE plpgsql
AS $$
DECLARE
  event_row reputation_aggregation_outbox%ROWTYPE;
  max_attempts_value INTEGER;
  next_status TEXT;
  delay_seconds INTEGER;
BEGIN
  IF p_environment NOT IN ('test', 'staging') THEN
    RAISE EXCEPTION 'Reputation aggregation worker is staging-only';
  END IF;
  IF p_error_code !~ '^[a-z0-9][a-z0-9_.-]{0,63}$' THEN
    RAISE EXCEPTION 'Reputation worker error code is invalid';
  END IF;
  SELECT * INTO event_row
  FROM reputation_aggregation_outbox
  WHERE id = p_event_id
  FOR UPDATE;
  IF NOT FOUND
     OR event_row.processing_status <> 'processing'
     OR event_row.lease_token IS DISTINCT FROM p_claim_token THEN
    RAISE EXCEPTION 'Reputation event lease is unavailable';
  END IF;

  SELECT max_attempts INTO max_attempts_value
  FROM reputation_worker_control
  WHERE environment = p_environment AND enabled AND simulation_only;
  max_attempts_value := COALESCE(max_attempts_value, 8);
  next_status := CASE
    WHEN event_row.attempt_count >= max_attempts_value THEN 'dead_letter'
    ELSE 'retry'
  END;
  delay_seconds := LEAST(
    3600,
    30 * power(2, LEAST(7, GREATEST(0, event_row.attempt_count - 1)))::integer
      + get_byte(digest(event_row.id::text, 'sha256'), 0) % 30
  );

  UPDATE reputation_aggregation_outbox
  SET processing_status = next_status,
      available_at = CASE
        WHEN next_status = 'retry' THEN p_now + make_interval(secs => delay_seconds)
        ELSE available_at
      END,
      lease_token = NULL, lease_owner_hash = NULL, lease_expires_at = NULL,
      processed_at = NULL, last_error_code = p_error_code
  WHERE id = event_row.id;
  INSERT INTO reputation_aggregation_event_action(
    event_id, action, attempt_count, worker_hash, error_code, created_at
  ) VALUES (
    event_row.id,
    CASE WHEN next_status = 'retry' THEN 'retry_scheduled' ELSE 'dead_lettered' END,
    event_row.attempt_count, event_row.lease_owner_hash, p_error_code, p_now
  );
  RETURN next_status;
END $$;

CREATE OR REPLACE FUNCTION reputation_requeue_dead_letter_event(
  p_event_id UUID,
  p_actor_party_id BIGINT,
  p_reason TEXT,
  p_now TIMESTAMPTZ
)
RETURNS UUID
LANGUAGE plpgsql
AS $$
DECLARE
  event_row reputation_aggregation_outbox%ROWTYPE;
BEGIN
  IF length(btrim(p_reason)) NOT BETWEEN 8 AND 500
     OR p_reason ~ '[[:cntrl:]]' THEN
    RAISE EXCEPTION 'Reputation event requeue reason is invalid';
  END IF;
  SELECT * INTO event_row
  FROM reputation_aggregation_outbox
  WHERE id = p_event_id
  FOR UPDATE;
  IF NOT FOUND OR event_row.processing_status <> 'dead_letter' THEN
    RAISE EXCEPTION 'Only dead-letter reputation events can be requeued';
  END IF;

  UPDATE reputation_aggregation_outbox
  SET processing_status = 'retry', attempt_count = 0, available_at = p_now,
      lease_token = NULL, lease_owner_hash = NULL, lease_expires_at = NULL,
      processed_at = NULL, last_error_code = NULL
  WHERE id = event_row.id;
  INSERT INTO reputation_aggregation_event_action(
    event_id, action, attempt_count, actor_party_id, reason, created_at
  ) VALUES (
    event_row.id, 'requeued', event_row.attempt_count,
    p_actor_party_id, btrim(p_reason), p_now
  );
  RETURN event_row.id;
END $$;

CREATE OR REPLACE VIEW reputation_worker_queue_metrics AS
SELECT
  processing_status,
  count(*)::bigint AS event_count,
  min(occurred_at) AS oldest_event_at,
  max(updated_at) AS latest_transition_at,
  COALESCE(
    extract(epoch FROM (
      now() - (min(occurred_at)
        FILTER (WHERE processing_status IN ('pending', 'retry')))
    )),
    0
  )::bigint AS oldest_due_age_seconds
FROM reputation_aggregation_outbox
GROUP BY processing_status;

CREATE OR REPLACE VIEW reputation_worker_event_metrics AS
SELECT
  event.event_type,
  event.algorithm_version,
  split_part(event.context_key, ':', 1) AS context_kind,
  event.processing_status,
  count(*)::bigint AS event_count,
  min(event.occurred_at) AS oldest_event_at,
  max(event.updated_at) AS latest_transition_at
FROM reputation_aggregation_outbox event
GROUP BY event.event_type, event.algorithm_version,
         split_part(event.context_key, ':', 1), event.processing_status;

CREATE OR REPLACE VIEW reputation_worker_processing_metrics AS
WITH attempts AS (
  SELECT
    action.event_id,
    action.attempt_count,
    max(action.created_at) FILTER (WHERE action.action = 'claimed') AS claimed_at,
    max(action.created_at) FILTER (
      WHERE action.action IN ('processed', 'fan_out', 'retry_scheduled', 'dead_lettered')
    ) AS completed_at
  FROM reputation_aggregation_event_action action
  GROUP BY action.event_id, action.attempt_count
)
SELECT
  event.event_type,
  event.algorithm_version,
  split_part(event.context_key, ':', 1) AS context_kind,
  count(*) FILTER (WHERE attempts.completed_at IS NOT NULL)::bigint AS completed_count,
  percentile_cont(0.95) WITHIN GROUP (
    ORDER BY extract(epoch FROM (attempts.completed_at - attempts.claimed_at))
  ) FILTER (
    WHERE attempts.claimed_at IS NOT NULL AND attempts.completed_at IS NOT NULL
  ) AS duration_seconds_p95
FROM attempts
JOIN reputation_aggregation_outbox event ON event.id = attempts.event_id
GROUP BY event.event_type, event.algorithm_version, split_part(event.context_key, ':', 1);

CREATE OR REPLACE VIEW reputation_candidate_freshness_metrics AS
SELECT
  candidate.formula_version_id,
  split_part(candidate.context_key, ':', 1) AS context_kind,
  candidate.confidence,
  count(*)::bigint AS candidate_count,
  max(extract(epoch FROM (now() - candidate.calculated_at)))::bigint
    AS oldest_candidate_age_seconds
FROM reputation_aggregate_candidate candidate
GROUP BY candidate.formula_version_id,
         split_part(candidate.context_key, ':', 1), candidate.confidence;

CREATE OR REPLACE VIEW reputation_worker_health AS
SELECT
  control.environment,
  control.enabled,
  control.simulation_only,
  count(*) FILTER (WHERE event.processing_status IN ('pending', 'retry'))::bigint
    AS queue_depth,
  count(*) FILTER (WHERE event.processing_status = 'processing')::bigint
    AS processing_count,
  count(*) FILTER (WHERE event.processing_status = 'dead_letter')::bigint
    AS dead_letter_count,
  COALESCE(
    extract(epoch FROM (
      now() - (min(event.occurred_at)
        FILTER (WHERE event.processing_status IN ('pending', 'retry')))
    )),
    0
  )::bigint AS oldest_due_age_seconds,
  max(event.processed_at) AS last_processed_at,
  max(event.updated_at) FILTER (WHERE event.processing_status = 'dead_letter')
    AS last_dead_letter_at
FROM reputation_worker_control control
LEFT JOIN reputation_aggregation_outbox event ON TRUE
GROUP BY control.environment, control.enabled, control.simulation_only;

COMMIT;
