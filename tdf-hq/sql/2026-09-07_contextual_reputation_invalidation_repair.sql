-- Forward-only repair for tuple invalidation and submitted-category edits.
-- The staging-worker migration is immutable because deployed databases can
-- already have its checksum recorded in the production migration ledger.
\set ON_ERROR_STOP on
BEGIN;

CREATE OR REPLACE FUNCTION reputation_emit_evaluation_tuple_events(
  p_evaluation_id UUID,
  p_subject_party_id BIGINT,
  p_context_key TEXT,
  p_formula_version_id TEXT,
  p_source_version BIGINT,
  p_event_type TEXT,
  p_occurred_at TIMESTAMPTZ
)
RETURNS INTEGER
LANGUAGE plpgsql
AS $$
DECLARE
  correlation UUID := gen_random_uuid();
  emitted INTEGER := 0;
  target RECORD;
BEGIN
  FOR target IN
    SELECT pair.subject_party_id, pair.category_id
    FROM (
      SELECT p_subject_party_id AS subject_party_id,
             item.category_id
      FROM reputation_evaluation_category item
      WHERE item.evaluation_id = p_evaluation_id
        AND NOT item.not_applicable
      UNION
      SELECT rank.compared_party_id, rank.category_id
      FROM reputation_evaluation_rank rank
      WHERE rank.evaluation_id = p_evaluation_id
    ) pair
    ORDER BY pair.subject_party_id, pair.category_id
  LOOP
    PERFORM reputation_enqueue_aggregation_event(
      gen_random_uuid(), p_event_type, target.subject_party_id,
      p_context_key, target.category_id, p_source_version,
      p_formula_version_id, correlation, NULL, p_occurred_at
    );
    emitted := emitted + 1;
  END LOOP;

  RETURN emitted;
END $$;

CREATE OR REPLACE FUNCTION reputation_evaluation_category_submission_guard()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  -- Parent deletion already emits the complete pre-cascade tuple set.
  IF TG_OP = 'DELETE' AND pg_trigger_depth() > 1 THEN
    RETURN OLD;
  END IF;

  IF TG_OP = 'INSERT' THEN
    PERFORM 1
    FROM reputation_evaluation evaluation
    WHERE evaluation.id = NEW.evaluation_id
    FOR UPDATE;
    IF EXISTS (
      SELECT 1
      FROM reputation_evaluation evaluation
      WHERE evaluation.id = NEW.evaluation_id
        AND evaluation.status = 'submitted'
    ) THEN
      RAISE EXCEPTION
        'Submitted reputation evaluation categories are immutable; return the evaluation to draft first';
    END IF;
    RETURN NEW;
  ELSIF TG_OP = 'DELETE' THEN
    PERFORM 1
    FROM reputation_evaluation evaluation
    WHERE evaluation.id = OLD.evaluation_id
    FOR UPDATE;
    IF EXISTS (
      SELECT 1
      FROM reputation_evaluation evaluation
      WHERE evaluation.id = OLD.evaluation_id
        AND evaluation.status = 'submitted'
    ) THEN
      RAISE EXCEPTION
        'Submitted reputation evaluation categories are immutable; return the evaluation to draft first';
    END IF;
    RETURN OLD;
  END IF;

  -- Lock moved parents in UUID order so concurrent submissions cannot observe
  -- a partial category mutation and cross-parent moves cannot deadlock.
  PERFORM 1
  FROM reputation_evaluation evaluation
  WHERE evaluation.id IN (OLD.evaluation_id, NEW.evaluation_id)
  ORDER BY evaluation.id
  FOR UPDATE;
  IF EXISTS (
    SELECT 1
    FROM reputation_evaluation evaluation
    WHERE evaluation.id IN (OLD.evaluation_id, NEW.evaluation_id)
      AND evaluation.status = 'submitted'
  ) THEN
    RAISE EXCEPTION
      'Submitted reputation evaluation categories are immutable; return the evaluation to draft first';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_evaluation_category_submission_guard
  ON reputation_evaluation_category;
CREATE TRIGGER trg_reputation_evaluation_category_submission_guard
  BEFORE INSERT OR UPDATE OR DELETE ON reputation_evaluation_category
  FOR EACH ROW
  EXECUTE FUNCTION reputation_evaluation_category_submission_guard();

CREATE OR REPLACE FUNCTION reputation_evaluation_outbox_trigger()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  old_context_key TEXT;
  tuple_changed BOOLEAN;
  evidence_changed BOOLEAN;
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NEW.status = 'submitted' THEN
      PERFORM reputation_emit_evaluation_events(
        NEW.id, 'evaluation.submitted', now()
      );
    END IF;
    RETURN NEW;
  END IF;

  tuple_changed :=
    OLD.interaction_id IS DISTINCT FROM NEW.interaction_id
    OR OLD.subject_party_id IS DISTINCT FROM NEW.subject_party_id
    OR OLD.formula_version_id IS DISTINCT FROM NEW.formula_version_id;
  evidence_changed :=
    tuple_changed
    OR OLD.evaluator_party_id IS DISTINCT FROM NEW.evaluator_party_id
    OR OLD.direction IS DISTINCT FROM NEW.direction
    OR OLD.submitted_at IS DISTINCT FROM NEW.submitted_at
    OR OLD.revision IS DISTINCT FROM NEW.revision;

  IF OLD.status = 'submitted' AND tuple_changed THEN
    SELECT lower(interaction.context_kind) || ':' || interaction.context_id
      INTO old_context_key
    FROM reputation_interaction interaction
    WHERE interaction.id = OLD.interaction_id;
    PERFORM reputation_emit_evaluation_tuple_events(
      OLD.id, OLD.subject_party_id, old_context_key,
      OLD.formula_version_id, NEW.revision, 'evaluation.edited', now()
    );
  END IF;

  IF NEW.status = 'submitted'
     AND OLD.status = 'draft' THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.submitted', now());
  ELSIF NEW.status = 'submitted'
     AND OLD.status NOT IN ('draft', 'submitted') THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.edited', now());
  ELSIF OLD.status = 'submitted'
     AND NEW.status IN ('draft', 'under_review', 'void') THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.invalidated', now());
  ELSIF OLD.status = 'submitted'
     AND NEW.status = 'submitted'
     AND evidence_changed THEN
    PERFORM reputation_emit_evaluation_events(NEW.id, 'evaluation.edited', now());
  END IF;
  RETURN NEW;
END $$;

COMMIT;
