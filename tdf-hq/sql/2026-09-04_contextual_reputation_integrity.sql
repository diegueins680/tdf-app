-- Guards that must hold even when a client bypasses application validation.
-- This is additive and can safely follow contextual_reputation on every node.
\set ON_ERROR_STOP on
BEGIN;

CREATE OR REPLACE FUNCTION reputation_evaluation_validate_participants()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  interaction_row reputation_interaction%ROWTYPE;
BEGIN
  SELECT * INTO interaction_row FROM reputation_interaction WHERE id = NEW.interaction_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Reputation interaction does not exist';
  END IF;

  IF interaction_row.status <> 'eligible' THEN
    RAISE EXCEPTION 'Only eligible verified interactions can receive an evaluation';
  END IF;

  IF NEW.direction = 'a_to_b'
     AND (NEW.evaluator_party_id <> interaction_row.party_a_id
          OR NEW.subject_party_id <> interaction_row.party_b_id) THEN
    RAISE EXCEPTION 'a_to_b must evaluate the other verified interaction participant';
  END IF;
  IF NEW.direction = 'b_to_a'
     AND (NEW.evaluator_party_id <> interaction_row.party_b_id
          OR NEW.subject_party_id <> interaction_row.party_a_id) THEN
    RAISE EXCEPTION 'b_to_a must evaluate the other verified interaction participant';
  END IF;

  IF NEW.status = 'submitted' AND NEW.submitted_at IS NULL THEN
    NEW.submitted_at := now();
  END IF;
  NEW.updated_at := now();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_evaluation_validate_participants ON reputation_evaluation;
CREATE TRIGGER trg_reputation_evaluation_validate_participants
  BEFORE INSERT OR UPDATE ON reputation_evaluation
  FOR EACH ROW EXECUTE FUNCTION reputation_evaluation_validate_participants();

CREATE OR REPLACE FUNCTION reputation_evaluation_category_validate_active()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM reputation_category WHERE id=NEW.category_id AND status='active') THEN
    RAISE EXCEPTION 'New evaluation categories must be active official categories';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_evaluation_category_validate_active ON reputation_evaluation_category;
CREATE TRIGGER trg_reputation_evaluation_category_validate_active
  BEFORE INSERT OR UPDATE ON reputation_evaluation_category
  FOR EACH ROW EXECUTE FUNCTION reputation_evaluation_category_validate_active();

COMMIT;
