-- Data-layer safety for user-proposed reputation categories. UI validation is
-- helpful, but it cannot be the only guard against sensitive criteria.
\set ON_ERROR_STOP on
BEGIN;

CREATE OR REPLACE FUNCTION reputation_category_validate_safety()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  normalized_name TEXT;
BEGIN
  normalized_name := lower(
    regexp_replace(
      btrim(NEW.name_es) || ' ' || btrim(NEW.name_en) || ' '
        || coalesce(NEW.description_es, '') || ' ' || coalesce(NEW.description_en, ''),
      '\s+', ' ', 'g'
    )
  );

  -- Categories based on protected or sensitive personal attributes are not a
  -- permissible reputation signal. This is intentionally conservative.
  IF normalized_name ~
    '(^|[^[:alpha:]])(raza|racial|etnia|etnico|étnico|religion|religión|religioso|sexo|sexual|genero|género|orientacion|orientación|discapacidad|discapacitado|condicion medica|condición médica|embarazo|pregnancy|race|racial|ethnic|ethnicity|religion|religious|sex|gender|sexual orientation|disability|disabled|medical condition|pregnan)([^[:alpha:]]|$)'
  THEN
    RAISE EXCEPTION 'Reputation categories cannot use sensitive or protected personal attributes' USING ERRCODE='23514';
  END IF;

  -- A user-created category starts proposed. Publication is a later, audited
  -- moderation decision; it must not be a side effect of creation.
  IF TG_OP = 'INSERT' AND NEW.created_by_party_id IS NOT NULL AND NEW.status <> 'proposed' THEN
    RAISE EXCEPTION 'User-created reputation categories must start proposed' USING ERRCODE='23514';
  END IF;

  -- Exact normalized duplicates are rejected early. Semantic equivalence is
  -- still surfaced to moderation rather than guessed by a database heuristic.
  IF EXISTS (
    SELECT 1
    FROM reputation_category existing
    WHERE existing.id <> NEW.id
      AND existing.status <> 'merged'
      AND (
        lower(btrim(existing.name_es)) = lower(btrim(NEW.name_es))
        OR lower(btrim(existing.name_en)) = lower(btrim(NEW.name_en))
      )
  ) THEN
    RAISE EXCEPTION 'Reputation category duplicates an existing category; use the existing category or submit it for moderation' USING ERRCODE='23505';
  END IF;

  NEW.updated_at := now();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_category_validate_safety ON reputation_category;
CREATE TRIGGER trg_reputation_category_validate_safety
  BEFORE INSERT OR UPDATE OF name_es, name_en, description_es, description_en, status, created_by_party_id
  ON reputation_category
  FOR EACH ROW EXECUTE FUNCTION reputation_category_validate_safety();

COMMENT ON FUNCTION reputation_category_validate_safety() IS
  'Rejects sensitive/discriminatory reputation criteria, direct publication of user proposals, and exact normalized duplicates.';

COMMIT;
