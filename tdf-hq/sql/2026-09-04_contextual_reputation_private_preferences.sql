-- Private preference and ranking persistence. These records describe what a
-- viewer values; they are deliberately not inputs to public reputation.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_personal_preference (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  context_kind TEXT NOT NULL DEFAULT 'general',
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','active','archived')),
  revision INTEGER NOT NULL DEFAULT 1 CHECK (revision > 0),
  preference_formula_version_id TEXT NOT NULL REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(owner_party_id, context_kind)
);

CREATE TABLE IF NOT EXISTS reputation_personal_preference_category (
  preference_id UUID NOT NULL REFERENCES reputation_personal_preference(id) ON DELETE CASCADE,
  category_id UUID NOT NULL REFERENCES reputation_category(id) ON DELETE RESTRICT,
  position SMALLINT NOT NULL CHECK (position > 0),
  weight NUMERIC(7,4) NOT NULL CHECK (weight >= 0 AND weight <= 100),
  not_applicable BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY(preference_id, category_id),
  UNIQUE(preference_id, position)
);

-- A private ranking may contain unverified comparisons. It remains physically
-- separate from verified evaluation ranks and public aggregation tables.
CREATE TABLE IF NOT EXISTS reputation_private_ranking_item (
  ranking_id UUID NOT NULL REFERENCES reputation_private_ranking(id) ON DELETE CASCADE,
  subject_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  position_group SMALLINT,
  excluded_reason TEXT CHECK (excluded_reason IN ('insufficient_information','not_comparable','not_applicable')),
  note TEXT CHECK (note IS NULL OR length(btrim(note)) <= 500),
  PRIMARY KEY(ranking_id, subject_party_id),
  CHECK ((position_group IS NOT NULL)::int + (excluded_reason IS NOT NULL)::int = 1),
  CHECK (position_group IS NULL OR position_group > 0)
);

CREATE INDEX IF NOT EXISTS reputation_personal_preference_owner_idx
  ON reputation_personal_preference(owner_party_id, status, updated_at DESC);
CREATE INDEX IF NOT EXISTS reputation_private_ranking_item_subject_idx
  ON reputation_private_ranking_item(subject_party_id);

CREATE OR REPLACE FUNCTION reputation_personal_preference_validate_active()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  applicable_count INTEGER;
  total_weight NUMERIC(10,4);
  monotonic BOOLEAN;
BEGIN
  IF NEW.status <> 'active' THEN
    NEW.updated_at := now();
    RETURN NEW;
  END IF;

  SELECT count(*), coalesce(sum(weight), 0), coalesce(bool_and(previous_weight >= weight), TRUE)
  INTO applicable_count, total_weight, monotonic
  FROM (
    SELECT weight, lag(weight) OVER (ORDER BY position) AS previous_weight
    FROM reputation_personal_preference_category
    WHERE preference_id = NEW.id AND NOT not_applicable
  ) ordered;

  IF applicable_count NOT BETWEEN 3 AND 10 THEN
    RAISE EXCEPTION 'Active personal preference requires between 3 and 10 applicable categories' USING ERRCODE='23514';
  END IF;
  IF total_weight <> 100.0000 THEN
    RAISE EXCEPTION 'Active personal preference weights must sum exactly to 100' USING ERRCODE='23514';
  END IF;
  IF NOT monotonic THEN
    RAISE EXCEPTION 'Personal preference weights must respect category priority order' USING ERRCODE='23514';
  END IF;

  NEW.updated_at := now();
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION reputation_personal_preference_category_validate_write()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  profile_status TEXT;
  profile_id UUID;
BEGIN
  profile_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.preference_id ELSE NEW.preference_id END;
  SELECT status INTO profile_status
  FROM reputation_personal_preference
  WHERE id = profile_id;

  IF profile_status IS NULL THEN
    RAISE EXCEPTION 'Personal preference profile does not exist' USING ERRCODE='23503';
  END IF;
  IF profile_status <> 'draft' THEN
    RAISE EXCEPTION 'Personal preference categories can only change while the profile is a draft' USING ERRCODE='23514';
  END IF;
  IF TG_OP <> 'DELETE' AND NOT EXISTS (
    SELECT 1 FROM reputation_category category
    WHERE category.id = NEW.category_id AND category.status = 'active'
  ) THEN
    RAISE EXCEPTION 'Personal preference requires active reputation categories' USING ERRCODE='23514';
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_personal_preference_validate_active ON reputation_personal_preference;
CREATE TRIGGER trg_reputation_personal_preference_validate_active
  BEFORE INSERT OR UPDATE OF status ON reputation_personal_preference
  FOR EACH ROW EXECUTE FUNCTION reputation_personal_preference_validate_active();

DROP TRIGGER IF EXISTS trg_reputation_personal_preference_category_validate_write ON reputation_personal_preference_category;
CREATE TRIGGER trg_reputation_personal_preference_category_validate_write
  BEFORE INSERT OR UPDATE OR DELETE ON reputation_personal_preference_category
  FOR EACH ROW EXECUTE FUNCTION reputation_personal_preference_category_validate_write();

COMMENT ON TABLE reputation_personal_preference IS
  'Private viewer priorities for personalized relevance. Never use in public reputation aggregation.';
COMMENT ON TABLE reputation_private_ranking_item IS
  'Private, potentially unverified comparisons. Never use in search, badges, recommendations, or public reputation.';

COMMIT;
