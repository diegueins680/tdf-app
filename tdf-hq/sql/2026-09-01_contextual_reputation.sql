-- Contextual reputation v1. Additive, idempotent schema.  It keeps private
-- ordinal preferences physically separate from public, verified aggregates.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_formula_version (
  id TEXT PRIMARY KEY,
  public_parameters JSONB NOT NULL,
  preference_parameters JSONB NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('draft','active','retired')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  activated_at TIMESTAMPTZ,
  created_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT
);

INSERT INTO reputation_formula_version(id, public_parameters, preference_parameters, status)
VALUES ('public-bayes-roc-v1',
  '{"priorStrength":8,"priorMean":50,"minimumVerifiedRatings":3,"halfLifeDays":365,"perEvaluatorCap":0.25}',
  '{"method":"rank-order-centroid","scale":100}', 'active')
ON CONFLICT (id) DO NOTHING;

CREATE TABLE IF NOT EXISTS reputation_category (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  slug TEXT NOT NULL UNIQUE CHECK (slug ~ '^[a-z0-9]+(?:-[a-z0-9]+)*$'),
  name_es TEXT NOT NULL CHECK (length(btrim(name_es)) BETWEEN 2 AND 100),
  name_en TEXT NOT NULL CHECK (length(btrim(name_en)) BETWEEN 2 AND 100),
  description_es TEXT NOT NULL DEFAULT '',
  description_en TEXT NOT NULL DEFAULT '',
  applicable_roles TEXT[] NOT NULL DEFAULT '{}',
  applicable_contexts TEXT[] NOT NULL DEFAULT '{}',
  default_position SMALLINT NOT NULL DEFAULT 100 CHECK (default_position > 0),
  institutional_weight NUMERIC(7,4) NOT NULL DEFAULT 0 CHECK (institutional_weight >= 0),
  version INTEGER NOT NULL DEFAULT 1 CHECK (version > 0),
  status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','archived','proposed','rejected','merged')),
  created_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  merged_into_id UUID REFERENCES reputation_category(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (status <> 'merged' OR merged_into_id IS NOT NULL)
);

INSERT INTO reputation_category(slug,name_es,name_en,description_es,description_en,default_position,institutional_weight)
VALUES
 ('trust-and-fulfilment','Confianza y cumplimiento','Trust and fulfilment','Cumple lo acordado en interacciones verificadas.','Keeps commitments in verified interactions.',1,25),
 ('quality','Calidad del servicio o trabajo','Service or work quality','Calidad contextual del resultado.','Contextual quality of the outcome.',2,20),
 ('communication','Comunicación','Communication','Claridad y trato durante la interacción.','Clarity and conduct during the interaction.',3,15),
 ('response-time','Tiempo de respuesta','Response time','Respuesta oportuna cuando era aplicable.','Timely response when applicable.',4,10),
 ('punctuality','Puntualidad','Punctuality','Respeto de horarios o plazos.','Respect for schedules or deadlines.',5,10),
 ('professionalism','Profesionalismo','Professionalism','Conducta profesional verificable.','Verifiable professional conduct.',6,10),
 ('collaboration','Colaboración','Collaboration','Capacidad de colaborar en el contexto evaluado.','Ability to collaborate in the evaluated context.',7,5),
 ('value-for-money','Relación calidad-precio','Value for money','Valor percibido sin ponderar el monto pagado.','Perceived value without weighting the amount paid.',8,5)
ON CONFLICT (slug) DO NOTHING;

CREATE TABLE IF NOT EXISTS reputation_interaction (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  context_kind TEXT NOT NULL,
  context_id TEXT NOT NULL,
  party_a_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  party_b_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  completed_at TIMESTAMPTZ NOT NULL,
  verified_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  status TEXT NOT NULL DEFAULT 'eligible' CHECK (status IN ('eligible','disputed','void','expired')),
  source_kind TEXT NOT NULL,
  source_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(source_kind, source_id),
  CHECK (party_a_id <> party_b_id)
);

CREATE TABLE IF NOT EXISTS reputation_evaluation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  interaction_id UUID NOT NULL REFERENCES reputation_interaction(id) ON DELETE RESTRICT,
  evaluator_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  subject_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  direction TEXT NOT NULL CHECK (direction IN ('a_to_b','b_to_a')),
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','submitted','under_review','void')),
  formula_version_id TEXT NOT NULL REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  revision INTEGER NOT NULL DEFAULT 1 CHECK (revision > 0),
  edit_deadline TIMESTAMPTZ NOT NULL,
  submitted_at TIMESTAMPTZ,
  idempotency_key TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(interaction_id, direction),
  UNIQUE(evaluator_party_id, idempotency_key),
  CHECK (evaluator_party_id <> subject_party_id)
);

CREATE TABLE IF NOT EXISTS reputation_evaluation_category (
  evaluation_id UUID NOT NULL REFERENCES reputation_evaluation(id) ON DELETE CASCADE,
  category_id UUID NOT NULL REFERENCES reputation_category(id) ON DELETE RESTRICT,
  position SMALLINT NOT NULL CHECK (position > 0),
  weight NUMERIC(7,4) NOT NULL CHECK (weight >= 0 AND weight <= 100),
  not_applicable BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY(evaluation_id, category_id),
  UNIQUE(evaluation_id, position)
);

CREATE TABLE IF NOT EXISTS reputation_evaluation_rank (
  evaluation_id UUID NOT NULL REFERENCES reputation_evaluation(id) ON DELETE CASCADE,
  category_id UUID NOT NULL REFERENCES reputation_category(id) ON DELETE RESTRICT,
  compared_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  position_group SMALLINT,
  excluded_reason TEXT CHECK (excluded_reason IN ('insufficient_information','not_comparable','not_applicable')),
  absolute_score SMALLINT CHECK (absolute_score BETWEEN 0 AND 100),
  PRIMARY KEY(evaluation_id, category_id, compared_party_id),
  CHECK ((position_group IS NOT NULL)::int + (excluded_reason IS NOT NULL)::int <= 1)
);

-- Private lists never join the public aggregation pipeline.
CREATE TABLE IF NOT EXISTS reputation_private_ranking (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  title TEXT NOT NULL CHECK (length(btrim(title)) BETWEEN 1 AND 120),
  context_kind TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','saved','archived')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS reputation_public_aggregate (
  subject_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  category_id UUID NOT NULL REFERENCES reputation_category(id) ON DELETE RESTRICT,
  context_kind TEXT NOT NULL,
  formula_version_id TEXT NOT NULL REFERENCES reputation_formula_version(id) ON DELETE RESTRICT,
  score NUMERIC(7,4) NOT NULL CHECK (score BETWEEN 0 AND 100),
  lower_bound NUMERIC(7,4) NOT NULL CHECK (lower_bound BETWEEN 0 AND 100),
  upper_bound NUMERIC(7,4) NOT NULL CHECK (upper_bound BETWEEN 0 AND 100),
  verified_count INTEGER NOT NULL CHECK (verified_count >= 0),
  confidence TEXT NOT NULL CHECK (confidence IN ('forming','low','moderate','high')),
  calculated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(subject_party_id, category_id, context_kind, formula_version_id)
);

CREATE TABLE IF NOT EXISTS reputation_audit_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(), actor_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  action TEXT NOT NULL, resource_kind TEXT NOT NULL, resource_id TEXT NOT NULL,
  reason TEXT, metadata JSONB NOT NULL DEFAULT '{}', created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS reputation_evaluation_subject_idx ON reputation_evaluation(subject_party_id, status, submitted_at DESC);
CREATE INDEX IF NOT EXISTS reputation_aggregate_lookup_idx ON reputation_public_aggregate(subject_party_id, context_kind, calculated_at DESC);
CREATE INDEX IF NOT EXISTS reputation_rank_comparison_idx ON reputation_evaluation_rank(category_id, compared_party_id);

COMMIT;
