-- Preserve verified experience-review history without fabricating ordinal
-- comparisons, reciprocal assessments, or public person-reputation scores.
--
-- This is intentionally a legacy evidence ledger, not an input to
-- reputation_public_aggregate. A later reviewed adapter may use a source row
-- to prove an interaction, but it must create its own directional context.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_legacy_signal (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  source_system TEXT NOT NULL CHECK (source_system IN ('experience_review')),
  source_review_id UUID NOT NULL,
  author_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  target_kind TEXT NOT NULL,
  target_id TEXT NOT NULL,
  interaction_source_kind TEXT NOT NULL,
  interaction_source_id TEXT NOT NULL,
  rating SMALLINT NOT NULL CHECK (rating BETWEEN 1 AND 5),
  source_status TEXT NOT NULL CHECK (source_status IN ('published','hidden','removed')),
  occurred_at TIMESTAMPTZ NOT NULL,
  imported_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_synced_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  aggregation_eligible BOOLEAN NOT NULL DEFAULT FALSE CHECK (aggregation_eligible = FALSE),
  provenance JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (source_system, source_review_id)
);

CREATE INDEX IF NOT EXISTS reputation_legacy_signal_author_idx
  ON reputation_legacy_signal(author_party_id, occurred_at DESC);

COMMENT ON TABLE reputation_legacy_signal IS
  'Historical verified review evidence retained with provenance. Never aggregate this table into public contextual reputation.';
COMMENT ON COLUMN reputation_legacy_signal.aggregation_eligible IS
  'Hard false invariant: legacy stars are not ordinal rankings and cannot affect public reputation.';

INSERT INTO reputation_legacy_signal (
  source_system, source_review_id, author_party_id, target_kind, target_id,
  interaction_source_kind, interaction_source_id, rating, source_status,
  occurred_at, provenance
)
SELECT
  'experience_review', review.id, review.author_party_id, review.target_kind,
  review.target_id, review.source_kind, review.source_id, review.rating,
  review.status, review.created_at,
  jsonb_build_object(
    'migration', '2026-09-04_contextual_reputation_legacy_signals',
    'originalCreatedAt', review.created_at,
    'originalUpdatedAt', review.updated_at,
    'publicAggregationEligible', false,
    'reason', 'legacy stars lack directional contextual ranking evidence'
  )
FROM experience_review review
ON CONFLICT (source_system, source_review_id) DO UPDATE
SET source_status = EXCLUDED.source_status,
    last_synced_at = now(),
    provenance = reputation_legacy_signal.provenance
      || jsonb_build_object('originalUpdatedAt', EXCLUDED.provenance->'originalUpdatedAt');

-- A durable audit entry makes the migration observable without recording a
-- person-level evaluator identity beyond the existing private source record.
INSERT INTO reputation_audit_log(action, resource_kind, resource_id, metadata)
SELECT
  'reputation.legacy-signal.backfill',
  'reputation_legacy_signal',
  'experience_review',
  jsonb_build_object(
    'migration', '2026-09-04_contextual_reputation_legacy_signals',
    'sourceRows', count(*),
    'importedRows', count(*) FILTER (WHERE status = 'published'),
    'nonPublishedRows', count(*) FILTER (WHERE status <> 'published'),
    'aggregationEligible', false
  )
FROM experience_review;

COMMIT;
