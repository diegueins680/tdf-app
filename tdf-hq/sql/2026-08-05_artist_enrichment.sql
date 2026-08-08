-- Durable artist discovery, provenance, review, audit, run, and media records.
-- This migration is additive and does not rewrite or delete artist data.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

DO $preflight$
BEGIN
  IF to_regclass('public.party') IS NULL
     OR to_regclass('public.artist_profile') IS NULL THEN
    RAISE EXCEPTION 'Cannot install artist enrichment schema: party or artist_profile is missing';
  END IF;
  IF EXISTS (
    SELECT lower(slug)
    FROM artist_profile
    WHERE slug IS NOT NULL AND btrim(slug) <> ''
    GROUP BY lower(slug)
    HAVING count(*) > 1
  ) THEN
    RAISE EXCEPTION 'Cannot install case-insensitive artist slugs until duplicate slugs are reviewed';
  END IF;
END
$preflight$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_profile_slug_ci
  ON artist_profile (lower(slug))
  WHERE slug IS NOT NULL AND btrim(slug) <> '';

CREATE TABLE IF NOT EXISTS artist_profile_enrichment (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  official_name TEXT,
  country TEXT,
  instagram_url TEXT,
  social_links TEXT,
  discography TEXT,
  achievements TEXT,
  hero_original_url TEXT,
  hero_square_url TEXT,
  hero_landscape_url TEXT,
  hero_responsive_urls TEXT,
  hero_focal_point TEXT,
  last_verified_at TIMESTAMPTZ,
  confidence DOUBLE PRECISION,
  review_status TEXT NOT NULL DEFAULT 'unverified',
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_artist_profile_enrichment UNIQUE (artist_party_id),
  CONSTRAINT ck_artist_profile_enrichment_confidence
    CHECK (confidence IS NULL OR (confidence >= 0 AND confidence <= 1)),
  CONSTRAINT ck_artist_profile_enrichment_status
    CHECK (review_status IN ('unverified','pending','verified','rejected','ambiguous'))
);

CREATE TABLE IF NOT EXISTS artist_inventory_reference (
  id BIGSERIAL PRIMARY KEY,
  idempotency_key TEXT NOT NULL,
  source_type TEXT NOT NULL,
  source_record_id TEXT NOT NULL,
  original_name TEXT NOT NULL,
  normalized_name TEXT NOT NULL,
  artist_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  social_artist_id BIGINT,
  aliases TEXT,
  evidence TEXT,
  confidence DOUBLE PRECISION,
  disposition TEXT NOT NULL DEFAULT 'discovered',
  first_seen_at TIMESTAMPTZ NOT NULL,
  last_seen_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_artist_inventory_reference UNIQUE (idempotency_key),
  CONSTRAINT ck_artist_inventory_reference_confidence
    CHECK (confidence IS NULL OR (confidence >= 0 AND confidence <= 1))
);

CREATE INDEX IF NOT EXISTS idx_artist_inventory_normalized_name
  ON artist_inventory_reference (normalized_name);
CREATE INDEX IF NOT EXISTS idx_artist_inventory_artist
  ON artist_inventory_reference (artist_party_id);
CREATE INDEX IF NOT EXISTS idx_artist_inventory_disposition
  ON artist_inventory_reference (disposition, last_seen_at DESC);

DO $inventory_fk$
BEGIN
  IF to_regclass('public.social_artist_profile') IS NOT NULL
     AND NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = 'fk_artist_inventory_social_artist') THEN
    ALTER TABLE artist_inventory_reference
      ADD CONSTRAINT fk_artist_inventory_social_artist
      FOREIGN KEY (social_artist_id) REFERENCES social_artist_profile(id) ON DELETE SET NULL;
  END IF;
END
$inventory_fk$;

CREATE TABLE IF NOT EXISTS artist_research_source (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  inventory_reference_id BIGINT REFERENCES artist_inventory_reference(id) ON DELETE SET NULL,
  source_url TEXT NOT NULL,
  source_type TEXT NOT NULL,
  retrieved_at TIMESTAMPTZ NOT NULL,
  supported_fields TEXT NOT NULL,
  attribution TEXT,
  content_hash TEXT,
  idempotency_key TEXT NOT NULL,
  CONSTRAINT unique_artist_research_source UNIQUE (idempotency_key),
  CONSTRAINT ck_artist_research_source_owner
    CHECK (artist_party_id IS NOT NULL OR inventory_reference_id IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_artist_research_source_artist
  ON artist_research_source (artist_party_id, retrieved_at DESC);
CREATE INDEX IF NOT EXISTS idx_artist_research_source_type
  ON artist_research_source (source_type, retrieved_at DESC);

CREATE TABLE IF NOT EXISTS artist_enrichment_suggestion (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  inventory_reference_id BIGINT REFERENCES artist_inventory_reference(id) ON DELETE SET NULL,
  field_name TEXT NOT NULL,
  current_value TEXT,
  proposed_value TEXT,
  confidence DOUBLE PRECISION NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  auto_publish BOOLEAN NOT NULL DEFAULT FALSE,
  evidence TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  decided_at TIMESTAMPTZ,
  decided_by BIGINT REFERENCES party(id) ON DELETE SET NULL,
  decision_note TEXT,
  CONSTRAINT unique_artist_enrichment_suggestion UNIQUE (idempotency_key),
  CONSTRAINT ck_artist_enrichment_suggestion_confidence
    CHECK (confidence >= 0 AND confidence <= 1),
  CONSTRAINT ck_artist_enrichment_suggestion_status
    CHECK (status IN ('pending','approved','rejected','superseded','auto_applied'))
);

CREATE INDEX IF NOT EXISTS idx_artist_suggestion_queue
  ON artist_enrichment_suggestion (status, confidence DESC, updated_at DESC);
CREATE INDEX IF NOT EXISTS idx_artist_suggestion_artist
  ON artist_enrichment_suggestion (artist_party_id, status);

CREATE TABLE IF NOT EXISTS artist_field_change (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  suggestion_id BIGINT REFERENCES artist_enrichment_suggestion(id) ON DELETE SET NULL,
  field_name TEXT NOT NULL,
  previous_value TEXT,
  new_value TEXT,
  evidence TEXT NOT NULL,
  confidence DOUBLE PRECISION NOT NULL,
  actor TEXT NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL,
  idempotency_key TEXT NOT NULL,
  CONSTRAINT unique_artist_field_change UNIQUE (idempotency_key),
  CONSTRAINT ck_artist_field_change_confidence
    CHECK (confidence >= 0 AND confidence <= 1)
);

CREATE INDEX IF NOT EXISTS idx_artist_field_change_history
  ON artist_field_change (artist_party_id, changed_at DESC);

CREATE TABLE IF NOT EXISTS artist_enrichment_run (
  id BIGSERIAL PRIMARY KEY,
  run_key TEXT NOT NULL,
  mode TEXT NOT NULL,
  scope TEXT NOT NULL,
  requested_artist_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  status TEXT NOT NULL,
  phase TEXT NOT NULL,
  checkpoint TEXT,
  counters TEXT,
  error_summary TEXT,
  started_at TIMESTAMPTZ NOT NULL,
  heartbeat_at TIMESTAMPTZ NOT NULL,
  finished_at TIMESTAMPTZ,
  CONSTRAINT unique_artist_enrichment_run UNIQUE (run_key),
  CONSTRAINT ck_artist_enrichment_run_mode CHECK (mode IN ('dry_run','production')),
  CONSTRAINT ck_artist_enrichment_run_status
    CHECK (status IN ('running','completed','failed','cancelled','blocked'))
);

CREATE INDEX IF NOT EXISTS idx_artist_enrichment_run_status
  ON artist_enrichment_run (status, started_at DESC);

-- PostgreSQL enforces at most one active full-platform run. Artist-specific
-- reruns retain their deterministic run key and never overlap the daily job.
CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_enrichment_active_full_run
  ON artist_enrichment_run ((scope))
  WHERE status = 'running' AND scope = 'full';

CREATE TABLE IF NOT EXISTS artist_identity_candidate (
  id BIGSERIAL PRIMARY KEY,
  inventory_reference_id BIGINT NOT NULL REFERENCES artist_inventory_reference(id) ON DELETE RESTRICT,
  artist_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
  provider TEXT NOT NULL,
  external_id TEXT,
  candidate_url TEXT,
  evidence TEXT NOT NULL,
  confidence DOUBLE PRECISION NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  idempotency_key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  decided_at TIMESTAMPTZ,
  decided_by BIGINT REFERENCES party(id) ON DELETE SET NULL,
  decision_note TEXT,
  CONSTRAINT unique_artist_identity_candidate UNIQUE (idempotency_key),
  CONSTRAINT ck_artist_identity_candidate_confidence
    CHECK (confidence >= 0 AND confidence <= 1),
  CONSTRAINT ck_artist_identity_candidate_status
    CHECK (status IN ('pending','approved','rejected','superseded'))
);

CREATE INDEX IF NOT EXISTS idx_artist_identity_candidate_queue
  ON artist_identity_candidate (status, confidence DESC, updated_at DESC);

CREATE TABLE IF NOT EXISTS artist_media_asset (
  id BIGSERIAL PRIMARY KEY,
  artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  asset_kind TEXT NOT NULL,
  source_url TEXT NOT NULL,
  source_attribution TEXT NOT NULL,
  retrieved_at TIMESTAMPTZ NOT NULL,
  source_content_hash TEXT NOT NULL,
  source_width INTEGER NOT NULL,
  source_height INTEGER NOT NULL,
  source_mime_type TEXT NOT NULL,
  source_byte_size BIGINT NOT NULL,
  content_hash TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  mime_type TEXT NOT NULL,
  byte_size BIGINT NOT NULL,
  rights_status TEXT NOT NULL,
  drive_file_id TEXT NOT NULL,
  public_url TEXT NOT NULL,
  parent_asset_id BIGINT REFERENCES artist_media_asset(id) ON DELETE SET NULL,
  focal_point TEXT,
  idempotency_key TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  CONSTRAINT unique_artist_media_asset UNIQUE (idempotency_key),
  CONSTRAINT unique_artist_media_drive_file UNIQUE (drive_file_id),
  CONSTRAINT ck_artist_media_asset_dimensions CHECK (width > 0 AND height > 0),
  CONSTRAINT ck_artist_media_asset_source_dimensions CHECK (source_width > 0 AND source_height > 0),
  CONSTRAINT ck_artist_media_asset_source_size CHECK (source_byte_size > 0),
  CONSTRAINT ck_artist_media_asset_size CHECK (byte_size > 0),
  CONSTRAINT ck_artist_media_asset_source_hash
    CHECK (source_content_hash ~ '^[0-9a-f]{64}([0-9a-f]{64})?$'),
  CONSTRAINT ck_artist_media_asset_hash
    CHECK (content_hash ~ '^[0-9a-f]{64}([0-9a-f]{64})?$'),
  CONSTRAINT ck_artist_media_asset_source_mime
    CHECK (source_mime_type IN ('image/jpeg','image/png','image/avif','image/webp')),
  CONSTRAINT ck_artist_media_asset_mime
    CHECK (mime_type IN ('image/avif','image/webp')),
  CONSTRAINT ck_artist_media_asset_rights
    CHECK (rights_status IN ('authorized','licensed'))
);

CREATE INDEX IF NOT EXISTS idx_artist_media_asset_artist
  ON artist_media_asset (artist_party_id, asset_kind, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_artist_media_asset_hash
  ON artist_media_asset (content_hash);

COMMIT;
