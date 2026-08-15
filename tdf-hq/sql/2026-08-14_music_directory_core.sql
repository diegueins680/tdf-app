-- TDF music directory and classifieds: additive, idempotent schema.
-- This migration never deletes or rewrites legacy Party, artist, venue, event,
-- chat, marketplace, booking, order, or DDEX records.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Search extensions improve ranking but are not required. Managed PostgreSQL
-- roles may be unable to install them; the deterministic fallback below keeps
-- development and restricted environments functional.
DO $$
BEGIN
  BEGIN
    CREATE EXTENSION IF NOT EXISTS unaccent;
  EXCEPTION WHEN insufficient_privilege OR undefined_file THEN
    RAISE NOTICE 'unaccent unavailable; using directory_normalize_text fallback';
  END;
  BEGIN
    CREATE EXTENSION IF NOT EXISTS pg_trgm;
  EXCEPTION WHEN insufficient_privilege OR undefined_file THEN
    RAISE NOTICE 'pg_trgm unavailable; trigram indexes will not be installed';
  END;
END
$$;

CREATE OR REPLACE FUNCTION directory_normalize_text(input_value TEXT)
RETURNS TEXT
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT trim(regexp_replace(
    translate(
      lower(coalesce(input_value, '')),
      'áàäâãéèëêíìïîóòöôõúùüûñç',
      'aaaaaeeeeiiiiooooouuuunc'
    ),
    '[^a-z0-9]+',
    ' ',
    'g'
  ));
$$;

CREATE OR REPLACE FUNCTION directory_stable_uuid(namespace_value TEXT, source_value TEXT)
RETURNS UUID
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT (
    substr(hash,1,8) || '-' || substr(hash,9,4) || '-5' || substr(hash,14,3) ||
    '-8' || substr(hash,18,3) || '-' || substr(hash,21,12)
  )::uuid
  FROM (SELECT md5(coalesce(namespace_value,'') || ':' || coalesce(source_value,'')) AS hash) value;
$$;

CREATE OR REPLACE FUNCTION directory_text_similarity(left_value TEXT, right_value TEXT)
RETURNS DOUBLE PRECISION
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN directory_normalize_text(right_value) = '' THEN 0
    WHEN directory_normalize_text(left_value) = directory_normalize_text(right_value) THEN 1
    WHEN directory_normalize_text(left_value) LIKE directory_normalize_text(right_value) || '%' THEN .85
    WHEN directory_normalize_text(left_value) LIKE '%' || directory_normalize_text(right_value) || '%' THEN .65
    ELSE 0
  END;
$$;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname='pg_trgm') THEN
    EXECUTE $function$
      CREATE OR REPLACE FUNCTION directory_text_similarity(left_value TEXT, right_value TEXT)
      RETURNS DOUBLE PRECISION LANGUAGE sql IMMUTABLE PARALLEL SAFE
      AS 'SELECT similarity(directory_normalize_text($1),directory_normalize_text($2))'
    $function$;
  END IF;
END
$$;

-- Generic localization layer for governed catalog entities. Existing ES/EN
-- columns remain compatible; PT and future locales use this table.
CREATE TABLE IF NOT EXISTS catalog_item_translation (
  catalog_id UUID NOT NULL REFERENCES catalog_definition(id),
  entity_id UUID NOT NULL,
  locale_id UUID NOT NULL REFERENCES locale_reference(id),
  name TEXT NOT NULL,
  description TEXT,
  synonyms JSONB NOT NULL DEFAULT '[]'::jsonb,
  source TEXT NOT NULL DEFAULT 'manual',
  created_by BIGINT REFERENCES party(id),
  approved_by BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (catalog_id, entity_id, locale_id),
  CHECK (length(trim(name)) BETWEEN 1 AND 160),
  CHECK (jsonb_typeof(synonyms) = 'array')
);

-- New domain catalogs. They deliberately have no foreign key to security_role
-- or security_permission: public professions cannot grant internal access.
CREATE TABLE IF NOT EXISTS profession (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id UUID NOT NULL REFERENCES catalog_definition(id),
  code TEXT NOT NULL UNIQUE,
  parent_id UUID REFERENCES profession(id),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  description_es TEXT,
  description_en TEXT,
  current_slug TEXT UNIQUE,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  sort_order INTEGER NOT NULL DEFAULT 0,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  workflow_state_id UUID NOT NULL REFERENCES workflow_state(id),
  created_by BIGINT REFERENCES party(id),
  updated_by BIGINT REFERENCES party(id),
  approved_by BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deprecated_at TIMESTAMPTZ,
  replacement_id UUID REFERENCES profession(id),
  source_name TEXT,
  source_version TEXT,
  version INTEGER NOT NULL DEFAULT 1,
  CHECK (code ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (current_slug IS NULL OR current_slug ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (jsonb_typeof(metadata) = 'object'),
  CHECK (replacement_id IS NULL OR replacement_id <> id)
);

CREATE TABLE IF NOT EXISTS classified_category (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id UUID NOT NULL REFERENCES catalog_definition(id),
  code TEXT NOT NULL UNIQUE,
  parent_id UUID REFERENCES classified_category(id),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  description_es TEXT,
  description_en TEXT,
  current_slug TEXT UNIQUE,
  requirements JSONB NOT NULL DEFAULT '{}'::jsonb,
  sort_order INTEGER NOT NULL DEFAULT 0,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  workflow_state_id UUID NOT NULL REFERENCES workflow_state(id),
  created_by BIGINT REFERENCES party(id),
  updated_by BIGINT REFERENCES party(id),
  approved_by BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deprecated_at TIMESTAMPTZ,
  replacement_id UUID REFERENCES classified_category(id),
  source_name TEXT,
  source_version TEXT,
  version INTEGER NOT NULL DEFAULT 1,
  CHECK (code ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (current_slug IS NULL OR current_slug ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (jsonb_typeof(requirements) = 'object'),
  CHECK (replacement_id IS NULL OR replacement_id <> id)
);

CREATE TABLE IF NOT EXISTS compensation_type (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id UUID NOT NULL REFERENCES catalog_definition(id),
  code TEXT NOT NULL UNIQUE,
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  description_es TEXT,
  description_en TEXT,
  current_slug TEXT UNIQUE,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  sort_order INTEGER NOT NULL DEFAULT 0,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  workflow_state_id UUID NOT NULL REFERENCES workflow_state(id),
  created_by BIGINT REFERENCES party(id),
  updated_by BIGINT REFERENCES party(id),
  approved_by BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deprecated_at TIMESTAMPTZ,
  replacement_id UUID REFERENCES compensation_type(id),
  source_name TEXT,
  source_version TEXT,
  version INTEGER NOT NULL DEFAULT 1,
  CHECK (code ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (current_slug IS NULL OR current_slug ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (jsonb_typeof(metadata) = 'object'),
  CHECK (replacement_id IS NULL OR replacement_id <> id)
);

CREATE TABLE IF NOT EXISTS metropolitan_area (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  catalog_id UUID NOT NULL REFERENCES catalog_definition(id),
  country_id UUID NOT NULL REFERENCES country_reference(id),
  subdivision_id UUID REFERENCES subdivision_reference(id),
  code TEXT NOT NULL UNIQUE,
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  description_es TEXT,
  description_en TEXT,
  current_slug TEXT UNIQUE,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION,
  sort_order INTEGER NOT NULL DEFAULT 0,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  workflow_state_id UUID NOT NULL REFERENCES workflow_state(id),
  source_name TEXT NOT NULL,
  source_version TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  deprecated_at TIMESTAMPTZ,
  replacement_id UUID REFERENCES metropolitan_area(id),
  version INTEGER NOT NULL DEFAULT 1,
  CHECK (code ~ '^[a-z0-9][a-z0-9-]{1,79}$'),
  CHECK (latitude IS NULL OR latitude BETWEEN -90 AND 90),
  CHECK (longitude IS NULL OR longitude BETWEEN -180 AND 180),
  CHECK (replacement_id IS NULL OR replacement_id <> id)
);

CREATE TABLE IF NOT EXISTS metropolitan_area_city (
  metropolitan_area_id UUID NOT NULL REFERENCES metropolitan_area(id) ON DELETE CASCADE,
  city_id UUID NOT NULL REFERENCES city_reference(id),
  primary_city BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (metropolitan_area_id, city_id)
);

-- Public profiles are projections over Party. Private Party fields are never
-- copied into this table.
CREATE TABLE IF NOT EXISTS directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_kind TEXT NOT NULL,
  public_name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  bio TEXT,
  experience_summary TEXT,
  credits_summary TEXT,
  portfolio JSONB NOT NULL DEFAULT '[]'::jsonb,
  links JSONB NOT NULL DEFAULT '[]'::jsonb,
  equipment_summary TEXT,
  rate_min_minor BIGINT,
  rate_max_minor BIGINT,
  currency_id UUID REFERENCES currency_reference(id),
  availability_status TEXT NOT NULL DEFAULT 'ask',
  onsite BOOLEAN NOT NULL DEFAULT TRUE,
  remote BOOLEAN NOT NULL DEFAULT FALSE,
  available_to_travel BOOLEAN NOT NULL DEFAULT FALSE,
  travel_radius_km NUMERIC(8,2),
  profile_status TEXT NOT NULL DEFAULT 'draft',
  visibility TEXT NOT NULL DEFAULT 'public',
  moderation_status TEXT NOT NULL DEFAULT 'allowed',
  completeness_score NUMERIC(5,4) NOT NULL DEFAULT 0,
  response_rate NUMERIC(5,4),
  median_response_minutes INTEGER,
  completed_interactions INTEGER NOT NULL DEFAULT 0,
  review_average NUMERIC(3,2),
  review_count INTEGER NOT NULL DEFAULT 0,
  public_contact_enabled BOOLEAN NOT NULL DEFAULT TRUE,
  canonical_profile_id UUID REFERENCES directory_profile(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  published_at TIMESTAMPTZ,
  archived_at TIMESTAMPTZ,
  suspended_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  CHECK (profile_kind IN ('person','artist','band','project','organization','company','venue','studio','agency','label','distributor','school')),
  CHECK (profile_status IN ('draft','pending_review','published','paused','archived','suspended','merged')),
  CHECK (visibility IN ('public','unlisted','private')),
  CHECK (moderation_status IN ('allowed','pending','blocked')),
  CHECK (availability_status IN ('available','limited','unavailable','ask')),
  CHECK (length(trim(public_name)) BETWEEN 1 AND 160),
  CHECK (slug ~ '^[a-z0-9][a-z0-9-]{1,119}$'),
  CHECK (jsonb_typeof(portfolio) = 'array'),
  CHECK (jsonb_typeof(links) = 'array'),
  CHECK (rate_min_minor IS NULL OR rate_min_minor >= 0),
  CHECK (rate_max_minor IS NULL OR rate_max_minor >= rate_min_minor),
  CHECK ((rate_min_minor IS NULL AND rate_max_minor IS NULL) OR currency_id IS NOT NULL),
  CHECK (travel_radius_km IS NULL OR travel_radius_km BETWEEN 0 AND 20000),
  CHECK (completeness_score BETWEEN 0 AND 1),
  CHECK (response_rate IS NULL OR response_rate BETWEEN 0 AND 1),
  CHECK (median_response_minutes IS NULL OR median_response_minutes >= 0),
  CHECK (completed_interactions >= 0),
  CHECK (review_average IS NULL OR review_average BETWEEN 1 AND 5),
  CHECK (review_count >= 0),
  CHECK (canonical_profile_id IS NULL OR canonical_profile_id <> id)
);

CREATE INDEX IF NOT EXISTS directory_profile_subject_idx
  ON directory_profile (subject_party_id, profile_status, updated_at DESC);
CREATE INDEX IF NOT EXISTS directory_profile_public_idx
  ON directory_profile (profile_status, visibility, moderation_status, updated_at DESC, id)
  WHERE profile_status = 'published' AND visibility = 'public' AND moderation_status = 'allowed';

CREATE TABLE IF NOT EXISTS directory_backfill_run (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  run_code TEXT NOT NULL,
  source_revision TEXT NOT NULL,
  dry_run BOOLEAN NOT NULL DEFAULT TRUE,
  status TEXT NOT NULL DEFAULT 'running',
  scanned_rows BIGINT NOT NULL DEFAULT 0,
  mapped_rows BIGINT NOT NULL DEFAULT 0,
  created_rows BIGINT NOT NULL DEFAULT 0,
  ambiguous_rows BIGINT NOT NULL DEFAULT 0,
  rejected_rows BIGINT NOT NULL DEFAULT 0,
  report JSONB NOT NULL DEFAULT '{}'::jsonb,
  started_by BIGINT REFERENCES party(id),
  started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  completed_at TIMESTAMPTZ,
  correlation_id TEXT NOT NULL,
  UNIQUE (run_code, source_revision, dry_run),
  CHECK (status IN ('running','completed','failed','reversed')),
  CHECK (scanned_rows >= 0 AND mapped_rows >= 0 AND created_rows >= 0 AND ambiguous_rows >= 0 AND rejected_rows >= 0),
  CHECK (jsonb_typeof(report) = 'object')
);

CREATE TABLE IF NOT EXISTS directory_profile_manager (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  can_view_private BOOLEAN NOT NULL DEFAULT TRUE,
  can_edit BOOLEAN NOT NULL DEFAULT FALSE,
  can_publish BOOLEAN NOT NULL DEFAULT FALSE,
  can_contact BOOLEAN NOT NULL DEFAULT FALSE,
  can_manage BOOLEAN NOT NULL DEFAULT FALSE,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  granted_by BIGINT REFERENCES party(id),
  source_claim_id UUID,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  revoked_at TIMESTAMPTZ,
  version INTEGER NOT NULL DEFAULT 1,
  PRIMARY KEY (profile_id, account_party_id),
  CHECK (active OR revoked_at IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS directory_profile_manager_account_idx
  ON directory_profile_manager (account_party_id, profile_id) WHERE active;

CREATE TABLE IF NOT EXISTS directory_profile_membership (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  member_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  organization_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  profession_id UUID REFERENCES profession(id),
  title TEXT,
  starts_on DATE,
  ends_on DATE,
  public BOOLEAN NOT NULL DEFAULT TRUE,
  approved_by_organization BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (member_profile_id <> organization_profile_id),
  CHECK (ends_on IS NULL OR starts_on IS NULL OR ends_on >= starts_on)
);
CREATE UNIQUE INDEX IF NOT EXISTS directory_profile_membership_uidx
  ON directory_profile_membership (
    member_profile_id,
    organization_profile_id,
    coalesce(profession_id, '00000000-0000-0000-0000-000000000000'::uuid)
  );

CREATE TABLE IF NOT EXISTS directory_legacy_link (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  legacy_kind TEXT NOT NULL,
  legacy_id TEXT NOT NULL,
  source_table TEXT NOT NULL,
  source_slug TEXT,
  source_url TEXT,
  provenance JSONB NOT NULL DEFAULT '{}'::jsonb,
  backfill_run_id UUID REFERENCES directory_backfill_run(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (legacy_kind, legacy_id),
  UNIQUE (profile_id, legacy_kind),
  CHECK (legacy_kind IN ('artist_profile','social_artist_profile','band','venue','service_ad','party')),
  CHECK (jsonb_typeof(provenance) = 'object')
);

-- One row per inspected legacy record makes dry-runs and applied backfills
-- reproducible. Ambiguous records are retained for review instead of being
-- silently attached to a Party or public profile.
CREATE TABLE IF NOT EXISTS directory_backfill_mapping (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  backfill_run_id UUID NOT NULL REFERENCES directory_backfill_run(id) ON DELETE CASCADE,
  source_table TEXT NOT NULL,
  source_id TEXT NOT NULL,
  source_slug TEXT,
  disposition TEXT NOT NULL,
  target_profile_id UUID REFERENCES directory_profile(id),
  reason_code TEXT,
  evidence JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (backfill_run_id, source_table, source_id),
  CHECK (disposition IN ('planned','mapped','created','ambiguous','skipped','rejected','reversed')),
  CHECK (disposition NOT IN ('mapped','created') OR target_profile_id IS NOT NULL),
  CHECK (jsonb_typeof(evidence) = 'object')
);
ALTER TABLE directory_backfill_mapping
  DROP CONSTRAINT IF EXISTS directory_backfill_mapping_disposition_check;
ALTER TABLE directory_backfill_mapping
  ADD CONSTRAINT directory_backfill_mapping_disposition_check
  CHECK (disposition IN ('planned','mapped','created','ambiguous','skipped','rejected','reversed')) NOT VALID;
ALTER TABLE directory_backfill_mapping
  VALIDATE CONSTRAINT directory_backfill_mapping_disposition_check;
CREATE INDEX IF NOT EXISTS directory_backfill_mapping_review_idx
  ON directory_backfill_mapping (disposition, source_table, source_id)
  WHERE disposition IN ('ambiguous','rejected');

CREATE TABLE IF NOT EXISTS directory_merge_operation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  source_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  target_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  status TEXT NOT NULL DEFAULT 'planned',
  reason TEXT NOT NULL,
  before_counts JSONB NOT NULL DEFAULT '{}'::jsonb,
  after_counts JSONB,
  requested_by BIGINT NOT NULL REFERENCES party(id),
  approved_by BIGINT REFERENCES party(id),
  executed_at TIMESTAMPTZ,
  reversed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  correlation_id TEXT NOT NULL UNIQUE,
  CHECK (source_profile_id <> target_profile_id),
  CHECK (status IN ('planned','approved','executed','reversed','rejected')),
  CHECK (jsonb_typeof(before_counts) = 'object'),
  CHECK (after_counts IS NULL OR jsonb_typeof(after_counts) = 'object')
);

CREATE TABLE IF NOT EXISTS directory_profile_profession (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  profession_id UUID NOT NULL REFERENCES profession(id),
  headline TEXT,
  years_experience NUMERIC(5,2),
  rate_min_minor BIGINT,
  rate_max_minor BIGINT,
  currency_id UUID REFERENCES currency_reference(id),
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id, profession_id),
  CHECK (years_experience IS NULL OR years_experience BETWEEN 0 AND 100),
  CHECK (rate_min_minor IS NULL OR rate_min_minor >= 0),
  CHECK (rate_max_minor IS NULL OR rate_max_minor >= rate_min_minor),
  CHECK ((rate_min_minor IS NULL AND rate_max_minor IS NULL) OR currency_id IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS directory_profile_instrument (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  instrument_id UUID NOT NULL REFERENCES instrument(id),
  proficiency TEXT,
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id, instrument_id),
  CHECK (proficiency IS NULL OR proficiency IN ('beginner','intermediate','advanced','professional'))
);

CREATE TABLE IF NOT EXISTS directory_profile_genre (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  genre_id UUID NOT NULL REFERENCES genre(id),
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id, genre_id)
);

CREATE TABLE IF NOT EXISTS directory_profile_service (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  service_offering_id UUID NOT NULL REFERENCES service_offering(id),
  service_ad_id BIGINT REFERENCES service_ad(id),
  bookable BOOLEAN NOT NULL DEFAULT FALSE,
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id, service_offering_id)
);

CREATE TABLE IF NOT EXISTS directory_profile_language (
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  language_id UUID NOT NULL REFERENCES language_reference(id),
  proficiency TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (profile_id, language_id),
  CHECK (proficiency IS NULL OR proficiency IN ('basic','conversational','professional','native'))
);

CREATE TABLE IF NOT EXISTS directory_profile_location (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  country_id UUID NOT NULL REFERENCES country_reference(id),
  subdivision_id UUID REFERENCES subdivision_reference(id),
  city_id UUID REFERENCES city_reference(id),
  metropolitan_area_id UUID REFERENCES metropolitan_area(id),
  sector_label TEXT,
  service_radius_km NUMERIC(8,2),
  public_latitude DOUBLE PRECISION,
  public_longitude DOUBLE PRECISION,
  precision TEXT NOT NULL DEFAULT 'city',
  primary_location BOOLEAN NOT NULL DEFAULT FALSE,
  onsite BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (service_radius_km IS NULL OR service_radius_km BETWEEN 0 AND 20000),
  CHECK (public_latitude IS NULL OR public_latitude BETWEEN -90 AND 90),
  CHECK (public_longitude IS NULL OR public_longitude BETWEEN -180 AND 180),
  CHECK ((public_latitude IS NULL) = (public_longitude IS NULL)),
  CHECK (precision IN ('country','region','metro','city','sector','commercial_exact')),
  CHECK (precision <> 'commercial_exact' OR public_latitude IS NOT NULL)
);
CREATE UNIQUE INDEX IF NOT EXISTS directory_profile_primary_location_uidx
  ON directory_profile_location (profile_id) WHERE primary_location;
CREATE UNIQUE INDEX IF NOT EXISTS directory_profile_location_scope_uidx
  ON directory_profile_location (
    profile_id,country_id,
    coalesce(subdivision_id,'00000000-0000-0000-0000-000000000000'::uuid),
    coalesce(city_id,'00000000-0000-0000-0000-000000000000'::uuid),
    coalesce(metropolitan_area_id,'00000000-0000-0000-0000-000000000000'::uuid),
    coalesce(sector_label,'')
  );
CREATE INDEX IF NOT EXISTS directory_profile_location_city_idx
  ON directory_profile_location (city_id, profile_id);

CREATE TABLE IF NOT EXISTS directory_private_location (
  profile_location_id UUID PRIMARY KEY REFERENCES directory_profile_location(id) ON DELETE CASCADE,
  exact_address TEXT,
  private_latitude DOUBLE PRECISION,
  private_longitude DOUBLE PRECISION,
  access_reason TEXT NOT NULL,
  created_by BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (private_latitude IS NULL OR private_latitude BETWEEN -90 AND 90),
  CHECK (private_longitude IS NULL OR private_longitude BETWEEN -180 AND 180),
  CHECK ((private_latitude IS NULL) = (private_longitude IS NULL)),
  CHECK (length(trim(access_reason)) BETWEEN 10 AND 500)
);

CREATE TABLE IF NOT EXISTS directory_profile_credit (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  credit_name TEXT NOT NULL,
  credit_role TEXT,
  claim_status TEXT NOT NULL DEFAULT 'declared',
  release_id UUID REFERENCES record_release(id),
  recording_id UUID REFERENCES recording(id),
  contributor_id UUID REFERENCES record_contributor(id),
  evidence_url TEXT,
  verified_by BIGINT REFERENCES party(id),
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (claim_status IN ('declared','claimed','verified','rejected')),
  CHECK ((claim_status = 'verified') = (verified_by IS NOT NULL AND verified_at IS NOT NULL))
);

CREATE TABLE IF NOT EXISTS directory_age_assurance (
  account_party_id BIGINT PRIMARY KEY REFERENCES party(id),
  assurance_status TEXT NOT NULL DEFAULT 'unknown',
  guardian_party_id BIGINT REFERENCES party(id),
  guardian_consent_status TEXT,
  evidence_reference TEXT,
  verified_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (assurance_status IN ('unknown','adult_attested','adult_verified','minor_restricted','guardian_pending','guardian_approved')),
  CHECK (guardian_consent_status IS NULL OR guardian_consent_status IN ('pending','approved','revoked','expired')),
  CHECK (assurance_status NOT IN ('guardian_pending','guardian_approved') OR guardian_party_id IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS directory_contact_preference (
  profile_id UUID PRIMARY KEY REFERENCES directory_profile(id) ON DELETE CASCADE,
  allow_profile_contacts BOOLEAN NOT NULL DEFAULT TRUE,
  allow_classified_applications BOOLEAN NOT NULL DEFAULT TRUE,
  allow_direct_invitations BOOLEAN NOT NULL DEFAULT TRUE,
  minimum_profile_completeness NUMERIC(5,4) NOT NULL DEFAULT 0,
  email_notifications BOOLEAN NOT NULL DEFAULT TRUE,
  push_notifications BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (minimum_profile_completeness BETWEEN 0 AND 1)
);

CREATE TABLE IF NOT EXISTS directory_profile_block (
  blocker_profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  blocked_profile_id UUID NOT NULL REFERENCES directory_profile(id) ON DELETE CASCADE,
  created_by BIGINT NOT NULL REFERENCES party(id),
  reason TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (blocker_profile_id, blocked_profile_id),
  CHECK (blocker_profile_id <> blocked_profile_id)
);

CREATE TABLE IF NOT EXISTS classified (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  author_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  category_id UUID NOT NULL REFERENCES classified_category(id),
  title TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  description TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft',
  moderation_status TEXT NOT NULL DEFAULT 'allowed',
  onsite BOOLEAN NOT NULL DEFAULT TRUE,
  remote BOOLEAN NOT NULL DEFAULT FALSE,
  available_to_travel BOOLEAN NOT NULL DEFAULT FALSE,
  service_radius_km NUMERIC(8,2),
  starts_at TIMESTAMPTZ,
  ends_at TIMESTAMPTZ,
  experience_level TEXT,
  compensation_type_id UUID REFERENCES compensation_type(id),
  budget_min_minor BIGINT,
  budget_max_minor BIGINT,
  currency_id UUID REFERENCES currency_reference(id),
  budget_negotiable BOOLEAN NOT NULL DEFAULT FALSE,
  service_offering_id UUID REFERENCES service_offering(id),
  service_ad_id BIGINT REFERENCES service_ad(id),
  expires_at TIMESTAMPTZ,
  published_at TIMESTAMPTZ,
  closed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  duplicate_fingerprint TEXT,
  CHECK (status IN ('draft','pending_moderation','published','paused','filled','expired','withdrawn','rejected','moderated')),
  CHECK (moderation_status IN ('allowed','pending','blocked')),
  CHECK (length(trim(title)) BETWEEN 5 AND 160),
  CHECK (length(trim(description)) BETWEEN 20 AND 10000),
  CHECK (slug ~ '^[a-z0-9][a-z0-9-]{1,159}$'),
  CHECK (onsite OR remote OR available_to_travel),
  CHECK (service_radius_km IS NULL OR service_radius_km BETWEEN 0 AND 20000),
  CHECK (ends_at IS NULL OR starts_at IS NULL OR ends_at >= starts_at),
  CHECK (experience_level IS NULL OR experience_level IN ('any','beginner','intermediate','advanced','professional')),
  CHECK (budget_min_minor IS NULL OR budget_min_minor >= 0),
  CHECK (budget_max_minor IS NULL OR budget_max_minor >= budget_min_minor),
  CHECK ((budget_min_minor IS NULL AND budget_max_minor IS NULL) OR currency_id IS NOT NULL),
  CHECK (expires_at IS NULL OR expires_at > created_at)
);
CREATE INDEX IF NOT EXISTS classified_public_idx
  ON classified (status, moderation_status, expires_at, published_at DESC, id)
  WHERE status = 'published' AND moderation_status = 'allowed';
CREATE UNIQUE INDEX IF NOT EXISTS classified_active_fingerprint_uidx
  ON classified (author_profile_id, duplicate_fingerprint)
  WHERE duplicate_fingerprint IS NOT NULL AND status IN ('pending_moderation','published','paused');

CREATE TABLE IF NOT EXISTS classified_profession (
  classified_id UUID NOT NULL REFERENCES classified(id) ON DELETE CASCADE,
  profession_id UUID NOT NULL REFERENCES profession(id),
  PRIMARY KEY (classified_id, profession_id)
);
CREATE TABLE IF NOT EXISTS classified_instrument (
  classified_id UUID NOT NULL REFERENCES classified(id) ON DELETE CASCADE,
  instrument_id UUID NOT NULL REFERENCES instrument(id),
  PRIMARY KEY (classified_id, instrument_id)
);
CREATE TABLE IF NOT EXISTS classified_genre (
  classified_id UUID NOT NULL REFERENCES classified(id) ON DELETE CASCADE,
  genre_id UUID NOT NULL REFERENCES genre(id),
  PRIMARY KEY (classified_id, genre_id)
);
CREATE TABLE IF NOT EXISTS classified_location (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  classified_id UUID NOT NULL REFERENCES classified(id) ON DELETE CASCADE,
  country_id UUID NOT NULL REFERENCES country_reference(id),
  subdivision_id UUID REFERENCES subdivision_reference(id),
  city_id UUID REFERENCES city_reference(id),
  metropolitan_area_id UUID REFERENCES metropolitan_area(id),
  service_radius_km NUMERIC(8,2),
  CHECK (service_radius_km IS NULL OR service_radius_km BETWEEN 0 AND 20000)
);
CREATE UNIQUE INDEX IF NOT EXISTS classified_location_scope_uidx
  ON classified_location (
    classified_id,country_id,
    coalesce(subdivision_id,'00000000-0000-0000-0000-000000000000'::uuid),
    coalesce(city_id,'00000000-0000-0000-0000-000000000000'::uuid),
    coalesce(metropolitan_area_id,'00000000-0000-0000-0000-000000000000'::uuid)
  );
CREATE INDEX IF NOT EXISTS classified_location_city_idx ON classified_location (city_id, classified_id);

CREATE TABLE IF NOT EXISTS classified_attachment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  classified_id UUID NOT NULL REFERENCES classified(id) ON DELETE CASCADE,
  asset_url TEXT NOT NULL,
  media_type TEXT NOT NULL,
  mime_type TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  checksum_sha256 TEXT NOT NULL,
  scan_status TEXT NOT NULL DEFAULT 'pending',
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (media_type IN ('image','audio','document','reference')),
  CHECK (size_bytes BETWEEN 1 AND 26214400),
  CHECK (checksum_sha256 ~ '^[a-f0-9]{64}$'),
  CHECK (scan_status IN ('pending','clean','rejected','failed'))
);

CREATE TABLE IF NOT EXISTS classified_application (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  classified_id UUID NOT NULL REFERENCES classified(id),
  applicant_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  message TEXT NOT NULL,
  portfolio JSONB NOT NULL DEFAULT '[]'::jsonb,
  availability_text TEXT,
  proposed_amount_minor BIGINT,
  currency_id UUID REFERENCES currency_reference(id),
  status TEXT NOT NULL DEFAULT 'submitted',
  idempotency_key TEXT NOT NULL,
  request_fingerprint TEXT NOT NULL,
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  withdrawn_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  UNIQUE (classified_id, applicant_profile_id, idempotency_key),
  CHECK (status IN ('submitted','viewed','shortlisted','accepted','rejected','withdrawn','conversation_open','converted')),
  CHECK (length(trim(message)) BETWEEN 10 AND 5000),
  CHECK (jsonb_typeof(portfolio) = 'array'),
  CHECK (proposed_amount_minor IS NULL OR proposed_amount_minor >= 0),
  CHECK (proposed_amount_minor IS NULL OR currency_id IS NOT NULL)
);
CREATE INDEX IF NOT EXISTS classified_application_classified_idx
  ON classified_application (classified_id, status, submitted_at DESC, id);
CREATE INDEX IF NOT EXISTS classified_application_profile_idx
  ON classified_application (applicant_profile_id, status, submitted_at DESC, id);

CREATE TABLE IF NOT EXISTS directory_invitation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  sender_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  target_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  classified_id UUID REFERENCES classified(id),
  message TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  idempotency_key TEXT NOT NULL,
  request_fingerprint TEXT NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL DEFAULT (now() + interval '30 days'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  UNIQUE (sender_profile_id, target_profile_id, classified_id, idempotency_key),
  CHECK (sender_profile_id <> target_profile_id),
  CHECK (status IN ('pending','accepted','declined','expired','withdrawn','blocked','conversation_open','converted')),
  CHECK (length(trim(message)) BETWEEN 10 AND 5000),
  CHECK (expires_at > created_at)
);

CREATE TABLE IF NOT EXISTS directory_conversation_context (
  chat_thread_id BIGINT NOT NULL REFERENCES chat_thread(id) ON DELETE CASCADE,
  context_kind TEXT NOT NULL,
  context_id UUID NOT NULL,
  idempotency_resource_id UUID,
  created_by BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (chat_thread_id, context_kind, context_id),
  CHECK (context_kind IN ('profile','classified','application','invitation'))
);
ALTER TABLE directory_conversation_context
  ADD COLUMN IF NOT EXISTS idempotency_resource_id UUID;
CREATE UNIQUE INDEX IF NOT EXISTS directory_conversation_context_idempotency_uidx
  ON directory_conversation_context(idempotency_resource_id)
  WHERE idempotency_resource_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS directory_favorite (
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  target_kind TEXT NOT NULL,
  target_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (account_party_id, target_kind, target_id),
  CHECK (target_kind IN ('profile','classified','event','venue')),
  CHECK (length(trim(target_id)) BETWEEN 1 AND 160)
);

CREATE TABLE IF NOT EXISTS directory_saved_search (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  name TEXT NOT NULL,
  canonical_query JSONB NOT NULL,
  query_hash TEXT NOT NULL,
  alerts_enabled BOOLEAN NOT NULL DEFAULT TRUE,
  alert_frequency TEXT NOT NULL DEFAULT 'daily',
  last_evaluated_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (account_party_id, query_hash),
  CHECK (length(trim(name)) BETWEEN 1 AND 120),
  CHECK (jsonb_typeof(canonical_query) = 'object'),
  CHECK (query_hash ~ '^[a-f0-9]{64}$'),
  CHECK (alert_frequency IN ('instant','daily','weekly','off'))
);

CREATE TABLE IF NOT EXISTS directory_alert_delivery (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  saved_search_id UUID NOT NULL REFERENCES directory_saved_search(id) ON DELETE CASCADE,
  result_kind TEXT NOT NULL,
  result_id TEXT NOT NULL,
  result_version BIGINT NOT NULL,
  internal_notification_id BIGINT REFERENCES notification(id),
  email_status TEXT NOT NULL DEFAULT 'disabled',
  push_status TEXT NOT NULL DEFAULT 'disabled',
  matched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  delivered_at TIMESTAMPTZ,
  UNIQUE (saved_search_id, result_kind, result_id, result_version),
  CHECK (result_kind IN ('profile','classified','event','venue')),
  CHECK (email_status IN ('disabled','pending','sent','failed')),
  CHECK (push_status IN ('disabled','pending','sent','failed'))
);

CREATE TABLE IF NOT EXISTS directory_claim (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  claimant_party_id BIGINT NOT NULL REFERENCES party(id),
  claim_type TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'draft',
  evidence JSONB NOT NULL DEFAULT '[]'::jsonb,
  reviewer_party_id BIGINT REFERENCES party(id),
  reviewer_notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  submitted_at TIMESTAMPTZ,
  reviewed_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  CHECK (claim_type IN ('profile','organization','venue','administration','credit')),
  CHECK (status IN ('draft','submitted','under_review','more_evidence_requested','approved','rejected','withdrawn')),
  CHECK (jsonb_typeof(evidence) = 'array'),
  CHECK (status <> 'approved' OR (reviewer_party_id IS NOT NULL AND reviewed_at IS NOT NULL))
);
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'directory_profile_manager_source_claim_fk'
      AND conrelid = 'directory_profile_manager'::regclass
  ) THEN
    ALTER TABLE directory_profile_manager
      ADD CONSTRAINT directory_profile_manager_source_claim_fk
      FOREIGN KEY (source_claim_id) REFERENCES directory_claim(id);
  END IF;
END
$$;

CREATE TABLE IF NOT EXISTS directory_verification (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  verification_type TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending',
  scope TEXT,
  evidence JSONB NOT NULL DEFAULT '[]'::jsonb,
  evidence_reference TEXT,
  reviewer_party_id BIGINT REFERENCES party(id),
  reviewer_notes TEXT,
  verified_by BIGINT REFERENCES party(id),
  verified_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1,
  CHECK (verification_type IN ('identity','organization','venue','ownership','administration','professional_credit')),
  CHECK (status IN ('pending','submitted','under_review','verified','rejected','expired','revoked')),
  CHECK (jsonb_typeof(evidence) = 'array'),
  CHECK (status <> 'verified' OR (verified_by IS NOT NULL AND verified_at IS NOT NULL))
);
CREATE UNIQUE INDEX IF NOT EXISTS directory_verification_scope_uidx
  ON directory_verification(profile_id,verification_type,coalesce(scope,''));

CREATE TABLE IF NOT EXISTS directory_interaction (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  interaction_kind TEXT NOT NULL,
  external_id TEXT NOT NULL,
  profile_a_id UUID NOT NULL REFERENCES directory_profile(id),
  profile_b_id UUID NOT NULL REFERENCES directory_profile(id),
  status TEXT NOT NULL,
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (interaction_kind, external_id),
  CHECK (interaction_kind IN ('booking','service_order','marketplace_order','event_collaboration','confirmed_collaboration')),
  CHECK (status IN ('pending','confirmed','completed','cancelled','disputed')),
  CHECK (profile_a_id <> profile_b_id),
  CHECK (status NOT IN ('confirmed','completed') OR verified_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS directory_review (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  interaction_id UUID NOT NULL REFERENCES directory_interaction(id),
  author_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  subject_profile_id UUID NOT NULL REFERENCES directory_profile(id),
  rating SMALLINT NOT NULL,
  body TEXT,
  status TEXT NOT NULL DEFAULT 'published',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (interaction_id, author_profile_id, subject_profile_id),
  CHECK (rating BETWEEN 1 AND 5),
  CHECK (author_profile_id <> subject_profile_id),
  CHECK (status IN ('pending','published','hidden','removed')),
  CHECK (body IS NULL OR length(trim(body)) BETWEEN 10 AND 2000)
);

CREATE TABLE IF NOT EXISTS directory_moderation_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  reporter_party_id BIGINT NOT NULL REFERENCES party(id),
  target_kind TEXT NOT NULL,
  target_id TEXT NOT NULL,
  reason_code TEXT NOT NULL,
  details TEXT,
  status TEXT NOT NULL DEFAULT 'open',
  duplicate_fingerprint TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (reporter_party_id, target_kind, target_id, reason_code),
  CHECK (target_kind IN ('profile','classified','application','invitation','event','venue','message')),
  CHECK (status IN ('open','linked','dismissed','actioned')),
  CHECK (details IS NULL OR length(trim(details)) BETWEEN 10 AND 3000)
);

CREATE TABLE IF NOT EXISTS directory_moderation_case (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  target_kind TEXT NOT NULL,
  target_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'open',
  priority TEXT NOT NULL DEFAULT 'normal',
  assigned_to BIGINT REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  closed_at TIMESTAMPTZ,
  version BIGINT NOT NULL DEFAULT 1,
  CHECK (status IN ('open','triaged','under_review','actioned','appealed','appeal_review','reversed','dismissed','closed')),
  CHECK (priority IN ('low','normal','high','urgent'))
);
CREATE UNIQUE INDEX IF NOT EXISTS directory_moderation_active_case_uidx
  ON directory_moderation_case(target_kind,target_id)
  WHERE status IN ('open','triaged','under_review','actioned','appealed','appeal_review');

CREATE TABLE IF NOT EXISTS directory_moderation_decision (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  case_id UUID NOT NULL REFERENCES directory_moderation_case(id),
  decision TEXT NOT NULL,
  reason_code TEXT NOT NULL,
  notes TEXT NOT NULL,
  actor_party_id BIGINT NOT NULL REFERENCES party(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (decision IN ('dismiss','warn','pause','remove','suspend','merge','reverse','close')),
  CHECK (length(trim(notes)) BETWEEN 10 AND 5000)
);

CREATE TABLE IF NOT EXISTS directory_rate_limit (
  scope TEXT NOT NULL,
  subject_hash TEXT NOT NULL,
  window_started_at TIMESTAMPTZ NOT NULL,
  count INTEGER NOT NULL DEFAULT 1,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (scope, subject_hash, window_started_at),
  CHECK (scope IN ('search','profile_create','classified_publish','application','invitation','contact','report')),
  CHECK (count > 0)
);

CREATE TABLE IF NOT EXISTS directory_idempotency (
  actor_party_id BIGINT NOT NULL REFERENCES party(id),
  operation TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  request_fingerprint TEXT NOT NULL,
  resource_kind TEXT NOT NULL,
  resource_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  expires_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (actor_party_id, operation, idempotency_key),
  CHECK (expires_at > created_at)
);

CREATE TABLE IF NOT EXISTS directory_audit_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  actor_party_id BIGINT REFERENCES party(id),
  action TEXT NOT NULL,
  entity_kind TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  previous_state TEXT,
  new_state TEXT,
  correlation_id TEXT NOT NULL,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (jsonb_typeof(metadata) = 'object'),
  CHECK (length(trim(correlation_id)) BETWEEN 8 AND 160)
);
CREATE INDEX IF NOT EXISTS directory_audit_entity_idx
  ON directory_audit_event (entity_kind, entity_id, created_at DESC);

CREATE TABLE IF NOT EXISTS directory_analytics_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  actor_scope_hash TEXT,
  event_name TEXT NOT NULL,
  entity_kind TEXT,
  entity_id_hash TEXT,
  city_id UUID REFERENCES city_reference(id),
  properties JSONB NOT NULL DEFAULT '{}'::jsonb,
  idempotency_key TEXT,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (event_name IN ('search_submitted','search_zero_results','filter_used','result_opened','profile_completed','classified_published','application_submitted','invitation_sent','contact_started','contact_accepted','first_response','match_converted','report_created','moderation_action')),
  CHECK (jsonb_typeof(properties) = 'object')
);
CREATE UNIQUE INDEX IF NOT EXISTS directory_analytics_idempotency_uidx
  ON directory_analytics_event (actor_scope_hash, event_name, idempotency_key)
  WHERE idempotency_key IS NOT NULL;

-- Search documents contain only public projection fields. Taxonomy arrays are
-- canonical UUIDs and support semantic/synonym matching without N+1 queries.
CREATE TABLE IF NOT EXISTS directory_search_document (
  entity_kind TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  slug TEXT NOT NULL,
  title TEXT NOT NULL,
  subtitle TEXT,
  summary TEXT,
  image_url TEXT,
  city_id UUID REFERENCES city_reference(id),
  city_name TEXT,
  country_code TEXT,
  public_latitude DOUBLE PRECISION,
  public_longitude DOUBLE PRECISION,
  location_precision TEXT,
  profession_ids UUID[] NOT NULL DEFAULT '{}',
  service_ids UUID[] NOT NULL DEFAULT '{}',
  instrument_ids UUID[] NOT NULL DEFAULT '{}',
  genre_ids UUID[] NOT NULL DEFAULT '{}',
  search_text TEXT NOT NULL DEFAULT '',
  search_vector TSVECTOR NOT NULL DEFAULT ''::tsvector,
  profile_completeness NUMERIC(5,4) NOT NULL DEFAULT 0,
  reputation_score NUMERIC(5,4) NOT NULL DEFAULT 0,
  availability_score NUMERIC(5,4) NOT NULL DEFAULT 0,
  onsite BOOLEAN NOT NULL DEFAULT FALSE,
  remote BOOLEAN NOT NULL DEFAULT FALSE,
  available_to_travel BOOLEAN NOT NULL DEFAULT FALSE,
  source_status TEXT NOT NULL,
  visibility TEXT NOT NULL DEFAULT 'public',
  moderation_status TEXT NOT NULL DEFAULT 'allowed',
  effective_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ,
  source_updated_at TIMESTAMPTZ NOT NULL,
  source_version BIGINT NOT NULL DEFAULT 1,
  sponsored BOOLEAN NOT NULL DEFAULT FALSE,
  sponsor_disclosure TEXT,
  PRIMARY KEY (entity_kind, entity_id),
  CHECK (entity_kind IN ('profile','classified','event','venue')),
  CHECK (public_latitude IS NULL OR public_latitude BETWEEN -90 AND 90),
  CHECK (public_longitude IS NULL OR public_longitude BETWEEN -180 AND 180),
  CHECK ((public_latitude IS NULL) = (public_longitude IS NULL)),
  CHECK (profile_completeness BETWEEN 0 AND 1),
  CHECK (reputation_score BETWEEN 0 AND 1),
  CHECK (availability_score BETWEEN 0 AND 1),
  CHECK (NOT sponsored OR length(trim(coalesce(sponsor_disclosure, ''))) >= 3)
);
CREATE INDEX IF NOT EXISTS directory_search_vector_idx
  ON directory_search_document USING GIN (search_vector);
CREATE INDEX IF NOT EXISTS directory_search_city_idx
  ON directory_search_document (city_id, source_updated_at DESC, entity_kind, entity_id);
CREATE INDEX IF NOT EXISTS directory_search_profession_idx
  ON directory_search_document USING GIN (profession_ids);
CREATE INDEX IF NOT EXISTS directory_search_service_idx
  ON directory_search_document USING GIN (service_ids);
CREATE INDEX IF NOT EXISTS directory_search_instrument_idx
  ON directory_search_document USING GIN (instrument_ids);
CREATE INDEX IF NOT EXISTS directory_search_genre_idx
  ON directory_search_document USING GIN (genre_ids);
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
    EXECUTE 'CREATE INDEX IF NOT EXISTS directory_search_text_trgm_idx ON directory_search_document USING GIN (search_text gin_trgm_ops)';
  END IF;
END
$$;

CREATE OR REPLACE VIEW directory_public_search_document AS
SELECT
  entity_kind, entity_id, slug, title, subtitle, summary, image_url,
  city_id, city_name, country_code, public_latitude, public_longitude,
  location_precision, profession_ids, service_ids, instrument_ids, genre_ids,
  search_text, search_vector, profile_completeness, reputation_score,
  availability_score, effective_at, expires_at, source_updated_at,
  source_version, sponsored, sponsor_disclosure, onsite, remote, available_to_travel
FROM directory_search_document
WHERE source_status = 'published'
  AND visibility = 'public'
  AND moderation_status = 'allowed'
  AND (effective_at IS NULL OR effective_at <= now())
  AND (expires_at IS NULL OR expires_at > now());

CREATE OR REPLACE VIEW directory_public_profile AS
SELECT
  profile.id,
  profile.profile_kind,
  profile.public_name,
  profile.slug,
  profile.bio,
  profile.experience_summary,
  profile.credits_summary,
  profile.portfolio,
  profile.links,
  profile.equipment_summary,
  profile.rate_min_minor,
  profile.rate_max_minor,
  profile.currency_id,
  profile.availability_status,
  profile.onsite,
  profile.remote,
  profile.available_to_travel,
  profile.travel_radius_km,
  profile.completeness_score,
  profile.response_rate,
  profile.median_response_minutes,
  profile.completed_interactions,
  profile.review_average,
  profile.review_count,
  profile.created_at,
  profile.updated_at,
  profile.published_at
FROM directory_profile profile
WHERE profile.profile_status = 'published'
  AND profile.visibility = 'public'
  AND profile.moderation_status = 'allowed';

-- A merged profile keeps its historical slug resolvable, but the public
-- payload is always projected from the current canonical profile.
CREATE OR REPLACE VIEW directory_public_profile_resolution AS
SELECT source.slug AS requested_slug, target.*
FROM directory_profile source
JOIN directory_public_profile target
  ON target.id = coalesce(source.canonical_profile_id, source.id)
WHERE source.id = target.id OR source.profile_status = 'merged';

-- Safe anonymous event/venue projections. Legacy venue address/contact and
-- exact coordinates intentionally do not cross this boundary. A city
-- reference centroid is safe to use for map clustering and approximate
-- distance; exact commercial coordinates require a later explicit opt-in.
CREATE OR REPLACE VIEW directory_public_event AS
SELECT
  event.id,
  event.title,
  event.description,
  event.start_time,
  event.end_time,
  event.timezone,
  event.price_cents,
  event.currency_id,
  event.capacity,
  event.venue_id,
  venue.name AS venue_name,
  city.id AS city_id,
  coalesce(city.name_es, venue.city) AS city_name,
  country.alpha2 AS country_code,
  city.latitude AS public_latitude,
  city.longitude AS public_longitude,
  event.updated_at
FROM social_event event
JOIN workflow_state state ON state.id=event.workflow_state_id
JOIN workflow_state_capability capability
  ON capability.state_id=state.id
 AND capability.capability_code='public-listable'
 AND capability.enabled
LEFT JOIN venue ON venue.id=event.venue_id
LEFT JOIN city_reference city ON city.id=venue.city_id
LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id, city.country_id)
WHERE state.active;

CREATE OR REPLACE VIEW directory_public_venue AS
SELECT DISTINCT
  venue.id,
  venue.name,
  city.id AS city_id,
  coalesce(city.name_es, venue.city) AS city_name,
  country.alpha2 AS country_code,
  city.latitude AS public_latitude,
  city.longitude AS public_longitude,
  venue.capacity,
  venue.updated_at
FROM venue
JOIN directory_public_event event ON event.venue_id=venue.id
LEFT JOIN city_reference city ON city.id=venue.city_id
LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id, city.country_id);

CREATE OR REPLACE FUNCTION directory_distance_km(
  from_latitude DOUBLE PRECISION,
  from_longitude DOUBLE PRECISION,
  to_latitude DOUBLE PRECISION,
  to_longitude DOUBLE PRECISION
) RETURNS DOUBLE PRECISION
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN from_latitude IS NULL OR from_longitude IS NULL
      OR to_latitude IS NULL OR to_longitude IS NULL THEN NULL
    ELSE 6371.0088 * 2 * asin(sqrt(
      power(sin(radians(to_latitude - from_latitude) / 2), 2) +
      cos(radians(from_latitude)) * cos(radians(to_latitude)) *
      power(sin(radians(to_longitude - from_longitude) / 2), 2)
    ))
  END;
$$;

CREATE OR REPLACE FUNCTION directory_refresh_profile_search(profile_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,image_url,city_id,city_name,
    country_code,public_latitude,public_longitude,location_precision,
    profession_ids,service_ids,instrument_ids,genre_ids,search_text,search_vector,
    profile_completeness,reputation_score,availability_score,onsite,remote,available_to_travel,source_status,
    visibility,moderation_status,effective_at,expires_at,source_updated_at,
    source_version,sponsored,sponsor_disclosure
  )
  SELECT
    'profile', profile.id::text, profile.slug, profile.public_name,
    nullif(concat_ws(' · ', profession_names.names, instrument_names.names), ''),
    profile.bio, NULL, location.city_id, city.name_es, country.alpha2,
    location.public_latitude, location.public_longitude, location.precision,
    coalesce(professions.ids, '{}'::uuid[]), coalesce(services.ids, '{}'::uuid[]),
    coalesce(instruments.ids, '{}'::uuid[]), coalesce(genres.ids, '{}'::uuid[]),
    search.content, to_tsvector('simple', search.content), profile.completeness_score,
    least(1, greatest(0, coalesce(profile.review_average / 5, 0))),
    CASE profile.availability_status WHEN 'available' THEN 1 WHEN 'limited' THEN .6 WHEN 'ask' THEN .35 ELSE 0 END,
    profile.onsite,profile.remote,profile.available_to_travel,
    profile.profile_status, profile.visibility, profile.moderation_status,
    profile.published_at, NULL, profile.updated_at, profile.version, FALSE, NULL
  FROM directory_profile profile
  LEFT JOIN LATERAL (
    SELECT item.* FROM directory_profile_location item
    WHERE item.profile_id=profile.id
    ORDER BY item.primary_location DESC, item.created_at, item.id LIMIT 1
  ) location ON TRUE
  LEFT JOIN city_reference city ON city.id=location.city_id
  LEFT JOIN country_reference country ON country.id=location.country_id
  LEFT JOIN LATERAL (SELECT array_agg(item.profession_id ORDER BY item.sort_order,item.profession_id) ids FROM directory_profile_profession item WHERE item.profile_id=profile.id) professions ON TRUE
  LEFT JOIN LATERAL (SELECT string_agg(coalesce(item.name_es,item.name_en), ' ') names FROM directory_profile_profession member JOIN profession item ON item.id=member.profession_id WHERE member.profile_id=profile.id) profession_names ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.service_offering_id ORDER BY item.sort_order,item.service_offering_id) ids FROM directory_profile_service item WHERE item.profile_id=profile.id) services ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.instrument_id ORDER BY item.sort_order,item.instrument_id) ids FROM directory_profile_instrument item WHERE item.profile_id=profile.id) instruments ON TRUE
  LEFT JOIN LATERAL (SELECT string_agg(coalesce(item.name_es,item.name_en), ' ') names FROM directory_profile_instrument member JOIN instrument item ON item.id=member.instrument_id WHERE member.profile_id=profile.id) instrument_names ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(item.genre_id ORDER BY item.sort_order,item.genre_id) ids FROM directory_profile_genre item WHERE item.profile_id=profile.id) genres ON TRUE
  LEFT JOIN LATERAL (
    SELECT directory_normalize_text(concat_ws(' ', profile.public_name,profile.bio,
      profile.experience_summary,profile.credits_summary,profile.equipment_summary,
      profession_names.names,instrument_names.names,
      (SELECT string_agg(coalesce(term.name_es,term.name_en), ' ') FROM directory_profile_genre member JOIN genre term ON term.id=member.genre_id WHERE member.profile_id=profile.id),
      (SELECT string_agg(coalesce(term.name_es,term.name_en), ' ') FROM directory_profile_service member JOIN service_offering term ON term.id=member.service_offering_id WHERE member.profile_id=profile.id)
    )) content
  ) search ON TRUE
  WHERE profile.id=profile_id_value
  ON CONFLICT (entity_kind,entity_id) DO UPDATE SET
    slug=EXCLUDED.slug,title=EXCLUDED.title,subtitle=EXCLUDED.subtitle,summary=EXCLUDED.summary,
    city_id=EXCLUDED.city_id,city_name=EXCLUDED.city_name,country_code=EXCLUDED.country_code,
    public_latitude=EXCLUDED.public_latitude,public_longitude=EXCLUDED.public_longitude,
    location_precision=EXCLUDED.location_precision,profession_ids=EXCLUDED.profession_ids,
    service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,
    genre_ids=EXCLUDED.genre_ids,search_text=EXCLUDED.search_text,
    search_vector=EXCLUDED.search_vector,profile_completeness=EXCLUDED.profile_completeness,
    reputation_score=EXCLUDED.reputation_score,availability_score=EXCLUDED.availability_score,
    onsite=EXCLUDED.onsite,remote=EXCLUDED.remote,available_to_travel=EXCLUDED.available_to_travel,
    source_status=EXCLUDED.source_status,visibility=EXCLUDED.visibility,
    moderation_status=EXCLUDED.moderation_status,effective_at=EXCLUDED.effective_at,
    source_updated_at=EXCLUDED.source_updated_at,source_version=EXCLUDED.source_version;
END;
$$;

CREATE OR REPLACE FUNCTION directory_refresh_classified_search(classified_id_value UUID)
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,profession_ids,service_ids,
    instrument_ids,genre_ids,search_text,search_vector,profile_completeness,
    reputation_score,availability_score,onsite,remote,available_to_travel,source_status,visibility,moderation_status,
    effective_at,expires_at,source_updated_at,source_version,sponsored,sponsor_disclosure
  )
  SELECT
    'classified', classified.id::text, classified.slug, classified.title,
    category.name_es, classified.description, location.city_id, city.name_es,
    country.alpha2, city.latitude, city.longitude, 'city',
    coalesce(professions.ids,'{}'::uuid[]),
    CASE WHEN classified.service_offering_id IS NULL THEN '{}'::uuid[] ELSE ARRAY[classified.service_offering_id] END,
    coalesce(instruments.ids,'{}'::uuid[]),coalesce(genres.ids,'{}'::uuid[]),
    search.content,to_tsvector('simple',search.content),0,0,
    CASE WHEN classified.status='published' THEN 1 ELSE 0 END,
    classified.onsite,classified.remote,classified.available_to_travel,
    classified.status,'public',classified.moderation_status,classified.published_at,
    classified.expires_at,classified.updated_at,classified.version,FALSE,NULL
  FROM classified
  JOIN classified_category category ON category.id=classified.category_id
  LEFT JOIN LATERAL (SELECT item.* FROM classified_location item WHERE item.classified_id=classified.id ORDER BY item.city_id NULLS LAST LIMIT 1) location ON TRUE
  LEFT JOIN city_reference city ON city.id=location.city_id
  LEFT JOIN country_reference country ON country.id=location.country_id
  LEFT JOIN LATERAL (SELECT array_agg(profession_id) ids FROM classified_profession WHERE classified_id=classified.id) professions ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(instrument_id) ids FROM classified_instrument WHERE classified_id=classified.id) instruments ON TRUE
  LEFT JOIN LATERAL (SELECT array_agg(genre_id) ids FROM classified_genre WHERE classified_id=classified.id) genres ON TRUE
  LEFT JOIN LATERAL (SELECT directory_normalize_text(concat_ws(' ',classified.title,classified.description,category.name_es,category.name_en)) content) search ON TRUE
  WHERE classified.id=classified_id_value
  ON CONFLICT (entity_kind,entity_id) DO UPDATE SET
    slug=EXCLUDED.slug,title=EXCLUDED.title,subtitle=EXCLUDED.subtitle,summary=EXCLUDED.summary,
    city_id=EXCLUDED.city_id,city_name=EXCLUDED.city_name,country_code=EXCLUDED.country_code,
    public_latitude=EXCLUDED.public_latitude,public_longitude=EXCLUDED.public_longitude,
    location_precision=EXCLUDED.location_precision,profession_ids=EXCLUDED.profession_ids,
    service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,
    genre_ids=EXCLUDED.genre_ids,search_text=EXCLUDED.search_text,
    search_vector=EXCLUDED.search_vector,availability_score=EXCLUDED.availability_score,
    onsite=EXCLUDED.onsite,remote=EXCLUDED.remote,available_to_travel=EXCLUDED.available_to_travel,
    source_status=EXCLUDED.source_status,moderation_status=EXCLUDED.moderation_status,
    effective_at=EXCLUDED.effective_at,expires_at=EXCLUDED.expires_at,
    source_updated_at=EXCLUDED.source_updated_at,source_version=EXCLUDED.source_version;
END;
$$;

CREATE OR REPLACE FUNCTION directory_refresh_legacy_event_search()
RETURNS VOID
LANGUAGE plpgsql
AS $$
BEGIN
  DELETE FROM directory_search_document WHERE entity_kind IN ('event','venue');
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,summary,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,effective_at,expires_at,
    source_updated_at,source_version,sponsored
  )
  SELECT 'event',event.id::text,'evento-'||event.id::text,event.title,event.venue_name,
    event.description,event.city_id,event.city_name,event.country_code,event.public_latitude,
    event.public_longitude,'city',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',event.title,event.description,event.venue_name,event.city_name))),
    'published','public','allowed',event.start_time,event.end_time,event.updated_at,1,FALSE
  FROM directory_public_event event;
  INSERT INTO directory_search_document (
    entity_kind,entity_id,slug,title,subtitle,city_id,city_name,country_code,
    public_latitude,public_longitude,location_precision,search_text,search_vector,
    source_status,visibility,moderation_status,source_updated_at,source_version,sponsored
  )
  SELECT 'venue',venue.id::text,'venue-'||venue.id::text,venue.name,venue.city_name,
    venue.city_id,venue.city_name,venue.country_code,venue.public_latitude,venue.public_longitude,
    'city',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name)),
    to_tsvector('simple',directory_normalize_text(concat_ws(' ',venue.name,venue.city_name))),
    'published','public','allowed',venue.updated_at,1,FALSE
  FROM directory_public_venue venue;
END;
$$;

-- Every newly visible document is evaluated against enabled saved searches.
-- The delivery uniqueness key is the idempotency boundary; only newly
-- inserted deliveries create an internal notification.
CREATE OR REPLACE FUNCTION directory_enqueue_saved_search_alerts()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.sponsored OR NEW.source_status<>'published' OR NEW.visibility<>'public'
     OR NEW.moderation_status<>'allowed' OR (NEW.expires_at IS NOT NULL AND NEW.expires_at<=now()) THEN
    RETURN NEW;
  END IF;
  WITH matches AS (
    SELECT saved.id,saved.account_party_id
    FROM directory_saved_search saved
    WHERE saved.alerts_enabled AND saved.alert_frequency<>'off'
      AND (saved.canonical_query->>'q' IS NULL OR saved.canonical_query->>'q'='' OR
        NEW.search_vector @@ plainto_tsquery('simple',directory_normalize_text(saved.canonical_query->>'q')) OR
        directory_text_similarity(NEW.search_text,saved.canonical_query->>'q')>=.2)
      AND (saved.canonical_query->>'entityType' IS NULL OR saved.canonical_query->>'entityType'=NEW.entity_kind)
      AND (saved.canonical_query->>'cityId' IS NULL OR saved.canonical_query->>'cityId'=NEW.city_id::text)
      AND CASE WHEN saved.canonical_query->>'professionId' IS NULL THEN TRUE WHEN saved.canonical_query->>'professionId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'professionId')::uuid=ANY(NEW.profession_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'instrumentId' IS NULL THEN TRUE WHEN saved.canonical_query->>'instrumentId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'instrumentId')::uuid=ANY(NEW.instrument_ids) ELSE FALSE END
      AND CASE WHEN saved.canonical_query->>'genreId' IS NULL THEN TRUE WHEN saved.canonical_query->>'genreId' ~* '^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$' THEN (saved.canonical_query->>'genreId')::uuid=ANY(NEW.genre_ids) ELSE FALSE END
  ), inserted AS (
    INSERT INTO directory_alert_delivery(saved_search_id,result_kind,result_id,result_version,email_status,push_status)
    SELECT matches.id,NEW.entity_kind,NEW.entity_id,NEW.source_version,'disabled','disabled'
    FROM matches
    ON CONFLICT(saved_search_id,result_kind,result_id,result_version) DO NOTHING
    RETURNING saved_search_id
  )
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,is_read,created_at)
  SELECT saved.account_party_id,'directory.saved-search-match','Nueva coincidencia en tu alerta',
    'Hay un nuevo resultado para "'||saved.name||'".','directory_alert',FALSE,now()
  FROM inserted JOIN directory_saved_search saved ON saved.id=inserted.saved_search_id;
  UPDATE directory_saved_search saved SET last_evaluated_at=now()
  WHERE EXISTS (SELECT 1 FROM directory_alert_delivery delivery WHERE delivery.saved_search_id=saved.id AND delivery.result_kind=NEW.entity_kind AND delivery.result_id=NEW.entity_id AND delivery.result_version=NEW.source_version);
  RETURN NEW;
END;
$$;
DROP TRIGGER IF EXISTS directory_search_alert_trigger ON directory_search_document;
CREATE TRIGGER directory_search_alert_trigger
AFTER INSERT OR UPDATE OF source_status,visibility,moderation_status,source_version ON directory_search_document
FOR EACH ROW EXECUTE FUNCTION directory_enqueue_saved_search_alerts();

-- Explicit classified transition relation.
CREATE OR REPLACE FUNCTION directory_classified_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN
LANGUAGE sql
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('draft','pending_moderation'), ('draft','published'), ('draft','withdrawn'),
    ('pending_moderation','published'), ('pending_moderation','rejected'), ('pending_moderation','withdrawn'),
    ('published','paused'), ('published','filled'), ('published','expired'), ('published','withdrawn'), ('published','moderated'),
    ('paused','published'), ('paused','filled'), ('paused','expired'), ('paused','withdrawn'), ('paused','moderated'),
    ('expired','published'), ('expired','withdrawn'),
    ('rejected','draft'), ('rejected','withdrawn'),
    ('moderated','draft'), ('moderated','withdrawn')
  );
$$;

CREATE OR REPLACE FUNCTION directory_guard_classified_transition()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NOT directory_classified_transition_allowed(OLD.status, NEW.status) THEN
    RAISE EXCEPTION 'undeclared classified transition: % -> %', OLD.status, NEW.status
      USING ERRCODE = '23514';
  END IF;
  IF NEW.status = 'published' THEN
    NEW.published_at := coalesce(OLD.published_at, NEW.published_at, now());
    NEW.expires_at := coalesce(NEW.expires_at, now() + interval '30 days');
    IF NEW.expires_at <= now() THEN
      RAISE EXCEPTION 'published classified must expire in the future' USING ERRCODE = '23514';
    END IF;
  END IF;
  IF NEW.status IN ('filled','withdrawn','rejected','moderated') THEN
    NEW.closed_at := coalesce(NEW.closed_at, now());
  END IF;
  NEW.updated_at := now();
  NEW.version := OLD.version + 1;
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_classified_transition_trigger ON classified;
CREATE TRIGGER directory_classified_transition_trigger
BEFORE UPDATE OF status ON classified
FOR EACH ROW EXECUTE FUNCTION directory_guard_classified_transition();

CREATE OR REPLACE FUNCTION directory_guard_profile_manager_claim()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.source_claim_id IS NOT NULL AND NOT EXISTS (
    SELECT 1 FROM directory_claim claim
    WHERE claim.id = NEW.source_claim_id
      AND claim.profile_id = NEW.profile_id
      AND claim.claimant_party_id = NEW.account_party_id
      AND claim.status = 'approved'
  ) THEN
    RAISE EXCEPTION 'claim-based manager grant requires an approved matching claim'
      USING ERRCODE = '42501';
  END IF;
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_profile_manager_claim_trigger ON directory_profile_manager;
CREATE TRIGGER directory_profile_manager_claim_trigger
BEFORE INSERT OR UPDATE ON directory_profile_manager
FOR EACH ROW EXECUTE FUNCTION directory_guard_profile_manager_claim();

CREATE OR REPLACE FUNCTION directory_guard_review_interaction()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM directory_interaction interaction
    WHERE interaction.id = NEW.interaction_id
      AND interaction.status = 'completed'
      AND interaction.verified_at IS NOT NULL
      AND (
        (interaction.profile_a_id = NEW.author_profile_id AND interaction.profile_b_id = NEW.subject_profile_id)
        OR
        (interaction.profile_b_id = NEW.author_profile_id AND interaction.profile_a_id = NEW.subject_profile_id)
      )
  ) THEN
    RAISE EXCEPTION 'review requires a verified completed interaction between both profiles'
      USING ERRCODE = '23514';
  END IF;
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_review_interaction_trigger ON directory_review;
CREATE TRIGGER directory_review_interaction_trigger
BEFORE INSERT OR UPDATE ON directory_review
FOR EACH ROW EXECUTE FUNCTION directory_guard_review_interaction();

CREATE OR REPLACE FUNCTION directory_withdraw_profile_surfaces()
RETURNS trigger
LANGUAGE plpgsql
AS $$
BEGIN
  IF NEW.profile_status IN ('archived','suspended','merged')
     AND OLD.profile_status IS DISTINCT FROM NEW.profile_status THEN
    UPDATE classified
      SET status = CASE WHEN status IN ('published','paused') THEN 'withdrawn' ELSE status END,
          closed_at = CASE WHEN status IN ('published','paused') THEN now() ELSE closed_at END,
          updated_at = now()
      WHERE author_profile_id = NEW.id
        AND status IN ('published','paused');
    DELETE FROM directory_search_document
      WHERE (entity_kind = 'profile' AND entity_id = NEW.id::text)
         OR (entity_kind = 'classified' AND entity_id IN (
              SELECT id::text FROM classified WHERE author_profile_id = NEW.id
            ));
  END IF;
  RETURN NEW;
END
$$;
DROP TRIGGER IF EXISTS directory_profile_surface_withdrawal_trigger ON directory_profile;
CREATE TRIGGER directory_profile_surface_withdrawal_trigger
AFTER UPDATE OF profile_status ON directory_profile
FOR EACH ROW EXECUTE FUNCTION directory_withdraw_profile_surfaces();

-- Non-destructive merge: historical child rows continue to reference the
-- source profile; its stable slug resolves to the canonical target. The
-- source row, legacy links, counts, and audit trail are never deleted.
CREATE OR REPLACE FUNCTION directory_execute_profile_merge(
  operation_id UUID,
  source_id UUID,
  target_id UUID,
  actor_id BIGINT,
  merge_reason TEXT
) RETURNS JSONB
LANGUAGE plpgsql
AS $$
DECLARE
  source_record directory_profile%ROWTYPE;
  target_record directory_profile%ROWTYPE;
  before_snapshot JSONB;
BEGIN
  IF operation_id IS NULL OR source_id IS NULL OR target_id IS NULL OR source_id = target_id THEN
    RAISE EXCEPTION 'invalid profile merge identifiers' USING ERRCODE = '22023';
  END IF;
  IF length(trim(merge_reason)) NOT BETWEEN 10 AND 2000 THEN
    RAISE EXCEPTION 'invalid profile merge reason' USING ERRCODE = '22023';
  END IF;
  IF EXISTS (SELECT 1 FROM directory_merge_operation WHERE id=operation_id AND status='executed') THEN
    RETURN (SELECT jsonb_build_object('id',id,'sourceProfileId',source_profile_id,'targetProfileId',target_profile_id,'status',status,'executedAt',executed_at,'beforeCounts',before_counts,'afterCounts',after_counts) FROM directory_merge_operation WHERE id=operation_id);
  END IF;

  PERFORM 1 FROM directory_profile WHERE id IN (source_id,target_id) ORDER BY id FOR UPDATE;
  SELECT * INTO source_record FROM directory_profile WHERE id=source_id;
  SELECT * INTO target_record FROM directory_profile WHERE id=target_id;
  IF source_record.id IS NULL OR target_record.id IS NULL THEN
    RAISE EXCEPTION 'merge profile not found' USING ERRCODE = 'P0002';
  END IF;
  IF source_record.profile_status='merged' OR target_record.profile_status='merged' THEN
    RAISE EXCEPTION 'merged profiles cannot be merged again' USING ERRCODE = '23514';
  END IF;

  before_snapshot := jsonb_build_object(
    'managers',(SELECT count(*) FROM directory_profile_manager WHERE profile_id=source_id),
    'legacyLinks',(SELECT count(*) FROM directory_legacy_link WHERE profile_id=source_id),
    'classifieds',(SELECT count(*) FROM classified WHERE author_profile_id=source_id),
    'applications',(SELECT count(*) FROM classified_application WHERE applicant_profile_id=source_id),
    'invitations',(SELECT count(*) FROM directory_invitation WHERE sender_profile_id=source_id OR target_profile_id=source_id),
    'credits',(SELECT count(*) FROM directory_profile_credit WHERE profile_id=source_id)
  );
  INSERT INTO directory_merge_operation
    (id,source_profile_id,target_profile_id,status,reason,before_counts,after_counts,requested_by,approved_by,executed_at,correlation_id)
  VALUES
    (operation_id,source_id,target_id,'executed',trim(merge_reason),before_snapshot,before_snapshot,actor_id,actor_id,now(),'profile-merge-'||operation_id::text);

  UPDATE directory_profile
  SET profile_status='merged',canonical_profile_id=target_id,updated_at=now(),version=version+1
  WHERE id=source_id;
  UPDATE directory_merge_operation
  SET after_counts=jsonb_build_object(
    'managers',(SELECT count(*) FROM directory_profile_manager WHERE profile_id=source_id),
    'legacyLinks',(SELECT count(*) FROM directory_legacy_link WHERE profile_id=source_id),
    'classifieds',(SELECT count(*) FROM classified WHERE author_profile_id=source_id),
    'applications',(SELECT count(*) FROM classified_application WHERE applicant_profile_id=source_id),
    'invitations',(SELECT count(*) FROM directory_invitation WHERE sender_profile_id=source_id OR target_profile_id=source_id),
    'credits',(SELECT count(*) FROM directory_profile_credit WHERE profile_id=source_id)
  ) WHERE id=operation_id;
  INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,previous_state,new_state,correlation_id,metadata)
  VALUES (actor_id,'profile.merge','profile',source_id::text,source_record.profile_status,'merged','profile-merge-'||operation_id::text,jsonb_build_object('canonicalProfileId',target_id,'reason',trim(merge_reason)));
  RETURN (SELECT jsonb_build_object('id',id,'sourceProfileId',source_profile_id,'targetProfileId',target_profile_id,'status',status,'executedAt',executed_at,'beforeCounts',before_counts,'afterCounts',after_counts) FROM directory_merge_operation WHERE id=operation_id);
END
$$;

-- Seed the new catalog definitions at append-only positions 48-51.
INSERT INTO catalog_definition
  (id, code, classification, entity_kind, name_es, name_en, public_read,
   sensitive, ordering_mode, workflow_id, cache_revision, active, version)
SELECT seed.id::uuid, seed.code, seed.classification, seed.entity_kind,
       seed.name_es, seed.name_en, TRUE, seed.sensitive, 'manual', workflow.id,
       1, TRUE, 1
FROM (VALUES
  ('10000000-0000-4000-8000-000000000048','professions','dynamic-business-catalog','profession','Profesiones musicales','Music professions',FALSE,'catalog-publication'),
  ('10000000-0000-4000-8000-000000000049','classified-categories','dynamic-business-catalog','classified_category','Categorías de clasificados','Classified categories',FALSE,'catalog-publication'),
  ('10000000-0000-4000-8000-000000000050','compensation-types','dynamic-business-catalog','compensation_type','Tipos de compensación','Compensation types',FALSE,'catalog-publication'),
  ('10000000-0000-4000-8000-000000000051','metropolitan-areas','governed-reference-data','metropolitan_area','Áreas metropolitanas','Metropolitan areas',TRUE,'governed-import')
) AS seed(id,code,classification,entity_kind,name_es,name_en,sensitive,workflow_code)
JOIN workflow_definition workflow ON workflow.code = seed.workflow_code
ON CONFLICT (code) DO NOTHING;

-- Geographic seed with explicit provenance. These are public reference
-- centroids, never a person's or household's precise location.
INSERT INTO subdivision_reference
  (id,country_id,code,subdivision_type,name_es,name_en,description_es,description_en,
   source_version,last_synced_at,active,sort_order,version)
SELECT '24000000-0000-4000-8000-000000000001'::uuid, country.id, 'EC-P', 'province',
       'Pichincha', 'Pichincha', 'Provincia del Ecuador', 'Province of Ecuador',
       'ISO 3166-2 reference; TDF snapshot 2026-08-14', now(), TRUE, 10, 1
FROM country_reference country WHERE country.alpha2='EC'
ON CONFLICT (country_id,code) DO NOTHING;

INSERT INTO city_reference
  (id,country_id,subdivision_id,code,name_es,name_en,description_es,description_en,
   timezone,latitude,longitude,source_name,source_version,last_synced_at,active,sort_order,version)
SELECT '24000000-0000-4000-8000-000000000002'::uuid, country.id, subdivision.id,
       'quito-ec-p', 'Quito', 'Quito', 'Distrito Metropolitano de Quito',
       'Metropolitan District of Quito', 'America/Guayaquil', -0.180653, -78.467834,
       'TDF curated geographic reference', '2026-08-14', now(), TRUE, 10, 1
FROM country_reference country
JOIN subdivision_reference subdivision ON subdivision.country_id=country.id AND subdivision.code='EC-P'
WHERE country.alpha2='EC'
ON CONFLICT (country_id,code) DO NOTHING;

INSERT INTO metropolitan_area
  (id,catalog_id,country_id,subdivision_id,code,name_es,name_en,description_es,description_en,
   current_slug,latitude,longitude,sort_order,active,workflow_state_id,source_name,source_version)
SELECT '25000000-0000-4000-8000-000000000001'::uuid, catalog.id, country.id,
       subdivision.id, 'quito-metropolitan-area', 'Área Metropolitana de Quito',
       'Quito Metropolitan Area', 'Quito y su área metropolitana',
       'Quito and its metropolitan area', 'quito', -0.180653, -78.467834, 10, TRUE,
       state.id, 'TDF curated geographic reference', '2026-08-14'
FROM catalog_definition catalog
JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
JOIN workflow_state state ON state.workflow_id=workflow.id AND state.code='published'
JOIN country_reference country ON country.alpha2='EC'
JOIN subdivision_reference subdivision ON subdivision.country_id=country.id AND subdivision.code='EC-P'
WHERE catalog.code='metropolitan-areas'
ON CONFLICT (code) DO NOTHING;

INSERT INTO metropolitan_area_city (metropolitan_area_id,city_id,primary_city)
SELECT metro.id, city.id, TRUE
FROM metropolitan_area metro
JOIN city_reference city ON city.code='quito-ec-p'
WHERE metro.code='quito-metropolitan-area'
ON CONFLICT (metropolitan_area_id,city_id) DO NOTHING;

-- Persisted, editable initial terms. UUIDs are stable and clients consume them
-- through APIs; no client constants duplicate these labels.
INSERT INTO profession
  (id,catalog_id,code,name_es,name_en,current_slug,metadata,sort_order,active,workflow_state_id,source_name,source_version)
SELECT seed.id::uuid, catalog.id, seed.code, seed.name_es, seed.name_en, seed.slug,
       seed.metadata::jsonb, seed.sort_order, TRUE, state.id, 'TDF directory seed', '2026-08-14'
FROM (VALUES
  ('21000000-0000-4000-8000-000000000001','artist','Artista solista','Solo artist','artista','{"group":"artist"}',10),
  ('21000000-0000-4000-8000-000000000002','vocalist','Vocalista','Vocalist','vocalista','{"group":"performance"}',20),
  ('21000000-0000-4000-8000-000000000003','instrumentalist','Instrumentista','Instrumentalist','instrumentista','{"group":"performance"}',30),
  ('21000000-0000-4000-8000-000000000004','songwriter','Compositor/a','Songwriter','compositor','{"group":"creation"}',40),
  ('21000000-0000-4000-8000-000000000005','music-producer','Productor/a musical','Music producer','productor-musical','{"group":"production"}',50),
  ('21000000-0000-4000-8000-000000000006','recording-engineer','Ingeniero/a de grabación','Recording engineer','ingeniero-grabacion','{"group":"production"}',60),
  ('21000000-0000-4000-8000-000000000007','mixing-engineer','Ingeniero/a de mezcla','Mixing engineer','ingeniero-mezcla','{"group":"production"}',70),
  ('21000000-0000-4000-8000-000000000008','mastering-engineer','Ingeniero/a de mastering','Mastering engineer','ingeniero-mastering','{"group":"production"}',80),
  ('21000000-0000-4000-8000-000000000009','live-sound-engineer','Ingeniero/a de sonido en vivo','Live sound engineer','ingeniero-sonido-vivo','{"group":"live"}',90),
  ('21000000-0000-4000-8000-000000000010','stage-technician','Técnico/a de escenario','Stage technician','tecnico-escenario','{"group":"live"}',100),
  ('21000000-0000-4000-8000-000000000011','lighting-technician','Técnico/a de iluminación','Lighting technician','tecnico-iluminacion','{"group":"live"}',110),
  ('21000000-0000-4000-8000-000000000012','manager','Manager artístico','Artist manager','manager-artistico','{"group":"business"}',120),
  ('21000000-0000-4000-8000-000000000013','booker','Booker','Booker','booker','{"group":"business"}',130),
  ('21000000-0000-4000-8000-000000000014','promoter','Promotor/a','Promoter','promotor','{"group":"business"}',140),
  ('21000000-0000-4000-8000-000000000015','publicist','Relacionista público/a','Publicist','relacionista-publico','{"group":"business"}',150),
  ('21000000-0000-4000-8000-000000000016','music-teacher','Profesor/a de música','Music teacher','profesor-musica','{"group":"education"}',160),
  ('21000000-0000-4000-8000-000000000017','dj','DJ','DJ','dj','{"group":"performance"}',170),
  ('21000000-0000-4000-8000-000000000018','photographer','Fotógrafo/a musical','Music photographer','fotografo-musical','{"group":"media"}',180),
  ('21000000-0000-4000-8000-000000000019','videographer','Videógrafo/a musical','Music videographer','videografo-musical','{"group":"media"}',190),
  ('21000000-0000-4000-8000-000000000020','designer','Diseñador/a gráfico/a','Graphic designer','disenador-grafico','{"group":"media"}',200)
) AS seed(id,code,name_es,name_en,slug,metadata,sort_order)
JOIN catalog_definition catalog ON catalog.code='professions'
JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
JOIN workflow_state state ON state.workflow_id=workflow.id AND state.code='published'
ON CONFLICT (code) DO NOTHING;

INSERT INTO classified_category
  (id,catalog_id,code,name_es,name_en,current_slug,requirements,sort_order,active,workflow_state_id,source_name,source_version)
SELECT seed.id::uuid, catalog.id, seed.code, seed.name_es, seed.name_en, seed.slug,
       seed.requirements::jsonb, seed.sort_order, TRUE, state.id, 'TDF directory seed', '2026-08-14'
FROM (VALUES
  ('22000000-0000-4000-8000-000000000001','seeking-musician','Busco músico','Seeking musician','busco-musico','{"required":["instrumentIds","locations"]}',10),
  ('22000000-0000-4000-8000-000000000002','seeking-band','Busco banda o proyecto','Seeking band or project','busco-banda-proyecto','{"required":["genreIds","locations"]}',20),
  ('22000000-0000-4000-8000-000000000003','seeking-producer','Busco productor','Seeking producer','busco-productor','{"required":["professionIds"]}',30),
  ('22000000-0000-4000-8000-000000000004','seeking-engineer','Busco ingeniero o técnico','Seeking engineer or technician','busco-ingeniero-tecnico','{"required":["professionIds"]}',40),
  ('22000000-0000-4000-8000-000000000005','seeking-management','Busco manager, booker o promotor','Seeking manager, booker or promoter','busco-management','{"required":["professionIds"]}',50),
  ('22000000-0000-4000-8000-000000000006','seeking-venue','Busco venue','Seeking venue','busco-venue','{"required":["locations","dateRange"]}',60),
  ('22000000-0000-4000-8000-000000000007','seeking-show','Busco una fecha o concierto','Seeking a show date','busco-fecha-concierto','{"required":["locations","dateRange"]}',70),
  ('22000000-0000-4000-8000-000000000008','audition','Audición o convocatoria','Audition or call','audicion-convocatoria','{"required":["expiresAt"]}',80),
  ('22000000-0000-4000-8000-000000000009','paid-work','Trabajo remunerado','Paid work','trabajo-remunerado','{"required":["compensationTypeId","budget"]}',90),
  ('22000000-0000-4000-8000-000000000010','collaboration','Colaboración','Collaboration','colaboracion','{"required":["compensationTypeId"]}',100),
  ('22000000-0000-4000-8000-000000000011','offering-services','Ofrezco servicios','Offering services','ofrezco-servicios','{"required":["serviceOfferingId"]}',110),
  ('22000000-0000-4000-8000-000000000012','equipment-sale-rental','Equipo en venta o alquiler','Equipment for sale or rent','equipo-venta-alquiler','{"required":["compensationTypeId"]}',120),
  ('22000000-0000-4000-8000-000000000013','room-studio-available','Sala o estudio disponible','Room or studio available','sala-estudio-disponible','{"required":["locations"]}',130),
  ('22000000-0000-4000-8000-000000000014','classes','Clases','Classes','clases','{"required":["locationsOrRemote"]}',140),
  ('22000000-0000-4000-8000-000000000015','label-distribution-opportunity','Oportunidad para sello o distribución','Label or distribution opportunity','oportunidad-sello-distribucion','{"required":["expiresAt"]}',150)
) AS seed(id,code,name_es,name_en,slug,requirements,sort_order)
JOIN catalog_definition catalog ON catalog.code='classified-categories'
JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
JOIN workflow_state state ON state.workflow_id=workflow.id AND state.code='published'
ON CONFLICT (code) DO NOTHING;

INSERT INTO compensation_type
  (id,catalog_id,code,name_es,name_en,current_slug,metadata,sort_order,active,workflow_state_id,source_name,source_version)
SELECT seed.id::uuid, catalog.id, seed.code, seed.name_es, seed.name_en, seed.slug,
       seed.metadata::jsonb, seed.sort_order, TRUE, state.id, 'TDF directory seed', '2026-08-14'
FROM (VALUES
  ('23000000-0000-4000-8000-000000000001','exact','Monto exacto','Exact amount','monto-exacto','{"budget":"exact"}',10),
  ('23000000-0000-4000-8000-000000000002','range','Rango','Range','rango','{"budget":"range"}',20),
  ('23000000-0000-4000-8000-000000000003','negotiable','Negociable','Negotiable','negociable','{"budget":"optional"}',30),
  ('23000000-0000-4000-8000-000000000004','exchange','Intercambio','Exchange','intercambio','{"budget":"forbidden"}',40),
  ('23000000-0000-4000-8000-000000000005','unpaid','No remunerado','Unpaid','no-remunerado','{"budget":"forbidden"}',50)
) AS seed(id,code,name_es,name_en,slug,metadata,sort_order)
JOIN catalog_definition catalog ON catalog.code='compensation-types'
JOIN workflow_definition workflow ON workflow.id=catalog.workflow_id
JOIN workflow_state state ON state.workflow_id=workflow.id AND state.code='published'
ON CONFLICT (code) DO NOTHING;

-- Search aliases normalize ES/EN/PT equivalences. Missing locale rows simply
-- skip the alias; rerunning after locale seed fills them idempotently.
INSERT INTO catalog_search_alias
  (id,catalog_id,entity_kind,entity_id,locale_id,term,normalized_term,source,created_at)
SELECT gen_random_uuid(), catalog.id, 'profession', profession.id, locale.id,
       alias.term, directory_normalize_text(alias.term), 'tdf-seed', now()
FROM (VALUES
  ('music-producer','es','productor'), ('music-producer','en','producer'), ('music-producer','pt','produtor musical'),
  ('instrumentalist','es','músico'), ('instrumentalist','en','musician'), ('instrumentalist','pt','músico'),
  ('instrumentalist','es','bajista'), ('instrumentalist','en','bass player'), ('instrumentalist','pt','baixista'),
  ('recording-engineer','es','ingeniero de sonido'), ('recording-engineer','en','audio engineer'), ('recording-engineer','pt','engenheiro de áudio'),
  ('manager','es','mánager'), ('manager','en','artist manager'), ('manager','pt','empresário artístico'),
  ('promoter','es','promotor musical'), ('promoter','en','music promoter'), ('promoter','pt','promotor musical')
) AS alias(profession_code,locale_code,term)
JOIN profession ON profession.code=alias.profession_code
JOIN catalog_definition catalog ON catalog.code='professions'
JOIN locale_reference locale ON locale.code=alias.locale_code
ON CONFLICT (catalog_id,entity_id,locale_id,normalized_term) DO NOTHING;

-- Portuguese labels are data, not client constants.
INSERT INTO catalog_item_translation
  (catalog_id,entity_id,locale_id,name,description,synonyms,source)
SELECT catalog.id, profession.id, locale.id, translated.name, NULL,
       translated.synonyms::jsonb, 'tdf-seed'
FROM (VALUES
  ('artist','Artista solo','[]'),
  ('vocalist','Vocalista','[]'),
  ('instrumentalist','Instrumentista','["músico","baixista"]'),
  ('songwriter','Compositor/a','[]'),
  ('music-producer','Produtor/a musical','["produtor"]'),
  ('recording-engineer','Engenheiro/a de gravação','["engenheiro de áudio"]'),
  ('mixing-engineer','Engenheiro/a de mixagem','[]'),
  ('mastering-engineer','Engenheiro/a de masterização','[]'),
  ('manager','Empresário/a artístico/a','["manager"]'),
  ('promoter','Promotor/a','[]'),
  ('music-teacher','Professor/a de música','[]')
) AS translated(code,name,synonyms)
JOIN profession ON profession.code=translated.code
JOIN catalog_definition catalog ON catalog.code='professions'
JOIN locale_reference locale ON locale.code='pt'
ON CONFLICT (catalog_id,entity_id,locale_id) DO NOTHING;

-- The shared catalog integrity function is installed by the normal backend
-- migration. Directory catalog rows are deprecated/replaced, never deleted.
DO $$
DECLARE table_name TEXT;
BEGIN
  IF to_regprocedure('catalog_prevent_hard_delete()') IS NOT NULL THEN
    FOREACH table_name IN ARRAY ARRAY['profession','classified_category','compensation_type','metropolitan_area'] LOOP
      EXECUTE format('DROP TRIGGER IF EXISTS catalog_no_hard_delete ON %I',table_name);
      EXECUTE format('CREATE TRIGGER catalog_no_hard_delete BEFORE DELETE ON %I FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete()',table_name);
    END LOOP;
  END IF;
END
$$;

COMMIT;
