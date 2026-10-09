-- Canonical, provider-neutral music release platform.
--
-- Additive migration. It deliberately does not mutate artist_release or the
-- older catalog/distribution schemas. Public exposure is disabled by feature
-- flags until the backfill report has been reviewed and the runtime is ready.
-- Requires init_schema, 2026-07-12_notification_table,
-- 2026-08-05_artist_enrichment, 2026-08-13_unified_checkout_core and
-- 2026-09-04_access_request_notification_types.
BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

DO $music_notification_contract$
DECLARE
  current_check TEXT;
BEGIN
  IF pg_catalog.to_regclass('public.notification') IS NULL THEN
    RAISE EXCEPTION 'public.notification is required for music release notifications';
  END IF;

  SELECT pg_catalog.pg_get_expr(constraint_row.conbin, constraint_row.conrelid, TRUE)
  INTO current_check
  FROM pg_catalog.pg_constraint constraint_row
  WHERE constraint_row.conrelid='public.notification'::pg_catalog.regclass
    AND constraint_row.conname='notification_notif_type_check'
    AND constraint_row.contype='c';

  -- Some installations intentionally use unrestricted TEXT because several
  -- independently deployed producers own notification types. Preserve that
  -- contract. Where an allowlist exists, retain its exact expression and add
  -- only this bounded namespace.
  IF current_check IS NOT NULL THEN
    ALTER TABLE public.notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE pg_catalog.format(
      'ALTER TABLE public.notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type = ANY (ARRAY[' ||
      quote_literal('music_release_ready_for_review') || ',' ||
      quote_literal('music_release_in_review') || ',' ||
      quote_literal('music_release_changes_requested') || ',' ||
      quote_literal('music_release_approved') || ',' ||
      quote_literal('music_release_scheduled') || ',' ||
      quote_literal('music_release_published') || ',' ||
      quote_literal('music_release_suspended') || ',' ||
      quote_literal('music_release_takedown_scheduled') || ',' ||
      quote_literal('music_release_withdrawn') || ',' ||
      quote_literal('music_release_infringement_reported') ||
      ']::text[])) NOT VALID',
      current_check
    );
    EXECUTE pg_catalog.format(
      'COMMENT ON CONSTRAINT notification_notif_type_check ON public.notification IS %L',
      'tdf_music_original:' || current_check
    );
    ALTER TABLE public.notification VALIDATE CONSTRAINT notification_notif_type_check;
  END IF;
END
$music_notification_contract$;

CREATE TABLE music_release (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  canonical_slug TEXT NOT NULL CHECK (canonical_slug ~ '^[a-z0-9]+(?:-[a-z0-9]+)*$'),
  release_kind TEXT NOT NULL CHECK (release_kind IN ('single','ep','album')),
  published_version_id UUID,
  withdrawn_at TIMESTAMPTZ,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (artist_party_id, canonical_slug)
);

CREATE TABLE artist_release_team_member (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  artist_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  member_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  role_code TEXT NOT NULL CHECK (role_code IN ('owner','admin','editor','uploader','reviewer','analyst')),
  permissions TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
  granted_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  granted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMPTZ,
  revoked_at TIMESTAMPTZ,
  UNIQUE (artist_party_id, member_party_id),
  CHECK (expires_at IS NULL OR expires_at > granted_at),
  CHECK (permissions <@ ARRAY[
    'release.read','release.create','release.edit','release.upload','release.submit',
    'release.schedule','release.analytics','release.downloads','release.team.manage'
  ]::TEXT[])
);

CREATE INDEX idx_artist_release_team_active
  ON artist_release_team_member(artist_party_id, member_party_id)
  WHERE revoked_at IS NULL;

CREATE TABLE music_release_version (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_id UUID NOT NULL REFERENCES music_release(id) ON DELETE RESTRICT,
  version_number INTEGER NOT NULL CHECK (version_number > 0),
  state TEXT NOT NULL DEFAULT 'draft' CHECK (state IN (
    'draft','uploading','processing','validation_failed','ready_for_review',
    'in_review','changes_requested','approved','scheduled','published',
    'suspended','cancelled','replacement_pending','takedown_scheduled','withdrawn'
  )),
  title TEXT NOT NULL CHECK (length(btrim(title)) BETWEEN 1 AND 500),
  subtitle TEXT,
  version_title TEXT,
  display_artist TEXT NOT NULL CHECK (length(btrim(display_artist)) BETWEEN 1 AND 500),
  title_language TEXT NOT NULL DEFAULT 'es' CHECK (title_language ~ '^[a-z]{2,3}(?:-[A-Z][a-z]{3})?(?:-[A-Z]{2}|-[0-9]{3})?$'),
  title_script TEXT,
  primary_genre_id UUID,
  secondary_genre_id UUID,
  explicit_content TEXT NOT NULL DEFAULT 'unknown' CHECK (explicit_content IN ('not_explicit','explicit','cleaned','unknown')),
  original_release_date DATE,
  release_at_utc TIMESTAMPTZ,
  release_timezone TEXT,
  embargo_until_utc TIMESTAMPTZ,
  takedown_at_utc TIMESTAMPTZ,
  takedown_timezone TEXT,
  label_name TEXT,
  catalog_number TEXT,
  recording_copyright_text TEXT,
  work_copyright_text TEXT,
  metadata_valid BOOLEAN NOT NULL DEFAULT FALSE,
  assets_valid BOOLEAN NOT NULL DEFAULT FALSE,
  rights_valid BOOLEAN NOT NULL DEFAULT FALSE,
  access_valid BOOLEAN NOT NULL DEFAULT FALSE,
  immutable_snapshot JSONB,
  snapshot_sha256 TEXT,
  correction_of_version_id UUID REFERENCES music_release_version(id) ON DELETE RESTRICT,
  replaces_version_id UUID REFERENCES music_release_version(id) ON DELETE RESTRICT,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  approved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  approved_at TIMESTAMPTZ,
  scheduled_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  published_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_id, version_number),
  CHECK ((release_at_utc IS NULL) = (release_timezone IS NULL)),
  CHECK ((takedown_at_utc IS NULL) = (takedown_timezone IS NULL)),
  CHECK (embargo_until_utc IS NULL OR release_at_utc IS NULL OR embargo_until_utc <= release_at_utc),
  CHECK ((approved_by IS NULL) = (approved_at IS NULL)),
  CHECK (state NOT IN ('approved','scheduled','published','replacement_pending','takedown_scheduled','withdrawn') OR approved_at IS NOT NULL),
  CHECK (state NOT IN ('scheduled','published') OR release_at_utc IS NOT NULL),
  CHECK (state <> 'takedown_scheduled' OR takedown_at_utc IS NOT NULL),
  CHECK (state <> 'published' OR (published_at IS NOT NULL AND immutable_snapshot IS NOT NULL AND snapshot_sha256 ~ '^[0-9a-f]{64}$'))
);

ALTER TABLE music_release
  ADD CONSTRAINT music_release_published_version_fk
  FOREIGN KEY (published_version_id) REFERENCES music_release_version(id) ON DELETE RESTRICT;

CREATE UNIQUE INDEX uq_music_release_one_published_version
  ON music_release_version(release_id) WHERE state = 'published';
CREATE UNIQUE INDEX uq_music_release_snapshot
  ON music_release_version(release_id, snapshot_sha256)
  WHERE snapshot_sha256 IS NOT NULL;
CREATE INDEX idx_music_release_version_scheduler
  ON music_release_version(release_at_utc, id) WHERE state = 'scheduled';
CREATE INDEX idx_music_release_version_takedown_scheduler
  ON music_release_version(takedown_at_utc, id) WHERE state = 'takedown_scheduled';

CREATE TABLE music_party (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  tdf_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  display_name TEXT NOT NULL CHECK (length(btrim(display_name)) BETWEEN 1 AND 500),
  legal_name TEXT,
  party_kind TEXT NOT NULL DEFAULT 'person' CHECK (party_kind IN ('person','organization','unknown')),
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX uq_music_party_tdf_party
  ON music_party(tdf_party_id) WHERE tdf_party_id IS NOT NULL;

CREATE TABLE music_party_identifier (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  music_party_id UUID NOT NULL REFERENCES music_party(id) ON DELETE RESTRICT,
  identifier_type TEXT NOT NULL CHECK (identifier_type IN ('isni','ipi','dpid','proprietary')),
  identifier_value TEXT NOT NULL CHECK (length(btrim(identifier_value)) > 0),
  provenance TEXT NOT NULL DEFAULT 'provided' CHECK (provenance IN ('provided','imported','authority_response')),
  verification_status TEXT NOT NULL DEFAULT 'unvalidated' CHECK (verification_status IN ('unvalidated','syntax_valid','authority_verified','invalid')),
  verification_authority TEXT,
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (music_party_id, identifier_type, identifier_value),
  CHECK ((verification_status = 'authority_verified') = (verification_authority IS NOT NULL AND verified_at IS NOT NULL))
);

CREATE TABLE music_recording (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  canonical_title TEXT NOT NULL CHECK (length(btrim(canonical_title)) BETWEEN 1 AND 500),
  subtitle TEXT,
  version_title TEXT,
  title_language TEXT NOT NULL DEFAULT 'es',
  title_script TEXT,
  duration_ms BIGINT CHECK (duration_ms > 0),
  explicit_content TEXT NOT NULL DEFAULT 'unknown' CHECK (explicit_content IN ('not_explicit','explicit','cleaned','unknown')),
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE music_release_track (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  disc_number INTEGER NOT NULL DEFAULT 1 CHECK (disc_number > 0),
  track_number INTEGER NOT NULL CHECK (track_number > 0),
  display_artist TEXT NOT NULL CHECK (length(btrim(display_artist)) > 0),
  is_primary_resource BOOLEAN NOT NULL DEFAULT TRUE,
  preview_start_ms BIGINT CHECK (preview_start_ms >= 0),
  preview_duration_ms BIGINT CHECK (preview_duration_ms > 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_version_id, disc_number, track_number),
  UNIQUE (release_version_id, recording_id),
  CHECK (preview_start_ms IS NULL OR preview_duration_ms IS NOT NULL)
);

CREATE TABLE music_credit (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID REFERENCES music_recording(id) ON DELETE RESTRICT,
  music_party_id UUID NOT NULL REFERENCES music_party(id) ON DELETE RESTRICT,
  credit_role TEXT NOT NULL CHECK (credit_role IN (
    'main_artist','featured_artist','display_artist','composer','lyricist','performer',
    'producer','engineer','mixer','mastering_engineer','publisher','label','other'
  )),
  display_order INTEGER NOT NULL DEFAULT 0 CHECK (display_order >= 0),
  notes TEXT,
  UNIQUE NULLS NOT DISTINCT (release_version_id, recording_id, music_party_id, credit_role)
);

CREATE TABLE music_identifier (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID REFERENCES music_recording(id) ON DELETE RESTRICT,
  identifier_type TEXT NOT NULL CHECK (identifier_type IN ('isrc','upc','ean','grid','proprietary')),
  identifier_value TEXT NOT NULL CHECK (length(btrim(identifier_value)) > 0),
  provenance TEXT NOT NULL DEFAULT 'provided' CHECK (provenance IN ('provided','imported','authority_response')),
  verification_status TEXT NOT NULL DEFAULT 'unvalidated' CHECK (verification_status IN ('unvalidated','syntax_valid','authority_verified','invalid')),
  verification_authority TEXT,
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((release_version_id IS NOT NULL)::INT + (recording_id IS NOT NULL)::INT = 1),
  CHECK ((verification_status = 'authority_verified') = (verification_authority IS NOT NULL AND verified_at IS NOT NULL))
);

CREATE UNIQUE INDEX uq_music_identifier_release
  ON music_identifier(release_version_id, identifier_type, identifier_value)
  WHERE release_version_id IS NOT NULL;
CREATE UNIQUE INDEX uq_music_identifier_recording
  ON music_identifier(recording_id, identifier_type, identifier_value)
  WHERE recording_id IS NOT NULL;

CREATE TABLE music_rights_declaration (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID REFERENCES music_recording(id) ON DELETE RESTRICT,
  rights_scope TEXT NOT NULL CHECK (rights_scope IN ('master','composition')),
  authority_basis TEXT NOT NULL CHECK (length(btrim(authority_basis)) > 0),
  territories TEXT[] NOT NULL,
  starts_on DATE NOT NULL,
  ends_on DATE,
  declared_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  declared_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  evidence_asset_id UUID,
  CHECK (cardinality(territories) > 0),
  CHECK (ends_on IS NULL OR ends_on >= starts_on)
);

CREATE TABLE music_rights_split (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  declaration_id UUID NOT NULL REFERENCES music_rights_declaration(id) ON DELETE RESTRICT,
  rights_holder_id UUID NOT NULL REFERENCES music_party(id) ON DELETE RESTRICT,
  basis_points INTEGER NOT NULL CHECK (basis_points BETWEEN 1 AND 10000),
  territories TEXT[] NOT NULL,
  starts_on DATE NOT NULL,
  ends_on DATE,
  accepted_terms_version TEXT,
  accepted_at TIMESTAMPTZ,
  UNIQUE (declaration_id, rights_holder_id, territories, starts_on),
  CHECK (cardinality(territories) > 0),
  CHECK (ends_on IS NULL OR ends_on >= starts_on),
  CHECK ((accepted_terms_version IS NULL) = (accepted_at IS NULL))
);

CREATE TABLE music_asset (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID REFERENCES music_recording(id) ON DELETE RESTRICT,
  parent_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  asset_role TEXT NOT NULL CHECK (asset_role IN (
    'master_audio','stream_audio','preview_audio','cover_original','cover_display',
    'thumbnail','rights_evidence','ddex_xml','ddex_manifest','ddex_package'
  )),
  storage_provider TEXT NOT NULL CHECK (storage_provider IN ('local_private','s3_compatible')),
  storage_class TEXT NOT NULL DEFAULT 'standard' CHECK (storage_class IN ('quarantine','standard','infrequent','archive')),
  bucket_name TEXT NOT NULL,
  object_key TEXT NOT NULL CHECK (object_key !~ '(^|/)(\.\.|~)(/|$)' AND object_key !~ '[[:space:]]'),
  original_filename TEXT,
  media_type TEXT NOT NULL,
  byte_size BIGINT NOT NULL CHECK (byte_size >= 0),
  sha256 TEXT NOT NULL CHECK (sha256 ~ '^[0-9a-f]{64}$'),
  etag TEXT,
  processing_state TEXT NOT NULL DEFAULT 'quarantined' CHECK (processing_state IN ('quarantined','uploaded','inspecting','valid','invalid','processing','ready','failed','deleted')),
  technical_metadata JSONB NOT NULL DEFAULT '{}'::JSONB,
  provenance JSONB NOT NULL DEFAULT '{}'::JSONB,
  immutable BOOLEAN NOT NULL DEFAULT FALSE,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  ready_at TIMESTAMPTZ,
  UNIQUE (release_version_id, asset_role, sha256),
  CHECK (asset_role <> 'master_audio' OR (recording_id IS NOT NULL AND parent_asset_id IS NULL)),
  CHECK (asset_role NOT IN ('stream_audio','preview_audio','cover_display','thumbnail') OR parent_asset_id IS NOT NULL),
  CHECK (processing_state <> 'ready' OR ready_at IS NOT NULL)
);

ALTER TABLE music_rights_declaration
  ADD CONSTRAINT music_rights_evidence_asset_fk
  FOREIGN KEY (evidence_asset_id) REFERENCES music_asset(id) ON DELETE RESTRICT;

CREATE INDEX idx_music_asset_release_role ON music_asset(release_version_id, asset_role);
CREATE INDEX idx_music_asset_object_location ON music_asset(storage_provider, bucket_name, object_key);
CREATE INDEX idx_music_asset_processing ON music_asset(processing_state, created_at)
  WHERE processing_state IN ('quarantined','uploaded','inspecting','processing','failed');

CREATE TABLE music_upload_session (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID REFERENCES music_recording(id) ON DELETE RESTRICT,
  asset_role TEXT NOT NULL CHECK (asset_role IN ('master_audio','cover_original','rights_evidence')),
  provider TEXT NOT NULL CHECK (provider IN ('local_private','s3_compatible')),
  bucket_name TEXT NOT NULL,
  quarantine_object_key TEXT NOT NULL,
  original_filename TEXT NOT NULL CHECK (length(btrim(original_filename)) BETWEEN 1 AND 500),
  provider_upload_id TEXT,
  expected_media_type TEXT,
  expected_size BIGINT NOT NULL CHECK (expected_size > 0),
  expected_sha256 TEXT NOT NULL CHECK (expected_sha256 ~ '^[0-9a-f]{64}$'),
  status TEXT NOT NULL DEFAULT 'initiated' CHECK (status IN ('initiated','uploading','completing','completed','cancelled','expired','failed')),
  idempotency_key TEXT NOT NULL,
  part_size_bytes INTEGER NOT NULL CHECK (part_size_bytes BETWEEN 5242880 AND 5368709120),
  expires_at TIMESTAMPTZ NOT NULL,
  completed_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMPTZ,
  UNIQUE (created_by, idempotency_key),
  UNIQUE (provider, bucket_name, quarantine_object_key),
  CHECK (expires_at > created_at),
  CHECK (status <> 'completed' OR (completed_asset_id IS NOT NULL AND completed_at IS NOT NULL))
);

CREATE TABLE music_upload_part (
  upload_session_id UUID NOT NULL REFERENCES music_upload_session(id) ON DELETE RESTRICT,
  part_number INTEGER NOT NULL CHECK (part_number BETWEEN 1 AND 10000),
  byte_size BIGINT NOT NULL CHECK (byte_size > 0),
  etag TEXT NOT NULL,
  sha256 TEXT NOT NULL CHECK (sha256 ~ '^[0-9a-f]{64}$'),
  uploaded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (upload_session_id, part_number)
);

CREATE TABLE music_processing_job (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  source_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  job_kind TEXT NOT NULL CHECK (job_kind IN (
    'inspect_audio','transcode_audio','measure_loudness','create_preview','inspect_artwork',
    'transform_artwork','generate_waveform','validate_release','publish_release',
    'withdraw_release','generate_ddex','purge_abandoned_upload'
  )),
  job_key TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'queued' CHECK (status IN ('queued','running','retry','succeeded','failed','dead_letter','cancelled')),
  attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  max_attempts INTEGER NOT NULL DEFAULT 5 CHECK (max_attempts BETWEEN 1 AND 20),
  run_after TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  locked_at TIMESTAMPTZ,
  locked_by TEXT,
  error_code TEXT,
  error_summary TEXT,
  output JSONB NOT NULL DEFAULT '{}'::JSONB,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (job_kind, job_key),
  CHECK (status NOT IN ('running') OR (locked_at IS NOT NULL AND locked_by IS NOT NULL))
);

CREATE INDEX idx_music_job_claim ON music_processing_job(run_after, created_at)
  WHERE status IN ('queued','retry');

CREATE TABLE music_availability_rule (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  release_track_id UUID REFERENCES music_release_track(id) ON DELETE RESTRICT,
  territory_mode TEXT NOT NULL DEFAULT 'include' CHECK (territory_mode IN ('include','exclude')),
  territories TEXT[] NOT NULL DEFAULT ARRAY['Worldwide']::TEXT[],
  starts_at TIMESTAMPTZ,
  ends_at TIMESTAMPTZ,
  listening_policy TEXT NOT NULL CHECK (listening_policy IN ('none','preview','full')),
  download_policy TEXT NOT NULL CHECK (download_policy IN ('none','free','purchase')),
  purchasable BOOLEAN NOT NULL DEFAULT FALSE,
  price_minor BIGINT,
  currency TEXT,
  downloadable_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE NULLS NOT DISTINCT (release_version_id, release_track_id),
  CHECK (cardinality(territories) > 0),
  CHECK (ends_at IS NULL OR starts_at IS NULL OR ends_at > starts_at),
  CHECK ((price_minor IS NULL) = (currency IS NULL)),
  CHECK (price_minor IS NULL OR price_minor >= 0),
  CHECK (currency IS NULL OR currency ~ '^[A-Z]{3}$'),
  CHECK (NOT purchasable OR (price_minor IS NOT NULL AND price_minor > 0)),
  CHECK (download_policy <> 'purchase' OR purchasable),
  CHECK (download_policy = 'none' OR downloadable_asset_id IS NOT NULL)
);

CREATE TABLE music_terms_acceptance (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  terms_kind TEXT NOT NULL CHECK (terms_kind IN ('publication_authority','distribution','privacy','download_sale')),
  terms_version TEXT NOT NULL,
  accepted_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  accepted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  evidence JSONB NOT NULL DEFAULT '{}'::JSONB,
  UNIQUE (release_version_id, terms_kind, terms_version, accepted_by)
);

CREATE TABLE music_editorial_comment (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  parent_comment_id UUID REFERENCES music_editorial_comment(id) ON DELETE RESTRICT,
  field_path TEXT,
  body TEXT NOT NULL CHECK (length(btrim(body)) BETWEEN 1 AND 10000),
  visibility TEXT NOT NULL DEFAULT 'artist_and_staff' CHECK (visibility IN ('artist_and_staff','staff_only')),
  resolution_state TEXT NOT NULL DEFAULT 'open' CHECK (resolution_state IN ('open','resolved','obsolete')),
  created_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  resolved_at TIMESTAMPTZ,
  CHECK ((resolved_by IS NULL) = (resolved_at IS NULL))
);

CREATE TABLE music_release_audit_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_id UUID NOT NULL REFERENCES music_release(id) ON DELETE RESTRICT,
  release_version_id UUID REFERENCES music_release_version(id) ON DELETE RESTRICT,
  actor_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL,
  idempotency_key TEXT,
  prior_state TEXT,
  next_state TEXT,
  data JSONB NOT NULL DEFAULT '{}'::JSONB,
  occurred_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX uq_music_release_audit_idempotency
  ON music_release_audit_event(release_id, idempotency_key)
  WHERE idempotency_key IS NOT NULL;
CREATE INDEX idx_music_release_audit_timeline
  ON music_release_audit_event(release_id, occurred_at, id);

CREATE TABLE music_legacy_sanitation_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  legacy_release_id BIGINT NOT NULL UNIQUE REFERENCES artist_release(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','in_progress','backfilled','ignored')),
  issues TEXT[] NOT NULL,
  canonical_release_id UUID REFERENCES music_release(id) ON DELETE RESTRICT,
  scan_attempts INTEGER NOT NULL DEFAULT 1 CHECK (scan_attempts > 0),
  last_error TEXT,
  last_scanned_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  resolved_at TIMESTAMPTZ,
  CHECK ((status IN ('backfilled','ignored')) = (resolved_at IS NOT NULL)),
  CHECK (status <> 'backfilled' OR canonical_release_id IS NOT NULL),
  CHECK ((resolved_by IS NULL) = (resolved_at IS NULL))
);

CREATE INDEX idx_music_legacy_sanitation_pending
  ON music_legacy_sanitation_item(legacy_release_id)
  WHERE status IN ('pending','in_progress');

CREATE TABLE music_infringement_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_id UUID NOT NULL REFERENCES music_release(id) ON DELETE RESTRICT,
  reporter_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  reporter_email_ciphertext BYTEA,
  reason_code TEXT NOT NULL CHECK (reason_code IN ('copyright','master_rights','composition_rights','impersonation','metadata','other')),
  description TEXT NOT NULL CHECK (length(btrim(description)) BETWEEN 1 AND 10000),
  evidence_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  idempotency_key TEXT NOT NULL CHECK (length(idempotency_key) BETWEEN 8 AND 200),
  status TEXT NOT NULL DEFAULT 'received' CHECK (status IN ('received','triage','investigating','actioned','dismissed')),
  assigned_to BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  resolution_notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_at TIMESTAMPTZ,
  UNIQUE NULLS NOT DISTINCT (reporter_party_id, idempotency_key),
  CHECK (status NOT IN ('actioned','dismissed') OR resolved_at IS NOT NULL)
);

CREATE TABLE music_purchase_order (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  buyer_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  availability_rule_id UUID NOT NULL REFERENCES music_availability_rule(id) ON DELETE RESTRICT,
  checkout_id UUID REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  state TEXT NOT NULL DEFAULT 'pending' CHECK (state IN ('pending','awaiting_payment','paid','cancelled','refunded','chargeback')),
  gross_minor BIGINT NOT NULL CHECK (gross_minor > 0),
  discount_minor BIGINT NOT NULL DEFAULT 0 CHECK (discount_minor >= 0),
  tax_minor BIGINT NOT NULL DEFAULT 0 CHECK (tax_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  net_minor BIGINT NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  idempotency_key TEXT NOT NULL,
  provider_reference TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  paid_at TIMESTAMPTZ,
  UNIQUE (buyer_party_id, idempotency_key),
  UNIQUE (checkout_id),
  CHECK (net_minor = gross_minor - discount_minor + tax_minor + fee_minor),
  CHECK (state <> 'paid' OR (checkout_id IS NOT NULL AND paid_at IS NOT NULL))
);

CREATE TABLE music_entitlement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  buyer_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  asset_id UUID NOT NULL REFERENCES music_asset(id) ON DELETE RESTRICT,
  source_kind TEXT NOT NULL CHECK (source_kind IN ('free_grant','purchase','staff_grant')),
  purchase_order_id UUID REFERENCES music_purchase_order(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','revoked','refunded','expired')),
  max_downloads INTEGER CHECK (max_downloads IS NULL OR max_downloads > 0),
  expires_at TIMESTAMPTZ,
  granted_by BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  granted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  revoked_at TIMESTAMPTZ,
  UNIQUE NULLS NOT DISTINCT (buyer_party_id, release_version_id, asset_id, source_kind, purchase_order_id),
  CHECK (source_kind <> 'purchase' OR purchase_order_id IS NOT NULL)
);

CREATE TABLE music_download_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entitlement_id UUID NOT NULL REFERENCES music_entitlement(id) ON DELETE RESTRICT,
  request_id UUID NOT NULL UNIQUE,
  ip_hash TEXT,
  user_agent_hash TEXT,
  authorized_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMPTZ,
  byte_count BIGINT CHECK (byte_count IS NULL OR byte_count >= 0)
);

CREATE TABLE music_favorite (
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (party_id, recording_id)
);

CREATE TABLE music_playlist (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  owner_party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  name TEXT NOT NULL CHECK (length(btrim(name)) BETWEEN 1 AND 200),
  visibility TEXT NOT NULL DEFAULT 'private' CHECK (visibility IN ('private','unlisted','public')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE music_playlist_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  playlist_id UUID NOT NULL REFERENCES music_playlist(id) ON DELETE CASCADE,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  position INTEGER NOT NULL CHECK (position >= 0),
  added_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  added_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT uq_music_playlist_item_position UNIQUE (playlist_id, position) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE music_playback_history (
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  last_release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  last_position_ms BIGINT NOT NULL DEFAULT 0 CHECK (last_position_ms >= 0),
  play_count BIGINT NOT NULL DEFAULT 0 CHECK (play_count >= 0),
  last_played_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (party_id, recording_id)
);

CREATE TABLE music_playback_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id UUID NOT NULL UNIQUE,
  schema_version INTEGER NOT NULL DEFAULT 1 CHECK (schema_version > 0),
  session_id UUID NOT NULL,
  sequence_number INTEGER NOT NULL CHECK (sequence_number >= 0),
  party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  anonymous_id_hash TEXT,
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL CHECK (event_type IN ('play_start','progress','pause','seek','complete','skip','error','buffering','quality_selected','download','purchase')),
  position_ms BIGINT NOT NULL DEFAULT 0 CHECK (position_ms >= 0),
  listened_delta_ms BIGINT NOT NULL DEFAULT 0 CHECK (listened_delta_ms >= 0 AND listened_delta_ms <= 60000),
  quality TEXT,
  territory_code TEXT,
  occurred_at TIMESTAMPTZ NOT NULL,
  received_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  fraud_flags TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
  eligible_play BOOLEAN NOT NULL DEFAULT FALSE,
  metadata JSONB NOT NULL DEFAULT '{}'::JSONB,
  UNIQUE (session_id, sequence_number),
  CHECK ((party_id IS NOT NULL)::INT + (anonymous_id_hash IS NOT NULL)::INT = 1),
  CHECK (territory_code IS NULL OR territory_code ~ '^[A-Z]{2}$')
);

CREATE INDEX idx_music_playback_aggregate
  ON music_playback_event(release_version_id, recording_id, occurred_at)
  WHERE eligible_play;
CREATE UNIQUE INDEX uq_music_playback_one_eligible_per_session_recording
  ON music_playback_event(session_id, recording_id)
  WHERE eligible_play;

CREATE OR REPLACE FUNCTION music_record_playback_event(
  p_event_id UUID,
  p_session_id UUID,
  p_sequence_number INTEGER,
  p_party_id BIGINT,
  p_anonymous_id_hash TEXT,
  p_release_version_id UUID,
  p_recording_id UUID,
  p_event_type TEXT,
  p_position_ms BIGINT,
  p_listened_delta_ms BIGINT,
  p_quality TEXT,
  p_territory_code TEXT,
  p_occurred_at TIMESTAMPTZ,
  p_metadata JSONB
) RETURNS TEXT LANGUAGE plpgsql AS $$
DECLARE
  track_duration_ms BIGINT;
  eligibility_threshold BIGINT;
  prior_listened_ms BIGINT;
  prior_eligible BOOLEAN;
  recent_eligible_count BIGINT;
  qualifies BOOLEAN;
  affected_rows INTEGER;
BEGIN
  -- Serialise one listener/session/recording tuple so two concurrent progress
  -- events cannot both become the one eligible play.
  PERFORM pg_advisory_xact_lock(hashtextextended(p_session_id::TEXT || ':' || p_recording_id::TEXT, 0));

  IF EXISTS (SELECT 1 FROM music_playback_event event WHERE event.event_id = p_event_id) THEN
    RETURN 'duplicate';
  END IF;
  IF EXISTS (
    SELECT 1 FROM music_playback_event event
    WHERE event.session_id = p_session_id AND event.sequence_number = p_sequence_number
  ) THEN
    RAISE EXCEPTION 'playback session sequence already belongs to another event' USING ERRCODE = '23505';
  END IF;

  SELECT recording.duration_ms INTO track_duration_ms
  FROM music_recording recording WHERE recording.id = p_recording_id;
  IF track_duration_ms IS NULL THEN
    RAISE EXCEPTION 'unknown music recording' USING ERRCODE = '23503';
  END IF;
  eligibility_threshold := LEAST(30000, (track_duration_ms * 8) / 10);

  SELECT COALESCE(SUM(event.listened_delta_ms), 0), COALESCE(BOOL_OR(event.eligible_play), FALSE)
  INTO prior_listened_ms, prior_eligible
  FROM music_playback_event event
  WHERE event.session_id = p_session_id
    AND event.recording_id = p_recording_id
    AND event.event_type IN ('progress','complete');

  SELECT count(*) INTO recent_eligible_count
  FROM music_playback_event event
  WHERE event.recording_id = p_recording_id
    AND event.eligible_play
    AND event.occurred_at > NOW() - INTERVAL '1 hour'
    AND (
      (p_party_id IS NOT NULL AND event.party_id = p_party_id)
      OR (p_anonymous_id_hash IS NOT NULL AND event.anonymous_id_hash = p_anonymous_id_hash)
    );

  qualifies := p_event_type IN ('progress','complete')
    AND NOT prior_eligible
    AND prior_listened_ms < eligibility_threshold
    AND prior_listened_ms + p_listened_delta_ms >= eligibility_threshold
    AND recent_eligible_count < 20;

  INSERT INTO music_playback_event(
    event_id,session_id,sequence_number,party_id,anonymous_id_hash,
    release_version_id,recording_id,event_type,position_ms,listened_delta_ms,
    quality,territory_code,occurred_at,eligible_play,fraud_flags,metadata
  ) VALUES (
    p_event_id,p_session_id,p_sequence_number,p_party_id,p_anonymous_id_hash,
    p_release_version_id,p_recording_id,p_event_type,p_position_ms,p_listened_delta_ms,
    p_quality,p_territory_code,p_occurred_at,qualifies,
    CASE WHEN recent_eligible_count >= 20 THEN ARRAY['hourly_recording_repetition']::TEXT[] ELSE ARRAY[]::TEXT[] END,
    COALESCE(p_metadata, '{}'::JSONB)
  ) ON CONFLICT (event_id) DO NOTHING;
  GET DIAGNOSTICS affected_rows = ROW_COUNT;
  RETURN CASE WHEN affected_rows = 1 THEN 'inserted' ELSE 'duplicate' END;
END;
$$;

CREATE TABLE music_daily_metric (
  metric_date DATE NOT NULL,
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  recording_id UUID NOT NULL REFERENCES music_recording(id) ON DELETE RESTRICT,
  territory_code TEXT NOT NULL DEFAULT 'ZZ',
  play_starts BIGINT NOT NULL DEFAULT 0 CHECK (play_starts >= 0),
  eligible_plays BIGINT NOT NULL DEFAULT 0 CHECK (eligible_plays >= 0),
  completions BIGINT NOT NULL DEFAULT 0 CHECK (completions >= 0),
  skips BIGINT NOT NULL DEFAULT 0 CHECK (skips >= 0),
  listened_ms BIGINT NOT NULL DEFAULT 0 CHECK (listened_ms >= 0),
  unique_listeners BIGINT NOT NULL DEFAULT 0 CHECK (unique_listeners >= 0),
  purchases BIGINT NOT NULL DEFAULT 0 CHECK (purchases >= 0),
  downloads BIGINT NOT NULL DEFAULT 0 CHECK (downloads >= 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (metric_date, release_version_id, recording_id, territory_code)
);

CREATE OR REPLACE FUNCTION music_rebuild_daily_metrics(target_date DATE)
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE
  affected_rows INTEGER;
BEGIN
  PERFORM pg_advisory_xact_lock(hashtextextended('music-daily-metric:' || target_date::TEXT, 0));
  DELETE FROM music_daily_metric WHERE metric_date = target_date;

  INSERT INTO music_daily_metric(
    metric_date,release_version_id,recording_id,territory_code,
    play_starts,eligible_plays,completions,skips,listened_ms,unique_listeners,
    purchases,downloads,updated_at
  )
  SELECT
    target_date,event.release_version_id,event.recording_id,
    COALESCE(event.territory_code,'ZZ'),
    count(*) FILTER (WHERE event.event_type='play_start'),
    count(*) FILTER (WHERE event.eligible_play),
    count(*) FILTER (WHERE event.event_type='complete'),
    count(*) FILTER (WHERE event.event_type='skip'),
    COALESCE(sum(event.listened_delta_ms),0),
    count(DISTINCT COALESCE('p:' || event.party_id::TEXT,'a:' || event.anonymous_id_hash)),
    0,0,NOW()
  FROM music_playback_event event
  WHERE event.occurred_at >= target_date::TIMESTAMPTZ
    AND event.occurred_at < (target_date + 1)::TIMESTAMPTZ
    AND NOT ('hourly_recording_repetition'=ANY(event.fraud_flags))
  GROUP BY event.release_version_id,event.recording_id,COALESCE(event.territory_code,'ZZ');

  INSERT INTO music_daily_metric(
    metric_date,release_version_id,recording_id,territory_code,purchases,updated_at
  )
  SELECT target_date,purchase.release_version_id,first_track.recording_id,'ZZ',count(*),NOW()
  FROM music_purchase_order purchase
  JOIN LATERAL (
    SELECT track.recording_id FROM music_release_track track
    WHERE track.release_version_id=purchase.release_version_id
    ORDER BY track.disc_number,track.track_number,track.id LIMIT 1
  ) first_track ON TRUE
  WHERE purchase.state IN ('paid','refunded','chargeback')
    AND purchase.paid_at >= target_date::TIMESTAMPTZ
    AND purchase.paid_at < (target_date + 1)::TIMESTAMPTZ
  GROUP BY purchase.release_version_id,first_track.recording_id
  ON CONFLICT(metric_date,release_version_id,recording_id,territory_code)
  DO UPDATE SET purchases=EXCLUDED.purchases,updated_at=NOW();

  INSERT INTO music_daily_metric(
    metric_date,release_version_id,recording_id,territory_code,downloads,updated_at
  )
  SELECT target_date,entitlement.release_version_id,
    COALESCE(asset.recording_id,first_track.recording_id),'ZZ',count(*),NOW()
  FROM music_download_event download
  JOIN music_entitlement entitlement ON entitlement.id=download.entitlement_id
  JOIN music_asset asset ON asset.id=entitlement.asset_id
  JOIN LATERAL (
    SELECT track.recording_id FROM music_release_track track
    WHERE track.release_version_id=entitlement.release_version_id
    ORDER BY track.disc_number,track.track_number,track.id LIMIT 1
  ) first_track ON TRUE
  WHERE download.authorized_at >= target_date::TIMESTAMPTZ
    AND download.authorized_at < (target_date + 1)::TIMESTAMPTZ
  GROUP BY entitlement.release_version_id,COALESCE(asset.recording_id,first_track.recording_id)
  ON CONFLICT(metric_date,release_version_id,recording_id,territory_code)
  DO UPDATE SET downloads=EXCLUDED.downloads,updated_at=NOW();

  SELECT count(*) INTO affected_rows FROM music_daily_metric WHERE metric_date=target_date;
  RETURN affected_rows;
END;
$$;

CREATE TABLE music_ddex_party_registry (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  party_name TEXT NOT NULL CHECK (length(btrim(party_name)) BETWEEN 1 AND 500),
  dpid TEXT NOT NULL UNIQUE CHECK (dpid ~ '^[A-Za-z0-9]{8,18}$'),
  party_role TEXT NOT NULL CHECK (party_role IN ('sender','recipient','both')),
  verification_authority TEXT NOT NULL CHECK (length(btrim(verification_authority)) BETWEEN 1 AND 500),
  verification_evidence JSONB NOT NULL CHECK (verification_evidence <> '{}'::JSONB),
  verified_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  verified_at TIMESTAMPTZ NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE music_ddex_export (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES music_release_version(id) ON DELETE RESTRICT,
  operation TEXT NOT NULL CHECK (operation IN ('new_release','update','takedown')),
  standard TEXT NOT NULL CHECK (standard = 'ERN'),
  ern_version TEXT NOT NULL CHECK (ern_version = '4.3.2'),
  release_profile TEXT NOT NULL CHECK (release_profile = 'Audio'),
  release_profile_version TEXT NOT NULL CHECK (release_profile_version = '2.3.1'),
  business_profile_version TEXT CHECK (business_profile_version IS NULL),
  avs_version TEXT NOT NULL CHECK (avs_version = '011'),
  structural_dictionary_version TEXT NOT NULL CHECK (structural_dictionary_version = 'DD-ERN-432'),
  choreography TEXT NOT NULL CHECK (choreography = 'Cloud Storage'),
  choreography_version TEXT NOT NULL CHECK (choreography_version = '1.8.1'),
  sender_registry_id UUID NOT NULL REFERENCES music_ddex_party_registry(id) ON DELETE RESTRICT,
  recipient_registry_id UUID NOT NULL REFERENCES music_ddex_party_registry(id) ON DELETE RESTRICT,
  sender_dpid TEXT NOT NULL,
  recipient_dpid TEXT NOT NULL,
  message_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'queued' CHECK (status IN ('queued','generating','validation_failed','valid','failed')),
  xml_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  manifest_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  package_asset_id UUID REFERENCES music_asset(id) ON DELETE RESTRICT,
  package_sha256 TEXT,
  validation_report JSONB NOT NULL DEFAULT '{}'::JSONB,
  canonical_snapshot_sha256 TEXT NOT NULL CHECK (canonical_snapshot_sha256 ~ '^[0-9a-f]{64}$'),
  idempotency_key TEXT NOT NULL CHECK (length(idempotency_key) BETWEEN 8 AND 200),
  generated_by BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  generated_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (recipient_dpid, message_id),
  UNIQUE (generated_by, idempotency_key),
  UNIQUE (release_version_id, recipient_dpid, operation, canonical_snapshot_sha256),
  CHECK (sender_dpid ~ '^[A-Za-z0-9]{8,18}$' AND recipient_dpid ~ '^[A-Za-z0-9]{8,18}$'),
  CHECK (sender_registry_id <> recipient_registry_id),
  CHECK (status <> 'valid' OR (
    xml_asset_id IS NOT NULL AND manifest_asset_id IS NOT NULL AND package_asset_id IS NOT NULL
    AND package_sha256 ~ '^[0-9a-f]{64}$' AND generated_at IS NOT NULL
  ))
);

CREATE OR REPLACE FUNCTION music_check_ddex_export(version_id UUID)
RETURNS TABLE(field_path TEXT, error_code TEXT, message TEXT)
LANGUAGE SQL STABLE AS $$
  SELECT 'version.state', 'version_not_approved', 'La exportación DDEX requiere una versión aprobada e inmutable.'
  FROM music_release_version version WHERE version.id=version_id
    AND (version.state NOT IN ('approved','scheduled','published','replacement_pending','takedown_scheduled','withdrawn')
      OR version.immutable_snapshot IS NULL OR version.snapshot_sha256 IS NULL)
  UNION ALL
  SELECT 'metadata.primaryGenreId', 'genre_missing', 'Selecciona un género canónico para el perfil Audio.'
  FROM music_release_version version WHERE version.id=version_id AND version.primary_genre_id IS NULL
  UNION ALL
  SELECT 'metadata.labelName', 'label_missing', 'DDEX requiere el sello responsable del release.'
  FROM music_release_version version WHERE version.id=version_id AND NULLIF(btrim(version.label_name),'') IS NULL
  UNION ALL
  SELECT 'identifiers.release', 'release_identifier_missing', 'Proporciona un UPC, EAN o GRid válido para el release.'
  WHERE NOT EXISTS (
    SELECT 1 FROM music_identifier identifier
    WHERE identifier.release_version_id=version_id AND identifier.identifier_type IN ('upc','ean','grid')
      AND identifier.verification_status IN ('syntax_valid','authority_verified')
  )
  UNION ALL
  SELECT 'tracks[' || track.track_number || '].isrc', 'isrc_missing', 'Cada grabación necesita un ISRC proporcionado y sintácticamente válido.'
  FROM music_release_track track
  WHERE track.release_version_id=version_id AND NOT EXISTS (
    SELECT 1 FROM music_identifier identifier WHERE identifier.recording_id=track.recording_id
      AND identifier.identifier_type='isrc' AND identifier.verification_status IN ('syntax_valid','authority_verified')
  )
  UNION ALL
  SELECT 'tracks[' || track.track_number || '].audio', 'delivery_audio_missing', 'Cada grabación necesita un recurso de audio listo para entrega.'
  FROM music_release_track track
  WHERE track.release_version_id=version_id AND NOT EXISTS (
    SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version_id
      AND asset.recording_id=track.recording_id AND asset.asset_role='stream_audio'
      AND asset.processing_state='ready' AND asset.media_type='audio/mp4'
  )
  UNION ALL
  SELECT 'assets.cover', 'delivery_cover_missing', 'El paquete DDEX necesita una portada procesada lista.'
  WHERE NOT EXISTS (
    SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version_id
      AND asset.asset_role='cover_display' AND asset.processing_state='ready'
      AND asset.media_type IN ('image/jpeg','image/jpg')
  )
  UNION ALL
  SELECT 'availability', 'deal_missing', 'DDEX requiere al menos un deal con territorios y vigencia.'
  WHERE NOT EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=version_id)
  UNION ALL
  SELECT 'availability', 'unsupported_deal_shape', 'El adaptador ERN 4.3.2 Audio v1 admite exactamente un deal a nivel de release; consolida las reglas por pista antes de exportar.'
  WHERE (SELECT count(*) FROM music_availability_rule rule WHERE rule.release_version_id=version_id) <> 1
     OR EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=version_id AND rule.release_track_id IS NOT NULL)
  UNION ALL
  SELECT 'availability.territoryMode', 'excluded_territories_unsupported', 'El adaptador DDEX v1 solo admite territorios incluidos; convierte la exclusión a una lista explícita revisada.'
  WHERE EXISTS (SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=version_id AND rule.territory_mode<>'include')
  UNION ALL
  SELECT 'availability.policy', 'non_streaming_deal_unsupported', 'El adaptador DDEX v1 solo representa streaming completo sin compra ni descarga; usa un adaptador posterior para otros modelos comerciales.'
  WHERE EXISTS (
    SELECT 1 FROM music_availability_rule rule WHERE rule.release_version_id=version_id
      AND (rule.listening_policy<>'full' OR rule.download_policy<>'none' OR rule.purchasable)
  );
$$;

CREATE OR REPLACE FUNCTION music_artist_is_verified(artist_id BIGINT)
RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT EXISTS (
    SELECT 1
    FROM artist_profile profile
    JOIN artist_profile_enrichment enrichment
      ON enrichment.artist_party_id = profile.artist_party_id
    WHERE profile.artist_party_id = artist_id
      AND enrichment.last_verified_at IS NOT NULL
      AND enrichment.review_status IN ('verified','approved')
  );
$$;

CREATE OR REPLACE FUNCTION music_create_release_correction(
  target_release_id UUID,
  source_version_id UUID,
  actor_id BIGINT
) RETURNS UUID LANGUAGE plpgsql AS $$
DECLARE
  new_version_id UUID := gen_random_uuid();
  next_version_number INTEGER;
  source_track RECORD;
  source_asset RECORD;
  source_rights RECORD;
  new_recording_id UUID;
  new_asset_id UUID;
  new_rights_id UUID;
  recording_map JSONB := '{}'::JSONB;
  asset_map JSONB := '{}'::JSONB;
BEGIN
  PERFORM 1 FROM music_release_version version
    WHERE version.id=source_version_id AND version.release_id=target_release_id
      AND version.state IN ('approved','scheduled','published','suspended','replacement_pending','takedown_scheduled','withdrawn')
    FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'source music release version is not an immutable correction source' USING ERRCODE='23514';
  END IF;

  SELECT COALESCE(MAX(version_number),0)+1 INTO next_version_number
    FROM music_release_version WHERE release_id=target_release_id;
  INSERT INTO music_release_version(
    id,release_id,version_number,state,title,subtitle,version_title,display_artist,
    title_language,title_script,primary_genre_id,secondary_genre_id,explicit_content,
    original_release_date,label_name,catalog_number,recording_copyright_text,
    work_copyright_text,correction_of_version_id,replaces_version_id,created_by
  )
  SELECT new_version_id,release_id,next_version_number,'draft',title,subtitle,version_title,
    display_artist,title_language,title_script,primary_genre_id,secondary_genre_id,
    explicit_content,original_release_date,label_name,catalog_number,
    recording_copyright_text,work_copyright_text,id,id,actor_id
  FROM music_release_version WHERE id=source_version_id;

  -- Recording metadata and identifiers are part of the immutable published
  -- graph. Clone the logical recording rows so a correction can alter them
  -- without changing what the earlier release version represented.
  FOR source_track IN
    SELECT track.*, recording.canonical_title, recording.subtitle AS recording_subtitle,
      recording.version_title AS recording_version_title,
      recording.title_language AS recording_title_language,
      recording.title_script AS recording_title_script,
      recording.duration_ms, recording.explicit_content AS recording_explicit_content
    FROM music_release_track track
    JOIN music_recording recording ON recording.id=track.recording_id
    WHERE track.release_version_id=source_version_id
    ORDER BY track.disc_number,track.track_number,track.id
  LOOP
    new_recording_id := gen_random_uuid();
    INSERT INTO music_recording(
      id,canonical_title,subtitle,version_title,title_language,title_script,
      duration_ms,explicit_content,created_by
    ) VALUES (
      new_recording_id,source_track.canonical_title,source_track.recording_subtitle,
      source_track.recording_version_title,source_track.recording_title_language,
      source_track.recording_title_script,source_track.duration_ms,
      source_track.recording_explicit_content,actor_id
    );
    INSERT INTO music_release_track(
      release_version_id,recording_id,disc_number,track_number,display_artist,
      is_primary_resource,preview_start_ms,preview_duration_ms
    ) VALUES (
      new_version_id,new_recording_id,source_track.disc_number,source_track.track_number,
      source_track.display_artist,source_track.is_primary_resource,
      source_track.preview_start_ms,source_track.preview_duration_ms
    );
    recording_map := recording_map || jsonb_build_object(source_track.recording_id::TEXT,new_recording_id::TEXT);
  END LOOP;

  INSERT INTO music_credit(
    release_version_id,recording_id,music_party_id,credit_role,display_order,notes
  ) SELECT new_version_id,
      CASE WHEN recording_id IS NULL THEN NULL ELSE (recording_map ->> recording_id::TEXT)::UUID END,
      music_party_id,credit_role,display_order,notes
    FROM music_credit WHERE release_version_id=source_version_id;

  INSERT INTO music_identifier(
    release_version_id,identifier_type,identifier_value,provenance,verification_status,
    verification_authority,verified_at
  ) SELECT new_version_id,identifier_type,identifier_value,provenance,verification_status,
      verification_authority,verified_at
    FROM music_identifier WHERE release_version_id=source_version_id;

  INSERT INTO music_identifier(
    recording_id,identifier_type,identifier_value,provenance,verification_status,
    verification_authority,verified_at
  ) SELECT (recording_map ->> recording_id::TEXT)::UUID,identifier_type,identifier_value,
      provenance,verification_status,verification_authority,verified_at
    FROM music_identifier
    WHERE recording_id IN (
      SELECT recording_id FROM music_release_track WHERE release_version_id=source_version_id
    );

  -- Multiple logical versions may safely reference the same immutable bytes.
  -- Clone locators and provenance, then remap all logical recording and parent
  -- references to the correction graph. The objects themselves are not copied.
  FOR source_asset IN
    SELECT * FROM music_asset
    WHERE release_version_id=source_version_id
      AND asset_role NOT IN ('ddex_xml','ddex_manifest','ddex_package')
    ORDER BY (parent_asset_id IS NOT NULL),created_at,id
  LOOP
    new_asset_id := gen_random_uuid();
    INSERT INTO music_asset(
      id,release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,
      storage_class,bucket_name,object_key,original_filename,media_type,byte_size,
      sha256,etag,processing_state,technical_metadata,provenance,immutable,created_by,ready_at
    ) VALUES (
      new_asset_id,new_version_id,
      CASE WHEN source_asset.recording_id IS NULL THEN NULL
        ELSE (recording_map ->> source_asset.recording_id::TEXT)::UUID END,
      CASE WHEN source_asset.parent_asset_id IS NULL THEN NULL
        ELSE (asset_map ->> source_asset.parent_asset_id::TEXT)::UUID END,
      source_asset.asset_role,source_asset.storage_provider,source_asset.storage_class,
      source_asset.bucket_name,source_asset.object_key,source_asset.original_filename,
      source_asset.media_type,source_asset.byte_size,source_asset.sha256,source_asset.etag,
      source_asset.processing_state,source_asset.technical_metadata,
      source_asset.provenance || jsonb_build_object('correctionSourceAssetId',source_asset.id),
      source_asset.immutable,actor_id,source_asset.ready_at
    );
    asset_map := asset_map || jsonb_build_object(source_asset.id::TEXT,new_asset_id::TEXT);
  END LOOP;

  FOR source_rights IN
    SELECT * FROM music_rights_declaration WHERE release_version_id=source_version_id ORDER BY declared_at,id
  LOOP
    INSERT INTO music_rights_declaration(
      release_version_id,recording_id,rights_scope,authority_basis,territories,
      starts_on,ends_on,declared_by,declared_at,evidence_asset_id
    ) VALUES (
      new_version_id,
      CASE WHEN source_rights.recording_id IS NULL THEN NULL
        ELSE (recording_map ->> source_rights.recording_id::TEXT)::UUID END,
      source_rights.rights_scope,
      source_rights.authority_basis,source_rights.territories,source_rights.starts_on,
      source_rights.ends_on,actor_id,NOW(),
      CASE WHEN source_rights.evidence_asset_id IS NULL THEN NULL
        ELSE COALESCE((asset_map ->> source_rights.evidence_asset_id::TEXT)::UUID,source_rights.evidence_asset_id) END
    ) RETURNING id INTO new_rights_id;
    INSERT INTO music_rights_split(
      declaration_id,rights_holder_id,basis_points,territories,starts_on,ends_on,
      accepted_terms_version,accepted_at
    ) SELECT new_rights_id,rights_holder_id,basis_points,territories,starts_on,ends_on,
        accepted_terms_version,accepted_at
      FROM music_rights_split WHERE declaration_id=source_rights.id;
  END LOOP;

  INSERT INTO music_availability_rule(
    release_version_id,release_track_id,territory_mode,territories,starts_at,ends_at,
    listening_policy,download_policy,purchasable,price_minor,currency,downloadable_asset_id
  )
  SELECT new_version_id,
    CASE WHEN rule.release_track_id IS NULL THEN NULL ELSE new_track.id END,
    rule.territory_mode,rule.territories,rule.starts_at,
    rule.ends_at,rule.listening_policy,rule.download_policy,rule.purchasable,
    rule.price_minor,rule.currency,
    CASE WHEN rule.downloadable_asset_id IS NULL THEN NULL
      ELSE (asset_map ->> rule.downloadable_asset_id::TEXT)::UUID END
  FROM music_availability_rule rule
  LEFT JOIN music_release_track old_track ON old_track.id=rule.release_track_id
  LEFT JOIN music_release_track new_track ON new_track.release_version_id=new_version_id
    AND new_track.recording_id=(recording_map ->> old_track.recording_id::TEXT)::UUID
  WHERE rule.release_version_id=source_version_id;

  PERFORM music_refresh_validation_flags(new_version_id);
  RETURN new_version_id;
END;
$$;

CREATE OR REPLACE FUNCTION music_can(actor_id BIGINT, artist_id BIGINT, permission_code TEXT)
RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT music_artist_is_verified(artist_id)
    AND (
      actor_id = artist_id
      OR EXISTS (
        SELECT 1 FROM artist_release_team_member membership
        WHERE membership.artist_party_id = artist_id
          AND membership.member_party_id = actor_id
          AND membership.revoked_at IS NULL
          AND (membership.expires_at IS NULL OR membership.expires_at > NOW())
          AND (
            membership.role_code IN ('owner','admin')
            OR permission_code = ANY(membership.permissions)
          )
      )
    );
$$;

CREATE OR REPLACE FUNCTION music_valid_transition(old_state TEXT, new_state TEXT)
RETURNS BOOLEAN LANGUAGE SQL IMMUTABLE AS $$
  SELECT old_state = new_state OR (old_state, new_state) IN (
    ('draft','uploading'),('draft','processing'),('draft','validation_failed'),('draft','ready_for_review'),('draft','cancelled'),
    ('uploading','processing'),('uploading','validation_failed'),('uploading','draft'),('uploading','cancelled'),
    ('processing','ready_for_review'),('processing','validation_failed'),('processing','cancelled'),
    ('validation_failed','draft'),('validation_failed','uploading'),('validation_failed','processing'),('validation_failed','ready_for_review'),('validation_failed','cancelled'),
    ('ready_for_review','in_review'),('ready_for_review','validation_failed'),('ready_for_review','draft'),('ready_for_review','cancelled'),
    ('in_review','changes_requested'),('in_review','approved'),('in_review','suspended'),
    ('changes_requested','draft'),('changes_requested','uploading'),('changes_requested','processing'),('changes_requested','ready_for_review'),('changes_requested','cancelled'),
    ('approved','scheduled'),('approved','replacement_pending'),('approved','suspended'),('approved','cancelled'),
    ('scheduled','published'),('scheduled','approved'),('scheduled','suspended'),('scheduled','cancelled'),
    ('published','suspended'),('published','replacement_pending'),('published','takedown_scheduled'),
    ('suspended','approved'),('suspended','takedown_scheduled'),('suspended','withdrawn'),
    ('replacement_pending','published'),('replacement_pending','takedown_scheduled'),
    ('takedown_scheduled','published'),('takedown_scheduled','withdrawn')
  );
$$;

CREATE OR REPLACE FUNCTION music_check_submission(version_id UUID)
RETURNS TABLE(field_path TEXT, error_code TEXT, message TEXT)
LANGUAGE SQL STABLE AS $$
  SELECT 'metadata', 'metadata_invalid', 'Completa y valida los metadatos editoriales.'
  FROM music_release_version v WHERE v.id = version_id AND NOT v.metadata_valid
  UNION ALL
  SELECT 'assets', 'assets_invalid', 'Carga y procesa todos los másteres y la portada.'
  FROM music_release_version v WHERE v.id = version_id AND NOT v.assets_valid
  UNION ALL
  SELECT 'rights', 'rights_invalid', 'Declara por separado derechos de máster y composición.'
  FROM music_release_version v WHERE v.id = version_id AND NOT v.rights_valid
  UNION ALL
  SELECT 'access', 'access_invalid', 'Configura escucha, territorios, compra y descarga.'
  FROM music_release_version v WHERE v.id = version_id AND NOT v.access_valid
  UNION ALL
  SELECT 'tracks', 'tracks_missing', 'Añade al menos una pista al lanzamiento.'
  WHERE NOT EXISTS (SELECT 1 FROM music_release_track t WHERE t.release_version_id = version_id)
  UNION ALL
  SELECT 'metadata.explicitContent', 'explicit_classification_missing', 'Clasifica el contenido explícito de todas las pistas.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    JOIN music_recording recording ON recording.id=track.recording_id
    WHERE track.release_version_id=version_id AND recording.explicit_content='unknown'
  )
  UNION ALL
  SELECT 'metadata.copyright', 'copyright_missing', 'Completa por separado el copyright de la grabación y de la obra.'
  FROM music_release_version version
  WHERE version.id=version_id AND (
    NULLIF(btrim(version.recording_copyright_text),'') IS NULL
    OR NULLIF(btrim(version.work_copyright_text),'') IS NULL
  )
  UNION ALL
  SELECT 'metadata.primaryGenreId', 'genre_missing', 'Selecciona un género principal vigente del catálogo canónico.'
  FROM music_release_version version
  WHERE version.id=version_id AND version.primary_genre_id IS NULL
  UNION ALL
  SELECT 'tracks.duration', 'duration_missing', 'Procesa cada máster para obtener una duración técnica válida.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    JOIN music_recording recording ON recording.id=track.recording_id
    WHERE track.release_version_id=version_id AND recording.duration_ms IS NULL
  )
  UNION ALL
  SELECT 'credits.main_artist', 'main_artist_missing', 'Añade al menos un artista principal.'
  WHERE NOT EXISTS (SELECT 1 FROM music_credit c WHERE c.release_version_id = version_id AND c.credit_role = 'main_artist')
  UNION ALL
  SELECT 'credits.composer', 'composer_missing', 'Añade al menos un compositor por grabación.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id
      AND NOT EXISTS (
        SELECT 1 FROM music_credit credit
        WHERE credit.release_version_id=version_id
          AND credit.credit_role='composer'
          AND (credit.recording_id IS NULL OR credit.recording_id=track.recording_id)
      )
  )
  UNION ALL
  SELECT 'rights.master', 'master_rights_missing', 'Declara los derechos de máster.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id
      AND NOT EXISTS (
        SELECT 1 FROM music_rights_declaration rights
        WHERE rights.release_version_id=version_id AND rights.rights_scope='master'
          AND (rights.recording_id IS NULL OR rights.recording_id=track.recording_id)
      )
  )
  UNION ALL
  SELECT 'rights.composition', 'composition_rights_missing', 'Declara los derechos de composición.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id
      AND NOT EXISTS (
        SELECT 1 FROM music_rights_declaration rights
        WHERE rights.release_version_id=version_id AND rights.rights_scope='composition'
          AND (rights.recording_id IS NULL OR rights.recording_id=track.recording_id)
      )
  )
  UNION ALL
  SELECT 'rights.splits', 'rights_splits_invalid', 'Cada declaración de derechos debe distribuir exactamente 10000 puntos básicos.'
  WHERE EXISTS (
    SELECT 1 FROM music_rights_declaration rights
    LEFT JOIN music_rights_split split ON split.declaration_id=rights.id
    WHERE rights.release_version_id=version_id
    GROUP BY rights.id HAVING COALESCE(SUM(split.basis_points),0)<>10000
  )
  UNION ALL
  SELECT 'assets.cover', 'cover_missing', 'Carga y procesa la portada original y su versión de publicación.'
  WHERE NOT EXISTS (
    SELECT 1 FROM music_asset asset
    WHERE asset.release_version_id=version_id AND asset.asset_role='cover_original'
      AND asset.processing_state IN ('valid','ready') AND asset.immutable
  ) OR NOT EXISTS (
    SELECT 1 FROM music_asset asset
    WHERE asset.release_version_id=version_id AND asset.asset_role='cover_display'
      AND asset.processing_state='ready'
  )
  UNION ALL
  SELECT 'assets.tracks', 'track_assets_missing', 'Cada pista necesita un máster validado y un derivado de streaming listo.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id AND (
      NOT EXISTS (
        SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version_id
          AND asset.recording_id=track.recording_id AND asset.asset_role='master_audio'
          AND asset.processing_state IN ('valid','ready') AND asset.immutable
      ) OR NOT EXISTS (
        SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version_id
          AND asset.recording_id=track.recording_id AND asset.asset_role='stream_audio'
          AND asset.processing_state='ready'
      )
    )
  )
  UNION ALL
  SELECT 'availability', 'track_availability_missing', 'Configura escucha y territorio para cada pista o para todo el lanzamiento.'
  WHERE EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.release_version_id=version_id
      AND NOT EXISTS (
        SELECT 1 FROM music_availability_rule rule
        WHERE rule.release_version_id=version_id
          AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id)
      )
  )
  UNION ALL
  SELECT 'terms.publication_authority', 'authority_terms_missing', 'Acepta la declaración de autoridad para publicar.'
  WHERE NOT EXISTS (SELECT 1 FROM music_terms_acceptance a WHERE a.release_version_id = version_id AND a.terms_kind = 'publication_authority')
  UNION ALL
  SELECT 'editorial.comments', 'open_change_requests', 'Resuelve todas las solicitudes de cambio abiertas.'
  WHERE EXISTS (SELECT 1 FROM music_editorial_comment c WHERE c.release_version_id = version_id AND c.resolution_state = 'open');
$$;

CREATE OR REPLACE FUNCTION music_refresh_validation_flags(version_id UUID)
RETURNS TABLE(metadata_valid BOOLEAN, assets_valid BOOLEAN, rights_valid BOOLEAN, access_valid BOOLEAN)
LANGUAGE plpgsql AS $$
BEGIN
  UPDATE music_release_version version SET
    metadata_valid =
      version.explicit_content <> 'unknown'
      AND version.primary_genre_id IS NOT NULL
      AND NULLIF(btrim(version.recording_copyright_text),'') IS NOT NULL
      AND NULLIF(btrim(version.work_copyright_text),'') IS NOT NULL
      AND EXISTS (SELECT 1 FROM music_release_track track WHERE track.release_version_id=version.id)
      AND NOT EXISTS (
        SELECT 1 FROM music_release_track track
        JOIN music_recording recording ON recording.id=track.recording_id
        WHERE track.release_version_id=version.id
          AND (recording.explicit_content='unknown' OR recording.duration_ms IS NULL)
      )
      AND EXISTS (
        SELECT 1 FROM music_credit credit
        WHERE credit.release_version_id=version.id AND credit.credit_role='main_artist'
      )
      AND NOT EXISTS (
        SELECT 1 FROM music_release_track track
        WHERE track.release_version_id=version.id
          AND NOT EXISTS (
            SELECT 1 FROM music_credit credit
            WHERE credit.release_version_id=version.id AND credit.credit_role='composer'
              AND (credit.recording_id IS NULL OR credit.recording_id=track.recording_id)
          )
      ),
    assets_valid =
      EXISTS (
        SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version.id
          AND asset.asset_role='cover_original' AND asset.processing_state IN ('valid','ready') AND asset.immutable
      )
      AND EXISTS (
        SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version.id
          AND asset.asset_role='cover_display' AND asset.processing_state='ready'
      )
      AND NOT EXISTS (
        SELECT 1 FROM music_release_track track
        WHERE track.release_version_id=version.id AND (
          NOT EXISTS (
            SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version.id
              AND asset.recording_id=track.recording_id AND asset.asset_role='master_audio'
              AND asset.processing_state IN ('valid','ready') AND asset.immutable
          ) OR NOT EXISTS (
            SELECT 1 FROM music_asset asset WHERE asset.release_version_id=version.id
              AND asset.recording_id=track.recording_id AND asset.asset_role='stream_audio'
              AND asset.processing_state='ready'
          )
        )
      ),
    rights_valid =
      NOT EXISTS (
        SELECT 1 FROM music_release_track track
        CROSS JOIN (VALUES ('master'),('composition')) required(scope)
        WHERE track.release_version_id=version.id
          AND NOT EXISTS (
            SELECT 1 FROM music_rights_declaration rights
            WHERE rights.release_version_id=version.id AND rights.rights_scope=required.scope
              AND (rights.recording_id IS NULL OR rights.recording_id=track.recording_id)
          )
      )
      AND NOT EXISTS (
        SELECT 1 FROM music_rights_declaration rights
        LEFT JOIN music_rights_split split ON split.declaration_id=rights.id
        WHERE rights.release_version_id=version.id
        GROUP BY rights.id HAVING COALESCE(SUM(split.basis_points),0)<>10000
      ),
    access_valid =
      NOT EXISTS (
        SELECT 1 FROM music_release_track track
        WHERE track.release_version_id=version.id
          AND NOT EXISTS (
            SELECT 1 FROM music_availability_rule rule
            WHERE rule.release_version_id=version.id
              AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id)
          )
      )
  WHERE version.id=version_id
    AND version.state IN ('draft','uploading','processing','validation_failed','ready_for_review','changes_requested');

  RETURN QUERY SELECT version.metadata_valid,version.assets_valid,version.rights_valid,version.access_valid
    FROM music_release_version version WHERE version.id=version_id;
END;
$$;

CREATE OR REPLACE FUNCTION music_validate_version_state()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NOT music_valid_transition(OLD.state, NEW.state) THEN
    RAISE EXCEPTION 'invalid music release transition: % -> %', OLD.state, NEW.state USING ERRCODE = '23514';
  END IF;

  IF NEW.state IN ('ready_for_review','in_review','approved','scheduled','published')
     AND EXISTS (SELECT 1 FROM music_check_submission(NEW.id)) THEN
    RAISE EXCEPTION 'music release version % has unresolved submission errors', NEW.id USING ERRCODE = '23514';
  END IF;

  IF NEW.state = 'scheduled' AND NEW.release_at_utc <= NOW() THEN
    RAISE EXCEPTION 'scheduled release time must be in the future' USING ERRCODE = '23514';
  END IF;

  IF OLD.state IN ('published','withdrawn') AND ROW(
    OLD.title, OLD.subtitle, OLD.version_title, OLD.display_artist, OLD.title_language,
    OLD.title_script, OLD.primary_genre_id, OLD.secondary_genre_id, OLD.explicit_content,
    OLD.original_release_date, OLD.label_name, OLD.catalog_number,
    OLD.recording_copyright_text, OLD.work_copyright_text, OLD.immutable_snapshot,
    OLD.snapshot_sha256
  ) IS DISTINCT FROM ROW(
    NEW.title, NEW.subtitle, NEW.version_title, NEW.display_artist, NEW.title_language,
    NEW.title_script, NEW.primary_genre_id, NEW.secondary_genre_id, NEW.explicit_content,
    NEW.original_release_date, NEW.label_name, NEW.catalog_number,
    NEW.recording_copyright_text, NEW.work_copyright_text, NEW.immutable_snapshot,
    NEW.snapshot_sha256
  ) THEN
    RAISE EXCEPTION 'published music release versions are immutable; create a correction version' USING ERRCODE = '23514';
  END IF;

  NEW.updated_at := NOW();
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_validate_version_state
BEFORE UPDATE ON music_release_version
FOR EACH ROW EXECUTE FUNCTION music_validate_version_state();

CREATE OR REPLACE FUNCTION music_validate_infringement_status()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.status IS DISTINCT FROM NEW.status AND NOT (
    (OLD.status='received' AND NEW.status IN ('triage','dismissed'))
    OR (OLD.status='triage' AND NEW.status IN ('investigating','actioned','dismissed'))
    OR (OLD.status='investigating' AND NEW.status IN ('actioned','dismissed'))
  ) THEN
    RAISE EXCEPTION 'invalid music infringement transition: % -> %', OLD.status, NEW.status USING ERRCODE='23514';
  END IF;
  NEW.updated_at := NOW();
  IF NEW.status IN ('actioned','dismissed') THEN
    NEW.resolved_at := COALESCE(NEW.resolved_at,NOW());
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_validate_infringement_status
BEFORE UPDATE ON music_infringement_report
FOR EACH ROW EXECUTE FUNCTION music_validate_infringement_status();

CREATE OR REPLACE FUNCTION music_validate_split_total()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  affected_declaration UUID := COALESCE(NEW.declaration_id, OLD.declaration_id);
  total_basis_points INTEGER;
BEGIN
  IF NOT EXISTS (SELECT 1 FROM music_rights_declaration WHERE id=affected_declaration) THEN
    RETURN NULL;
  END IF;
  SELECT COALESCE(SUM(basis_points), 0) INTO total_basis_points
  FROM music_rights_split WHERE declaration_id = affected_declaration;
  IF total_basis_points <> 10000 THEN
    RAISE EXCEPTION 'rights splits for declaration % total %, expected 10000 basis points', affected_declaration, total_basis_points USING ERRCODE = '23514';
  END IF;
  RETURN NULL;
END;
$$;

CREATE CONSTRAINT TRIGGER trg_music_validate_split_total
AFTER INSERT OR UPDATE OR DELETE ON music_rights_split
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW EXECUTE FUNCTION music_validate_split_total();

CREATE OR REPLACE FUNCTION music_protect_append_only()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% is append-only', TG_TABLE_NAME USING ERRCODE = '23514';
END;
$$;

CREATE TRIGGER trg_music_audit_append_only
BEFORE UPDATE OR DELETE ON music_release_audit_event
FOR EACH ROW EXECUTE FUNCTION music_protect_append_only();
CREATE TRIGGER trg_music_playback_append_only
BEFORE UPDATE OR DELETE ON music_playback_event
FOR EACH ROW EXECUTE FUNCTION music_protect_append_only();
CREATE TRIGGER trg_music_download_append_only
BEFORE UPDATE OR DELETE ON music_download_event
FOR EACH ROW EXECUTE FUNCTION music_protect_append_only();

CREATE OR REPLACE FUNCTION music_protect_asset()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.immutable AND (
    TG_OP = 'DELETE' OR OLD.storage_provider IS DISTINCT FROM NEW.storage_provider
    OR OLD.bucket_name IS DISTINCT FROM NEW.bucket_name
    OR OLD.object_key IS DISTINCT FROM NEW.object_key
    OR OLD.byte_size IS DISTINCT FROM NEW.byte_size
    OR OLD.sha256 IS DISTINCT FROM NEW.sha256
  ) THEN
    RAISE EXCEPTION 'immutable music asset bytes and location cannot be changed' USING ERRCODE = '23514';
  END IF;
  RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
END;
$$;

CREATE TRIGGER trg_music_asset_immutable
BEFORE UPDATE OR DELETE ON music_asset
FOR EACH ROW EXECUTE FUNCTION music_protect_asset();

CREATE OR REPLACE FUNCTION music_release_version_is_locked(version_id UUID)
RETURNS BOOLEAN LANGUAGE SQL STABLE AS $$
  SELECT EXISTS (
    SELECT 1 FROM music_release_version version
    WHERE version.id = version_id
      AND version.state IN (
        'approved','scheduled','published','suspended','replacement_pending',
        'takedown_scheduled','withdrawn'
      )
  );
$$;

CREATE OR REPLACE FUNCTION music_protect_version_content()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  old_row JSONB := CASE WHEN TG_OP = 'INSERT' THEN '{}'::JSONB ELSE to_jsonb(OLD) END;
  new_row JSONB := CASE WHEN TG_OP = 'DELETE' THEN '{}'::JSONB ELSE to_jsonb(NEW) END;
  old_version UUID;
  new_version UUID;
BEGIN
  IF TG_TABLE_NAME = 'music_rights_split' THEN
    SELECT declaration.release_version_id INTO old_version
      FROM music_rights_declaration declaration
      WHERE declaration.id = NULLIF(old_row ->> 'declaration_id', '')::UUID;
    SELECT declaration.release_version_id INTO new_version
      FROM music_rights_declaration declaration
      WHERE declaration.id = NULLIF(new_row ->> 'declaration_id', '')::UUID;
  ELSIF TG_TABLE_NAME = 'music_identifier' THEN
    old_version := NULLIF(old_row ->> 'release_version_id', '')::UUID;
    new_version := NULLIF(new_row ->> 'release_version_id', '')::UUID;
    IF old_version IS NULL THEN
      SELECT track.release_version_id INTO old_version
        FROM music_release_track track
        WHERE track.recording_id = NULLIF(old_row ->> 'recording_id', '')::UUID
          AND music_release_version_is_locked(track.release_version_id)
        LIMIT 1;
    END IF;
    IF new_version IS NULL THEN
      SELECT track.release_version_id INTO new_version
        FROM music_release_track track
        WHERE track.recording_id = NULLIF(new_row ->> 'recording_id', '')::UUID
          AND music_release_version_is_locked(track.release_version_id)
        LIMIT 1;
    END IF;
  ELSE
    old_version := NULLIF(old_row ->> 'release_version_id', '')::UUID;
    new_version := NULLIF(new_row ->> 'release_version_id', '')::UUID;
  END IF;

  -- Consumer acceptance of download-sale terms is append-only evidence and is
  -- intentionally allowed after publication. DDEX package assets are likewise
  -- generated from the frozen snapshot after approval.
  IF TG_TABLE_NAME = 'music_terms_acceptance'
     AND COALESCE(new_row ->> 'terms_kind', old_row ->> 'terms_kind') = 'download_sale' THEN
    RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
  END IF;
  IF TG_TABLE_NAME = 'music_asset'
     AND COALESCE(new_row ->> 'asset_role', old_row ->> 'asset_role') IN ('ddex_xml','ddex_manifest','ddex_package') THEN
    RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
  END IF;

  IF (old_version IS NOT NULL AND music_release_version_is_locked(old_version))
     OR (new_version IS NOT NULL AND music_release_version_is_locked(new_version)) THEN
    RAISE EXCEPTION 'approved music release content is immutable; create a correction version'
      USING ERRCODE = '23514';
  END IF;
  RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
END;
$$;

CREATE TRIGGER trg_music_track_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_release_track
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_credit_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_credit
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_identifier_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_identifier
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_rights_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_rights_declaration
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_split_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_rights_split
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_asset_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_asset
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_availability_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_availability_rule
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();
CREATE TRIGGER trg_music_terms_version_immutable
BEFORE INSERT OR UPDATE OR DELETE ON music_terms_acceptance
FOR EACH ROW EXECUTE FUNCTION music_protect_version_content();

CREATE OR REPLACE FUNCTION music_protect_recording_content()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM music_release_track track
    WHERE track.recording_id = OLD.id
      AND music_release_version_is_locked(track.release_version_id)
  ) THEN
    RAISE EXCEPTION 'recording is referenced by an approved release; create a new recording/version'
      USING ERRCODE = '23514';
  END IF;
  RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
END;
$$;

CREATE TRIGGER trg_music_recording_version_immutable
BEFORE UPDATE OR DELETE ON music_recording
FOR EACH ROW EXECUTE FUNCTION music_protect_recording_content();

CREATE OR REPLACE FUNCTION music_checkout_require_verified_payment()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'music_download'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND (
       NEW.paid_minor <> NEW.total_minor
       OR NOT EXISTS (
         SELECT 1
         FROM music_purchase_order purchase
         JOIN commerce_payment_attempt attempt ON attempt.checkout_id = NEW.id
         JOIN commerce_provider_binding binding
           ON binding.payment_attempt_id = attempt.id
          AND binding.provider = attempt.provider
          AND binding.environment = attempt.environment
          AND binding.merchant_account_ref = attempt.merchant_account_ref
          AND binding.merchant_reference = NEW.domain_order_id
          AND binding.amount_minor = NEW.total_minor
          AND binding.currency = NEW.currency
         WHERE purchase.id::TEXT = NEW.domain_order_id
           AND purchase.checkout_id = NEW.id
           AND purchase.gross_minor = NEW.total_minor
           AND purchase.currency = NEW.currency
           AND attempt.status = 'succeeded'
           AND attempt.environment = NEW.environment
           AND attempt.amount_minor = NEW.total_minor
           AND attempt.currency = NEW.currency
           AND (
             (attempt.provider = 'datafast' AND binding.resource_type IN ('checkout','payment'))
             OR (attempt.provider = 'paypal' AND binding.resource_type = 'capture')
             OR (attempt.provider = 'stripe' AND binding.resource_type = 'payment')
           )
       )
     ) THEN
    RAISE EXCEPTION 'music checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_checkout_require_verified_payment
BEFORE UPDATE OF status, paid_minor ON commerce_checkout_session
FOR EACH ROW EXECUTE FUNCTION music_checkout_require_verified_payment();

CREATE OR REPLACE FUNCTION music_sync_verified_checkout()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
DECLARE
  purchase music_purchase_order%ROWTYPE;
  entitlement_asset UUID;
  provider_reference_value TEXT;
BEGIN
  IF NEW.domain_type <> 'music_download' OR OLD.status IS NOT DISTINCT FROM NEW.status THEN
    RETURN NEW;
  END IF;

  SELECT * INTO purchase FROM music_purchase_order
  WHERE id::TEXT = NEW.domain_order_id AND checkout_id = NEW.id
  FOR UPDATE;
  IF purchase.id IS NULL THEN
    RAISE EXCEPTION 'music checkout is not linked to its canonical purchase order';
  END IF;

  IF NEW.status = 'paid' THEN
    SELECT binding.provider_resource_id INTO provider_reference_value
    FROM commerce_payment_attempt attempt
    JOIN commerce_provider_binding binding ON binding.payment_attempt_id = attempt.id
    WHERE attempt.checkout_id = NEW.id AND attempt.status = 'succeeded'
    ORDER BY attempt.updated_at DESC, attempt.id DESC LIMIT 1;

    UPDATE music_purchase_order
    SET state = 'paid', paid_at = COALESCE(paid_at, NEW.paid_at, NOW()),
        provider_reference = COALESCE(provider_reference, provider_reference_value)
    WHERE id = purchase.id AND state IN ('pending','awaiting_payment');

    SELECT rule.downloadable_asset_id INTO entitlement_asset
      FROM music_availability_rule rule WHERE rule.id = purchase.availability_rule_id;
    IF entitlement_asset IS NULL THEN
      RAISE EXCEPTION 'paid music purchase has no downloadable asset';
    END IF;
    INSERT INTO music_entitlement(
      buyer_party_id,release_version_id,asset_id,source_kind,purchase_order_id,status,max_downloads
    ) VALUES (
      purchase.buyer_party_id,purchase.release_version_id,entitlement_asset,'purchase',purchase.id,'active',10
    ) ON CONFLICT (buyer_party_id,release_version_id,asset_id,source_kind,purchase_order_id)
      DO UPDATE SET status='active',revoked_at=NULL;

    INSERT INTO music_release_audit_event(
      release_id,release_version_id,actor_party_id,event_type,data
    ) SELECT version.release_id,purchase.release_version_id,purchase.buyer_party_id,'purchase_verified',
        jsonb_build_object('purchase_order_id',purchase.id,'checkout_id',NEW.id)
      FROM music_release_version version WHERE version.id=purchase.release_version_id;
  ELSIF NEW.status = 'refunded' THEN
    UPDATE music_purchase_order SET state='refunded' WHERE id=purchase.id AND state='paid';
    UPDATE music_entitlement SET status='refunded',revoked_at=COALESCE(revoked_at,NOW())
      WHERE purchase_order_id=purchase.id AND status='active';
  ELSIF NEW.status IN ('disputed','chargeback') THEN
    UPDATE music_purchase_order SET state='chargeback' WHERE id=purchase.id AND state IN ('paid','refunded');
    UPDATE music_entitlement SET status='revoked',revoked_at=COALESCE(revoked_at,NOW())
      WHERE purchase_order_id=purchase.id AND status='active';
  ELSIF NEW.status IN ('cancelled','expired','failed') THEN
    UPDATE music_purchase_order SET state='cancelled'
      WHERE id=purchase.id AND state IN ('pending','awaiting_payment');
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_sync_verified_checkout
AFTER UPDATE OF status ON commerce_checkout_session
FOR EACH ROW EXECUTE FUNCTION music_sync_verified_checkout();

CREATE OR REPLACE FUNCTION music_record_state_event()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.state IS DISTINCT FROM NEW.state THEN
    INSERT INTO music_release_audit_event(
      release_id, release_version_id, actor_party_id, event_type, prior_state, next_state, data
    ) VALUES (
      NEW.release_id, NEW.id, NULL, 'state_changed', OLD.state, NEW.state,
      jsonb_build_object('database_actor', session_user)
    );

    IF NEW.state IN (
      'ready_for_review','in_review','changes_requested','approved','scheduled',
      'published','suspended','takedown_scheduled','withdrawn'
    ) THEN
      INSERT INTO notification(
        recipient_party_id, notif_type, title, body, target_type, target_id
      )
      SELECT DISTINCT
        recipient.party_id,
        'music_release_' || NEW.state,
        CASE NEW.state
          WHEN 'ready_for_review' THEN 'Lanzamiento listo para revisión'
          WHEN 'in_review' THEN 'Lanzamiento en revisión'
          WHEN 'changes_requested' THEN 'Cambios solicitados en un lanzamiento'
          WHEN 'approved' THEN 'Lanzamiento aprobado'
          WHEN 'scheduled' THEN 'Lanzamiento programado'
          WHEN 'published' THEN 'Lanzamiento publicado'
          WHEN 'suspended' THEN 'Lanzamiento suspendido'
          WHEN 'takedown_scheduled' THEN 'Retiro de lanzamiento programado'
          ELSE 'Lanzamiento retirado'
        END,
        NEW.title || ' cambió al estado ' || NEW.state || '.',
        'music_release',
        release.artist_party_id
      FROM music_release release
      CROSS JOIN LATERAL (
        SELECT release.artist_party_id AS party_id
        UNION
        SELECT membership.member_party_id
        FROM artist_release_team_member membership
        WHERE membership.artist_party_id=release.artist_party_id
          AND membership.revoked_at IS NULL
          AND (membership.expires_at IS NULL OR membership.expires_at > NOW())
        UNION
        SELECT role.party_id
        FROM party_role role
        WHERE NEW.state='ready_for_review' AND role.role='admin' AND role.active
      ) recipient
      WHERE release.id=NEW.release_id;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_record_state_event
AFTER UPDATE OF state ON music_release_version
FOR EACH ROW EXECUTE FUNCTION music_record_state_event();

CREATE OR REPLACE FUNCTION music_notify_infringement_report()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO notification(
    recipient_party_id, notif_type, title, body, target_type, target_id
  )
  SELECT DISTINCT
    recipient.party_id,
    'music_release_infringement_reported',
    'Nuevo reporte sobre un lanzamiento',
    version.title || ' recibió un reporte que requiere revisión.',
    'music_release',
    release.artist_party_id
  FROM music_release release
  JOIN music_release_version version ON version.id=release.published_version_id
  CROSS JOIN LATERAL (
    SELECT release.artist_party_id AS party_id
    UNION
    SELECT membership.member_party_id
    FROM artist_release_team_member membership
    WHERE membership.artist_party_id=release.artist_party_id
      AND membership.revoked_at IS NULL
      AND (membership.expires_at IS NULL OR membership.expires_at > NOW())
      AND membership.role_code IN ('owner','admin')
    UNION
    SELECT role.party_id FROM party_role role WHERE role.role='admin' AND role.active
  ) recipient
  WHERE release.id=NEW.release_id;
  RETURN NEW;
END;
$$;

CREATE TRIGGER trg_music_notify_infringement_report
AFTER INSERT ON music_infringement_report
FOR EACH ROW EXECUTE FUNCTION music_notify_infringement_report();

CREATE OR REPLACE FUNCTION music_scan_legacy_release_sanitation(
  after_legacy_id BIGINT DEFAULT 0,
  batch_size INTEGER DEFAULT 100
)
RETURNS TABLE(scanned_count INTEGER, next_cursor BIGINT) LANGUAGE plpgsql AS $$
DECLARE
  scanned INTEGER;
  cursor_value BIGINT;
BEGIN
  IF after_legacy_id < 0 THEN
    RAISE EXCEPTION 'after_legacy_id must be non-negative';
  END IF;
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  WITH candidates AS (
    SELECT legacy.*
    FROM artist_release legacy
    WHERE legacy.id > after_legacy_id
    ORDER BY legacy.id
    LIMIT batch_size
  ), upserted AS (
    INSERT INTO music_legacy_sanitation_item(
      legacy_release_id, issues, scan_attempts, last_scanned_at
    )
    SELECT
      candidate.id,
      ARRAY_REMOVE(ARRAY[
        CASE WHEN NULLIF(btrim(candidate.title), '') IS NULL THEN 'missing_title' END,
        CASE WHEN candidate.release_date IS NULL THEN 'missing_release_date' END,
        CASE WHEN NULLIF(btrim(COALESCE(candidate.cover_image_url, '')), '') IS NULL THEN 'missing_cover' END,
        CASE WHEN NULLIF(btrim(COALESCE(candidate.spotify_url, '')), '') IS NULL
               AND NULLIF(btrim(COALESCE(candidate.youtube_url, '')), '') IS NULL THEN 'missing_audio_source' END,
        'missing_release_kind', 'missing_master', 'missing_rights', 'missing_credits',
        'missing_access_policy'
      ], NULL),
      1,
      NOW()
    FROM candidates candidate
    ON CONFLICT (legacy_release_id) DO UPDATE
      SET issues = EXCLUDED.issues,
          scan_attempts = music_legacy_sanitation_item.scan_attempts + 1,
          last_scanned_at = NOW()
      WHERE music_legacy_sanitation_item.status IN ('pending','in_progress')
    RETURNING legacy_release_id
  )
  SELECT count(*)::INTEGER, COALESCE(max(candidate.id), after_legacy_id)
  INTO scanned, cursor_value
  FROM candidates candidate;

  RETURN QUERY SELECT scanned, cursor_value;
END;
$$;

CREATE OR REPLACE FUNCTION music_publish_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
BEGIN
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  FOR candidate IN
    SELECT v.id, v.release_id, v.replaces_version_id
    FROM music_release_version v
    JOIN music_release r ON r.id = v.release_id
    WHERE v.state = 'scheduled'
      AND v.release_at_utc <= NOW()
      AND (v.embargo_until_utc IS NULL OR v.embargo_until_utc <= NOW())
      AND r.withdrawn_at IS NULL
    ORDER BY v.release_at_utc, v.id
    FOR UPDATE OF v, r SKIP LOCKED
    LIMIT batch_size
  LOOP
    IF candidate.replaces_version_id IS NOT NULL THEN
      UPDATE music_release_version AS replaced
      SET state = 'replacement_pending', updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'published';

      UPDATE music_release_version AS replaced
      SET state = 'takedown_scheduled',
          takedown_at_utc = COALESCE(takedown_at_utc, NOW()),
          takedown_timezone = COALESCE(takedown_timezone, 'UTC'),
          updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'replacement_pending';

      UPDATE music_release_version AS replaced
      SET state = 'withdrawn', updated_at = NOW()
      WHERE replaced.id = candidate.replaces_version_id
        AND replaced.release_id = candidate.release_id
        AND replaced.state = 'takedown_scheduled';
    END IF;

    UPDATE music_release_version
    SET state = 'published', published_at = COALESCE(published_at, NOW()), updated_at = NOW()
    WHERE id = candidate.id AND state = 'scheduled';

    IF FOUND THEN
      UPDATE music_release
      SET published_version_id = candidate.id, updated_at = NOW()
      WHERE id = candidate.release_id AND published_version_id IS DISTINCT FROM candidate.id;
      release_id := candidate.release_id;
      version_id := candidate.id;
      RETURN NEXT;
    END IF;
  END LOOP;
END;
$$;

CREATE OR REPLACE FUNCTION music_withdraw_due(batch_size INTEGER DEFAULT 100)
RETURNS TABLE(release_id UUID, version_id UUID) LANGUAGE plpgsql AS $$
DECLARE
  candidate RECORD;
BEGIN
  IF batch_size < 1 OR batch_size > 1000 THEN
    RAISE EXCEPTION 'batch_size must be between 1 and 1000';
  END IF;

  FOR candidate IN
    SELECT version.id, version.release_id
    FROM music_release_version version
    JOIN music_release release ON release.id = version.release_id
    WHERE version.state = 'takedown_scheduled'
      AND version.takedown_at_utc <= NOW()
    ORDER BY version.takedown_at_utc, version.id
    FOR UPDATE OF version, release SKIP LOCKED
    LIMIT batch_size
  LOOP
    UPDATE music_release_version
    SET state = 'withdrawn', updated_at = NOW()
    WHERE id = candidate.id AND state = 'takedown_scheduled';

    IF FOUND THEN
      UPDATE music_release
      SET published_version_id = CASE WHEN published_version_id = candidate.id THEN NULL ELSE published_version_id END,
          withdrawn_at = CASE WHEN published_version_id = candidate.id THEN COALESCE(withdrawn_at, NOW()) ELSE withdrawn_at END,
          updated_at = NOW()
      WHERE id = candidate.release_id;
      release_id := candidate.release_id;
      version_id := candidate.id;
      RETURN NEXT;
    END IF;
  END LOOP;
END;
$$;

CREATE VIEW music_public_release AS
SELECT
  r.id,
  r.artist_party_id,
  r.canonical_slug,
  r.release_kind,
  v.id AS release_version_id,
  v.version_number,
  v.title,
  v.subtitle,
  v.version_title,
  v.display_artist,
  v.explicit_content,
  v.original_release_date,
  v.release_at_utc,
  v.label_name,
  v.catalog_number,
  v.recording_copyright_text,
  v.work_copyright_text,
  v.published_at
FROM music_release r
JOIN music_release_version v ON v.id = r.published_version_id
WHERE r.withdrawn_at IS NULL
  AND v.state = 'published'
  AND v.published_at IS NOT NULL
  AND v.published_at <= NOW()
  AND (v.embargo_until_utc IS NULL OR v.embargo_until_utc <= NOW());

-- The asset identifier is opaque but not an authorization secret. Repeat the
-- release/territory/time policy for every public signature, including artwork.
CREATE OR REPLACE FUNCTION music_public_asset_accessible(
  target_asset_id UUID,
  territory_code TEXT
) RETURNS BOOLEAN LANGUAGE sql STABLE AS $$
  SELECT EXISTS (
    SELECT 1
    FROM music_asset asset
    JOIN music_public_release public
      ON public.release_version_id=asset.release_version_id
    WHERE asset.id=target_asset_id
      AND asset.processing_state='ready'
      AND EXISTS (
        SELECT 1
        FROM music_availability_rule visibility
        WHERE visibility.release_version_id=public.release_version_id
          AND (visibility.starts_at IS NULL OR visibility.starts_at<=NOW())
          AND (visibility.ends_at IS NULL OR visibility.ends_at>NOW())
          AND (
            (
              visibility.territory_mode='include'
              AND (
                'Worldwide'=ANY(visibility.territories)
                OR territory_code=ANY(visibility.territories)
              )
            )
            OR (
              visibility.territory_mode='exclude'
              AND territory_code IS NOT NULL
              AND NOT (territory_code=ANY(visibility.territories))
            )
          )
      )
      AND (
        asset.asset_role IN ('cover_display','thumbnail')
        OR (
          asset.asset_role IN ('stream_audio','preview_audio')
          AND EXISTS (
            SELECT 1
            FROM music_release_track track
            JOIN music_availability_rule rule
              ON rule.release_version_id=public.release_version_id
             AND (rule.release_track_id IS NULL OR rule.release_track_id=track.id)
            WHERE track.release_version_id=public.release_version_id
              AND track.recording_id=asset.recording_id
              AND (
                (asset.asset_role='stream_audio' AND rule.listening_policy='full')
                OR (
                  asset.asset_role='preview_audio'
                  AND rule.listening_policy IN ('preview','full')
                )
              )
              AND (rule.starts_at IS NULL OR rule.starts_at<=NOW())
              AND (rule.ends_at IS NULL OR rule.ends_at>NOW())
              AND (
                (
                  rule.territory_mode='include'
                  AND (
                    'Worldwide'=ANY(rule.territories)
                    OR territory_code=ANY(rule.territories)
                  )
                )
                OR (
                  rule.territory_mode='exclude'
                  AND territory_code IS NOT NULL
                  AND NOT (territory_code=ANY(rule.territories))
                )
              )
          )
        )
      )
  );
$$;

CREATE VIEW music_legacy_release_sanitation_queue AS
SELECT
  legacy.id AS legacy_release_id,
  legacy.artist_party_id,
  legacy.title,
  sanitation.status,
  sanitation.issues,
  sanitation.scan_attempts,
  sanitation.last_error,
  sanitation.last_scanned_at,
  sanitation.canonical_release_id,
  legacy.created_at AS legacy_created_at
FROM artist_release legacy
JOIN music_legacy_sanitation_item sanitation ON sanitation.legacy_release_id=legacy.id
WHERE sanitation.status IN ('pending','in_progress');

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('music_releases.authoring', FALSE, 'sandbox', 'Enable after runtime and migration verification'),
  ('music_releases.authoring', FALSE, 'production', 'Enable after artist-team authorization review'),
  ('music_releases.processing', FALSE, 'sandbox', 'Enable after storage and FFmpeg worker verification'),
  ('music_releases.processing', FALSE, 'production', 'Enable after production object-store credentials and observability'),
  ('music_releases.public', FALSE, 'sandbox', 'Enable after embargo and withdrawal integration verification'),
  ('music_releases.public', FALSE, 'production', 'Enable only after legacy compatibility validation'),
  ('music_releases.commerce', FALSE, 'sandbox', 'Enable after canonical checkout adapter verification'),
  ('music_releases.commerce', FALSE, 'production', 'Enable after finance and refund review'),
  ('music_releases.ddex_export', FALSE, 'sandbox', 'Enable after licensed official XSD/profile runtime validation'),
  ('music_releases.ddex_export', FALSE, 'production', 'Requires real sender/recipient DPID and DDEX implementation licence')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
