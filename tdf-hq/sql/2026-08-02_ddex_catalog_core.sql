-- DDEX & Catalog Core Schema
-- Date: 2026-08-02
-- Description: Foundation tables for DDEX Gateway and Canonical Catalog

-- Enable UUID extension if not already enabled
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- =========================================================
-- CATALOG CORE TABLES
-- =========================================================

-- 1. Catalog Releases
CREATE TABLE IF NOT EXISTS catalog_release (
    id SERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    sub_title TEXT,
    release_type TEXT NOT NULL CHECK (release_type IN ('Album', 'Single', 'EP', 'Compilation', 'LiveAlbum', 'RemixAlbum', 'Soundtrack', 'SpokenWord')),
    release_date TIMESTAMPTZ,
    original_release_date TIMESTAMPTZ,
    label_name TEXT,
    status TEXT NOT NULL DEFAULT 'Draft' CHECK (status IN ('Draft', 'Active', 'Takedown')),
    copyright_line TEXT,
    phonographic_copyright_line TEXT,
    genre TEXT,
    cover_art_asset_id INTEGER, -- FK to catalog_asset
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_catalog_release_type ON catalog_release(release_type);
CREATE INDEX IF NOT EXISTS idx_catalog_release_status ON catalog_release(status);

-- 2. Catalog Resources (Recordings, Videos, Images)
CREATE TABLE IF NOT EXISTS catalog_resource (
    id SERIAL PRIMARY KEY,
    resource_type TEXT NOT NULL CHECK (resource_type IN ('SoundRecording', 'MusicVideo', 'Image', 'Text', 'Software')),
    title TEXT NOT NULL,
    version TEXT,
    duration_ms INTEGER,
    language_code TEXT,
    is_explicit BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 3. Release-Resource Link (Tracklist)
CREATE TABLE IF NOT EXISTS catalog_release_resource (
    id SERIAL PRIMARY KEY,
    release_id INTEGER NOT NULL REFERENCES catalog_release(id) ON DELETE CASCADE,
    resource_id INTEGER NOT NULL REFERENCES catalog_resource(id) ON DELETE CASCADE,
    disc_number INTEGER NOT NULL DEFAULT 1,
    sequence INTEGER NOT NULL,
    is_primary BOOLEAN NOT NULL DEFAULT TRUE,
    UNIQUE (release_id, disc_number, sequence)
);

CREATE INDEX IF NOT EXISTS idx_catalog_release_resource_release ON catalog_release_resource(release_id);

-- 4. Identifiers (ISRC, UPC, IPI, ISNI, DPID)
CREATE TABLE IF NOT EXISTS catalog_identifier (
    id SERIAL PRIMARY KEY,
    entity_id INTEGER NOT NULL,
    entity_type TEXT NOT NULL CHECK (entity_type IN ('Release', 'Resource', 'Party')),
    scheme TEXT NOT NULL CHECK (scheme IN ('ISRC', 'UPC', 'EAN', 'GRid', 'IPI', 'ISNI', 'DPID', 'Proprietary')),
    value TEXT NOT NULL,
    namespace TEXT,
    UNIQUE (entity_id, entity_type, scheme, value)
);

CREATE INDEX IF NOT EXISTS idx_catalog_identifier_value ON catalog_identifier(value);
CREATE INDEX IF NOT EXISTS idx_catalog_identifier_entity ON catalog_identifier(entity_id, entity_type);

-- 5. Credits
CREATE TABLE IF NOT EXISTS catalog_credit (
    id SERIAL PRIMARY KEY,
    entity_id INTEGER NOT NULL,
    entity_type TEXT NOT NULL CHECK (entity_type IN ('Release', 'Resource')),
    party_id INTEGER NOT NULL, -- FK to existing 'party' table
    role TEXT NOT NULL CHECK (role IN ('MainArtist', 'FeaturedArtist', 'Producer', 'Engineer', 'Mixer', 'MasteringEngineer', 'Composer', 'Lyricist', 'Arranger', 'Performer', 'StudioMusician')),
    credit_text TEXT,
    sequence INTEGER
);

CREATE INDEX IF NOT EXISTS idx_catalog_credit_party ON catalog_credit(party_id);

-- 6. Deals
CREATE TABLE IF NOT EXISTS catalog_deal (
    id SERIAL PRIMARY KEY,
    release_id INTEGER REFERENCES catalog_release(id),
    resource_id INTEGER REFERENCES catalog_resource(id),
    model TEXT NOT NULL CHECK (model IN ('ExclusiveLicense', 'DistributionAgreement', 'AdministrationDeal', 'PressAndDistribution')),
    start_date TIMESTAMPTZ NOT NULL,
    end_date TIMESTAMPTZ,
    takedown_date TIMESTAMPTZ,
    partner_name TEXT NOT NULL
);

-- 7. Deal Territories
CREATE TABLE IF NOT EXISTS catalog_deal_territory (
    id SERIAL PRIMARY KEY,
    deal_id INTEGER NOT NULL REFERENCES catalog_deal(id) ON DELETE CASCADE,
    territory_code TEXT NOT NULL, -- ISO 3166 or 'Worldwide'
    is_included BOOLEAN NOT NULL DEFAULT TRUE
);

-- 8. Assets (Files)
CREATE TABLE IF NOT EXISTS catalog_asset (
    id SERIAL PRIMARY KEY,
    asset_type TEXT NOT NULL CHECK (asset_type IN ('AudioFile', 'ImageFile', 'DocumentFile', 'VideoFile')),
    uri TEXT NOT NULL, -- Private S3/GCS URI
    logical_name TEXT NOT NULL,
    mime_type TEXT NOT NULL,
    size_bytes BIGINT NOT NULL,
    sha256 TEXT NOT NULL,
    metadata_json JSONB
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_catalog_asset_sha256 ON catalog_asset(sha256);

-- 9. Source Links (Provenance from DDEX)
CREATE TABLE IF NOT EXISTS catalog_source_link (
    id SERIAL PRIMARY KEY,
    entity_id INTEGER NOT NULL,
    entity_type TEXT NOT NULL,
    ddex_document_id INTEGER, -- FK to ddex_document
    ddex_xpath_reference TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- =========================================================
-- DDEX GATEWAY TABLES
-- =========================================================

-- 10. DDEX Documents
CREATE TABLE IF NOT EXISTS ddex_document (
    id SERIAL PRIMARY KEY,
    file_name TEXT NOT NULL,
    private_uri TEXT NOT NULL,
    sha256 TEXT NOT NULL UNIQUE,
    size_bytes BIGINT NOT NULL,
    family TEXT NOT NULL, -- 'ERN', 'RIN', 'DSR'
    version TEXT NOT NULL, -- '4.3.2', '2.1'
    namespace TEXT,
    message_type TEXT,
    status TEXT NOT NULL DEFAULT 'received' CHECK (status IN ('received', 'quarantined', 'queued', 'validating', 'invalid', 'valid', 'mapping_required', 'ready_to_import', 'importing', 'imported', 'import_failed', 'superseded')),
    uploaded_by INTEGER NOT NULL, -- FK to app_user
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ddex_document_status ON ddex_document(status);
CREATE INDEX IF NOT EXISTS idx_ddex_document_sha256 ON ddex_document(sha256);

-- 11. Message Headers (Parsed)
CREATE TABLE IF NOT EXISTS ddex_message_header (
    id SERIAL PRIMARY KEY,
    document_id INTEGER NOT NULL REFERENCES ddex_document(id) ON DELETE CASCADE,
    message_id TEXT NOT NULL,
    thread_id TEXT,
    sender_dpid TEXT,
    recipient_dpid TEXT,
    created_date TIMESTAMPTZ,
    control_type TEXT
);

-- 12. Validation Runs
CREATE TABLE IF NOT EXISTS ddex_validation_run (
    id SERIAL PRIMARY KEY,
    document_id INTEGER NOT NULL REFERENCES ddex_document(id) ON DELETE CASCADE,
    validator_version TEXT,
    schema_version TEXT,
    started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    finished_at TIMESTAMPTZ,
    result TEXT CHECK (result IN ('Success', 'Failure', 'Warning')),
    error_count INTEGER DEFAULT 0,
    warning_count INTEGER DEFAULT 0
);

-- 13. Validation Issues
CREATE TABLE IF NOT EXISTS ddex_validation_issue (
    id SERIAL PRIMARY KEY,
    validation_run_id INTEGER NOT NULL REFERENCES ddex_validation_run(id) ON DELETE CASCADE,
    severity TEXT NOT NULL CHECK (severity IN ('Error', 'Warning', 'Info')),
    layer TEXT NOT NULL CHECK (layer IN ('XML', 'XSD', 'AVS', 'Business')),
    code TEXT,
    line_number INTEGER,
    column_number INTEGER,
    xpath_ref TEXT,
    message TEXT NOT NULL,
    suggestion TEXT
);

-- 14. Import Plans
CREATE TABLE IF NOT EXISTS ddex_import_plan (
    id SERIAL PRIMARY KEY,
    document_id INTEGER NOT NULL REFERENCES ddex_document(id) ON DELETE CASCADE,
    status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft', 'resolved', 'committed', 'abandoned')),
    snapshot_json JSONB NOT NULL,
    version INTEGER NOT NULL DEFAULT 1,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 15. Import Runs (Execution Log)
CREATE TABLE IF NOT EXISTS ddex_import_run (
    id SERIAL PRIMARY KEY,
    plan_id INTEGER NOT NULL REFERENCES ddex_import_plan(id),
    actor_id INTEGER NOT NULL,
    status TEXT NOT NULL CHECK (status IN ('Pending', 'Running', 'Success', 'Failed', 'RolledBack')),
    started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    finished_at TIMESTAMPTZ,
    error_message TEXT
);

-- 16. Import Changes (Audit per entity)
CREATE TABLE IF NOT EXISTS ddex_import_change (
    id SERIAL PRIMARY KEY,
    import_run_id INTEGER NOT NULL REFERENCES ddex_import_run(id),
    entity_type TEXT NOT NULL,
    entity_id INTEGER,
    operation TEXT NOT NULL CHECK (operation IN ('Create', 'Update', 'Skip')),
    previous_state JSONB,
    new_state JSONB
);

-- 17. Exports
CREATE TABLE IF NOT EXISTS ddex_export (
    id SERIAL PRIMARY KEY,
    release_id INTEGER NOT NULL REFERENCES catalog_release(id),
    partner_id INTEGER, -- FK to ddex_partner
    ern_version TEXT NOT NULL,
    profile_name TEXT,
    xml_checksum TEXT NOT NULL,
    private_uri TEXT NOT NULL,
    validation_result TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 18. Partners
CREATE TABLE IF NOT EXISTS ddex_partner (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    dpid TEXT,
    allowed_versions TEXT[] NOT NULL DEFAULT '{"4.3.2"}',
    rules_json JSONB,
    naming_convention TEXT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE
);

-- 19. Background Jobs
CREATE TABLE IF NOT EXISTS ddex_job (
    id SERIAL PRIMARY KEY,
    job_type TEXT NOT NULL CHECK (job_type IN ('Validate', 'Import', 'Export', 'Cleanup')),
    entity_id INTEGER NOT NULL, -- Document ID, Plan ID, etc.
    status TEXT NOT NULL DEFAULT 'Pending' CHECK (status IN ('Pending', 'Processing', 'Completed', 'Failed', 'Retry')),
    attempts INTEGER NOT NULL DEFAULT 0,
    leased_until TIMESTAMPTZ,
    last_error TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ddex_job_status ON ddex_job(status, leased_until);

-- =========================================================
-- TRIGGERS FOR UPDATED_AT
-- =========================================================
CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Only create triggers if they don't already exist
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_catalog_release_updated_at') THEN
    CREATE TRIGGER trg_catalog_release_updated_at BEFORE UPDATE ON catalog_release FOR EACH ROW EXECUTE PROCEDURE trigger_set_timestamp();
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_catalog_resource_updated_at') THEN
    CREATE TRIGGER trg_catalog_resource_updated_at BEFORE UPDATE ON catalog_resource FOR EACH ROW EXECUTE PROCEDURE trigger_set_timestamp();
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'trg_ddex_job_updated_at') THEN
    CREATE TRIGGER trg_ddex_job_updated_at BEFORE UPDATE ON ddex_job FOR EACH ROW EXECUTE PROCEDURE trigger_set_timestamp();
  END IF;
END $$;

-- =========================================================
-- COMMENTS
-- =========================================================
COMMENT ON TABLE catalog_release IS 'Canonical releases independent of DDEX format';
COMMENT ON TABLE ddex_document IS 'Immutable record of received DDEX messages';
COMMENT ON TABLE ddex_job IS 'Queue for background processing without external broker';
