-- Metadata table for TDF App (sessions/assets)
CREATE TABLE IF NOT EXISTS metadata_records (
  id SERIAL PRIMARY KEY,
  catalog_id TEXT UNIQUE NOT NULL,
  artist_name TEXT NOT NULL,
  project_title TEXT NOT NULL,
  session_type TEXT NOT NULL,
  record_date DATE,
  release_date DATE,
  location TEXT,
  roles JSONB DEFAULT '{}'::jsonb,
  recording_chain JSONB DEFAULT '{}'::jsonb,
  rights_holder JSONB DEFAULT '{}'::jsonb,
  license_status TEXT,
  bpm INTEGER,
  key TEXT,
  genre TEXT,
  mood TEXT[],
  asset_links JSONB DEFAULT '{}'::jsonb,
  notes TEXT,
  created_at TIMESTAMPTZ DEFAULT now(),
  updated_at TIMESTAMPTZ DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_metadata_records_catalog_id ON metadata_records(catalog_id);
CREATE INDEX IF NOT EXISTS idx_metadata_records_artist ON metadata_records(artist_name);
CREATE INDEX IF NOT EXISTS idx_metadata_records_project ON metadata_records(project_title);
CREATE INDEX IF NOT EXISTS idx_metadata_records_session_type ON metadata_records(session_type);

CREATE OR REPLACE FUNCTION trg_metadata_records_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS metadata_records_set_updated_at ON metadata_records;
CREATE TRIGGER metadata_records_set_updated_at
BEFORE UPDATE ON metadata_records
FOR EACH ROW EXECUTE FUNCTION trg_metadata_records_updated_at();
