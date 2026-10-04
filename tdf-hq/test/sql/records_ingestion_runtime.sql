-- Runs only in the disposable catalog fixture. Real canonical defaults are
-- supplied here because the legacy migration fixture predates ORM defaults.
CREATE TABLE party(id bigint PRIMARY KEY);
CREATE TABLE social_sync_account(id bigserial PRIMARY KEY,platform text,external_user_id text,party_id bigint,artist_profile_id bigint,records_ingestion jsonb,last_synced_at timestamptz,updated_at timestamptz);
CREATE TABLE session_external_resource(resource_id uuid);
ALTER TABLE editorial_collection ADD COLUMN workflow_state_id uuid;
UPDATE editorial_collection SET workflow_state_id=(SELECT s.id FROM workflow_state s JOIN catalog_definition c ON c.workflow_id=s.workflow_id WHERE c.code='records-recordings' AND s.code='published');
ALTER TABLE record_external_resource ALTER id SET DEFAULT gen_random_uuid(), ALTER active SET DEFAULT true, ALTER created_at SET DEFAULT now(), ALTER updated_at SET DEFAULT now(), ALTER version SET DEFAULT 1;
ALTER TABLE recording ALTER id SET DEFAULT gen_random_uuid(), ALTER active SET DEFAULT true, ALTER created_at SET DEFAULT now(), ALTER updated_at SET DEFAULT now(), ALTER version SET DEFAULT 1, ALTER sort_order SET DEFAULT 0, ALTER usage_count SET DEFAULT 0;
ALTER TABLE recording_external_resource ALTER id SET DEFAULT gen_random_uuid();
ALTER TABLE collection_recording ALTER id SET DEFAULT gen_random_uuid(), ALTER featured SET DEFAULT false;

ALTER TABLE catalog_backfill_run ALTER id SET DEFAULT gen_random_uuid(), ALTER safety_threshold SET DEFAULT 0, ALTER scanned_rows SET DEFAULT 0, ALTER mapped_rows SET DEFAULT 0, ALTER ambiguous_rows SET DEFAULT 0, ALTER rejected_rows SET DEFAULT 0;
