-- Apply after the foundation fixture and migration in a disposable database.
ALTER TABLE event_logistics_dependency ADD COLUMN created_at timestamptz NOT NULL DEFAULT now();
CREATE TABLE event_logistics_assignment (
  id bigserial PRIMARY KEY,
  activity_id bigint NOT NULL REFERENCES event_logistics_activity(id),
  party_id text, external_name text, external_phone text, external_email text,
  created_at timestamptz NOT NULL DEFAULT now()
);
