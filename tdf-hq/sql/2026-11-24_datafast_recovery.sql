-- A saved Dataweb checkout can predate its provider operation. Keep query
-- progress on its immutable binding across worker restarts and API replicas.
BEGIN;
CREATE TABLE IF NOT EXISTS commerce_datafast_recovery_query (
  binding_id UUID PRIMARY KEY REFERENCES commerce_provider_binding(id) ON DELETE RESTRICT,
  attempts INTEGER NOT NULL CHECK (attempts BETWEEN 1 AND 24),
  next_query_at TIMESTAMPTZ NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('pending','completed','review')),
  outcome TEXT NOT NULL
);
COMMIT;
