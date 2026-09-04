-- Intentionally preserve the historical ledger on application rollback. It is
-- audit evidence and has no public aggregate reader. Re-enabling an older
-- application is therefore safe without deleting review provenance.
\set ON_ERROR_STOP on
BEGIN;

INSERT INTO reputation_audit_log(action, resource_kind, resource_id, metadata)
VALUES (
  'reputation.legacy-signal.rollback-preserved',
  'reputation_legacy_signal',
  'experience_review',
  jsonb_build_object(
    'rollback', '2026-09-04_contextual_reputation_legacy_signals',
    'rowsPreserved', (SELECT count(*) FROM reputation_legacy_signal),
    'reason', 'history is retained; no public aggregate consumes it'
  )
);

COMMIT;
