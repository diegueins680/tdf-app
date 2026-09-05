-- Preserve private preference/ranking history on application rollback. Neither
-- table has a public reader, so retaining it is safer than deleting it.
\set ON_ERROR_STOP on
BEGIN;

INSERT INTO reputation_audit_log(action, resource_kind, resource_id, metadata)
VALUES (
  'reputation.private-preferences.rollback-preserved',
  'reputation_personal_preference',
  'all',
  jsonb_build_object(
    'rollback', '2026-09-04_contextual_reputation_private_preferences',
    'preferencesPreserved', (SELECT count(*) FROM reputation_personal_preference),
    'privateRankingItemsPreserved', (SELECT count(*) FROM reputation_private_ranking_item),
    'reason', 'private history remains private and is not a public aggregate input'
  )
);

COMMIT;
