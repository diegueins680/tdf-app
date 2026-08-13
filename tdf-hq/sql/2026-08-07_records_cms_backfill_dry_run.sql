\set ON_ERROR_STOP on

-- Temporary tables make the deterministic per-value report reviewable while
-- the final ROLLBACK guarantees that no persistent row can change.
BEGIN;
SET LOCAL statement_timeout='120s';
SET LOCAL lock_timeout='2s';
SET LOCAL idle_in_transaction_session_timeout='150s';

\ir 2026-08-07_records_cms_backfill_source.sql

SELECT jsonb_build_object(
  'report','records-cms-source',
  'publishedRows',(SELECT count(*) FROM records_cms_latest),
  'requiredSpanishContainers',(SELECT count(*) FROM records_cms_latest WHERE locale='es' AND slug IN ('records-releases','records-recordings','records-sessions')),
  'items',count(*),
  'mapped',count(*) FILTER (WHERE mapping_status='mapped'),
  'ambiguous',count(*) FILTER (WHERE mapping_status='ambiguous'),
  'unresolved',count(*) FILTER (WHERE mapping_status='unresolved'),
  'rejected',count(*) FILTER (WHERE mapping_status='rejected'),
  'byKind',jsonb_build_object(
    'release',count(*) FILTER (WHERE entity_kind='release'),
    'recording',count(*) FILTER (WHERE entity_kind='recording'),
    'session',count(*) FILTER (WHERE entity_kind='session')
  )
) FROM records_cms_candidates;

SELECT jsonb_build_object(
  'sourceRecordId',cms_content_id::text || ':' || source_order,
  'slug',slug,
  'locale',locale,
  'cmsVersion',cms_version,
  'kind',entity_kind,
  'externalId',external_id,
  'title',item_title,
  'status',mapping_status,
  'evidence',evidence
)
FROM records_cms_candidates
ORDER BY slug, locale, source_order;

SELECT jsonb_build_object(
  'report','records-collection-resources',
  'rows',count(*),
  'valid',count(*) FILTER (WHERE external_id IS NOT NULL AND canonical_url ~ '^https://'),
  'invalid',count(*) FILTER (WHERE external_id IS NULL OR canonical_url IS NULL OR canonical_url !~ '^https://')
) FROM records_cms_collection_resources;

ROLLBACK;
