\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'records-cms-cutover-2026-08-07'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 0
\endif

BEGIN;
SET LOCAL statement_timeout='15min';
SET LOCAL lock_timeout='2s';
SET LOCAL idle_in_transaction_session_timeout='16min';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-records-cms-backfill-v1',0));

\ir 2026-08-07_records_cms_backfill_source.sql

-- No normalized write occurs until all deterministic identity checks pass.
WITH safety AS (
  SELECT
    (SELECT count(*) FROM records_cms_candidates WHERE mapping_status<>'mapped')
    + GREATEST(0, 3-(SELECT count(*) FROM records_cms_latest
        WHERE locale='es' AND slug IN ('records-releases','records-recordings','records-sessions')))
    + (SELECT count(*) FROM records_cms_latest
        WHERE jsonb_typeof(payload) IS DISTINCT FROM 'object'
           OR (slug='records-releases' AND jsonb_typeof(payload->'tracks') IS DISTINCT FROM 'array')
           OR (slug IN ('records-recordings','records-sessions') AND jsonb_typeof(payload->'videos') IS DISTINCT FROM 'array'))
    + (SELECT count(*) FROM records_cms_collection_resources
        WHERE external_id IS NULL OR canonical_url IS NULL OR canonical_url !~ '^https://')
    AS issue_count
)
SELECT 1 / CASE WHEN issue_count <= :safety_threshold THEN 1 ELSE 0 END AS safety_gate
FROM safety;

INSERT INTO catalog_backfill_run (
  id,run_code,candidate_revision,dry_run,status,safety_threshold,started_at,correlation_id
)
VALUES (
  gen_random_uuid(),:'run_code',:'candidate_revision',FALSE,'mapping',:safety_threshold,
  now(),:'run_code' || ':' || :'candidate_revision'
)
ON CONFLICT (run_code,candidate_revision,dry_run)
DO UPDATE SET status='mapping',safety_threshold=EXCLUDED.safety_threshold,completed_at=NULL;

SELECT id AS records_backfill_run_id
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

CREATE TEMP TABLE records_entity_source ON COMMIT DROP AS
SELECT entity_kind,external_id,
  COALESCE(max(item_title) FILTER (WHERE locale='es'),max(item_title)) AS title_es,
  COALESCE(max(item_title) FILTER (WHERE locale='en'),max(item_title) FILTER (WHERE locale='es'),max(item_title)) AS title_en,
  COALESCE(max(item_description) FILTER (WHERE locale='es'),max(item_description)) AS description_es,
  COALESCE(max(item_description) FILTER (WHERE locale='en'),max(item_description) FILTER (WHERE locale='es'),max(item_description)) AS description_en,
  COALESCE(max(contributor_credit) FILTER (WHERE locale='es'),max(contributor_credit)) AS contributor_credit,
  max(canonical_url) AS canonical_url,
  max(duration_ms) AS duration_ms,
  max(image_url) AS image_url,
  min(sort_order) AS sort_order,
  max(slug) FILTER (WHERE slug LIKE 'records-%-%' AND slug NOT IN ('records-releases','records-recordings','records-sessions')) AS historical_slug
FROM records_cms_candidates
WHERE mapping_status='mapped'
GROUP BY entity_kind,external_id;

-- Explicit editorial collections replace the former container slugs.
INSERT INTO editorial_collection (
  id,catalog_id,code,collection_type,name_es,name_en,description_es,description_en,
  public_route,sort_order,active,workflow_state_id,created_at,updated_at,published_revision,version
)
SELECT gen_random_uuid(),catalog.id,seed.code,seed.kind,seed.name_es,seed.name_en,
  seed.description_es,seed.description_en,'/records',seed.sort_order,TRUE,state.id,
  now(),now(),1,1
FROM (VALUES
  ('tdf-records-releases','release','RELEASES by TDF','RELEASES by TDF','Lanzamientos oficiales del sello.','Official label releases.',10),
  ('tdf-records-recordings','recording','Videos recientes TDF Records','Recent TDF Records videos','Grabaciones publicadas por TDF Records.','Recordings published by TDF Records.',20),
  ('tdf-records-sessions','session','TDF Live Sessions','TDF Live Sessions','Sesiones en vivo publicadas por TDF.','Live sessions published by TDF.',30)
) seed(code,kind,name_es,name_en,description_es,description_en,sort_order)
JOIN catalog_definition catalog ON catalog.code='editorial-collections'
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published'
ON CONFLICT (code) DO NOTHING;

UPDATE editorial_collection collection
SET name_es=source.title,updated_at=now(),version=collection.version+1
FROM records_cms_latest source
WHERE source.locale='es' AND source.title IS NOT NULL
  AND collection.code=CASE source.slug
    WHEN 'records-releases' THEN 'tdf-records-releases'
    WHEN 'records-recordings' THEN 'tdf-records-recordings'
    WHEN 'records-sessions' THEN 'tdf-records-sessions' END
  AND collection.name_es IS DISTINCT FROM source.title;

UPDATE editorial_collection collection
SET name_en=source.title,updated_at=now(),version=collection.version+1
FROM records_cms_latest source
WHERE source.locale='en' AND source.title IS NOT NULL
  AND collection.code=CASE source.slug
    WHEN 'records-releases' THEN 'tdf-records-releases'
    WHEN 'records-recordings' THEN 'tdf-records-recordings'
    WHEN 'records-sessions' THEN 'tdf-records-sessions' END
  AND collection.name_en IS DISTINCT FROM source.title;

-- Preserve exact historic credits as one reviewed credited-ensemble. Splitting
-- comma-separated labels would be an unproven identity decision.
INSERT INTO record_contributor (
  id,catalog_id,code,contributor_kind,name_es,name_en,sort_order,active,
  workflow_state_id,created_at,updated_at,version
)
SELECT DISTINCT gen_random_uuid(),catalog.id,
  'legacy-credit-' || left(encode(digest(lower(btrim(source.contributor_credit)),'sha256'),'hex'),20),
  'credited-ensemble',source.contributor_credit,source.contributor_credit,0,TRUE,state.id,
  now(),now(),1
FROM records_entity_source source
JOIN catalog_definition catalog ON catalog.code='record-contributors'
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published'
ON CONFLICT (code) DO NOTHING;

INSERT INTO record_external_resource (
  id,provider_id,external_code,resource_kind,canonical_url,duration_ms,thumbnail_url,
  active,created_at,updated_at,version
)
SELECT gen_random_uuid(),provider.id,source.external_id,
  CASE WHEN source.entity_kind='release' THEN 'audio-track' ELSE 'video' END,
  source.canonical_url,source.duration_ms,source.image_url,TRUE,now(),now(),1
FROM records_entity_source source
JOIN external_provider provider
  ON provider.code=CASE WHEN source.entity_kind='release' THEN 'spotify' ELSE 'youtube' END
  AND provider.active
ON CONFLICT (provider_id,resource_kind,external_code) DO UPDATE
SET canonical_url=EXCLUDED.canonical_url,duration_ms=EXCLUDED.duration_ms,
    thumbnail_url=EXCLUDED.thumbnail_url,updated_at=EXCLUDED.updated_at,
    version=record_external_resource.version+1
WHERE (record_external_resource.canonical_url,record_external_resource.duration_ms,record_external_resource.thumbnail_url)
  IS DISTINCT FROM (EXCLUDED.canonical_url,EXCLUDED.duration_ms,EXCLUDED.thumbnail_url);

INSERT INTO record_release (
  id,catalog_id,code,release_type_id,title_es,title_en,description_es,description_en,
  current_slug,sort_order,active,workflow_state_id,created_at,updated_at,published_revision,usage_count,version
)
SELECT gen_random_uuid(),catalog.id,'spotify-release-' || source.external_id,type.id,
  source.title_es,source.title_en,source.description_es,source.description_en,
  source.historical_slug,source.sort_order,TRUE,state.id,now(),now(),1,0,1
FROM records_entity_source source
JOIN catalog_definition catalog ON catalog.code='records-releases'
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published'
JOIN release_type_reference type ON type.code='single' AND type.active
WHERE source.entity_kind='release'
ON CONFLICT (code) DO NOTHING;

-- Every released track is also an explicitly related sound recording.
INSERT INTO recording (
  id,catalog_id,code,recording_type_id,title_es,title_en,description_es,description_en,
  duration_ms,current_slug,sort_order,active,workflow_state_id,created_at,updated_at,
  published_revision,usage_count,version
)
SELECT gen_random_uuid(),catalog.id,
  CASE source.entity_kind WHEN 'release' THEN 'spotify-recording-' || source.external_id
    WHEN 'recording' THEN 'youtube-recording-' || source.external_id
    ELSE 'youtube-session-recording-' || source.external_id END,
  type.id,source.title_es,source.title_en,source.description_es,source.description_en,
  source.duration_ms,CASE WHEN source.entity_kind='recording' THEN source.historical_slug END,
  source.sort_order,TRUE,state.id,now(),now(),1,0,1
FROM records_entity_source source
JOIN catalog_definition catalog ON catalog.code='records-recordings'
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published'
JOIN recording_type_reference type ON type.code=CASE WHEN source.entity_kind='release' THEN 'sound-recording' ELSE 'music-video' END AND type.active
ON CONFLICT (code) DO NOTHING;

INSERT INTO recording_session (
  id,catalog_id,code,session_type_id,title_es,title_en,description_es,description_en,
  current_slug,sort_order,active,workflow_state_id,created_at,updated_at,published_revision,usage_count,version
)
SELECT gen_random_uuid(),catalog.id,'youtube-session-' || source.external_id,type.id,
  source.title_es,source.title_en,source.description_es,source.description_en,
  source.historical_slug,source.sort_order,TRUE,state.id,now(),now(),1,0,1
FROM records_entity_source source
JOIN catalog_definition catalog ON catalog.code='records-sessions'
JOIN workflow_state state ON state.workflow_id=catalog.workflow_id AND state.code='published'
JOIN recording_session_type type ON type.code='recording' AND type.active
WHERE source.entity_kind='session'
ON CONFLICT (code) DO NOTHING;

-- Canonical relationships and curated ordering.
INSERT INTO release_recording (id,release_id,recording_id,disc_number,sort_order,primary_recording)
SELECT gen_random_uuid(),release.id,recording.id,1,0,TRUE
FROM records_entity_source source
JOIN record_release release ON release.code='spotify-release-' || source.external_id
JOIN recording recording ON recording.code='spotify-recording-' || source.external_id
WHERE source.entity_kind='release'
ON CONFLICT (release_id,recording_id) DO NOTHING;

INSERT INTO session_recording (id,session_id,recording_id,sort_order,primary_recording)
SELECT gen_random_uuid(),session.id,recording.id,0,TRUE
FROM records_entity_source source
JOIN recording_session session ON session.code='youtube-session-' || source.external_id
JOIN recording recording ON recording.code='youtube-session-recording-' || source.external_id
WHERE source.entity_kind='session'
ON CONFLICT (session_id,recording_id) DO NOTHING;

INSERT INTO release_contributor (id,release_id,contributor_id,credit_role,sort_order,primary_credit)
SELECT gen_random_uuid(),release.id,contributor.id,'primary-artist',0,TRUE
FROM records_entity_source source
JOIN record_release release ON release.code='spotify-release-' || source.external_id
JOIN record_contributor contributor ON contributor.code='legacy-credit-' || left(encode(digest(lower(btrim(source.contributor_credit)),'sha256'),'hex'),20)
WHERE source.entity_kind='release'
ON CONFLICT (release_id,contributor_id,credit_role) DO NOTHING;

INSERT INTO recording_contributor (id,recording_id,contributor_id,credit_role,sort_order,primary_credit)
SELECT gen_random_uuid(),recording.id,contributor.id,'primary-artist',0,TRUE
FROM records_entity_source source
JOIN recording recording ON recording.code=CASE source.entity_kind
  WHEN 'release' THEN 'spotify-recording-' || source.external_id
  WHEN 'recording' THEN 'youtube-recording-' || source.external_id
  ELSE 'youtube-session-recording-' || source.external_id END
JOIN record_contributor contributor ON contributor.code='legacy-credit-' || left(encode(digest(lower(btrim(source.contributor_credit)),'sha256'),'hex'),20)
ON CONFLICT (recording_id,contributor_id,credit_role) DO NOTHING;

INSERT INTO session_contributor (id,session_id,contributor_id,credit_role,sort_order,primary_credit)
SELECT gen_random_uuid(),session.id,contributor.id,'guest',0,TRUE
FROM records_entity_source source
JOIN recording_session session ON session.code='youtube-session-' || source.external_id
JOIN record_contributor contributor ON contributor.code='legacy-credit-' || left(encode(digest(lower(btrim(source.contributor_credit)),'sha256'),'hex'),20)
WHERE source.entity_kind='session'
ON CONFLICT (session_id,contributor_id,credit_role) DO NOTHING;

-- An earlier candidate seed called the same canonical relationship
-- "primary-stream". Consolidate that duplicate junction deterministically;
-- the referenced release and resource entities are never deleted.
DELETE FROM release_external_resource legacy
USING release_external_resource canonical
WHERE legacy.release_id=canonical.release_id
  AND legacy.resource_id=canonical.resource_id
  AND legacy.relation_kind='primary-stream'
  AND canonical.relation_kind='primary-audio';

UPDATE release_external_resource
SET relation_kind='primary-audio'
WHERE relation_kind='primary-stream';

INSERT INTO release_external_resource (id,release_id,resource_id,relation_kind,sort_order,primary_resource)
SELECT gen_random_uuid(),release.id,resource.id,'primary-audio',0,TRUE
FROM records_entity_source source
JOIN record_release release ON release.code='spotify-release-' || source.external_id
JOIN external_provider provider ON provider.code='spotify'
JOIN record_external_resource resource ON resource.provider_id=provider.id AND resource.resource_kind='audio-track' AND resource.external_code=source.external_id
WHERE source.entity_kind='release'
ON CONFLICT (release_id,resource_id,relation_kind) DO NOTHING;

INSERT INTO recording_external_resource (id,recording_id,resource_id,relation_kind,sort_order,primary_resource)
SELECT gen_random_uuid(),recording.id,resource.id,'primary-media',0,TRUE
FROM records_entity_source source
JOIN recording recording ON recording.code=CASE source.entity_kind
  WHEN 'release' THEN 'spotify-recording-' || source.external_id
  WHEN 'recording' THEN 'youtube-recording-' || source.external_id
  ELSE 'youtube-session-recording-' || source.external_id END
JOIN external_provider provider ON provider.code=CASE WHEN source.entity_kind='release' THEN 'spotify' ELSE 'youtube' END
JOIN record_external_resource resource ON resource.provider_id=provider.id
  AND resource.resource_kind=CASE WHEN source.entity_kind='release' THEN 'audio-track' ELSE 'video' END
  AND resource.external_code=source.external_id
ON CONFLICT (recording_id,resource_id,relation_kind) DO NOTHING;

INSERT INTO session_external_resource (id,session_id,resource_id,relation_kind,sort_order,primary_resource)
SELECT gen_random_uuid(),session.id,resource.id,'primary-video',0,TRUE
FROM records_entity_source source
JOIN recording_session session ON session.code='youtube-session-' || source.external_id
JOIN external_provider provider ON provider.code='youtube'
JOIN record_external_resource resource ON resource.provider_id=provider.id AND resource.resource_kind='video' AND resource.external_code=source.external_id
WHERE source.entity_kind='session'
ON CONFLICT (session_id,resource_id,relation_kind) DO NOTHING;

INSERT INTO collection_release (id,collection_id,release_id,sort_order,featured)
SELECT gen_random_uuid(),collection.id,release.id,source.sort_order,FALSE
FROM records_entity_source source
JOIN editorial_collection collection ON collection.code='tdf-records-releases'
JOIN record_release release ON release.code='spotify-release-' || source.external_id
WHERE source.entity_kind='release'
ON CONFLICT (collection_id,release_id) DO NOTHING;

INSERT INTO collection_recording (id,collection_id,recording_id,sort_order,featured)
SELECT gen_random_uuid(),collection.id,recording.id,source.sort_order,FALSE
FROM records_entity_source source
JOIN editorial_collection collection ON collection.code='tdf-records-recordings'
JOIN recording recording ON recording.code='youtube-recording-' || source.external_id
WHERE source.entity_kind='recording'
ON CONFLICT (collection_id,recording_id) DO NOTHING;

INSERT INTO collection_session (id,collection_id,session_id,sort_order,featured)
SELECT gen_random_uuid(),collection.id,session.id,source.sort_order,FALSE
FROM records_entity_source source
JOIN editorial_collection collection ON collection.code='tdf-records-sessions'
JOIN recording_session session ON session.code='youtube-session-' || source.external_id
WHERE source.entity_kind='session'
ON CONFLICT (collection_id,session_id) DO NOTHING;

-- Provider-level collection links remain typed relationships too.
INSERT INTO record_external_resource (
  id,provider_id,external_code,resource_kind,canonical_url,thumbnail_url,active,created_at,updated_at,version
)
SELECT gen_random_uuid(),provider.id,source.external_id,source.resource_kind,
  source.canonical_url,source.thumbnail_url,TRUE,now(),now(),1
FROM records_cms_collection_resources source
JOIN external_provider provider ON provider.code=source.provider_code AND provider.active
WHERE source.external_id IS NOT NULL AND source.canonical_url ~ '^https://'
ON CONFLICT (provider_id,resource_kind,external_code) DO UPDATE
SET canonical_url=EXCLUDED.canonical_url,thumbnail_url=EXCLUDED.thumbnail_url,
    updated_at=EXCLUDED.updated_at,version=record_external_resource.version+1
WHERE (record_external_resource.canonical_url,record_external_resource.thumbnail_url)
  IS DISTINCT FROM (EXCLUDED.canonical_url,EXCLUDED.thumbnail_url);

INSERT INTO collection_external_resource (id,collection_id,resource_id,relation_kind,sort_order,primary_resource)
SELECT gen_random_uuid(),collection.id,resource.id,source.resource_kind,0,TRUE
FROM records_cms_collection_resources source
JOIN editorial_collection collection ON collection.code=source.collection_code
JOIN external_provider provider ON provider.code=source.provider_code
JOIN record_external_resource resource ON resource.provider_id=provider.id
  AND resource.resource_kind=source.resource_kind AND resource.external_code=source.external_id
WHERE source.external_id IS NOT NULL AND source.canonical_url ~ '^https://'
ON CONFLICT (collection_id,resource_id,relation_kind) DO NOTHING;

-- Per-value immutable source mappings support review, evidence, and reruns.
INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'records_backfill_run_id'::uuid,'cms_content',
  CASE candidate.entity_kind WHEN 'release' THEN 'payload.tracks[]'
    WHEN 'recording' THEN 'payload.videos[]' ELSE 'payload.videos[]' END,
  candidate.cms_content_id::text || ':' || candidate.source_order,candidate.item::text,
  COALESCE(candidate.external_id,''),catalog.id,
  CASE candidate.entity_kind
    WHEN 'release' THEN (SELECT id FROM record_release WHERE code='spotify-release-' || candidate.external_id)
    WHEN 'recording' THEN (SELECT id FROM recording WHERE code='youtube-recording-' || candidate.external_id)
    ELSE (SELECT id FROM recording_session WHERE code='youtube-session-' || candidate.external_id)
  END,
  candidate.mapping_status,candidate.evidence,1,now()
FROM records_cms_candidates candidate
JOIN catalog_definition catalog ON catalog.code=CASE candidate.entity_kind
  WHEN 'release' THEN 'records-releases'
  WHEN 'recording' THEN 'records-recordings'
  ELSE 'records-sessions' END
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET normalized_value=EXCLUDED.normalized_value,entity_id=EXCLUDED.entity_id,
  status=EXCLUDED.status,evidence=EXCLUDED.evidence,source_count=EXCLUDED.source_count;

INSERT INTO catalog_slug_alias (
  id,catalog_id,entity_kind,entity_id,scope,slug,current,redirect_status,created_at
)
SELECT gen_random_uuid(),mapping.catalog_id,
  CASE candidate.entity_kind WHEN 'release' THEN 'record-release'
    WHEN 'recording' THEN 'recording' ELSE 'recording-session' END,
  mapping.entity_id,'records-public',candidate.slug,FALSE,308,now()
FROM records_cms_candidates candidate
JOIN catalog_migration_mapping mapping
  ON mapping.run_id=:'records_backfill_run_id'::uuid
 AND mapping.source_record_id=candidate.cms_content_id::text || ':' || candidate.source_order
 AND mapping.status='mapped'
WHERE candidate.slug NOT IN ('records-releases','records-recordings','records-sessions')
ON CONFLICT (scope,slug) DO NOTHING;

INSERT INTO catalog_audit_event (
  id,catalog_id,entity_id,operation,occurred_at,source_platform,reason,
  correlation_id,result,affected_relationships
)
SELECT gen_random_uuid(),mapping.catalog_id,mapping.entity_id,'records-cms-backfilled',now(),
  'production-migration',mapping.evidence,
  :'run_code' || ':cms_content:' || mapping.source_record_id,mapping.status,
  jsonb_build_object('sourceTable','cms_content','sourceRecordId',mapping.source_record_id,
    'originalValue',mapping.original_value,'normalizedProviderId',mapping.normalized_value)
FROM catalog_migration_mapping mapping
WHERE mapping.run_id=:'records_backfill_run_id'::uuid AND mapping.entity_id IS NOT NULL
  AND NOT EXISTS (SELECT 1 FROM catalog_audit_event audit
    WHERE audit.correlation_id=:'run_code' || ':cms_content:' || mapping.source_record_id);

UPDATE catalog_backfill_run
SET status='completed',
  scanned_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'records_backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'records_backfill_run_id'::uuid AND status='mapped'),
  ambiguous_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'records_backfill_run_id'::uuid AND status='ambiguous'),
  rejected_rows=(SELECT count(*) FROM catalog_migration_mapping WHERE run_id=:'records_backfill_run_id'::uuid AND status IN ('unresolved','rejected')),
  completed_at=now(),
  report=jsonb_build_object(
    'sourceCmsRows',(SELECT count(*) FROM records_cms_latest),
    'mapped',(SELECT count(*) FROM records_cms_candidates WHERE mapping_status='mapped'),
    'ambiguous',(SELECT count(*) FROM records_cms_candidates WHERE mapping_status='ambiguous'),
    'unresolvedOrRejected',(SELECT count(*) FROM records_cms_candidates WHERE mapping_status IN ('unresolved','rejected')),
    'releases',(SELECT count(*) FROM records_entity_source WHERE entity_kind='release'),
    'recordings',(SELECT count(*) FROM records_entity_source WHERE entity_kind='recording'),
    'sessions',(SELECT count(*) FROM records_entity_source WHERE entity_kind='session'),
    'legacyCmsRowsPreserved',TRUE
  )::text
WHERE id=:'records_backfill_run_id'::uuid;

SELECT jsonb_build_object(
  'runId',id,'runCode',run_code,'revision',candidate_revision,'status',status,
  'scanned',scanned_rows,'mapped',mapped_rows,'ambiguous',ambiguous_rows,
  'rejected',rejected_rows,'report',report
)
FROM catalog_backfill_run WHERE id=:'records_backfill_run_id'::uuid;

COMMIT;
