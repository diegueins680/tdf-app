-- PostgreSQL integration test for the structured Records CMS cutover.
-- Run only against a disposable database initialized by the candidate app.

\set ON_ERROR_STOP on
\set run_code 'records-cms-backfill-postgres-integration-v2'
\set candidate_revision 'integration-fixture-v2'
\set safety_threshold 0

CREATE TEMP TABLE records_cms_before AS
SELECT count(*) AS cms_rows,
  md5(string_agg(id::text || ':' || slug || ':' || locale || ':' || version::text || ':' ||
    status || ':' || COALESCE(payload::text,''),'|' ORDER BY id)) AS cms_digest
FROM cms_content;

\ir ../../sql/2026-08-07_records_cms_backfill_dry_run.sql
\ir ../../sql/2026-08-07_records_cms_backfill_apply.sql

CREATE TEMP TABLE records_cms_after_first AS
SELECT
  (SELECT count(*) FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    WHERE run.run_code=:'run_code' AND run.candidate_revision=:'candidate_revision'
      AND NOT run.dry_run) AS mappings,
  (SELECT count(*) FROM catalog_audit_event
    WHERE operation='records-cms-backfilled'
      AND correlation_id LIKE :'run_code' || ':cms_content:%') AS audits,
  (SELECT count(*) FROM collection_release membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-releases') AS release_memberships,
  (SELECT count(*) FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-recordings') AS recording_memberships,
  (SELECT count(*) FROM collection_session membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-sessions') AS session_memberships,
  (SELECT sum(version) FROM record_external_resource) AS resource_version_sum,
  (SELECT md5(COALESCE(string_agg(
      release_id::text || ':' || resource_id::text || ':' || relation_kind || ':' ||
      sort_order::text || ':' || primary_resource::text,
      '|' ORDER BY release_id,resource_id,relation_kind), ''))
    FROM release_external_resource) AS release_resource_digest;

DO $integration$
DECLARE
  before_row records_cms_before%ROWTYPE;
  current_rows bigint;
  current_digest text;
  run_status text;
  mapped_count bigint;
  unresolved_rows bigint;
BEGIN
  SELECT * INTO before_row FROM records_cms_before;
  SELECT count(*),md5(string_agg(id::text || ':' || slug || ':' || locale || ':' || version::text || ':' ||
    status || ':' || COALESCE(payload::text,''),'|' ORDER BY id))
    INTO current_rows,current_digest FROM cms_content;
  SELECT run.status,run.mapped_rows,run.ambiguous_rows+run.rejected_rows
    INTO run_status,mapped_count,unresolved_rows
  FROM catalog_backfill_run run
  WHERE run.run_code='records-cms-backfill-postgres-integration-v2'
    AND run.candidate_revision='integration-fixture-v2' AND NOT run.dry_run;

  IF current_rows<>before_row.cms_rows OR current_digest<>before_row.cms_digest THEN
    RAISE EXCEPTION 'backfill mutated legacy CMS rows';
  END IF;
  IF run_status<>'completed' OR mapped_count<>78 OR unresolved_rows<>0 THEN
    RAISE EXCEPTION 'unexpected run result: status=%, mapped=%, unresolved=%',
      run_status,mapped_count,unresolved_rows;
  END IF;
  IF EXISTS (
    SELECT 1 FROM catalog_migration_mapping mapping
    JOIN catalog_backfill_run run ON run.id=mapping.run_id
    WHERE run.run_code='records-cms-backfill-postgres-integration-v2'
      AND run.candidate_revision='integration-fixture-v2' AND NOT run.dry_run
      AND (mapping.status<>'mapped' OR mapping.entity_id IS NULL)
  ) THEN
    RAISE EXCEPTION 'a Records source value was not deterministically mapped';
  END IF;
  IF EXISTS (
    SELECT 1 FROM release_external_resource
    WHERE relation_kind='primary-stream'
  ) THEN
    RAISE EXCEPTION 'legacy primary-stream relationships were not consolidated';
  END IF;
END
$integration$;

-- The same apply may update its run summary but must not duplicate identities,
-- relationships, source mappings, audit events, or semantic resource versions.
\ir ../../sql/2026-08-07_records_cms_backfill_apply.sql

DO $integration$
DECLARE
  first_row records_cms_after_first%ROWTYPE;
  current_mappings bigint;
  current_audits bigint;
  current_releases bigint;
  current_recordings bigint;
  current_sessions bigint;
  current_resource_versions bigint;
  current_release_resource_digest text;
BEGIN
  SELECT * INTO first_row FROM records_cms_after_first;
  SELECT count(*) INTO current_mappings
  FROM catalog_migration_mapping mapping
  JOIN catalog_backfill_run run ON run.id=mapping.run_id
  WHERE run.run_code='records-cms-backfill-postgres-integration-v2'
    AND run.candidate_revision='integration-fixture-v2' AND NOT run.dry_run;
  SELECT count(*) INTO current_audits FROM catalog_audit_event
  WHERE operation='records-cms-backfilled'
    AND correlation_id LIKE 'records-cms-backfill-postgres-integration-v2:cms_content:%';
  SELECT count(*) INTO current_releases FROM collection_release membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-releases';
  SELECT count(*) INTO current_recordings FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-recordings';
  SELECT count(*) INTO current_sessions FROM collection_session membership
    JOIN editorial_collection collection ON collection.id=membership.collection_id
    WHERE collection.code='tdf-records-sessions';
  SELECT sum(version) INTO current_resource_versions FROM record_external_resource;
  SELECT md5(COALESCE(string_agg(
      release_id::text || ':' || resource_id::text || ':' || relation_kind || ':' ||
      sort_order::text || ':' || primary_resource::text,
      '|' ORDER BY release_id,resource_id,relation_kind), ''))
    INTO current_release_resource_digest
  FROM release_external_resource;

  IF (current_mappings,current_audits,current_releases,current_recordings,current_sessions,
      current_resource_versions,current_release_resource_digest)
    IS DISTINCT FROM
    (first_row.mappings,first_row.audits,first_row.release_memberships,
      first_row.recording_memberships,first_row.session_memberships,
      first_row.resource_version_sum,first_row.release_resource_digest) THEN
    RAISE EXCEPTION 'Records apply is not idempotent';
  END IF;
END
$integration$;

\ir ../../sql/2026-08-07_records_cms_backfill_rollback.sql
\ir ../../sql/2026-08-07_records_cms_backfill_rollback.sql

DO $integration$
DECLARE
  run_status text;
  before_row records_cms_before%ROWTYPE;
  current_rows bigint;
  current_digest text;
BEGIN
  SELECT status INTO run_status FROM catalog_backfill_run
  WHERE run_code='records-cms-backfill-postgres-integration-v2'
    AND candidate_revision='integration-fixture-v2' AND NOT dry_run;
  SELECT * INTO before_row FROM records_cms_before;
  SELECT count(*),md5(string_agg(id::text || ':' || slug || ':' || locale || ':' || version::text || ':' ||
    status || ':' || COALESCE(payload::text,''),'|' ORDER BY id))
    INTO current_rows,current_digest FROM cms_content;
  IF run_status<>'rolled-back' THEN
    RAISE EXCEPTION 'expected rolled-back status, got %',run_status;
  END IF;
  IF current_rows<>before_row.cms_rows OR current_digest<>before_row.cms_digest THEN
    RAISE EXCEPTION 'rollback changed preserved CMS rows';
  END IF;
END
$integration$;

SELECT 'Records CMS backfill PostgreSQL integration checks passed' AS result;
