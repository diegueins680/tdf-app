-- Run while the writer is disabled. Retain additive columns and the audit so
-- both old/new binaries remain compatible. Refuse to erase subsequent edits.
BEGIN;
SET LOCAL lock_timeout = '5s';
SELECT pg_advisory_xact_lock(hashtextextended('records-resource-verification', 0));
UPDATE record_external_resource r SET
 thumbnail_url=old->>'thumbnailUrl',availability=old->>'availability',
 availability_reason=old->>'availabilityReason',verified_at=(old->>'verifiedAt')::timestamptz,
 version=(old->>'version')::bigint,updated_at=now()
FROM catalog_backfill_run b, LATERAL jsonb_array_elements(b.report::jsonb->'before') old
WHERE b.run_code='records-resource-availability-2026-09-18' AND b.candidate_revision='provider-deleted-v1'
 AND NOT b.dry_run AND r.id=(old->>'id')::uuid
 AND r.version=(old->>'version')::bigint+1
 AND r.availability='unavailable' AND r.availability_reason='removed_by_uploader'
 AND r.verified_at='2026-09-18T16:24:00Z';
COMMIT;
