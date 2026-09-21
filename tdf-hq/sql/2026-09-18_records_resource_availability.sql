-- Additive resource status and audited correction for provider-deleted videos.
-- Evidence: docs/records-ingestion/2026-09-18-thumbnail-evidence.md.
BEGIN;
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '2min';
SELECT pg_advisory_xact_lock(hashtextextended('records-resource-verification', 0));
ALTER TABLE record_external_resource ADD COLUMN IF NOT EXISTS availability text;
ALTER TABLE record_external_resource ADD COLUMN IF NOT EXISTS availability_reason text;
ALTER TABLE record_external_resource ADD COLUMN IF NOT EXISTS verified_at timestamptz;
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_constraint WHERE conname='record_resource_availability_valid'
                 AND conrelid='record_external_resource'::regclass) THEN
    ALTER TABLE record_external_resource ADD CONSTRAINT record_resource_availability_valid
      CHECK (availability IS NULL OR availability IN ('available','unavailable'));
  END IF;
END $$;

-- Keep the original identities, memberships and editorial fields. Never borrow
-- thumbnails from the newer, differently edited uploads with similar titles.
INSERT INTO catalog_backfill_run
(id,run_code,candidate_revision,dry_run,status,safety_threshold,scanned_rows,
 mapped_rows,ambiguous_rows,rejected_rows,started_at,completed_at,report,correlation_id)
SELECT gen_random_uuid(),'records-resource-availability-2026-09-18','provider-deleted-v1',false,
 'completed',0,count(*),count(*),0,0,now(),now(),
 jsonb_build_object('evidence','youtube-watch-page:removed-by-uploader:2026-09-18',
   'before',coalesce(jsonb_agg(jsonb_build_object('id',r.id,'thumbnailUrl',r.thumbnail_url,
     'availability',r.availability,'availabilityReason',r.availability_reason,
     'verifiedAt',r.verified_at,'version',r.version)), '[]'::jsonb))::text,
 'records-resource-availability-2026-09-18'
FROM record_external_resource r JOIN external_provider p ON p.id=r.provider_id
WHERE p.code='youtube' AND r.resource_kind='video'
  AND r.external_code IN ('ooPsIHsikYU','Cb7VGZJ6apo')
  AND r.canonical_url='https://www.youtube.com/watch?v=' || r.external_code
  AND r.verified_at IS NULL
ON CONFLICT (run_code,candidate_revision,dry_run) DO NOTHING;

UPDATE record_external_resource r SET
 availability='unavailable',availability_reason='removed_by_uploader',
 verified_at='2026-09-18T16:24:00Z',
 thumbnail_url=CASE WHEN r.thumbnail_url='https://i.ytimg.com/vi/' || r.external_code || '/hqdefault.jpg'
                    THEN NULL ELSE r.thumbnail_url END,
 version=r.version+1,updated_at=now()
FROM catalog_backfill_run b, LATERAL jsonb_array_elements(b.report::jsonb->'before') old
WHERE b.run_code='records-resource-availability-2026-09-18' AND b.candidate_revision='provider-deleted-v1'
 AND NOT b.dry_run AND r.id=(old->>'id')::uuid AND r.version=(old->>'version')::bigint;
COMMIT;
