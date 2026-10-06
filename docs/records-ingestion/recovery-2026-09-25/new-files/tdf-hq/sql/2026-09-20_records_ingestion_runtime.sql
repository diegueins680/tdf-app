-- Canonical provider accounts, catalog resources and catalog run ledger only.
-- Disabled until an administrator verifies and approves a channel and collection.
BEGIN;
SET LOCAL lock_timeout='5s';
ALTER TABLE social_sync_account ADD COLUMN IF NOT EXISTS records_ingestion jsonb;
ALTER TABLE record_external_resource ADD COLUMN IF NOT EXISTS provider_metadata jsonb;
ALTER TABLE record_external_resource ADD COLUMN IF NOT EXISTS source_account_id bigint REFERENCES social_sync_account(id);
CREATE TABLE IF NOT EXISTS records_ingestion_control (
  singleton boolean PRIMARY KEY DEFAULT true CHECK(singleton),
  enabled boolean NOT NULL DEFAULT false,
  interval_seconds integer NOT NULL DEFAULT 3600 CHECK(interval_seconds BETWEEN 300 AND 86400),
  updated_at timestamptz NOT NULL DEFAULT now()
);
INSERT INTO records_ingestion_control(singleton) VALUES(true) ON CONFLICT DO NOTHING;
CREATE TABLE IF NOT EXISTS records_ingestion_quota (
  day date PRIMARY KEY, reserved_units integer NOT NULL CHECK(reserved_units BETWEEN 0 AND 9000)
);
CREATE TABLE IF NOT EXISTS records_ingestion_change (
  id bigserial PRIMARY KEY,
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  resource_id uuid REFERENCES record_external_resource(id),
  source_account_id bigint NOT NULL REFERENCES social_sync_account(id),
  action text NOT NULL,
  before_value jsonb,
  after_value jsonb,
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS records_ingestion_admin_audit (
  id bigserial PRIMARY KEY, actor_id bigint NOT NULL REFERENCES party(id),
  action text NOT NULL, details jsonb NOT NULL, created_at timestamptz NOT NULL DEFAULT now()
);
-- No approval, enablement, import or publication is performed by schema rollout.
CREATE OR REPLACE FUNCTION tdf_ingest_public_video(source_id bigint, run_key uuid, payload jsonb)
RETURNS text LANGUAGE plpgsql AS $$
DECLARE
  account social_sync_account%ROWTYPE;
  run catalog_backfill_run%ROWTYPE;
  resource record_external_resource%ROWTYPE;
  previous jsonb;
  published_state uuid;
  target_collection uuid;
  recording_key uuid;
  recording_count integer;
  resource_key uuid;
  video_id text := payload->>'id';
  verified timestamptz := (payload->>'verifiedAt')::timestamptz;
  video_duration_ms bigint := (payload->>'durationSeconds')::bigint * 1000;
  image_url text := payload->>'thumbnailUrl';
  insertion_order bigint;
  outcome text := 'updated';
BEGIN
  PERFORM 1 FROM records_ingestion_control WHERE singleton AND enabled FOR SHARE;
  IF NOT FOUND THEN RAISE EXCEPTION 'records ingestion is stopped'; END IF;
  PERFORM pg_advisory_xact_lock(20260920, source_id::integer);
  SELECT * INTO STRICT account FROM social_sync_account WHERE id=source_id FOR UPDATE;
  IF account.platform IS DISTINCT FROM 'youtube' OR account.records_ingestion->>'enabled' IS DISTINCT FROM 'true'
     OR nullif(account.records_ingestion->>'approvalReference','') IS NULL
     OR account.records_ingestion->>'approvedAt' IS NULL THEN
    RAISE EXCEPTION 'source is not approved and enabled';
  END IF;
  SELECT * INTO STRICT run FROM catalog_backfill_run WHERE id=run_key FOR UPDATE;
  IF run.status<>'running' OR run.dry_run
     OR run.report::jsonb->>'sourceAccountId' IS DISTINCT FROM source_id::text THEN
    RAISE EXCEPTION 'invalid ingestion run';
  END IF;
  IF (video_id ~ '^[A-Za-z0-9_-]{11}$') IS NOT TRUE OR payload->>'channelId' IS DISTINCT FROM account.external_user_id
     OR payload->>'privacyStatus' IS DISTINCT FROM 'public' OR payload->>'uploadStatus' IS DISTINCT FROM 'processed'
     OR payload->>'eligible' IS DISTINCT FROM 'true' OR verified IS NULL
     OR nullif(btrim(payload->>'title'),'') IS NULL OR video_duration_ms IS NULL OR video_duration_ms<=0
     OR (payload->>'publishedAt')::timestamptz IS NULL THEN
    RAISE EXCEPTION 'video metadata is not eligible';
  END IF;
  IF image_url IS NOT NULL AND image_url !~ ('^https://i\.ytimg\.com/vi(_webp)?/'||video_id||'/[A-Za-z0-9_.-]+([?].*)?$') THEN
    RAISE EXCEPTION 'thumbnail identity mismatch';
  END IF;
  target_collection := (account.records_ingestion->>'collectionId')::uuid;
  SELECT s.id INTO STRICT published_state FROM workflow_state s
    JOIN catalog_definition c ON c.workflow_id=s.workflow_id
    WHERE c.code='records-recordings' AND c.active AND s.code='published' AND s.active;
  PERFORM 1 FROM editorial_collection WHERE id=target_collection
    AND collection_type='recording' AND active AND workflow_state_id=published_state FOR UPDATE;
  IF NOT FOUND THEN RAISE EXCEPTION 'approved recording collection is unavailable'; END IF;
  SELECT r.* INTO resource FROM record_external_resource r
    JOIN external_provider p ON p.id=r.provider_id
    WHERE p.code='youtube' AND r.resource_kind='video' AND r.external_code=video_id FOR UPDATE OF r;
  IF FOUND THEN
    IF resource.source_account_id IS NOT NULL AND resource.source_account_id<>source_id THEN
      RAISE EXCEPTION 'resource belongs to another verified source';
    END IF;
    IF resource.canonical_url <> 'https://www.youtube.com/watch?v='||video_id
       AND resource.canonical_url NOT LIKE 'https://www.youtube.com/watch?v='||video_id||'&%' THEN
      RETURN 'reviewed';
    END IF;
    IF resource.verified_at>=verified THEN RETURN 'stale'; END IF;
    IF NOT resource.active THEN RETURN 'reviewed'; END IF;
    resource_key:=resource.id;
    previous:=to_jsonb(resource);
    -- A verified provider refresh owns only generated/provider URLs or the last
    -- exact value it wrote. Preserve all other editorial artwork.
    IF resource.thumbnail_url IS NOT NULL
       AND resource.thumbnail_url !~ ('^https://i\.ytimg\.com/vi(_webp)?/'||video_id||'/')
       AND resource.thumbnail_url IS DISTINCT FROM resource.provider_metadata->>'thumbnailUrl' THEN
      image_url:=resource.thumbnail_url;
    END IF;
    IF (resource.provider_metadata - 'verifiedAt') = (payload - 'verifiedAt')
       AND resource.availability='available' AND resource.thumbnail_url IS NOT DISTINCT FROM image_url THEN
      outcome:='unchanged';
    END IF;
    UPDATE record_external_resource SET provider_metadata=payload,source_account_id=source_id,
      verified_at=verified,availability='available',availability_reason=NULL,
      thumbnail_url=image_url,
      duration_ms=CASE WHEN resource.duration_ms IS NULL OR resource.duration_ms=(resource.provider_metadata->>'durationSeconds')::bigint*1000 THEN video_duration_ms ELSE resource.duration_ms END,
      version=version+CASE WHEN outcome='unchanged' THEN 0 ELSE 1 END,
      updated_at=CASE WHEN outcome='unchanged' THEN updated_at ELSE now() END
      WHERE id=resource_key;
  ELSE
    outcome:='created';
    INSERT INTO record_external_resource(provider_id,external_code,resource_kind,canonical_url,
      duration_ms,thumbnail_url,availability,verified_at,provider_metadata,source_account_id)
    SELECT id,video_id,'video','https://www.youtube.com/watch?v='||video_id,
      video_duration_ms,image_url,'available',verified,payload,source_id FROM external_provider
      WHERE code='youtube' AND active RETURNING id INTO STRICT resource_key;
  END IF;
  -- Curated sessions retain their existing classification and membership.
  IF NOT EXISTS (SELECT 1 FROM session_external_resource WHERE resource_id=resource_key) THEN
    SELECT count(*),min(recording_id::text)::uuid INTO recording_count,recording_key
      FROM recording_external_resource WHERE resource_id=resource_key;
    IF recording_count>1 THEN RAISE EXCEPTION 'ambiguous recording association'; END IF;
    IF recording_key IS NULL THEN
      INSERT INTO recording(catalog_id,code,recording_type_id,title_es,title_en,
        description_es,description_en,duration_ms,workflow_state_id,published_revision)
      SELECT c.id,'youtube-recording-'||video_id,t.id,payload->>'title',payload->>'title',
        payload->>'description',payload->>'description',video_duration_ms,published_state,1
      FROM catalog_definition c CROSS JOIN recording_type_reference t
      WHERE c.code='records-recordings' AND c.active AND t.code='music-video' AND t.active
      RETURNING id INTO STRICT recording_key;
      INSERT INTO recording_external_resource(recording_id,resource_id,relation_kind,sort_order,primary_resource)
        VALUES(recording_key,resource_key,'primary-media',0,true);
      -- Existing editorial order is untouched. New automated items are ordered
      -- by verified publication time, never by recording or ingestion time.
      SELECT candidate INTO STRICT insertion_order FROM generate_series(
        -extract(epoch FROM (payload->>'publishedAt')::timestamptz)::bigint,
        -extract(epoch FROM (payload->>'publishedAt')::timestamptz)::bigint-1000,-1) candidate
        WHERE NOT EXISTS (SELECT 1 FROM collection_recording
          WHERE collection_id=target_collection AND sort_order=candidate) LIMIT 1;
      INSERT INTO collection_recording(collection_id,recording_id,sort_order)
        VALUES(target_collection,recording_key,insertion_order);
    END IF;
  END IF;
  -- Refresh only fields whose value still equals the last provider snapshot.
  -- An editor's different title, description, duration or publication state wins.
  IF recording_key IS NOT NULL AND previous->'provider_metadata' IS NOT NULL THEN
    UPDATE recording SET
      title_es=CASE WHEN title_es=previous->'provider_metadata'->>'title' THEN payload->>'title' ELSE title_es END,
      title_en=CASE WHEN title_en=previous->'provider_metadata'->>'title' THEN payload->>'title' ELSE title_en END,
      description_es=CASE WHEN description_es=previous->'provider_metadata'->>'description' THEN payload->>'description' ELSE description_es END,
      description_en=CASE WHEN description_en=previous->'provider_metadata'->>'description' THEN payload->>'description' ELSE description_en END,
      duration_ms=CASE WHEN recording.duration_ms=(previous->'provider_metadata'->>'durationSeconds')::bigint*1000 THEN (payload->>'durationSeconds')::bigint*1000 ELSE recording.duration_ms END,
      version=version+1,updated_at=now()
    WHERE id=recording_key AND active AND outcome='updated';
  END IF;
  IF outcome<>'unchanged' THEN
    INSERT INTO records_ingestion_change(run_id,resource_id,source_account_id,action,before_value,after_value)
      SELECT run_key,resource_key,source_id,outcome,previous,to_jsonb(r)
      FROM record_external_resource r WHERE r.id=resource_key;
  END IF;
  RETURN outcome;
END $$;
COMMIT;
