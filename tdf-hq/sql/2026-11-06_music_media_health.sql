-- Source-versioned observations. Public suppression requires two permanent
-- observations at least 30 minutes apart. Transient failures never set invalid.
BEGIN;
SET LOCAL lock_timeout = '10s';
CREATE TABLE IF NOT EXISTS music_media_health (
  source_kind text NOT NULL CHECK (source_kind IN ('asset','legacy')),
  source_id text NOT NULL,
  source_version text NOT NULL,
  status text NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','valid','temporary','invalid')),
  reason text,
  checked_at timestamptz,
  first_failure_at timestamptz,
  next_check_at timestamptz NOT NULL DEFAULT now(),
  failure_count integer NOT NULL DEFAULT 0 CHECK (failure_count>=0),
  lease_token uuid,
  lease_until timestamptz,
  PRIMARY KEY(source_kind,source_id,source_version)
);
CREATE INDEX IF NOT EXISTS music_media_health_due ON music_media_health(next_check_at);
CREATE OR REPLACE VIEW music_media_source AS
 SELECT 'asset'::text AS source_kind,a.id::text AS source_id,
   encode(digest(concat_ws('|',a.storage_provider,a.bucket_name,a.object_key,a.sha256,a.byte_size,a.media_type),'sha256'),'hex') AS source_version,
   r.artist_party_id,a.release_version_id,
   jsonb_build_object('bucket',a.bucket_name,'key',a.object_key,'sha256',a.sha256,'mediaType',a.media_type) AS locator
 FROM music_asset a JOIN music_release_version v ON v.id=a.release_version_id
 JOIN music_release r ON r.id=v.release_id
 WHERE a.asset_role IN ('stream_audio','preview_audio') AND a.processing_state='ready'
 UNION ALL
 SELECT 'legacy',r.id::text||':'||u.slot,encode(digest(u.url,'sha256'),'hex'),r.artist_party_id,NULL::uuid,
   jsonb_build_object('url',u.url,'releaseId',r.id,'slot',u.slot)
 FROM artist_release r CROSS JOIN LATERAL (VALUES ('spotify',r.spotify_url),('youtube',r.youtube_url)) u(slot,url)
 WHERE u.url ~ '^https://' AND u.url !~* '^https://([^/]*\.)?(spotify\.com|youtube\.com|youtu\.be|youtube-nocookie\.com|spotify\.link|spoti\.fi)([:/]|$)';

CREATE OR REPLACE FUNCTION music_media_eligible(kind text,identity text)
RETURNS boolean LANGUAGE sql STABLE SET search_path=pg_catalog,public AS $$
 SELECT EXISTS(SELECT 1 FROM public.music_media_source s
 WHERE s.source_kind=kind AND s.source_id=identity AND NOT EXISTS(
   SELECT 1 FROM public.music_media_health h WHERE h.source_kind=s.source_kind
   AND h.source_id=s.source_id AND h.source_version=s.source_version AND h.status='invalid'));
$$;

-- A bounded resumable sweep: insertion is idempotent, expired leases are
-- reclaimable, old versions remain diagnostic history but can never block new bytes.
CREATE OR REPLACE FUNCTION music_media_reconcile(batch_size integer DEFAULT 100)
RETURNS integer LANGUAGE plpgsql SET search_path=pg_catalog,public AS $$
DECLARE changed integer;
BEGIN
 IF batch_size<1 OR batch_size>1000 THEN RAISE EXCEPTION 'media batch must be 1..1000'; END IF;
 INSERT INTO public.music_media_health(source_kind,source_id,source_version)
 SELECT s.source_kind,s.source_id,s.source_version FROM public.music_media_source s
 WHERE NOT EXISTS(SELECT 1 FROM public.music_media_health h WHERE h.source_kind=s.source_kind
   AND h.source_id=s.source_id AND h.source_version=s.source_version)
 ORDER BY s.source_kind,s.source_id LIMIT batch_size ON CONFLICT DO NOTHING;
 GET DIAGNOSTICS changed=ROW_COUNT;
 RETURN changed;
END $$;
CREATE OR REPLACE FUNCTION music_media_claim(token uuid)
RETURNS jsonb LANGUAGE plpgsql SET search_path=pg_catalog,public AS $$
DECLARE result jsonb;
BEGIN
 WITH candidate AS (
 SELECT h.source_kind,h.source_id,h.source_version FROM public.music_media_health h
 JOIN public.music_media_source s USING(source_kind,source_id,source_version)
 WHERE h.next_check_at<=now() AND (h.lease_until IS NULL OR h.lease_until<now())
 ORDER BY h.next_check_at,h.source_kind,h.source_id LIMIT 1 FOR UPDATE OF h SKIP LOCKED
 ), claimed AS (
 UPDATE public.music_media_health h SET lease_token=token,lease_until=now()+interval '90 seconds'
 FROM candidate c WHERE (h.source_kind,h.source_id,h.source_version)=(c.source_kind,c.source_id,c.source_version)
 RETURNING h.*)
 SELECT jsonb_build_object('kind',h.source_kind,'id',h.source_id,'version',h.source_version,'locator',s.locator)
 INTO result FROM claimed h JOIN public.music_media_source s USING(source_kind,source_id,source_version);
 RETURN result;
END $$;
CREATE OR REPLACE FUNCTION music_media_observe(token uuid,outcome text,diagnosis text)
RETURNS boolean LANGUAGE plpgsql SET search_path=pg_catalog,public AS $$
DECLARE h public.music_media_health; permanent boolean;
BEGIN
 IF outcome NOT IN ('valid','temporary','permanent') OR diagnosis NOT IN
 ('decodable','missing','unsupported','offline','timeout','server','authentication','expired','unsafe_url','too_large','inconclusive','transport')
 THEN RAISE EXCEPTION 'invalid media observation'; END IF;
 SELECT * INTO h FROM public.music_media_health WHERE lease_token=token AND lease_until>now() FOR UPDATE;
 IF NOT FOUND OR NOT EXISTS(SELECT 1 FROM public.music_media_source s WHERE
 (s.source_kind,s.source_id,s.source_version)=(h.source_kind,h.source_id,h.source_version)) THEN RETURN false; END IF;
 permanent := outcome='permanent' AND h.reason=diagnosis AND h.failure_count>=1
   AND h.first_failure_at<=now()-interval '30 minutes';
 UPDATE public.music_media_health SET
 status=CASE WHEN outcome='valid' THEN 'valid' WHEN permanent THEN 'invalid'
   WHEN h.status='invalid' THEN 'invalid' ELSE 'temporary' END,
 reason=diagnosis,checked_at=now(),
 first_failure_at=CASE WHEN outcome='valid' THEN NULL WHEN outcome='permanent' AND h.reason=diagnosis
   THEN COALESCE(h.first_failure_at,now()) WHEN outcome='permanent' THEN now() ELSE NULL END,
 failure_count=CASE WHEN outcome='valid' THEN 0 ELSE h.failure_count+1 END,
 next_check_at=now()+CASE WHEN outcome='valid' OR permanent OR h.status='invalid' THEN interval '24 hours'
   WHEN outcome='permanent' THEN interval '30 minutes' ELSE interval '15 minutes' END,
 lease_token=NULL,lease_until=NULL
 WHERE (source_kind,source_id,source_version)=(h.source_kind,h.source_id,h.source_version);
 RETURN true;
END $$;

-- The existing ingestion pipeline fully decodes audio before creating ready
-- derivatives. Reuse that outcome; other ingestion paths queue a fresh check.
CREATE OR REPLACE FUNCTION music_media_ingested() RETURNS trigger
LANGUAGE plpgsql SET search_path=pg_catalog,public AS $$
BEGIN
 INSERT INTO public.music_media_health(source_kind,source_id,source_version,status,reason,checked_at,next_check_at)
 SELECT s.source_kind,s.source_id,s.source_version,
 CASE WHEN TG_TABLE_NAME='music_asset' AND to_jsonb(NEW)->'provenance'->>'pipeline'='audio-v2' THEN 'valid' ELSE 'pending' END,
 CASE WHEN TG_TABLE_NAME='music_asset' AND to_jsonb(NEW)->'provenance'->>'pipeline'='audio-v2' THEN 'decodable' END,
 CASE WHEN TG_TABLE_NAME='music_asset' AND to_jsonb(NEW)->'provenance'->>'pipeline'='audio-v2' THEN now() END,
 CASE WHEN TG_TABLE_NAME='music_asset' AND to_jsonb(NEW)->'provenance'->>'pipeline'='audio-v2' THEN now()+interval '24 hours' ELSE now() END
 FROM public.music_media_source s WHERE
 (TG_TABLE_NAME='music_asset' AND s.source_kind='asset' AND s.source_id=NEW.id::text)
 OR (TG_TABLE_NAME='artist_release' AND s.source_kind='legacy' AND s.locator->>'releaseId'=NEW.id::text)
 ON CONFLICT DO NOTHING;
 RETURN NEW;
END $$;
CREATE OR REPLACE TRIGGER trg_music_media_ingested AFTER INSERT OR UPDATE ON music_asset
 FOR EACH ROW EXECUTE FUNCTION music_media_ingested();
CREATE OR REPLACE TRIGGER trg_legacy_media_ingested AFTER INSERT OR UPDATE ON artist_release
 FOR EACH ROW EXECUTE FUNCTION music_media_ingested();

-- Every public asset signature and catalog source already uses this policy.
DO $$ BEGIN
 IF to_regprocedure('public.music_public_asset_accessible_before_health(uuid,text)') IS NULL THEN
 ALTER FUNCTION public.music_public_asset_accessible(uuid,text) RENAME TO music_public_asset_accessible_before_health;
 END IF;
END $$;
CREATE OR REPLACE FUNCTION music_public_asset_accessible(target_asset_id uuid,territory_code text)
RETURNS boolean LANGUAGE sql STABLE SET search_path=pg_catalog,public AS $$
 SELECT public.music_public_asset_accessible_before_health(target_asset_id,territory_code)
 AND (NOT EXISTS(SELECT 1 FROM public.music_asset WHERE id=target_asset_id AND asset_role IN ('stream_audio','preview_audio'))
      OR public.music_media_eligible('asset',target_asset_id::text));
$$;
COMMIT;
