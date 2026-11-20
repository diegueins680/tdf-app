-- Preserve supported browser URL forms without widening the HTTPS-only server fetch boundary.
-- Relative URLs are returned to the authorized browser; the worker never fetches them.
BEGIN;
SET LOCAL lock_timeout='10s';
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
   jsonb_build_object('url',CASE WHEN u.url ~ '^https?://' OR u.url LIKE '/%' THEN u.url ELSE 'https://'||u.url END,'releaseId',r.id,'slot',u.slot)
 FROM artist_release r CROSS JOIN LATERAL (VALUES ('spotify',r.spotify_url),('youtube',r.youtube_url)) u(slot,url)
 WHERE btrim(u.url)<>'' AND (u.url !~ '^[a-zA-Z][a-zA-Z0-9+.-]*:' OR u.url ~ '^https?://')
 AND u.url !~ '[[:cntrl:]\\]' AND u.url !~ '^(https?:)?//[^/]*@'
 AND u.url !~* '^(https?://|//)?([^/]*\.)?(spotify\.com|youtube\.com|youtu\.be|youtube-nocookie\.com|spotify\.link|spoti\.fi)([:/]|$)';
COMMIT;
