-- Independent oracle: original public eligibility clauses at protected main
-- 512d590e9; production now shares typed helpers. No provider delivery.
\set ON_ERROR_STOP on
BEGIN;
SET CONSTRAINTS ALL IMMEDIATE;
INSERT INTO party(id,display_name,is_org,created_at) VALUES (900051,'Synthetic alert parity owner',false,now());
CREATE TEMP TABLE tdf_alert_repro_results(name text PRIMARY KEY,query jsonb,document_kind text,document_id text,source_version bigint,public_match boolean,alert_count bigint,notification_count bigint,expected_public boolean,expected_current_alert boolean);
CREATE FUNCTION pg_temp.public_matches(doc_id text,kind text,q jsonb) RETURNS boolean LANGUAGE sql AS $public$
 WITH input AS (SELECT
 coalesce(q->>'q','')::text q,
 CASE WHEN q->>'entityType'='all' THEN NULL ELSE q->>'entityType' END::text entity_type,
 (q->>'cityId')::uuid city_id,(q->>'latitude')::float8 latitude,(q->>'longitude')::float8 longitude,(q->>'radiusKm')::float8 radius_km,
 (q->>'professionId')::uuid profession_id,(q->>'serviceId')::uuid service_id,(q->>'instrumentId')::uuid instrument_id,(q->>'genreId')::uuid genre_id,
 (q->>'remote')::boolean remote_only,(q->>'available')::boolean available_only,(q->>'dateFrom')::timestamptz date_from,(q->>'dateTo')::timestamptz date_to,(q->>'profileKind')::text profile_kind),
 documents AS (SELECT document.*,profile.profile_kind,('/directorio/'||document.slug)::text canonical_path FROM directory_public_search_document document LEFT JOIN directory_profile profile ON document.entity_kind='profile' AND profile.id::text=document.entity_id WHERE document.entity_kind<>'profile' OR (profile.profile_status='published' AND profile.visibility='public' AND profile.moderation_status='allowed' AND profile.canonical_profile_id IS NULL) UNION ALL SELECT 'profile','artist-'||artist.artist_party_id::text,artist.artist_party_id::text,party.display_name,NULL,artist.bio,artist.hero_image_url,city.id,artist.city,artist.country_code,NULL::float8,NULL::float8,'city','{}'::uuid[],'{}'::uuid[],'{}'::uuid[],ARRAY(SELECT genre_id FROM artist_profile_genre_membership WHERE artist_party_id=artist.artist_party_id),text.content,to_tsvector('simple',text.content),0::numeric,0::numeric,0::numeric,artist.created_at,NULL::timestamptz,coalesce(artist.updated_at,artist.created_at),1::bigint,FALSE,NULL,FALSE,FALSE,FALSE,'artist','/artista/'||artist.artist_party_id::text FROM artist_profile artist JOIN party ON party.id=artist.artist_party_id LEFT JOIN LATERAL (SELECT (array_agg(id))[1] id FROM city_reference WHERE active AND directory_normalize_text(name_es)=directory_normalize_text(artist.city) HAVING count(*)=1) city ON TRUE CROSS JOIN LATERAL (SELECT directory_normalize_text(concat_ws(' ',party.display_name,artist.slug,artist.bio,artist.city,artist.country_code,artist.genres,artist.highlights)) content) text WHERE ((SELECT q FROM input)='' OR artist.artist_party_id IN (SELECT id FROM party WHERE directory_normalize_text(display_name) LIKE '%'||directory_normalize_text((SELECT q FROM input))||'%' UNION SELECT artist_party_id FROM artist_profile WHERE directory_normalize_text(coalesce(slug,'')||' '||coalesce(bio,'')||' '||coalesce(city,'')||' '||coalesce(country_code,'')||' '||coalesce(genres,'')||' '||coalesce(highlights,'')) LIKE '%'||directory_normalize_text((SELECT q FROM input))||'%')) AND NOT EXISTS (SELECT 1 FROM directory_profile profile WHERE profile.subject_party_id=artist.artist_party_id) AND NOT EXISTS (SELECT 1 FROM directory_legacy_link link WHERE link.legacy_kind='artist_profile' AND link.legacy_id=artist.id::text)), 
 matched AS (SELECT document.entity_id FROM documents document CROSS JOIN input
 WHERE document.entity_id=doc_id AND document.entity_kind=kind AND NOT document.sponsored AND (document.entity_kind<>'event' OR NOT EXISTS (SELECT 1 FROM external_event_ref event_ref WHERE event_ref.event_id::text=document.entity_id AND lower(trim(event_ref.source_status))='suppressed')) AND (input.entity_type IS NULL OR document.entity_kind=input.entity_type OR (input.entity_type='artist' AND document.profile_kind IN ('artist','band','project'))) AND (input.entity_type IS NOT NULL OR (SELECT raw.source_profile_id FROM directory_search_document raw WHERE raw.entity_kind=document.entity_kind AND raw.entity_id=document.entity_id) IS NULL) AND (input.profile_kind IS NULL OR document.profile_kind=input.profile_kind) AND (input.city_id IS NULL OR document.city_id=input.city_id) AND (input.q='' OR document.search_vector @@ plainto_tsquery('simple',directory_normalize_text(input.q)) OR document.search_text LIKE '%'||directory_normalize_text(input.q)||'%' OR directory_text_similarity(document.search_text,input.q)>=.2 OR EXISTS (SELECT 1 FROM catalog_search_alias alias WHERE alias.entity_id=ANY(document.profession_ids||document.service_ids||document.instrument_ids||document.genre_ids) AND (alias.normalized_term LIKE directory_normalize_text(input.q)||'%' OR directory_normalize_text(input.q) LIKE alias.normalized_term||'%'))) AND (document.entity_kind IN ('event','venue') OR ((input.profession_id IS NULL OR input.profession_id=ANY(document.profession_ids)) AND (input.service_id IS NULL OR input.service_id=ANY(document.service_ids)) AND (input.instrument_id IS NULL OR input.instrument_id=ANY(document.instrument_ids)) AND (input.remote_only IS DISTINCT FROM TRUE OR document.remote) AND (input.available_only IS DISTINCT FROM TRUE OR document.availability_score>0))) AND (input.genre_id IS NULL OR input.genre_id=ANY(document.genre_ids)) AND (input.latitude IS NULL OR input.radius_km IS NULL OR directory_distance_km(input.latitude,input.longitude,document.public_latitude,document.public_longitude)<=input.radius_km) AND (input.date_from IS NULL OR document.expires_at IS NULL OR document.expires_at>=input.date_from) AND (input.date_to IS NULL OR document.effective_at IS NULL OR document.effective_at<=input.date_to) )
 SELECT EXISTS(SELECT 1 FROM matched)
$public$;
CREATE FUNCTION pg_temp.record_case(label text,q jsonb,patch jsonb,want_public boolean,want_current_alert boolean,enabled boolean DEFAULT true,frequency text DEFAULT 'daily') RETURNS void LANGUAGE plpgsql AS $case$
DECLARE
 pid uuid:=gen_random_uuid();sid uuid:=gen_random_uuid();kind text:=coalesce(patch->>'kind','profile');doc_id text:=pid::text;venue_value bigint;event_value bigint;version bigint;actual_public boolean;alerts bigint;notifications bigint;
BEGIN
 IF kind='profile' THEN
  INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,profile_status,visibility,moderation_status,completeness_score)
  VALUES(pid,900051,coalesce(patch->>'profileKind','person'),'Synthetic '||label,'alert-repro-'||label,'Synthetic profile for deterministic alert parity regression.','draft','public','allowed',.8);
  UPDATE directory_profile SET profile_status='published',published_at=now() WHERE id=pid;
  PERFORM directory_refresh_profile_search(pid);
 ELSIF kind IN ('venue','event') THEN
  INSERT INTO venue(name,city,created_at,updated_at) VALUES('Synthetic venue '||label,'Quito',now(),now()) RETURNING id INTO venue_value;
  INSERT INTO social_event(organizer_party_id,title,description,venue_id,event_type_id,workflow_state_id,timezone,start_time,end_time,metadata,created_at,updated_at)
  VALUES(NULL,'Synthetic public event '||label,'Synthetic lineup',venue_value,(SELECT id FROM event_type ORDER BY sort_order,id LIMIT 1),
   (SELECT state.id FROM workflow_state state JOIN workflow_state_capability capability ON capability.state_id=state.id AND capability.capability_code='public-listable' AND capability.enabled WHERE state.active LIMIT 1),
   'America/Guayaquil',now()+interval '1 day',now()+interval '2 days','{"isPublic":true}',now(),now()) RETURNING id INTO event_value;
  doc_id:=CASE WHEN kind='venue' THEN venue_value::text ELSE event_value::text END;
 END IF;
 INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash,alerts_enabled,alert_frequency)
 VALUES(sid,900051,label,q,md5(label)||md5('alert-parity-'||label),enabled,frequency);
 INSERT INTO directory_search_document(entity_kind,entity_id,slug,title,search_text,search_vector,profession_ids,service_ids,instrument_ids,genre_ids,remote,availability_score,source_status,visibility,moderation_status,effective_at,expires_at,source_updated_at,source_version,sponsored,sponsor_disclosure)
 VALUES(kind,doc_id,'alert-repro-'||label,'Synthetic '||label,coalesce(patch->>'searchText','synthetic indexed profile'),to_tsvector('simple',coalesce(patch->>'searchText','synthetic indexed profile')),
 ARRAY(SELECT jsonb_array_elements_text(coalesce(patch->'professionIds','[]'))::uuid),ARRAY(SELECT jsonb_array_elements_text(coalesce(patch->'serviceIds','[]'))::uuid),ARRAY(SELECT jsonb_array_elements_text(coalesce(patch->'instrumentIds','[]'))::uuid),ARRAY(SELECT jsonb_array_elements_text(coalesce(patch->'genreIds','[]'))::uuid),
 coalesce((patch->>'remote')::boolean,false),coalesce((patch->>'availability')::numeric,0),coalesce(patch->>'status','published'),coalesce(patch->>'visibility','public'),coalesce(patch->>'moderation','allowed'),coalesce((patch->>'effectiveAt')::timestamptz,now()-interval '1 hour'),(patch->>'expiresAt')::timestamptz,now(),1,coalesce((patch->>'sponsored')::boolean,false),CASE WHEN (patch->>'sponsored')::boolean THEN 'Synthetic sponsor' END)
 ON CONFLICT(entity_kind,entity_id) DO UPDATE SET
 search_text=EXCLUDED.search_text,search_vector=EXCLUDED.search_vector,profession_ids=EXCLUDED.profession_ids,service_ids=EXCLUDED.service_ids,instrument_ids=EXCLUDED.instrument_ids,genre_ids=EXCLUDED.genre_ids,remote=EXCLUDED.remote,availability_score=EXCLUDED.availability_score,source_status=EXCLUDED.source_status,visibility=EXCLUDED.visibility,moderation_status=EXCLUDED.moderation_status,effective_at=EXCLUDED.effective_at,expires_at=EXCLUDED.expires_at,sponsored=EXCLUDED.sponsored,sponsor_disclosure=EXCLUDED.sponsor_disclosure,source_updated_at=now(),source_version=directory_search_document.source_version+1
 RETURNING source_version INTO version;
 IF want_public IS NULL THEN actual_public:=NULL; ELSE SELECT pg_temp.public_matches(doc_id,kind,coalesce(patch->'publicQuery',q)) INTO actual_public; END IF;
 SELECT count(*),count(internal_notification_id) INTO alerts,notifications FROM directory_alert_delivery WHERE saved_search_id=sid AND result_kind=kind AND result_id=doc_id AND result_version=version;
 INSERT INTO tdf_alert_repro_results VALUES(label,q,kind,doc_id,version,actual_public,alerts,notifications,want_public,want_current_alert);
 IF actual_public IS DISTINCT FROM want_public OR (alerts>0) IS DISTINCT FROM want_current_alert OR alerts<>notifications THEN RAISE EXCEPTION 'unexpected characterization %: public %, alerts %, notifications %',label,actual_public,alerts,notifications; END IF;
END
$case$;
SELECT pg_temp.record_case('baseline','{}','{}',true,true);
SELECT pg_temp.record_case('service-ignored','{"serviceId":"10000000-0000-4000-8000-000000000001"}','{}',false,false);
SELECT pg_temp.record_case('remote-ignored','{"remote":true}','{}',false,false);
SELECT pg_temp.record_case('available-ignored','{"available":true}','{}',false,false);
SELECT pg_temp.record_case('profile-kind-ignored','{"profileKind":"artist"}','{}',false,false);
SELECT pg_temp.record_case('date-to-ignored',jsonb_build_object('dateTo',now()-interval '2 hours'),'{}',false,false);
SELECT pg_temp.record_case('date-from-ignored',jsonb_build_object('dateFrom',now()+interval '2 hours'),jsonb_build_object('expiresAt',now()+interval '1 hour'),false,false);
SELECT pg_temp.record_case('mobile-all-excluded','{"entityType":"all"}','{}',true,true);
SELECT pg_temp.record_case('artist-alias-excluded','{"entityType":"artist"}','{"profileKind":"artist"}',true,true);
SELECT pg_temp.record_case('substring-excluded','{"q":"embeddedsubstring"}',jsonb_build_object('searchText','alphaembeddedsubstringomega '||(SELECT string_agg(md5(i::text),' ') FROM generate_series(1,100)i)),true,true);
SELECT pg_temp.record_case('profession-control','{"professionId":"10000000-0000-4000-8000-000000000001"}','{}',false,false);
SELECT pg_temp.record_case('instrument-control','{"instrumentId":"10000000-0000-4000-8000-000000000001"}','{}',false,false);
SELECT pg_temp.record_case('genre-control','{"genreId":"10000000-0000-4000-8000-000000000001"}','{}',false,false);
SELECT pg_temp.record_case('private-control','{}','{"visibility":"private"}',false,false);
SELECT pg_temp.record_case('moderation-control','{}','{"moderation":"blocked"}',false,false);
SELECT pg_temp.record_case('draft-control','{}','{"status":"draft"}',false,false);
SELECT pg_temp.record_case('expired-control','{}',jsonb_build_object('expiresAt',now()-interval '1 minute'),false,false);
SELECT pg_temp.record_case('disabled-control','{}','{}',true,false,false);
SELECT pg_temp.record_case('frequency-off-control','{}','{}',true,false,true,'off');
SELECT pg_temp.record_case('venue-people-filter-control','{"serviceId":"10000000-0000-4000-8000-000000000001","remote":true,"available":true}','{"kind":"venue"}',true,true);
SELECT pg_temp.record_case('event-people-filter-control','{"serviceId":"10000000-0000-4000-8000-000000000001","remote":true,"available":true}','{"kind":"event"}',true,true);
DO $alias$
DECLARE aliasrow catalog_search_alias%ROWTYPE;
BEGIN
 SELECT * INTO aliasrow FROM catalog_search_alias ORDER BY id LIMIT 1;
 IF aliasrow.id IS NULL THEN RAISE EXCEPTION 'catalog alias fixture missing'; END IF;
 INSERT INTO catalog_search_alias(catalog_id,entity_kind,entity_id,locale_id,term,normalized_term,source)
 VALUES(aliasrow.catalog_id,aliasrow.entity_kind,aliasrow.entity_id,aliasrow.locale_id,'ultracanonicalalertalias','ultracanonicalalertalias','synthetic-alert-parity');
 PERFORM pg_temp.record_case('catalog-alias-excluded','{"q":"ultracanonicalalertalias"}',jsonb_build_object('professionIds',jsonb_build_array(aliasrow.entity_id),'searchText','Unrelated synthetic profile'),true,true);
END
$alias$;
DO $derived$
DECLARE pid uuid:=gen_random_uuid();cid uuid;sid uuid:=gen_random_uuid();q jsonb:='{}';v bigint;n bigint;pub boolean;
BEGIN
 INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,bio,profile_status,visibility,moderation_status,completeness_score)
 VALUES(pid,900051,'artist','Synthetic derived control','alert-repro-derived-control','Synthetic artist biography for derived listing parity control.','draft','public','allowed',.8);
 UPDATE directory_profile SET profile_status='published',published_at=now() WHERE id=pid;
 SELECT id INTO cid FROM classified WHERE source_profile_id=pid;
 IF cid IS NULL THEN RAISE EXCEPTION 'real derived listing fixture missing'; END IF;
 INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash) VALUES(sid,900051,'derived-mixed-control',q,repeat('e',64));
 UPDATE directory_search_document SET source_version=source_version+1 WHERE entity_kind='classified' AND entity_id=cid::text RETURNING source_version INTO v;
 pub:=pg_temp.public_matches(cid::text,'classified',q);
 SELECT count(*) INTO n FROM directory_alert_delivery WHERE saved_search_id=sid AND result_id=cid::text AND result_version=v;
 IF pub OR n<>0 THEN RAISE EXCEPTION 'derived listing leaked into mixed query'; END IF;
 INSERT INTO tdf_alert_repro_results VALUES('derived-mixed-control',q,'classified',cid::text,v,pub,n,n,false,false);
 sid:=gen_random_uuid();q:='{"entityType":"classified"}';
 INSERT INTO directory_saved_search(id,account_party_id,name,canonical_query,query_hash) VALUES(sid,900051,'derived-explicit-control',q,repeat('f',64));
 UPDATE directory_search_document SET source_version=source_version+1 WHERE entity_kind='classified' AND entity_id=cid::text RETURNING source_version INTO v;
 pub:=pg_temp.public_matches(cid::text,'classified',q);
 SELECT count(*) INTO n FROM directory_alert_delivery WHERE saved_search_id=sid AND result_id=cid::text AND result_version=v;
 IF NOT pub OR n<>1 THEN RAISE EXCEPTION 'derived listing not available to classified query'; END IF;
 INSERT INTO tdf_alert_repro_results VALUES('derived-explicit-control',q,'classified',cid::text,v,pub,n,n,true,true);
END
$derived$;
DO $dedup$
DECLARE r tdf_alert_repro_results%ROWTYPE;n bigint;sid uuid;
BEGIN
 SELECT * INTO r FROM tdf_alert_repro_results WHERE name='baseline';
 SELECT id INTO sid FROM directory_saved_search WHERE name='baseline';
 UPDATE directory_search_document SET source_status=source_status WHERE entity_kind=r.document_kind AND entity_id=r.document_id;
 SELECT count(*) INTO n FROM directory_alert_delivery WHERE saved_search_id=sid AND result_kind=r.document_kind AND result_id=r.document_id AND result_version=r.source_version;
 IF n<>1 THEN RAISE EXCEPTION 'same-version delivery replay not deduplicated'; END IF;
 UPDATE directory_search_document SET source_version=source_version+1 WHERE entity_kind=r.document_kind AND entity_id=r.document_id;
 SELECT count(*) INTO n FROM directory_alert_delivery WHERE saved_search_id=sid AND result_kind=r.document_kind AND result_id=r.document_id;
 IF n<>2 THEN RAISE EXCEPTION 'new version not delivered exactly once'; END IF;
END
$dedup$;
SELECT pg_temp.record_case('malformed-genre-string','{"genreId":"not-a-uuid"}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-genre-object','{"genreId":{"bad":true}}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-remote-string','{"remote":"not-a-boolean"}','{}',NULL,false);
SELECT pg_temp.record_case('public-query-trim',jsonb_build_object('q',E'\t synthetic '||U&'\00A0'),'{}',true,true);
SELECT pg_temp.record_case('city-control','{"cityId":"10000000-0000-4000-8000-000000000001"}','{}',false,false);
SELECT pg_temp.record_case('remote-positive','{"remote":true}','{"remote":true}',true,true);
SELECT pg_temp.record_case('available-positive','{"available":true}','{"availability":1}',true,true);
SELECT pg_temp.record_case('service-positive','{"serviceId":"10000000-0000-4000-8000-000000000001"}','{"serviceIds":["10000000-0000-4000-8000-000000000001"]}',true,true);
SELECT pg_temp.record_case('text-negative','{"q":"qqzzxxunmatchable"}','{}',false,false);
SELECT pg_temp.record_case('historical-radius','{"radiusKm":10}','{}',NULL,false);
SELECT pg_temp.record_case('historical-coordinates','{"latitude":-0.18,"longitude":-78.46}','{}',NULL,false);
SELECT pg_temp.record_case('historical-nested-location','{"location":{"latitude":-0.18,"longitude":-78.46}}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-date-relative','{"dateFrom":"tomorrow"}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-date-infinity','{"dateFrom":"infinity"}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-date-calendar','{"dateTo":"2026-02-30T00:00:00Z"}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-date-ambiguous','{"dateTo":"10/25/2026"}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-boolean-number','{"available":1}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-query-array','{"q":[]}','{}',NULL,false);
SELECT pg_temp.record_case('malformed-query-length',jsonb_build_object('q',repeat('a',161)),'{}',NULL,false);
SELECT pg_temp.record_case('date-positive',jsonb_build_object('dateFrom',now()-interval '2 hours','dateTo',now()+interval '2 hours'),jsonb_build_object('expiresAt',now()+interval '1 hour'),true,true);


SELECT pg_temp.record_case('high-offset-exact-bound',
 '{"dateTo":"2026-10-25T12:00:00+23:59"}',
 '{"effectiveAt":"2026-10-24T12:01:00Z","publicQuery":{"dateTo":"2026-10-24T12:01:00Z"}}',true,true);
SELECT pg_temp.record_case('high-offset-outside-bound',
 '{"dateTo":"2026-10-25T12:00:00+23:59"}',
 '{"effectiveAt":"2026-10-24T12:01:00.000001Z","publicQuery":{"dateTo":"2026-10-24T12:01:00Z"}}',false,false);
DO $uuid$
DECLARE field_name TEXT; raw_value TEXT;
BEGIN
 FOREACH field_name IN ARRAY ARRAY['cityId','professionId','serviceId','instrumentId','genreId'] LOOP
  FOREACH raw_value IN ARRAY ARRAY['','   ','10000000000040008000000000000001','{10000000-0000-4000-8000-000000000001}'] LOOP
   PERFORM pg_temp.record_case('invalid-'||lower(field_name)||'-'||substr(md5(raw_value),1,12),jsonb_build_object(field_name,raw_value),'{}',NULL,false);
  END LOOP;
 END LOOP;
 PERFORM pg_temp.record_case('uuid-null-remains-optional','{"genreId":null}','{}',true,true);
 PERFORM pg_temp.record_case('uuid-uppercase-valid',
  '{"genreId":"AAAAAAAA-0000-F000-C000-000000000001"}',
  '{"genreIds":["aaaaaaaa-0000-f000-c000-000000000001"]}',true,true);
END
$uuid$;
DO $dates$
DECLARE raw_value TEXT; normalized TIMESTAMPTZ; accepted BOOLEAN;
BEGIN
 FOREACH raw_value IN ARRAY ARRAY[
  '2026-10-25T12:00:00Z','2026-10-25 12:00:00Z','2026-10-25T12:00Z'
 ] LOOP
  SELECT valid,date_from INTO accepted,normalized FROM directory_parse_saved_search(jsonb_build_object('dateFrom',raw_value));
  IF accepted IS DISTINCT FROM TRUE OR normalized IS DISTINCT FROM '2026-10-25T12:00:00Z'::timestamptz THEN RAISE EXCEPTION 'public UTCTime valid form rejected: %',raw_value; END IF;
 END LOOP;
 FOREACH raw_value IN ARRAY ARRAY['2026-10-25T12:00:00+05:30','2026-10-25T12:00:00+0530'] LOOP
  SELECT valid,date_from INTO accepted,normalized FROM directory_parse_saved_search(jsonb_build_object('dateFrom',raw_value));
  IF accepted IS DISTINCT FROM TRUE OR normalized IS DISTINCT FROM '2026-10-25T06:30:00Z'::timestamptz THEN RAISE EXCEPTION 'public UTCTime offset rejected: %',raw_value; END IF;
 END LOOP;
 SELECT valid,date_from INTO accepted,normalized FROM directory_parse_saved_search('{"dateFrom":"2026-10-25T12:00:00+23:59"}');
 IF accepted IS DISTINCT FROM TRUE OR normalized IS DISTINCT FROM '2026-10-24T12:01:00Z'::timestamptz THEN RAISE EXCEPTION 'public UTCTime maximum offset rejected'; END IF;
 SELECT valid,date_from INTO accepted,normalized FROM directory_parse_saved_search('{"dateFrom":"2026-10-25T12:00:60.5Z"}');
 IF accepted IS DISTINCT FROM TRUE OR normalized IS DISTINCT FROM '2026-10-25T12:01:00.5Z'::timestamptz THEN RAISE EXCEPTION 'public UTCTime leap second normalization rejected'; END IF;
 FOREACH raw_value IN ARRAY ARRAY['2026-10-25t12:00:00z','2026-10-25T12:00:00z','2026-10-25T24:00:00Z','2026-10-25T12:00:61Z','2026-10-25T12:00:00+25:00','tomorrow','infinity','2026-10-25','2026-02-30T12:00:00Z'] LOOP
  SELECT valid INTO accepted FROM directory_parse_saved_search(jsonb_build_object('dateFrom',raw_value));
  IF accepted IS DISTINCT FROM FALSE THEN RAISE EXCEPTION 'nonpublic timestamp accepted: %',raw_value; END IF;
 END LOOP;
END
$dates$;

SELECT jsonb_build_object('cases',(SELECT count(*) FROM tdf_alert_repro_results),'mismatches',(SELECT count(*) FROM tdf_alert_repro_results WHERE public_match IS NOT NULL AND public_match IS DISTINCT FROM (alert_count>0) AND name NOT IN('disabled-control','frequency-off-control')),'providerDeliveries',(SELECT count(*) FROM directory_alert_delivery WHERE email_status<>'disabled' OR push_status<>'disabled'));
ROLLBACK;
