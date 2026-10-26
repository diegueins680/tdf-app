BEGIN;

-- The reference-data lookup is evaluated once per public query (or saved
-- query), not once per candidate document. Ranking remains owned by the API.
CREATE OR REPLACE FUNCTION directory_query_alias_entity_ids(query_text TEXT)
RETURNS UUID[] LANGUAGE sql STABLE PARALLEL SAFE AS $$
  SELECT CASE WHEN coalesce(query_text, '') = '' THEN '{}'::uuid[] ELSE
    coalesce((SELECT array_agg(DISTINCT alias.entity_id)
      FROM catalog_search_alias alias
      WHERE alias.normalized_term LIKE directory_normalize_text(query_text)||'%'
         OR directory_normalize_text(query_text) LIKE alias.normalized_term||'%'), '{}'::uuid[])
    END;
$$;

-- Scalar SQL without subqueries permits planner inlining. Both public search
-- and alerts call this exact predicate; no per-document JSON conversion.
CREATE OR REPLACE FUNCTION directory_matches_non_geo_filters(
  document_kind TEXT, document_profile_kind TEXT, document_city_id UUID,
  document_profession_ids UUID[], document_service_ids UUID[],
  document_instrument_ids UUID[], document_genre_ids UUID[],
  document_search_text TEXT, document_search_vector TSVECTOR,
  document_remote BOOLEAN, document_availability_score NUMERIC,
  document_effective_at TIMESTAMPTZ, document_expires_at TIMESTAMPTZ,
  query_text TEXT, query_alias_ids UUID[], query_profile_kind TEXT,
  query_city_id UUID, query_profession_id UUID, query_service_id UUID,
  query_instrument_id UUID, query_genre_id UUID, query_remote BOOLEAN,
  query_available BOOLEAN, query_date_from TIMESTAMPTZ, query_date_to TIMESTAMPTZ
) RETURNS BOOLEAN LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT
    (query_profile_kind IS NULL OR document_profile_kind=query_profile_kind)
    AND (query_city_id IS NULL OR document_city_id=query_city_id)
    AND (query_text=''
      OR document_search_vector @@ plainto_tsquery('simple',directory_normalize_text(query_text))
      OR document_search_text LIKE '%'||directory_normalize_text(query_text)||'%'
      OR directory_text_similarity(document_search_text,query_text)>=.2
      OR query_alias_ids && (document_profession_ids||document_service_ids||document_instrument_ids||document_genre_ids))
    AND (document_kind IN ('event','venue') OR (
      (query_profession_id IS NULL OR query_profession_id=ANY(document_profession_ids))
      AND (query_service_id IS NULL OR query_service_id=ANY(document_service_ids))
      AND (query_instrument_id IS NULL OR query_instrument_id=ANY(document_instrument_ids))
      AND (query_remote IS DISTINCT FROM TRUE OR document_remote)
      AND (query_available IS DISTINCT FROM TRUE OR document_availability_score>0)))
    AND (query_genre_id IS NULL OR query_genre_id=ANY(document_genre_ids))
    AND (query_date_from IS NULL OR document_expires_at IS NULL OR document_expires_at>=query_date_from)
    AND (query_date_to IS NULL OR document_effective_at IS NULL OR document_effective_at<=query_date_to);
$$;

CREATE OR REPLACE FUNCTION directory_matches_entity_and_derivation(
  document_kind TEXT, document_profile_kind TEXT, document_source_profile_id UUID,
  query_entity_type TEXT
) RETURNS BOOLEAN LANGUAGE sql IMMUTABLE PARALLEL SAFE AS $$
  SELECT
    (query_entity_type IS NULL OR document_kind=query_entity_type
      OR (query_entity_type='artist' AND document_profile_kind IN ('artist','band','project')))
    AND (query_entity_type IS NOT NULL OR document_source_profile_id IS NULL);
$$;

-- Only this boundary decodes persisted JSON. Invalid/unsupported historical
-- values cannot turn into broad matches or abort an unrelated index write.
CREATE OR REPLACE FUNCTION directory_parse_saved_search(canonical JSONB)
RETURNS TABLE (
  valid BOOLEAN, reason TEXT, query_text TEXT, entity_type TEXT, profile_kind TEXT,
  city_id UUID, profession_id UUID, service_id UUID, instrument_id UUID, genre_id UUID,
  remote_only BOOLEAN, available_only BOOLEAN, date_from TIMESTAMPTZ,
  date_to TIMESTAMPTZ, alias_entity_ids UUID[]
) LANGUAGE plpgsql STABLE AS $$
DECLARE
  field_name TEXT;
  strip_characters CONSTANT TEXT := U&'\0009\000A\000B\000C\000D\0020\00A0\1680\2000\2001\2002\2003\2004\2005\2006\2007\2008\2009\200A\202F\205F\3000';
  raw_value TEXT;
  radius_value NUMERIC;
  date_parts TEXT[];
  calendar_year INTEGER;
  clock_hour INTEGER;
  clock_minute INTEGER;
  clock_second NUMERIC;
  zone_digits TEXT;
  zone_hour INTEGER;
  zone_minute INTEGER;
  zone_offset INTEGER;
  local_instant TIMESTAMP;
  parsed_instant TIMESTAMPTZ;
BEGIN
  valid:=FALSE; reason:='invalid_query';
  IF jsonb_typeof(canonical) IS DISTINCT FROM 'object' THEN RETURN NEXT; RETURN; END IF;
  IF EXISTS (SELECT 1 FROM jsonb_object_keys(canonical) AS field(name)
    WHERE name NOT IN ('q','entityType','profileKind','cityId','professionId',
      'serviceId','instrumentId','genreId','remote','available','dateFrom','dateTo',
      'latitude','longitude','radiusKm')) THEN RETURN NEXT; RETURN; END IF;
  IF (canonical ? 'latitude' AND canonical->'latitude'<>'null'::jsonb)
     OR (canonical ? 'longitude' AND canonical->'longitude'<>'null'::jsonb) THEN
    reason:='coordinates_not_allowed'; RETURN NEXT; RETURN;
  END IF;
  FOREACH field_name IN ARRAY ARRAY['q','entityType','profileKind','cityId',
    'professionId','serviceId','instrumentId','genreId','dateFrom','dateTo'] LOOP
    IF canonical ? field_name AND jsonb_typeof(canonical->field_name) NOT IN ('string','null') THEN
      RETURN NEXT; RETURN;
    END IF;
  END LOOP;
  FOREACH field_name IN ARRAY ARRAY['remote','available'] LOOP
    IF canonical ? field_name AND jsonb_typeof(canonical->field_name) NOT IN ('boolean','null') THEN
      RETURN NEXT; RETURN;
    END IF;
  END LOOP;
  query_text:=btrim(coalesce(canonical->>'q',''),strip_characters);
  IF length(query_text)>160 OR query_text ~ U&'[\0001-\001F\007F-\009F]' THEN RETURN NEXT; RETURN; END IF;
  entity_type:=nullif(btrim(canonical->>'entityType',strip_characters),'');
  IF entity_type='all' THEN entity_type:=NULL; END IF;
  IF entity_type IS NOT NULL AND entity_type NOT IN ('profile','artist','classified','event','venue') THEN
    RETURN NEXT; RETURN;
  END IF;
  profile_kind:=nullif(btrim(canonical->>'profileKind',strip_characters),'');
  IF profile_kind IS NOT NULL AND profile_kind NOT IN ('person','artist','band','project') THEN
    RETURN NEXT; RETURN;
  END IF;
  -- Public UUID query parameters require the canonical hyphenated form.
  -- PostgreSQL accepts braces/compact forms; public search does not. Never
  -- normalize empty/whitespace values into an absent filter and broaden it.
  FOREACH field_name IN ARRAY ARRAY['cityId','professionId','serviceId','instrumentId','genreId'] LOOP
    raw_value:=canonical->>field_name;
    IF raw_value IS NOT NULL AND raw_value !~ '^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$' THEN
      RETURN NEXT; RETURN;
    END IF;
  END LOOP;
  -- Each cast is inside this guarded decoder. Do not copy unguarded casts to
  -- the trigger's WHERE clause; old canonical_query values were not typed.
  city_id:=(canonical->>'cityId')::uuid;
  profession_id:=(canonical->>'professionId')::uuid;
  service_id:=(canonical->>'serviceId')::uuid;
  instrument_id:=(canonical->>'instrumentId')::uuid;
  genre_id:=(canonical->>'genreId')::uuid;
  remote_only:=(canonical->>'remote')::boolean;
  available_only:=(canonical->>'available')::boolean;
  FOREACH field_name IN ARRAY ARRAY['dateFrom','dateTo'] LOOP
    raw_value:=canonical->>field_name;
    IF raw_value IS NOT NULL THEN
      -- Decode the public UTCTime ISO forms explicitly. PostgreSQL otherwise
      -- accepts relative words/24:00 and rejects public offsets above15hours.
      date_parts:=regexp_match(raw_value,
        '^(([+-]?[0-9]{4,})-([0-9]{2})-([0-9]{2}))[T ](([0-9]{2}):([0-9]{2})(:([0-9]{2})(\.[0-9]+)?)?)(Z|[+-][0-9]{2}(:?[0-9]{2})?)$');
      IF date_parts IS NULL THEN RETURN NEXT; RETURN; END IF;
      calendar_year:=date_parts[2]::integer;
      -- Public ISO years are astronomical; PostgreSQL make_date uses1BC=-1.
      IF calendar_year<=0 THEN calendar_year:=calendar_year-1; END IF;
      clock_hour:=date_parts[6]::integer; clock_minute:=date_parts[7]::integer;
      clock_second:=(coalesce(date_parts[9],'0')||coalesce(date_parts[10],''))::numeric;
      IF clock_hour>23 OR clock_minute>59 OR clock_second>=61 THEN RETURN NEXT; RETURN; END IF;
      zone_offset:=0;
      IF date_parts[11]<>'Z' THEN
        zone_digits:=replace(substr(date_parts[11],2),':','');
        zone_hour:=substr(zone_digits,1,2)::integer;
        zone_minute:=CASE WHEN length(zone_digits)=4 THEN substr(zone_digits,3,2)::integer ELSE 0 END;
        IF zone_hour>23 OR zone_minute>59 THEN RETURN NEXT; RETURN; END IF;
        zone_offset:=(zone_hour*60+zone_minute)*CASE WHEN left(date_parts[11],1)='-' THEN -1 ELSE 1 END;
      END IF;
      local_instant:=make_date(calendar_year,date_parts[3]::integer,date_parts[4]::integer)::timestamp
        +make_interval(hours=>clock_hour,mins=>clock_minute,secs=>clock_second::double precision);
      parsed_instant:=(local_instant-make_interval(mins=>zone_offset)) AT TIME ZONE 'UTC';
      IF field_name='dateFrom' THEN date_from:=parsed_instant;
      ELSE date_to:=parsed_instant; END IF;
    END IF;
  END LOOP;
  IF canonical ? 'radiusKm' AND canonical->'radiusKm'<>'null'::jsonb THEN
    IF jsonb_typeof(canonical->'radiusKm')<>'number' THEN RETURN NEXT; RETURN; END IF;
    radius_value:=(canonical->>'radiusKm')::numeric;
    IF radius_value<=0 OR radius_value>500 THEN RETURN NEXT; RETURN; END IF;
    reason:='unsupported_radius'; RETURN NEXT; RETURN;
  END IF;
  alias_entity_ids:=directory_query_alias_entity_ids(query_text);
  valid:=TRUE; reason:=NULL; RETURN NEXT;
EXCEPTION WHEN invalid_text_representation OR datetime_field_overflow
  OR invalid_datetime_format OR numeric_value_out_of_range THEN
  valid:=FALSE; reason:='invalid_query'; RETURN NEXT;
END;
$$;

CREATE OR REPLACE FUNCTION directory_enqueue_saved_search_alerts()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  current_profile_kind TEXT;
BEGIN
  IF NEW.sponsored OR NEW.source_status<>'published' OR NEW.visibility<>'public'
     OR NEW.moderation_status<>'allowed' OR (NEW.expires_at IS NOT NULL AND NEW.expires_at<=now()) THEN
    RETURN NEW;
  END IF;
  -- Reuse the same public projection, including authoritative event/venue
  -- visibility. Profile eligibility matches the public documents CTE.
  IF NOT EXISTS (
    SELECT 1 FROM directory_public_search_document document
    LEFT JOIN directory_profile profile ON document.entity_kind='profile' AND profile.id::text=document.entity_id
    WHERE document.entity_kind=NEW.entity_kind AND document.entity_id=NEW.entity_id
      AND (document.entity_kind<>'profile' OR (profile.profile_status='published'
        AND profile.visibility='public' AND profile.moderation_status='allowed'
        AND profile.canonical_profile_id IS NULL))
      AND (document.entity_kind<>'event' OR NOT EXISTS (
        SELECT 1 FROM external_event_ref event_ref
        WHERE event_ref.event_id::text=document.entity_id AND lower(trim(event_ref.source_status))='suppressed'))
  ) THEN RETURN NEW; END IF;
  IF NEW.entity_kind='profile' THEN
    SELECT profile.profile_kind INTO current_profile_kind
    FROM directory_profile profile WHERE profile.id::text=NEW.entity_id;
  END IF;
  WITH matches AS (
    SELECT saved.id,saved.account_party_id
    FROM directory_saved_search saved
    CROSS JOIN LATERAL directory_parse_saved_search(saved.canonical_query) filters
    WHERE saved.alerts_enabled AND saved.alert_frequency<>'off' AND filters.valid
      AND directory_matches_entity_and_derivation(NEW.entity_kind,current_profile_kind,NEW.source_profile_id,filters.entity_type)
      AND directory_matches_non_geo_filters(NEW.entity_kind,current_profile_kind,NEW.city_id,
        NEW.profession_ids,NEW.service_ids,NEW.instrument_ids,NEW.genre_ids,
        NEW.search_text,NEW.search_vector,NEW.remote,NEW.availability_score,NEW.effective_at,NEW.expires_at,
        filters.query_text,filters.alias_entity_ids,filters.profile_kind,filters.city_id,
        filters.profession_id,filters.service_id,filters.instrument_id,filters.genre_id,
        filters.remote_only,filters.available_only,filters.date_from,filters.date_to)
  ), inserted AS (
    INSERT INTO directory_alert_delivery(saved_search_id,result_kind,result_id,result_version,email_status,push_status)
    SELECT matches.id,NEW.entity_kind,NEW.entity_id,NEW.source_version,'disabled','disabled'
    FROM matches ON CONFLICT(saved_search_id,result_kind,result_id,result_version) DO NOTHING
    RETURNING id,saved_search_id
  )
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at)
  SELECT saved.account_party_id,'directory.saved-search-match','Nueva coincidencia en tu alerta',
    'Hay un nuevo resultado para "'||saved.name||'".','directory_alert',inserted.id::text,FALSE,now()
  FROM inserted JOIN directory_saved_search saved ON saved.id=inserted.saved_search_id;
  UPDATE directory_alert_delivery delivery SET internal_notification_id=notification.id
  FROM notification WHERE delivery.id::text=notification.target_key
    AND notification.target_type='directory_alert' AND delivery.internal_notification_id IS NULL;
  UPDATE directory_saved_search saved SET last_evaluated_at=now()
  WHERE EXISTS (SELECT 1 FROM directory_alert_delivery delivery WHERE delivery.saved_search_id=saved.id
    AND delivery.result_kind=NEW.entity_kind AND delivery.result_id=NEW.entity_id AND delivery.result_version=NEW.source_version);
  RETURN NEW;
END;
$$;
COMMIT;
