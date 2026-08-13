-- Shared, read-only discovery for the Records CMS cutover. Callers own the
-- surrounding transaction. All identity decisions use provider IDs parsed
-- from persisted URLs/codes; labels are never used to guess identity.

CREATE TEMP TABLE records_cms_latest ON COMMIT DROP AS
SELECT DISTINCT ON (slug, locale)
  id AS cms_content_id,
  slug,
  locale,
  version AS cms_version,
  title,
  payload::jsonb AS payload,
  published_at
FROM cms_content
WHERE status='published'
  AND (
    slug IN ('records-releases','records-recordings','records-sessions')
    OR slug LIKE 'records-release-%'
    OR slug LIKE 'records-recording-%'
    OR slug LIKE 'records-session-%'
  )
ORDER BY slug, locale, version DESC, published_at DESC NULLS LAST, id DESC;

CREATE TEMP TABLE records_cms_candidates ON COMMIT DROP AS
WITH collection_items AS (
  SELECT latest.*, item.item, item.ordinality::integer AS source_order
  FROM records_cms_latest latest
  CROSS JOIN LATERAL jsonb_array_elements(
    CASE
      WHEN latest.slug='records-releases' AND jsonb_typeof(latest.payload->'tracks')='array'
        THEN latest.payload->'tracks'
      WHEN latest.slug IN ('records-recordings','records-sessions')
          AND jsonb_typeof(latest.payload->'videos')='array'
        THEN latest.payload->'videos'
      ELSE '[]'::jsonb
    END
  ) WITH ORDINALITY AS item(item, ordinality)
  WHERE latest.slug IN ('records-releases','records-recordings','records-sessions')
), individual_items AS (
  SELECT latest.*, latest.payload AS item, 1 AS source_order
  FROM records_cms_latest latest
  WHERE latest.slug LIKE 'records-release-%'
     OR latest.slug LIKE 'records-recording-%'
     OR latest.slug LIKE 'records-session-%'
), source_items AS (
  SELECT * FROM collection_items
  UNION ALL
  SELECT * FROM individual_items
), extracted AS (
  SELECT source.*,
    CASE
      WHEN slug='records-releases' OR slug LIKE 'records-release-%' THEN 'release'
      WHEN slug='records-recordings' OR slug LIKE 'records-recording-%' THEN 'recording'
      ELSE 'session'
    END AS entity_kind,
    NULLIF(btrim(item->>'title'),'') AS item_title,
    NULLIF(btrim(CASE
      WHEN slug='records-sessions' OR slug LIKE 'records-session-%' THEN item->>'guests'
      ELSE item->>'artist'
    END),'') AS contributor_credit,
    COALESCE(
      NULLIF(btrim(item->>'spotifyUrl'),''),
      NULLIF(btrim(item->>'url'),''),
      (
        SELECT NULLIF(btrim(link->>'url'),'')
        FROM jsonb_array_elements(
          CASE WHEN jsonb_typeof(item->'links')='array' THEN item->'links' ELSE '[]'::jsonb END
        ) link
        WHERE lower(COALESCE(link->>'platform',''))='spotify'
        LIMIT 1
      )
    ) AS release_url,
    COALESCE(NULLIF(btrim(item->>'url'),''), NULLIF(btrim(item->>'youtubeUrl'),'')) AS video_url,
    NULLIF(btrim(item->>'durationMs'),'') AS duration_ms_raw,
    NULLIF(btrim(item->>'duration'),'') AS duration_text_raw,
    COALESCE(NULLIF(btrim(item->>'description'),''), NULLIF(btrim(item->>'blurb'),'')) AS item_description,
    COALESCE(NULLIF(btrim(item->>'cover'),''), NULLIF(btrim(item->>'image'),'')) AS image_url,
    CASE WHEN COALESCE(item->>'sortOrder','') ~ '^[-]?[0-9]+$'
      THEN (item->>'sortOrder')::integer ELSE source_order END AS sort_order
  FROM source_items source
), identified AS (
  SELECT extracted.*,
    CASE
      WHEN entity_kind='release' THEN COALESCE(
        NULLIF(btrim(item->>'trackId'),''),
        substring(release_url from '/track/([A-Za-z0-9]{22})')
      )
      ELSE COALESCE(
        NULLIF(btrim(item->>'youtubeId'),''),
        substring(video_url from '[?&]v=([A-Za-z0-9_-]{6,32})'),
        substring(video_url from 'youtu[.]be/([A-Za-z0-9_-]{6,32})')
      )
    END AS external_id,
    CASE WHEN entity_kind='release' THEN release_url ELSE video_url END AS canonical_url,
    CASE
      WHEN duration_ms_raw ~ '^[0-9]+$' THEN duration_ms_raw::integer
      WHEN duration_text_raw ~ '^[0-9]+:[0-5][0-9]$' THEN
        (split_part(duration_text_raw,':',1)::integer * 60
          + split_part(duration_text_raw,':',2)::integer) * 1000
      WHEN duration_text_raw ~ '^[0-9]+:[0-5][0-9]:[0-5][0-9]$' THEN
        (split_part(duration_text_raw,':',1)::integer * 3600
          + split_part(duration_text_raw,':',2)::integer * 60
          + split_part(duration_text_raw,':',3)::integer) * 1000
      ELSE NULL
    END AS duration_ms
  FROM extracted
), identity_summary AS (
  SELECT entity_kind,external_id,count(DISTINCT canonical_url) AS canonical_url_count
  FROM identified
  WHERE external_id IS NOT NULL
  GROUP BY entity_kind,external_id
), counted AS (
  SELECT identified.*,
    count(*) OVER (PARTITION BY identified.entity_kind, identified.locale, identified.external_id) AS identity_count,
    COALESCE(summary.canonical_url_count,0) AS canonical_url_count
  FROM identified
  LEFT JOIN identity_summary summary
    ON summary.entity_kind=identified.entity_kind AND summary.external_id=identified.external_id
)
SELECT counted.*,
  CASE
    WHEN jsonb_typeof(item)<>'object' THEN 'rejected'
    WHEN item_title IS NULL THEN 'unresolved'
    WHEN contributor_credit IS NULL THEN 'unresolved'
    WHEN external_id IS NULL THEN 'unresolved'
    WHEN entity_kind='release' AND external_id !~ '^[A-Za-z0-9]{22}$' THEN 'unresolved'
    WHEN entity_kind<>'release' AND external_id !~ '^[A-Za-z0-9_-]{6,32}$' THEN 'unresolved'
    WHEN canonical_url IS NULL OR canonical_url !~ '^https://' THEN 'unresolved'
    WHEN duration_ms_raw IS NOT NULL AND duration_ms_raw !~ '^[0-9]+$' THEN 'rejected'
    WHEN duration_text_raw IS NOT NULL
      AND duration_text_raw !~ '^[0-9]+:[0-5][0-9]$'
      AND duration_text_raw !~ '^[0-9]+:[0-5][0-9]:[0-5][0-9]$' THEN 'rejected'
    WHEN identity_count>1 THEN 'ambiguous'
    WHEN canonical_url_count>1 THEN 'ambiguous'
    ELSE 'mapped'
  END AS mapping_status,
  CASE
    WHEN jsonb_typeof(item)<>'object' THEN 'item is not a JSON object'
    WHEN item_title IS NULL THEN 'missing title'
    WHEN contributor_credit IS NULL THEN 'missing exact contributor credit'
    WHEN external_id IS NULL THEN 'provider id is absent and cannot be parsed deterministically from URL'
    WHEN canonical_url IS NULL OR canonical_url !~ '^https://' THEN 'canonical provider URL is missing or not HTTPS'
    WHEN duration_ms_raw IS NOT NULL AND duration_ms_raw !~ '^[0-9]+$' THEN 'durationMs is not a non-negative integer'
    WHEN duration_text_raw IS NOT NULL
      AND duration_text_raw !~ '^[0-9]+:[0-5][0-9]$'
      AND duration_text_raw !~ '^[0-9]+:[0-5][0-9]:[0-5][0-9]$' THEN 'duration is not mm:ss or h:mm:ss'
    WHEN identity_count>1 THEN 'provider id occurs more than once for this entity kind and locale'
    WHEN canonical_url_count>1 THEN 'provider id resolves to conflicting canonical URLs across locales'
    ELSE 'unique provider id plus validated HTTPS provider URL; exact credit preserved without splitting'
  END AS evidence
FROM counted;

CREATE TEMP TABLE records_cms_collection_resources ON COMMIT DROP AS
SELECT latest.cms_content_id, latest.slug, latest.locale, latest.cms_version,
  CASE latest.slug
    WHEN 'records-releases' THEN 'tdf-records-releases'
    WHEN 'records-recordings' THEN 'tdf-records-recordings'
    WHEN 'records-sessions' THEN 'tdf-records-sessions'
  END AS collection_code,
  CASE WHEN latest.slug='records-releases' THEN 'spotify' ELSE 'youtube' END AS provider_code,
  CASE WHEN latest.slug='records-recordings' THEN 'channel' ELSE 'playlist' END AS resource_kind,
  CASE latest.slug
    WHEN 'records-releases' THEN substring(latest.payload->>'playlistUrl' from '/playlist/([A-Za-z0-9]+)')
    WHEN 'records-recordings' THEN substring(latest.payload->>'channelUrl' from 'youtube[.]com/([^/?]+)')
    WHEN 'records-sessions' THEN substring(latest.payload->>'playlistUrl' from '[?&]list=([A-Za-z0-9_-]+)')
  END AS external_id,
  CASE latest.slug
    WHEN 'records-releases' THEN latest.payload->>'playlistUrl'
    WHEN 'records-recordings' THEN latest.payload->>'channelUrl'
    WHEN 'records-sessions' THEN latest.payload->>'playlistUrl'
  END AS canonical_url,
  COALESCE(latest.payload->>'playlistCover', latest.payload->>'cover') AS thumbnail_url
FROM records_cms_latest latest
WHERE latest.slug IN ('records-releases','records-recordings','records-sessions');
