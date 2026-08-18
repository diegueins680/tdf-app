-- Restore the original profile-image host projection without deleting data.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION directory_profile_primary_image_url(portfolio_value JSONB)
RETURNS TEXT
LANGUAGE SQL
IMMUTABLE
PARALLEL SAFE
AS $$
  SELECT candidate.image_url
  FROM jsonb_array_elements(
    CASE WHEN jsonb_typeof(portfolio_value) = 'array' THEN portfolio_value ELSE '[]'::jsonb END
  ) WITH ORDINALITY entry(value, ordinality)
  CROSS JOIN LATERAL (VALUES
    (nullif(btrim(entry.value->>'thumbnailUrl'), ''), 0),
    (nullif(btrim(entry.value->>'url'), ''), 1)
  ) candidate(image_url, priority)
  WHERE jsonb_typeof(entry.value) = 'object'
    AND coalesce(entry.value->>'itemType', entry.value->>'kind') = 'image'
    AND candidate.image_url IS NOT NULL
    AND strpos(candidate.image_url, chr(92)) = 0
    AND (
      candidate.image_url ~* '^https{0,1}://[a-z0-9.-]+(:[0-9]+)?(/|$)'
      OR candidate.image_url ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'
    )
  ORDER BY entry.ordinality, candidate.priority
  LIMIT 1;
$$;

UPDATE directory_search_document document
SET image_url = directory_profile_primary_image_url(profile.portfolio)
FROM directory_profile profile
WHERE document.entity_kind = 'profile'
  AND document.entity_id = profile.id::text
  AND document.image_url IS DISTINCT FROM directory_profile_primary_image_url(profile.portfolio);

COMMIT;
