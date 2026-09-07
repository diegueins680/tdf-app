-- Recoverable rollback for the 2026-09-06 TDF Records YouTube ingestion.
-- Catalog rows and evidence are retained: the 27 additions are deactivated and
-- the six previously published recording entries regain their former order and
-- metadata. Reapplying the forward migration restores the complete snapshot.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '2min';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-records-youtube-catalog-2026-09-06', 0));

CREATE TEMP TABLE records_youtube_rollback_source (
    youtube_id TEXT PRIMARY KEY,
    snapshot_order INTEGER UNIQUE NOT NULL,
    prior_order INTEGER,
    prior_title TEXT,
    prior_duration_ms BIGINT,
    prior_description TEXT
) ON COMMIT DROP;

INSERT INTO records_youtube_rollback_source (
    youtube_id, snapshot_order, prior_order, prior_title, prior_duration_ms,
    prior_description
) VALUES
    ('ooPsIHsikYU', 1, NULL, NULL, NULL, NULL),
    ('Cb7VGZJ6apo', 2, NULL, NULL, NULL, NULL),
    ('f2BabxM1Pjc', 3, 1, 'Federico Molinari @ TDF Electro Sessions', 2654000, 'DJ set publicado en el canal TDF Records.'),
    ('rRkAeNB0R14', 4, 2, 'Just One Nite @ TDF Electro Sessions', 3381000, 'DJ set publicado en el canal TDF Records.'),
    ('wZQAlIqllQY', 5, 3, 'Morex DJ Set @ TDF Electro Sessions', 3741000, 'DJ set publicado en el canal TDF Records.'),
    ('Re3lL-myniY', 6, NULL, NULL, NULL, NULL),
    ('YXBDWshbw18', 7, NULL, NULL, NULL, NULL),
    ('YDODXZ4lyRk', 8, 4, 'Diego Saá @ TDF Electro Sessions', 2889000, 'Live set publicado en el canal TDF Records.'),
    ('1hKWOram3aw', 9, 5, 'Everaldo Vasco @ TDF Sessions', 5178000, 'Sesión publicada en el canal TDF Records.'),
    ('xqeey8SrH8M', 10, 6, 'COHEMA @ TDF Sessions', 3600000, 'Sesión publicada en el canal TDF Records.'),
    ('PamFdN2RjUI', 11, NULL, NULL, NULL, NULL),
    ('YLz4dnol3yE', 12, NULL, NULL, NULL, NULL),
    ('eU9Y9Rah7RA', 13, NULL, NULL, NULL, NULL),
    ('Vqdp_AfB36Q', 14, NULL, NULL, NULL, NULL),
    ('aRaeiPtyav4', 15, NULL, NULL, NULL, NULL),
    ('EqxpOaVs6FU', 16, NULL, NULL, NULL, NULL),
    ('g1U0xwX8XDo', 17, NULL, NULL, NULL, NULL),
    ('SEXPiDKvlpI', 18, NULL, NULL, NULL, NULL),
    ('y0TXaWlmpyQ', 19, NULL, NULL, NULL, NULL),
    ('xdPLcTASCxA', 20, NULL, NULL, NULL, NULL),
    ('bHObIKaoAZE', 21, NULL, NULL, NULL, NULL),
    ('G5tgENvi4SQ', 22, NULL, NULL, NULL, NULL),
    ('2r1P5sTBsLE', 23, NULL, NULL, NULL, NULL),
    ('gCNSV_rJQMQ', 24, NULL, NULL, NULL, NULL),
    ('ZJTXaPPIo2Q', 25, NULL, NULL, NULL, NULL),
    ('Sbb_LQAilLY', 26, NULL, NULL, NULL, NULL),
    ('hQx_eOTQOGg', 27, NULL, NULL, NULL, NULL),
    ('QXxt-DELqo0', 28, NULL, NULL, NULL, NULL),
    ('2m7HiIxTPvM', 29, NULL, NULL, NULL, NULL),
    ('nC4vict39i4', 30, NULL, NULL, NULL, NULL),
    ('hexW4ROF0r0', 31, NULL, NULL, NULL, NULL),
    ('msDxB2NU3c4', 32, NULL, NULL, NULL, NULL),
    ('8Cv0RGQdJA4', 33, NULL, NULL, NULL, NULL);

CREATE TEMP TABLE records_youtube_retained_order (
    recording_id UUID PRIMARY KEY,
    prior_order BIGINT UNIQUE NOT NULL
) ON COMMIT DROP;

INSERT INTO records_youtube_retained_order (recording_id, prior_order)
SELECT
    (entry.value ->> 'recordingId')::UUID,
    (entry.value ->> 'sortOrder')::BIGINT
FROM catalog_backfill_run run
CROSS JOIN LATERAL jsonb_array_elements(
    coalesce(run.report, '{}')::jsonb -> 'retainedCollectionOrder'
) entry
WHERE run.run_code = 'records-youtube-catalog-2026-09-06'
  AND run.candidate_revision = 'youtube-channel-UCx9Jpaw_XDrMtIdzWYlU51g-videos-2026-09-06'
  AND NOT run.dry_run;

DO $order_preflight$
DECLARE
    membership_count BIGINT;
BEGIN
    SELECT count(*)
    INTO membership_count
    FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id = membership.collection_id
    WHERE collection.code = 'tdf-records-recordings';

    IF membership_count >= 1000000 THEN
        RAISE EXCEPTION 'The TDF Records recording collection is too large to roll back safely';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM collection_recording membership
        JOIN editorial_collection collection ON collection.id = membership.collection_id
        WHERE collection.code = 'tdf-records-recordings'
          AND membership.sort_order BETWEEN -9000000000000000000 AND -8999999999999000001
    ) THEN
        RAISE EXCEPTION 'The temporary TDF Records rollback ordering range is occupied';
    END IF;
END
$order_preflight$;

-- Vacate every current position, then restore the six prior channel rows and
-- every unrelated membership captured by the forward migration. Channel rows
-- introduced by the migration remain retained at a high inactive-only range.
WITH ranked_membership AS (
    SELECT
        membership.id,
        row_number() OVER (
            ORDER BY membership.sort_order, membership.id
        ) AS temporary_order
    FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id = membership.collection_id
    WHERE collection.code = 'tdf-records-recordings'
)
UPDATE collection_recording membership
SET sort_order = -9000000000000000000 + ranked_membership.temporary_order
FROM ranked_membership
WHERE membership.id = ranked_membership.id;

UPDATE collection_recording membership
SET sort_order = CASE
        WHEN source.prior_order IS NOT NULL THEN source.prior_order
        ELSE 9000000000000000000 + source.snapshot_order
    END
FROM editorial_collection collection,
     recording,
     records_youtube_rollback_source source
WHERE membership.collection_id = collection.id
  AND membership.recording_id = recording.id
  AND collection.code = 'tdf-records-recordings'
  AND recording.code = 'youtube-recording-' || source.youtube_id;

UPDATE collection_recording membership
SET sort_order = retained.prior_order
FROM editorial_collection collection,
     records_youtube_retained_order retained
WHERE membership.collection_id = collection.id
  AND membership.recording_id = retained.recording_id
  AND collection.code = 'tdf-records-recordings';

WITH post_migration_membership AS (
    SELECT
        membership.id,
        row_number() OVER (
            ORDER BY membership.sort_order, membership.id
        ) AS retained_order
    FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id = membership.collection_id
    WHERE collection.code = 'tdf-records-recordings'
      AND membership.sort_order < 0
)
UPDATE collection_recording membership
SET sort_order = 8000000000000000000 + post_migration_membership.retained_order
FROM post_migration_membership
WHERE membership.id = post_migration_membership.id;

UPDATE recording
SET active = FALSE,
    updated_at = now(),
    version = version + 1
FROM records_youtube_rollback_source source
WHERE recording.code = 'youtube-recording-' || source.youtube_id
  AND source.prior_order IS NULL
  AND recording.active;

UPDATE recording
SET title_es = source.prior_title,
    title_en = source.prior_title,
    description_es = source.prior_description,
    description_en = source.prior_description,
    duration_ms = source.prior_duration_ms,
    sort_order = source.prior_order,
    active = TRUE,
    updated_at = now(),
    version = version + 1
FROM records_youtube_rollback_source source
WHERE recording.code = 'youtube-recording-' || source.youtube_id
  AND source.prior_order IS NOT NULL
  AND (
      recording.title_es,
      recording.title_en,
      recording.description_es,
      recording.description_en,
      recording.duration_ms,
      recording.sort_order,
      recording.active
  ) IS DISTINCT FROM (
      source.prior_title,
      source.prior_title,
      source.prior_description,
      source.prior_description,
      source.prior_duration_ms,
      source.prior_order,
      TRUE
  );

UPDATE record_external_resource resource
SET duration_ms = source.prior_duration_ms,
    thumbnail_url = NULL,
    updated_at = now(),
    version = version + 1
FROM records_youtube_rollback_source source,
     external_provider provider
WHERE provider.id = resource.provider_id
  AND provider.code = 'youtube'
  AND resource.resource_kind = 'video'
  AND resource.external_code = source.youtube_id
  AND source.prior_order IS NOT NULL
  AND (resource.duration_ms, resource.thumbnail_url)
      IS DISTINCT FROM (source.prior_duration_ms, NULL::TEXT);

UPDATE catalog_definition
SET source_name = NULL,
    source_version = NULL,
    source_effective_date = NULL,
    last_synced_at = NULL,
    cache_revision = cache_revision + 1,
    updated_at = now(),
    version = version + 1
WHERE code = 'records-recordings'
  AND source_version = 'UCx9Jpaw_XDrMtIdzWYlU51g/videos@2026-09-06';

UPDATE catalog_backfill_run
SET status = 'rolled-back',
    completed_at = now(),
    report = (
        coalesce(report, '{}')::jsonb
        || jsonb_build_object('rolledBackAt', now())
    )::text
WHERE run_code = 'records-youtube-catalog-2026-09-06'
  AND candidate_revision = 'youtube-channel-UCx9Jpaw_XDrMtIdzWYlU51g-videos-2026-09-06'
  AND NOT dry_run;

DO $validation$
BEGIN
    IF (
        SELECT count(*)
        FROM records_youtube_rollback_source source
        JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
        JOIN editorial_collection collection ON collection.code = 'tdf-records-recordings'
        JOIN collection_recording membership
          ON membership.collection_id = collection.id
         AND membership.recording_id = recording.id
        WHERE source.prior_order IS NOT NULL
          AND recording.active
          AND membership.sort_order = source.prior_order
          AND recording.duration_ms = source.prior_duration_ms
    ) <> 6 THEN
        RAISE EXCEPTION 'The six pre-existing TDF Records videos were not restored';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM records_youtube_rollback_source source
        JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
        WHERE source.prior_order IS NULL
          AND recording.active
    ) THEN
        RAISE EXCEPTION 'A recording introduced by the YouTube ingestion remains active';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM records_youtube_retained_order retained
        LEFT JOIN editorial_collection collection
          ON collection.code = 'tdf-records-recordings'
        LEFT JOIN collection_recording membership
          ON membership.collection_id = collection.id
         AND membership.recording_id = retained.recording_id
         AND membership.sort_order = retained.prior_order
        WHERE membership.id IS NULL
    ) THEN
        RAISE EXCEPTION 'A retained non-channel recording did not regain its original order';
    END IF;
END
$validation$;

COMMIT;
