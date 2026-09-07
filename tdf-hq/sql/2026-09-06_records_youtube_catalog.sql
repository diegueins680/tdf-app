-- Reconcile the public TDF Records YouTube channel with the persisted Records
-- catalog. The source snapshot is the channel's Videos tab on 2026-09-06:
-- https://www.youtube.com/@tdf.records/videos
--
-- The five numbered TDF Live Sessions already live in the separate Sessions
-- collection. This migration adds/reconciles the other 33 channel uploads in
-- reverse-publication order, including the 27 recordings missing in production.

BEGIN;

SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '2min';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-records-youtube-catalog-2026-09-06', 0));

DO $preflight$
DECLARE
    missing_dependencies TEXT;
BEGIN
    SELECT string_agg(required.name, ', ' ORDER BY required.name)
    INTO missing_dependencies
    FROM (VALUES
        ('catalog_backfill_run'),
        ('catalog_definition'),
        ('catalog_migration_mapping'),
        ('collection_recording'),
        ('editorial_collection'),
        ('external_provider'),
        ('record_contributor'),
        ('record_external_resource'),
        ('recording'),
        ('recording_contributor'),
        ('recording_external_resource'),
        ('recording_type_reference'),
        ('workflow_state')
    ) AS required(name)
    WHERE to_regclass('public.' || required.name) IS NULL;

    IF missing_dependencies IS NOT NULL THEN
        RAISE EXCEPTION 'Records YouTube ingestion requires: %', missing_dependencies;
    END IF;

    IF NOT EXISTS (
        SELECT 1
        FROM catalog_definition
        WHERE code = 'records-recordings' AND active
    ) OR NOT EXISTS (
        SELECT 1
        FROM catalog_definition
        WHERE code = 'record-contributors' AND active
    ) OR NOT EXISTS (
        SELECT 1
        FROM recording_type_reference
        WHERE code = 'music-video' AND active
    ) OR NOT EXISTS (
        SELECT 1
        FROM external_provider
        WHERE code = 'youtube' AND active
    ) OR NOT EXISTS (
        SELECT 1
        FROM editorial_collection
        WHERE code = 'tdf-records-recordings'
          AND collection_type = 'recording'
          AND active
    ) THEN
        RAISE EXCEPTION 'The published Records recording catalog is not available';
    END IF;

    IF (
        SELECT count(*)
        FROM workflow_state state
        JOIN catalog_definition catalog ON catalog.workflow_id = state.workflow_id
        WHERE catalog.code IN ('records-recordings', 'record-contributors')
          AND state.code = 'published'
          AND state.active
    ) <> 2 THEN
        RAISE EXCEPTION 'The Records catalogs require an active published workflow state';
    END IF;
END
$preflight$;

CREATE TEMP TABLE records_youtube_source (
    sort_order INTEGER PRIMARY KEY,
    youtube_id TEXT UNIQUE NOT NULL CHECK (youtube_id ~ '^[A-Za-z0-9_-]{11}$'),
    title TEXT NOT NULL CHECK (btrim(title) <> ''),
    contributor TEXT NOT NULL CHECK (btrim(contributor) <> ''),
    duration_ms BIGINT NOT NULL CHECK (duration_ms > 0),
    description_es TEXT NOT NULL,
    description_en TEXT NOT NULL
) ON COMMIT DROP;

INSERT INTO records_youtube_source (
    sort_order, youtube_id, title, contributor, duration_ms, description_es, description_en
) VALUES
    (1,  'ooPsIHsikYU', 'Llama Este Pez @ Sereno Moreno Live Set Pt 1', 'Llama Este Pez', 756000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (2,  'Cb7VGZJ6apo', 'Llama Este Pez @ Sereno Moreno Live Set Pt 2', 'Llama Este Pez', 294000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (3,  'f2BabxM1Pjc', 'Federico Molinari @ TDF Electro Sessions', 'Federico Molinari', 2655000, 'DJ set publicado en el canal TDF Records.', 'DJ set published on the TDF Records YouTube channel.'),
    (4,  'rRkAeNB0R14', 'Just One Nite @ TDF Electro Sessions', 'Just One Nite', 3382000, 'DJ set publicado en el canal TDF Records.', 'DJ set published on the TDF Records YouTube channel.'),
    (5,  'wZQAlIqllQY', 'Morex DJ Set @ TDF Electro Sessions', 'Morex', 3742000, 'DJ set publicado en el canal TDF Records.', 'DJ set published on the TDF Records YouTube channel.'),
    (6,  'Re3lL-myniY', 'Diego Saá Live Set @ Rio Hostel Buritaca | 2h30 Electro/Minimal House | TDF Sessions #spacetrip', 'Diego Saá', 9062000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (7,  'YXBDWshbw18', 'Diego Saá Live @ Domo Pululahua | Electro-Micro-Minimal House – 44 min Impro Set', 'Diego Saá', 2664000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (8,  'YDODXZ4lyRk', 'Diego Saá @ TDF Electro Sessions', 'Diego Saá', 2889000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (9,  '1hKWOram3aw', 'Everaldo Vasco @ TDF Sessions', 'Everaldo Vasco', 5179000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (10, 'xqeey8SrH8M', 'COHEMA @ TDF Sessions', 'COHEMA', 3600000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (11, 'PamFdN2RjUI', 'AVR @ TDF Sessions', 'AVR', 2795000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (12, 'YLz4dnol3yE', 'Owen @ TDF Sessions', 'Owen', 3673000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (13, 'eU9Y9Rah7RA', 'MELANIA @ TDF SESSIONS', 'MELANIA', 3390000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (14, 'Vqdp_AfB36Q', 'ELI LASSO @ TDF SESSIONS', 'ELI LASSO', 3408000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (15, 'aRaeiPtyav4', 'LYSERGICMAN @ TDF SESSIONS', 'LYSERGICMAN', 3891000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (16, 'EqxpOaVs6FU', 'Semiazas @ TDF Sessions', 'Semiazas', 2728000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (17, 'g1U0xwX8XDo', 'Juan Diego @ TDF Sessions', 'Juan Diego', 4934000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (18, 'SEXPiDKvlpI', 'MOOD PATTERN @ TDF SESSIONS', 'MOOD PATTERN', 2734000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (19, 'y0TXaWlmpyQ', 'LE CHU B2B LABII @ TDF SESSIONS', 'LE CHU B2B LABII', 3809000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (20, 'xdPLcTASCxA', 'ALEJANDRO ROMERO @ TDF SESSIONS', 'ALEJANDRO ROMERO', 2305000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (21, 'bHObIKaoAZE', 'La Bestia Quiñonez - Agua Que va a Caer @ TDF SESSIONS', 'La Bestia Quiñonez', 294000, 'Presentación publicada en el canal TDF Records.', 'Performance published on the TDF Records YouTube channel.'),
    (22, 'G5tgENvi4SQ', 'ESTEBAN MUÑOZ @ TDF SESSIONS', 'ESTEBAN MUÑOZ', 3430000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (23, '2r1P5sTBsLE', 'JULIO DIAZ @ TDF SESSIONS', 'JULIO DIAZ', 3919000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (24, 'gCNSV_rJQMQ', 'Fabro @ TDF SESSIONS', 'Fabro', 3974000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (25, 'ZJTXaPPIo2Q', 'DATI DICE @ TDF SESSIONS', 'DATI DICE', 2577000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (26, 'Sbb_LQAilLY', 'MELANIA @ TDF ESTUDIO', 'MELANIA', 2437000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (27, 'hQx_eOTQOGg', 'Agus @ TDF SESSIONS', 'Agus', 3181000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (28, 'QXxt-DELqo0', 'Diego Saá (Live set modular) @ TDF SESSIONS', 'Diego Saá', 1652000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (29, '2m7HiIxTPvM', 'La Clau @ TDF SESSIONS', 'La Clau', 3895000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (30, 'nC4vict39i4', 'JUANO LEDESMA @ TDF SESSIONS', 'JUANO LEDESMA', 2479000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (31, 'hexW4ROF0r0', 'DANI ALBAN @ TDF SESSIONS', 'DANI ALBAN', 2759000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.'),
    (32, 'msDxB2NU3c4', 'Liquid Paper Girl @ TDF SESSIONS (Liveset)', 'Liquid Paper Girl', 2825000, 'Live set publicado en el canal TDF Records.', 'Live set published on the TDF Records YouTube channel.'),
    (33, '8Cv0RGQdJA4', 'Diego Saá @ TDF ESTUDIO', 'Diego Saá', 2030000, 'Sesión publicada en el canal TDF Records.', 'Session published on the TDF Records YouTube channel.');

DO $source_validation$
BEGIN
    IF (SELECT count(*) FROM records_youtube_source) <> 33
       OR (SELECT min(sort_order) FROM records_youtube_source) <> 1
       OR (SELECT max(sort_order) FROM records_youtube_source) <> 33 THEN
        RAISE EXCEPTION 'The TDF Records YouTube source snapshot must contain exactly positions 1 through 33';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM records_youtube_source source
        WHERE source.youtube_id IN (
            '9387ent0ELc', '5SpnEELSNqw', '97PnHRn8IGs', 'e24-id_Ix8s', 'z7RpdrL4P4A'
        )
    ) THEN
        RAISE EXCEPTION 'Numbered TDF Live Sessions must remain in the Sessions collection';
    END IF;
END
$source_validation$;

INSERT INTO catalog_backfill_run (
    id, run_code, candidate_revision, dry_run, status, safety_threshold,
    scanned_rows, mapped_rows, ambiguous_rows, rejected_rows, started_at,
    completed_at, report, correlation_id
) VALUES (
    gen_random_uuid(),
    'records-youtube-catalog-2026-09-06',
    'youtube-channel-UCx9Jpaw_XDrMtIdzWYlU51g-videos-2026-09-06',
    FALSE,
    'mapping',
    0,
    33,
    0,
    0,
    0,
    now(),
    NULL,
    NULL,
    'records-youtube-catalog:UCx9Jpaw_XDrMtIdzWYlU51g:2026-09-06'
)
ON CONFLICT (run_code, candidate_revision, dry_run) DO UPDATE
SET status = 'mapping',
    scanned_rows = 33,
    mapped_rows = 0,
    ambiguous_rows = 0,
    rejected_rows = 0,
    started_at = now(),
    completed_at = NULL,
    report = NULL;

INSERT INTO record_contributor (
    id, catalog_id, code, contributor_kind, name_es, name_en, sort_order,
    active, workflow_state_id, created_at, updated_at, version
)
SELECT DISTINCT ON (lower(btrim(source.contributor)))
    gen_random_uuid(),
    catalog.id,
    'legacy-credit-' || left(encode(digest(lower(btrim(source.contributor)), 'sha256'), 'hex'), 20),
    'artist',
    source.contributor,
    source.contributor,
    0,
    TRUE,
    state.id,
    now(),
    now(),
    1
FROM records_youtube_source source
JOIN catalog_definition catalog ON catalog.code = 'record-contributors'
JOIN workflow_state state
  ON state.workflow_id = catalog.workflow_id
 AND state.code = 'published'
 AND state.active
ORDER BY lower(btrim(source.contributor)), source.sort_order
ON CONFLICT (code) DO NOTHING;

INSERT INTO record_external_resource (
    id, provider_id, external_code, resource_kind, canonical_url, duration_ms,
    thumbnail_url, active, created_at, updated_at, version
)
SELECT
    gen_random_uuid(),
    provider.id,
    source.youtube_id,
    'video',
    'https://www.youtube.com/watch?v=' || source.youtube_id,
    source.duration_ms,
    'https://i.ytimg.com/vi/' || source.youtube_id || '/hqdefault.jpg',
    TRUE,
    now(),
    now(),
    1
FROM records_youtube_source source
JOIN external_provider provider ON provider.code = 'youtube' AND provider.active
ON CONFLICT (provider_id, resource_kind, external_code) DO UPDATE
SET canonical_url = EXCLUDED.canonical_url,
    duration_ms = EXCLUDED.duration_ms,
    thumbnail_url = EXCLUDED.thumbnail_url,
    active = TRUE,
    updated_at = now(),
    version = record_external_resource.version + 1
WHERE (
    record_external_resource.canonical_url,
    record_external_resource.duration_ms,
    record_external_resource.thumbnail_url,
    record_external_resource.active
) IS DISTINCT FROM (
    EXCLUDED.canonical_url,
    EXCLUDED.duration_ms,
    EXCLUDED.thumbnail_url,
    TRUE
);

INSERT INTO recording (
    id, catalog_id, code, recording_type_id, title_es, title_en,
    description_es, description_en, duration_ms, current_slug, sort_order,
    active, workflow_state_id, created_at, updated_at, published_revision,
    usage_count, version
)
SELECT
    gen_random_uuid(),
    catalog.id,
    'youtube-recording-' || source.youtube_id,
    recording_type.id,
    source.title,
    source.title,
    source.description_es,
    source.description_en,
    source.duration_ms,
    'youtube-recording-' || source.youtube_id,
    source.sort_order,
    TRUE,
    state.id,
    now(),
    now(),
    1,
    0,
    1
FROM records_youtube_source source
JOIN catalog_definition catalog ON catalog.code = 'records-recordings'
JOIN workflow_state state
  ON state.workflow_id = catalog.workflow_id
 AND state.code = 'published'
 AND state.active
JOIN recording_type_reference recording_type
  ON recording_type.code = 'music-video'
 AND recording_type.active
ON CONFLICT (code) DO UPDATE
SET title_es = EXCLUDED.title_es,
    title_en = EXCLUDED.title_en,
    description_es = EXCLUDED.description_es,
    description_en = EXCLUDED.description_en,
    duration_ms = EXCLUDED.duration_ms,
    sort_order = EXCLUDED.sort_order,
    active = TRUE,
    workflow_state_id = EXCLUDED.workflow_state_id,
    updated_at = now(),
    published_revision = GREATEST(recording.published_revision, 1),
    version = recording.version + 1
WHERE (
    recording.title_es,
    recording.title_en,
    recording.description_es,
    recording.description_en,
    recording.duration_ms,
    recording.sort_order,
    recording.active,
    recording.workflow_state_id
) IS DISTINCT FROM (
    EXCLUDED.title_es,
    EXCLUDED.title_en,
    EXCLUDED.description_es,
    EXCLUDED.description_en,
    EXCLUDED.duration_ms,
    EXCLUDED.sort_order,
    TRUE,
    EXCLUDED.workflow_state_id
);

INSERT INTO recording_contributor (
    id, recording_id, contributor_id, credit_role, sort_order, primary_credit
)
SELECT
    gen_random_uuid(),
    recording.id,
    contributor.id,
    'primary-artist',
    0,
    TRUE
FROM records_youtube_source source
JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
JOIN record_contributor contributor
  ON contributor.code = 'legacy-credit-' || left(encode(digest(lower(btrim(source.contributor)), 'sha256'), 'hex'), 20)
ON CONFLICT DO NOTHING;

INSERT INTO recording_external_resource (
    id, recording_id, resource_id, relation_kind, sort_order, primary_resource
)
SELECT
    gen_random_uuid(),
    recording.id,
    resource.id,
    'primary-media',
    0,
    TRUE
FROM records_youtube_source source
JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
JOIN external_provider provider ON provider.code = 'youtube' AND provider.active
JOIN record_external_resource resource
  ON resource.provider_id = provider.id
 AND resource.resource_kind = 'video'
 AND resource.external_code = source.youtube_id
ON CONFLICT DO NOTHING;

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
        RAISE EXCEPTION 'The TDF Records recording collection is too large to reorder safely';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM collection_recording membership
        JOIN editorial_collection collection ON collection.id = membership.collection_id
        WHERE collection.code = 'tdf-records-recordings'
          AND membership.sort_order BETWEEN -9000000000000000000 AND -8999999999999000001
    ) THEN
        RAISE EXCEPTION 'The temporary TDF Records ordering range is occupied';
    END IF;
END
$order_preflight$;

-- Vacate the collection's ordering range before the new reverse-publication
-- order is applied. Non-channel recordings are retained and later placed after
-- the 33 channel uploads in their existing relative order.
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

INSERT INTO collection_recording (
    id, collection_id, recording_id, sort_order, featured
)
SELECT
    gen_random_uuid(),
    collection.id,
    recording.id,
    source.sort_order,
    FALSE
FROM records_youtube_source source
JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
JOIN editorial_collection collection ON collection.code = 'tdf-records-recordings'
ON CONFLICT (collection_id, recording_id) DO UPDATE
SET sort_order = EXCLUDED.sort_order;

WITH unrelated_membership AS (
    SELECT
        membership.id,
        row_number() OVER (
            ORDER BY membership.sort_order, membership.id
        ) AS retained_order
    FROM collection_recording membership
    JOIN editorial_collection collection ON collection.id = membership.collection_id
    JOIN recording ON recording.id = membership.recording_id
    LEFT JOIN records_youtube_source source
      ON recording.code = 'youtube-recording-' || source.youtube_id
    WHERE collection.code = 'tdf-records-recordings'
      AND source.youtube_id IS NULL
)
UPDATE collection_recording membership
SET sort_order = 33 + unrelated_membership.retained_order
FROM unrelated_membership
WHERE membership.id = unrelated_membership.id;

INSERT INTO catalog_migration_mapping (
    id, run_id, source_table, source_column, source_record_id, original_value,
    normalized_value, catalog_id, entity_id, status, evidence, source_count,
    created_at
)
SELECT
    gen_random_uuid(),
    run.id,
    'youtube_channel_uploads',
    'video_id',
    source.youtube_id,
    jsonb_build_object(
        'channelId', 'UCx9Jpaw_XDrMtIdzWYlU51g',
        'position', source.sort_order,
        'title', source.title,
        'durationMs', source.duration_ms,
        'url', 'https://www.youtube.com/watch?v=' || source.youtube_id
    )::text,
    'youtube-recording-' || source.youtube_id,
    catalog.id,
    recording.id,
    'mapped',
    'Public video ID, title, duration, and channel order observed on the TDF Records Videos tab on 2026-09-06.',
    1,
    now()
FROM records_youtube_source source
JOIN catalog_backfill_run run
  ON run.run_code = 'records-youtube-catalog-2026-09-06'
 AND run.candidate_revision = 'youtube-channel-UCx9Jpaw_XDrMtIdzWYlU51g-videos-2026-09-06'
 AND NOT run.dry_run
JOIN catalog_definition catalog ON catalog.code = 'records-recordings'
JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
ON CONFLICT (run_id, source_table, source_column, source_record_id, original_value) DO UPDATE
SET normalized_value = EXCLUDED.normalized_value,
    catalog_id = EXCLUDED.catalog_id,
    entity_id = EXCLUDED.entity_id,
    status = EXCLUDED.status,
    evidence = EXCLUDED.evidence,
    source_count = EXCLUDED.source_count;

WITH source_change AS (
    UPDATE catalog_definition
    SET source_name = 'youtube-channel',
        source_version = 'UCx9Jpaw_XDrMtIdzWYlU51g/videos@2026-09-06',
        source_effective_date = DATE '2026-09-06',
        last_synced_at = now(),
        cache_revision = cache_revision + 1,
        updated_at = now(),
        version = version + 1
    WHERE code = 'records-recordings'
      AND (
          source_name,
          source_version,
          source_effective_date
      ) IS DISTINCT FROM (
          'youtube-channel',
          'UCx9Jpaw_XDrMtIdzWYlU51g/videos@2026-09-06',
          DATE '2026-09-06'
      )
    RETURNING 1
)
UPDATE editorial_collection
SET updated_at = now(),
    version = version + 1
WHERE code = 'tdf-records-recordings'
  AND EXISTS (SELECT 1 FROM source_change);

DO $validation$
DECLARE
    ingestion_run_id UUID;
BEGIN
    IF (
        SELECT count(*)
        FROM records_youtube_source source
        JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
        JOIN workflow_state state ON state.id = recording.workflow_state_id
        WHERE recording.active
          AND state.code = 'published'
          AND recording.title_es = source.title
          AND recording.duration_ms = source.duration_ms
    ) <> 33 THEN
        RAISE EXCEPTION 'Not all 33 TDF Records channel videos were published as recordings';
    END IF;

    IF (
        SELECT count(*)
        FROM records_youtube_source source
        JOIN external_provider provider ON provider.code = 'youtube' AND provider.active
        JOIN record_external_resource resource
          ON resource.provider_id = provider.id
         AND resource.resource_kind = 'video'
         AND resource.external_code = source.youtube_id
        JOIN recording_external_resource relationship ON relationship.resource_id = resource.id
        JOIN recording ON recording.id = relationship.recording_id
        WHERE recording.code = 'youtube-recording-' || source.youtube_id
          AND resource.active
          AND relationship.primary_resource
    ) <> 33 THEN
        RAISE EXCEPTION 'Not all TDF Records recordings have an active primary YouTube resource';
    END IF;

    IF (
        SELECT count(*)
        FROM records_youtube_source source
        JOIN editorial_collection collection ON collection.code = 'tdf-records-recordings'
        JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
        JOIN collection_recording membership
          ON membership.collection_id = collection.id
         AND membership.recording_id = recording.id
         AND membership.sort_order = source.sort_order
    ) <> 33 THEN
        RAISE EXCEPTION 'The TDF Records recording collection is incomplete or out of channel order';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM collection_recording membership
        JOIN editorial_collection collection ON collection.id = membership.collection_id
        JOIN recording ON recording.id = membership.recording_id
        LEFT JOIN records_youtube_source source
          ON recording.code = 'youtube-recording-' || source.youtube_id
        WHERE collection.code = 'tdf-records-recordings'
          AND source.youtube_id IS NULL
          AND membership.sort_order <= 33
    ) THEN
        RAISE EXCEPTION 'A non-channel recording precedes the complete TDF YouTube snapshot';
    END IF;

    IF (
        SELECT count(*)
        FROM records_youtube_source source
        JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id
        JOIN recording_contributor relationship ON relationship.recording_id = recording.id
        JOIN record_contributor contributor ON contributor.id = relationship.contributor_id
        WHERE contributor.active
          AND relationship.primary_credit
          AND contributor.code = 'legacy-credit-' || left(encode(digest(lower(btrim(source.contributor)), 'sha256'), 'hex'), 20)
    ) <> 33 THEN
        RAISE EXCEPTION 'Not all TDF Records recordings have the expected primary contributor credit';
    END IF;

    SELECT id
    INTO ingestion_run_id
    FROM catalog_backfill_run
    WHERE run_code = 'records-youtube-catalog-2026-09-06'
      AND candidate_revision = 'youtube-channel-UCx9Jpaw_XDrMtIdzWYlU51g-videos-2026-09-06'
      AND NOT dry_run;

    IF ingestion_run_id IS NULL OR (
        SELECT count(*)
        FROM catalog_migration_mapping
        WHERE catalog_migration_mapping.run_id = ingestion_run_id
          AND status = 'mapped'
    ) <> 33 THEN
        RAISE EXCEPTION 'The TDF Records YouTube ingestion evidence is incomplete';
    END IF;

    UPDATE catalog_backfill_run
    SET status = 'complete',
        scanned_rows = 33,
        mapped_rows = 33,
        ambiguous_rows = 0,
        rejected_rows = 0,
        completed_at = now(),
        report = jsonb_build_object(
            'channelId', 'UCx9Jpaw_XDrMtIdzWYlU51g',
            'channelVideos', 38,
            'recordings', 33,
            'numberedLiveSessions', 5,
            'newProductionRecordings', 27,
            'sourceObservedOn', '2026-09-06'
        )::text
    WHERE id = ingestion_run_id;
END
$validation$;

COMMIT;
