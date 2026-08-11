\set ON_ERROR_STOP on
\timing on

-- Disposable database benchmark. The enclosing transaction guarantees that
-- synthetic operational records never survive the test.
BEGIN;

INSERT INTO operations_domain_event (
  id, organization_id, branch_id, event_type, aggregate_type, aggregate_id,
  source_system, source_channel, correlation_key, deduplication_key,
  occurred_at, continuous_sla, payload
)
SELECT
  gen_random_uuid(),
  '00000000-0000-4000-8000-000000000001'::uuid,
  '00000000-0000-4000-8000-000000000002'::uuid,
  'benchmark.action_required',
  'manual',
  'benchmark-' || sequence_number,
  'benchmark',
  'system',
  'ops-benchmark:' || sequence_number,
  encode(digest('ops-benchmark:' || sequence_number, 'sha256'), 'hex'),
  now() - make_interval(secs => sequence_number % 86400),
  TRUE,
  jsonb_build_object(
    'priority', (ARRAY['urgent', 'high', 'normal', 'low'])[(sequence_number % 4) + 1],
    'titleEs', 'Trabajo de rendimiento ' || sequence_number,
    'titleEn', 'Performance work ' || sequence_number,
    'descriptionEs', 'Registro sintético no persistente',
    'descriptionEn', 'Non-persistent synthetic record',
    'metadata', jsonb_build_object('terminal', false, 'benchmark', true)
  )
FROM generate_series(1, 10000) AS generated(sequence_number)
ON CONFLICT DO NOTHING;

DO $$
DECLARE
  batch_number INTEGER;
BEGIN
  FOR batch_number IN 1..20 LOOP
    PERFORM * FROM operations_process_outbox_batch(500, 'operations-benchmark');
  END LOOP;
END;
$$;

ANALYZE operations_work_item;
ANALYZE operations_work_item_event;
ANALYZE operations_stream_event;

CREATE TEMP TABLE operations_benchmark_timing (
  sample INTEGER NOT NULL,
  query_name TEXT NOT NULL,
  duration_ms DOUBLE PRECISION NOT NULL
) ON COMMIT DROP;

DO $$
DECLARE
  sample_number INTEGER;
  started_at TIMESTAMPTZ;
  ignored_count BIGINT;
BEGIN
  FOR sample_number IN 1..100 LOOP
    started_at := clock_timestamp();
    SELECT count(*) INTO ignored_count FROM (
      SELECT item.id
      FROM operations_work_item item
      WHERE item.organization_id = '00000000-0000-4000-8000-000000000001'::uuid
        AND item.branch_id = '00000000-0000-4000-8000-000000000002'::uuid
        AND item.status NOT IN ('resolved', 'archived')
        AND to_tsvector('simple',
          COALESCE(item.title_es,'') || ' ' || COALESCE(item.title_en,'') || ' ' ||
          COALESCE(item.description_es,'') || ' ' || COALESCE(item.description_en,'') || ' ' ||
          COALESCE(item.entity_id,'') || ' ' || COALESCE(item.correlation_key,''))
          @@ plainto_tsquery('simple', 'Performance')
      ORDER BY
        array_position(ARRAY['urgent','high','normal','low'], item.priority),
        item.due_at NULLS LAST,
        item.created_at DESC,
        item.id DESC
      LIMIT 50
    ) AS bounded_page;
    INSERT INTO operations_benchmark_timing
    VALUES (sample_number, 'filtered_inbox_50', EXTRACT(EPOCH FROM clock_timestamp() - started_at) * 1000);
  END LOOP;
END;
$$;

SELECT
  query_name,
  round(percentile_cont(0.50) WITHIN GROUP (ORDER BY duration_ms)::numeric, 3) AS p50_ms,
  round(percentile_cont(0.95) WITHIN GROUP (ORDER BY duration_ms)::numeric, 3) AS p95_ms,
  round(max(duration_ms)::numeric, 3) AS max_ms,
  count(*) AS samples
FROM operations_benchmark_timing
GROUP BY query_name;

SELECT
  count(*) FILTER (WHERE aggregate_id LIKE 'benchmark-%') AS synthetic_events,
  (SELECT count(*) FROM operations_work_item WHERE correlation_key LIKE 'ops-benchmark:%') AS synthetic_threads,
  (SELECT count(*) FROM operations_work_item_event event
    JOIN operations_work_item item ON item.id = event.work_item_id
    WHERE item.correlation_key LIKE 'ops-benchmark:%') AS synthetic_thread_events,
  (SELECT count(*) FROM operations_outbox
    WHERE aggregate_type = 'manual' AND aggregate_id LIKE 'benchmark-%' AND status = 'processed') AS processed_outbox
FROM operations_domain_event;

ROLLBACK;
