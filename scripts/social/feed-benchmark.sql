-- Run after benchmark.sql: 50,000 additional posts, 1/1,000 in a followed club.
-- All identities and content are synthetic. Publication uses the actual batch function.
INSERT INTO fan_club VALUES(2,6);
INSERT INTO fan_follow VALUES(100007,5);
INSERT INTO fan_club_post
  SELECT 100000+n,CASE WHEN n%1000=0 THEN 1 ELSE 2 END,
    CASE WHEN n%1000=0 THEN 5 ELSE 6 END,NULL,'Synthetic post','Synthetic content',false,
    '2026-02-01'::timestamptz+n*interval '1 second'
  FROM generate_series(1,50000) n;
DO $$ BEGIN WHILE social_v2_publish_batch()>0 LOOP NULL; END LOOP; END $$;
ANALYZE;
SET statement_timeout='30s';
EXPLAIN (ANALYZE,BUFFERS) SELECT social_v2_feed(100007,NULL,20);
EXPLAIN (ANALYZE,BUFFERS) SELECT social_v2_feed(100008,NULL,20);
DO $$ DECLARE started timestamptz; n integer; BEGIN
  FOR n IN 1..20 LOOP
    started:=clock_timestamp(); PERFORM social_v2_feed(100007,NULL,20);
    INSERT INTO social_benchmark_samples VALUES('following sparse 50k posts',extract(epoch FROM clock_timestamp()-started)*1000);
    started:=clock_timestamp(); PERFORM social_v2_feed(100008,NULL,20);
    INSERT INTO social_benchmark_samples VALUES('following empty 50k posts',extract(epoch FROM clock_timestamp()-started)*1000);
  END LOOP;
END $$;
SELECT kind,count(*) samples,round(percentile_cont(0.5) WITHIN GROUP(ORDER BY elapsed_ms)::numeric,3) p50_ms,
  round(percentile_cont(0.95) WITHIN GROUP(ORDER BY elapsed_ms)::numeric,3) p95_ms,
  percentile_cont(0.95) WITHIN GROUP(ORDER BY elapsed_ms)<=200 AS within_200ms
FROM social_benchmark_samples GROUP BY kind ORDER BY kind;
