-- Synthetic local fixture; no production data or production scale claim.
\timing on
INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(6,110005) n;
INSERT INTO user_credential SELECT n,n,true FROM generate_series(6,110005) n;
INSERT INTO social_v2_preference(party_id,discoverable)
  SELECT n,true FROM generate_series(7,10006) n;
INSERT INTO social_v2_pair(party_a,party_b,follow_a)
  SELECT 6,n,true FROM generate_series(7,10006) n;
INSERT INTO social_v2_pair(party_a,party_b,follow_a)
  SELECT n,110005,true FROM generate_series(10007,100006) n;
ANALYZE;
SET statement_timeout='5s';
SELECT 'PostgreSQL version',version();
SELECT 'synthetic degree',d,count(*) FROM (
  SELECT count(*) d FROM (SELECT party_a actor FROM social_v2_pair UNION ALL SELECT party_b FROM social_v2_pair) e
  GROUP BY actor) degrees GROUP BY d ORDER BY d;
SELECT 'pair storage bytes',pg_total_relation_size('social_v2_pair');
-- EXPLAIN ANALYZE reports the actual execution of each fixed-size page, not a plan estimate.
EXPLAIN (ANALYZE,BUFFERS) SELECT social_v2_discover(6,20);
EXPLAIN (ANALYZE,BUFFERS) SELECT social_v2_discover(100007,20);
EXPLAIN (ANALYZE,BUFFERS) SELECT social_v2_feed(6,NULL,20);
SET statement_timeout='60s';
CREATE TEMP TABLE social_benchmark_samples(kind text,elapsed_ms double precision);
DO $$ DECLARE started timestamptz; n integer; BEGIN
  FOR n IN 1..20 LOOP
    started:=clock_timestamp(); PERFORM social_v2_discover(6,20);
    INSERT INTO social_benchmark_samples VALUES('discover hub',extract(epoch FROM clock_timestamp()-started)*1000);
    started:=clock_timestamp(); PERFORM social_v2_discover(100007,20);
    INSERT INTO social_benchmark_samples VALUES('discover sparse',extract(epoch FROM clock_timestamp()-started)*1000);
    started:=clock_timestamp(); PERFORM social_v2_feed(6,NULL,20);
    INSERT INTO social_benchmark_samples VALUES('following empty',extract(epoch FROM clock_timestamp()-started)*1000);
  END LOOP;
END $$;
SELECT kind,count(*) samples,round(percentile_cont(0.5) WITHIN GROUP(ORDER BY elapsed_ms)::numeric,3) p50_ms,
  round(percentile_cont(0.95) WITHIN GROUP(ORDER BY elapsed_ms)::numeric,3) p95_ms,
  percentile_cont(0.95) WITHIN GROUP(ORDER BY elapsed_ms)<=200 AS within_200ms
FROM social_benchmark_samples GROUP BY kind ORDER BY kind;
