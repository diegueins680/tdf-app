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
