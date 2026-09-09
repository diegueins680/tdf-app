#!/bin/sh
set -eu

TDF_MERCH_REPUTATION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MERCH_REPUTATION_CONTAINER="tdf-merch-reputation-migration-$$"
TDF_MERCH_REPUTATION_DATABASE="tdf_merch_reputation_test"
TDF_MERCH_REPUTATION_USE_DOCKER=false

cleanup() {
  if [ "$TDF_MERCH_REPUTATION_USE_DOCKER" = true ]; then
    docker rm -f "$TDF_MERCH_REPUTATION_CONTAINER" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -n "${TDF_MERCH_REPUTATION_DATABASE_URL:-}" ]; then
  psql_exec() {
    PGOPTIONS='-c statement_timeout=15000' \
      psql "$TDF_MERCH_REPUTATION_DATABASE_URL" -X -v ON_ERROR_STOP=1 "$@"
  }
  apply_file() {
    PGOPTIONS='-c statement_timeout=15000' \
      psql "$TDF_MERCH_REPUTATION_DATABASE_URL" -X -v ON_ERROR_STOP=1 \
      < "$TDF_MERCH_REPUTATION_ROOT/$1" >/dev/null
  }
else
  TDF_MERCH_REPUTATION_USE_DOCKER=true
  docker run --rm -d \
    --name "$TDF_MERCH_REPUTATION_CONTAINER" \
    -e POSTGRES_PASSWORD=merch-reputation-test \
    -e POSTGRES_DB="$TDF_MERCH_REPUTATION_DATABASE" \
    postgres:16-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_MERCH_REPUTATION_CONTAINER" \
    psql -U postgres -d "$TDF_MERCH_REPUTATION_DATABASE" -Atqc 'SELECT 1' \
    >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 30 ]; then
      echo "Merch reputation migration database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done

  psql_exec() {
    docker exec -i -e "PGOPTIONS=-c statement_timeout=15000" "$TDF_MERCH_REPUTATION_CONTAINER" \
      psql -X -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MERCH_REPUTATION_DATABASE" "$@"
  }
  apply_file() {
    docker exec -i -e "PGOPTIONS=-c statement_timeout=15000" "$TDF_MERCH_REPUTATION_CONTAINER" \
      psql -X -v ON_ERROR_STOP=1 -U postgres -d "$TDF_MERCH_REPUTATION_DATABASE" \
      < "$TDF_MERCH_REPUTATION_ROOT/$1" >/dev/null
  }
fi

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

psql_exec <<'SQL' >/dev/null
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE party (
  id BIGINT PRIMARY KEY,
  display_name TEXT NOT NULL,
  public_avatar_url TEXT
);
CREATE TABLE band (
  id BIGINT PRIMARY KEY,
  party_id BIGINT NOT NULL REFERENCES party(id)
);
CREATE TABLE band_member (
  band_id BIGINT NOT NULL REFERENCES band(id),
  party_id BIGINT NOT NULL REFERENCES party(id),
  PRIMARY KEY (band_id, party_id)
);
INSERT INTO party(id,display_name) VALUES
  (1,'Synthetic owner'),(2,'Synthetic admin'),(3,'Synthetic buyer'),
  (4,'Synthetic buyer two'),(5,'Synthetic band member'),(6,'Synthetic moderator'),
  (7,'Synthetic reporter'),(8,'Synthetic other owner'),(9,'Synthetic appeal reviewer');
INSERT INTO band(id,party_id) VALUES (10,1);
INSERT INTO band_member(band_id,party_id) VALUES (10,5);
SQL

apply_file tdf-hq/sql/2026-09-08_merch_reputation.sql
apply_file tdf-hq/sql/2026-09-08_merch_reputation.sql

concurrent_suggestion_sql="BEGIN;
SELECT merch_reputation_submit_category_suggestion(
  3::BIGINT,'store','Synthetic concurrency category',
  'Synthetic category used only to verify concurrent idempotent retries.',
  'category-concurrency-0001','development'
);
SELECT pg_sleep(0.5);
COMMIT;"
psql_exec -Atqc "$concurrent_suggestion_sql" >/dev/null &
first_concurrent_pid=$!
psql_exec -Atqc "$concurrent_suggestion_sql" >/dev/null &
second_concurrent_pid=$!
wait "$first_concurrent_pid"
wait "$second_concurrent_pid"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_reputation_category_suggestion WHERE normalized_key='synthetic-concurrency-category';")" \
  "1" \
  "Concurrent category suggestion created once"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_reputation_idempotency WHERE idempotency_key='category-concurrency-0001';")" \
  "1" \
  "Concurrent retry recorded one idempotency result"

psql_exec <<'SQL' >/dev/null
DO $$
DECLARE disabled_count INTEGER;
BEGIN
  SELECT count(*) INTO disabled_count FROM merch_reputation_feature_flag
    WHERE environment IN ('staging','production') AND enabled;
  IF disabled_count <> 0 THEN RAISE EXCEPTION 'remote environment flags must install disabled'; END IF;
  IF (SELECT count(*) FROM merch_reputation_feature_flag) <> 27 THEN
    RAISE EXCEPTION 'all nine independent flags must exist in three environments';
  END IF;
END $$;

INSERT INTO merch_store(id,owner_party_id,artist_party_id,slug,name,status,identity_verified_at) VALUES
  ('10000000-0000-4000-8000-000000000001',1,1,'synthetic-store','Synthetic Store','published',NOW()),
  ('10000000-0000-4000-8000-000000000002',8,NULL,'new-synthetic-store','New Synthetic Store','published',NULL);
INSERT INTO merch_store_member(store_id,party_id,member_role) VALUES
  ('10000000-0000-4000-8000-000000000001',2,'admin');
INSERT INTO merch_product(id,store_id,slug,name,status) VALUES
  ('20000000-0000-4000-8000-000000000001','10000000-0000-4000-8000-000000000001','shirt','Synthetic Shirt','published'),
  ('20000000-0000-4000-8000-000000000002','10000000-0000-4000-8000-000000000002','poster','Synthetic Poster','published');

INSERT INTO merch_order(
  id,store_id,buyer_party_id,order_state,payment_state,fulfillment_state,
  verified_buyer_at,verified_payment_at,delivered_at
) VALUES
  ('30000000-0000-4000-8000-000000000001','10000000-0000-4000-8000-000000000001',3,'fulfilled','verified','delivered',NOW()-INTERVAL '8 days',NOW()-INTERVAL '8 days',NOW()-INTERVAL '7 days'),
  ('30000000-0000-4000-8000-000000000002','10000000-0000-4000-8000-000000000001',3,'fulfilled','verified','delivered',NOW()-INTERVAL '7 days',NOW()-INTERVAL '7 days',NOW()-INTERVAL '6 days'),
  ('30000000-0000-4000-8000-000000000003','10000000-0000-4000-8000-000000000001',4,'fulfilled','verified','delivered',NOW()-INTERVAL '6 days',NOW()-INTERVAL '6 days',NOW()-INTERVAL '5 days'),
  ('30000000-0000-4000-8000-000000000004','10000000-0000-4000-8000-000000000001',3,'fulfilled','verified','delivered',NOW()-INTERVAL '5 days',NOW()-INTERVAL '5 days',NOW()-INTERVAL '4 days'),
  ('30000000-0000-4000-8000-000000000005','10000000-0000-4000-8000-000000000001',4,'fulfilled','verified','delivered',NOW()-INTERVAL '4 days',NOW()-INTERVAL '4 days',NOW()-INTERVAL '3 days'),
  ('30000000-0000-4000-8000-000000000006','10000000-0000-4000-8000-000000000002',3,'fulfilled','verified','delivered',NOW()-INTERVAL '3 days',NOW()-INTERVAL '3 days',NOW()-INTERVAL '2 days'),
  ('30000000-0000-4000-8000-000000000090','10000000-0000-4000-8000-000000000001',4,'confirmed','verified','preparing',NOW(),NOW(),NULL),
  ('30000000-0000-4000-8000-000000000091','10000000-0000-4000-8000-000000000001',1,'fulfilled','verified','delivered',NOW(),NOW(),NOW()-INTERVAL '1 day'),
  ('30000000-0000-4000-8000-000000000092','10000000-0000-4000-8000-000000000001',2,'fulfilled','verified','delivered',NOW(),NOW(),NOW()-INTERVAL '1 day'),
  ('30000000-0000-4000-8000-000000000093','10000000-0000-4000-8000-000000000001',5,'fulfilled','verified','delivered',NOW(),NOW(),NOW()-INTERVAL '1 day');
INSERT INTO merch_order(
  id,store_id,buyer_party_id,order_state,payment_state,fulfillment_state,
  verified_buyer_at,verified_payment_at,cancelled_at,cancellation_resolved_at
) VALUES (
  '30000000-0000-4000-8000-000000000094','10000000-0000-4000-8000-000000000001',4,
  'cancelled','verified','cancelled',NOW()-INTERVAL '3 days',NOW()-INTERVAL '3 days',
  NOW()-INTERVAL '2 days',NOW()-INTERVAL '1 day'
);

INSERT INTO merch_order_line(id,order_id,product_id,store_id,quantity,variant_snapshot,fulfillment_state,delivered_at)
SELECT
  ('40000000-0000-4000-8000-' || lpad(n::TEXT,12,'0'))::UUID,
  ('30000000-0000-4000-8000-' || lpad(n::TEXT,12,'0'))::UUID,
  CASE WHEN n=6 THEN '20000000-0000-4000-8000-000000000002'::UUID
    ELSE '20000000-0000-4000-8000-000000000001'::UUID END,
  CASE WHEN n=6 THEN '10000000-0000-4000-8000-000000000002'::UUID
    ELSE '10000000-0000-4000-8000-000000000001'::UUID END,
  1,'{"size":"synthetic-medium"}'::jsonb,'delivered',NOW()-INTERVAL '1 day'
FROM generate_series(1,6) n;
INSERT INTO merch_order_line(id,order_id,product_id,store_id,quantity,fulfillment_state)
VALUES ('40000000-0000-4000-8000-000000000090','30000000-0000-4000-8000-000000000090',
  '20000000-0000-4000-8000-000000000001','10000000-0000-4000-8000-000000000001',1,'pending');
INSERT INTO merch_order_line(id,order_id,product_id,store_id,quantity,fulfillment_state)
VALUES ('40000000-0000-4000-8000-000000000094','30000000-0000-4000-8000-000000000094',
  '20000000-0000-4000-8000-000000000001','10000000-0000-4000-8000-000000000001',1,'cancelled');

DO $$
BEGIN
  IF merch_review_evidence_is_eligible('store','30000000-0000-4000-8000-000000000090',4) THEN
    RAISE EXCEPTION 'pre-delivery order was eligible';
  END IF;
  IF merch_review_evidence_is_eligible('store','30000000-0000-4000-8000-000000000091',1)
    OR merch_review_evidence_is_eligible('store','30000000-0000-4000-8000-000000000092',2)
    OR merch_review_evidence_is_eligible('store','30000000-0000-4000-8000-000000000093',5) THEN
    RAISE EXCEPTION 'known related account was eligible';
  END IF;
  IF NOT merch_review_evidence_is_eligible('store','30000000-0000-4000-8000-000000000094',4)
    OR merch_review_evidence_is_eligible('product','40000000-0000-4000-8000-000000000094',4) THEN
    RAISE EXCEPTION 'cancelled-order communication/product eligibility is incorrect';
  END IF;
END $$;

SELECT merch_reputation_submit_review(
  3::BIGINT,'store','30000000-0000-4000-8000-000000000001',NULL,5::SMALLINT,FALSE,
  'Synthetic verified experience.',
  '{"preparation_dispatch":5,"communication":4,"packaging":5}'::jsonb,
  '[]'::jsonb,0,'store-review-create-0001','development');
SELECT merch_reputation_submit_review(
  3::BIGINT,'store','30000000-0000-4000-8000-000000000001',NULL,5::SMALLINT,FALSE,
  'Synthetic verified experience.',
  '{"preparation_dispatch":5,"communication":4,"packaging":5}'::jsonb,
  '[]'::jsonb,0,'store-review-create-0001','development');
SELECT merch_reputation_submit_review(
  3::BIGINT,'store','30000000-0000-4000-8000-000000000001',NULL,4::SMALLINT,TRUE,
  'Synthetic resolved experience.',
  '{"preparation_dispatch":4,"communication":5,"packaging":4,"problem_resolution":5}'::jsonb,
  '[]'::jsonb,1,'store-review-edit-000001','development');
SELECT merch_reputation_submit_review(
  3::BIGINT,'product','30000000-0000-4000-8000-000000000001','40000000-0000-4000-8000-000000000001',4::SMALLINT,FALSE,
  'Synthetic product experience.',
  '{"description_accuracy":5,"product_quality":4}'::jsonb,
  '[]'::jsonb,0,'product-review-create-1','development');
SELECT merch_reputation_submit_review(
  4::BIGINT,'store','30000000-0000-4000-8000-000000000094',NULL,3::SMALLINT,TRUE,
  'Synthetic cancellation communication experience.',
  '{"communication":3,"problem_resolution":4}'::jsonb,
  '[]'::jsonb,0,'cancel-review-create-1','development');

SELECT merch_reputation_set_priorities(
  3::BIGINT,'store','["communication","packaging","preparation_dispatch","problem_resolution"]'::jsonb,
  0,'merch-priority-create-1');
SELECT merch_reputation_set_priorities(
  3::BIGINT,'store','["communication","packaging","preparation_dispatch","problem_resolution"]'::jsonb,
  0,'merch-priority-create-1');
SELECT merch_reputation_set_priorities(
  3::BIGINT,'store','["problem_resolution","communication","packaging","preparation_dispatch"]'::jsonb,
  1,'merch-priority-edit-001');

DO $$
DECLARE created_result JSONB; duplicate_result JSONB; decision_result JSONB; suggestion_id UUID;
BEGIN
  created_result:=merch_reputation_submit_category_suggestion(
    3::BIGINT,'store','Claridad de tallas',
    'Qué tan clara y útil fue la información de tallas publicada.',
    'category-suggestion-create-1','development');
  duplicate_result:=merch_reputation_submit_category_suggestion(
    4::BIGINT,'store','Claridad de tallas',
    'Otra definición sintética que normaliza a la misma categoría.',
    'category-suggestion-duplicate-1','development');
  suggestion_id:=(created_result->>'suggestionId')::UUID;
  IF NOT (created_result->>'created')::BOOLEAN
    OR (duplicate_result->>'created')::BOOLEAN
    OR duplicate_result->>'status'<>'duplicate' THEN
    RAISE EXCEPTION 'category suggestion deduplication failed';
  END IF;
  BEGIN
    PERFORM merch_reputation_decide_category_suggestion(
      6::BIGINT,suggestion_id,'approved',5,'{"passed":false}','{"passed":true}',
      'Synthetic approval must fail without completed bias evidence.',
      'category-decision-invalid-01');
    RAISE EXCEPTION 'expected category bias gate rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected category bias gate rejection' THEN RAISE; END IF;
  END;
  decision_result:=merch_reputation_decide_category_suggestion(
    6::BIGINT,suggestion_id,'approved',25,'{"passed":true,"synthetic":true}',
    '{"passed":true,"synthetic":true}',
    'Synthetic governed approval with sample, bias, and utility evidence.',
    'category-decision-approve-1');
  PERFORM merch_reputation_decide_category_suggestion(
    6::BIGINT,suggestion_id,'approved',25,'{"passed":true,"synthetic":true}',
    '{"passed":true,"synthetic":true}',
    'Synthetic governed approval with sample, bias, and utility evidence.',
    'category-decision-approve-1');
  IF decision_result->>'status'<>'approved'
    OR (decision_result->>'affectsPublicScore')::BOOLEAN THEN
    RAISE EXCEPTION 'approved suggestion changed current public score governance';
  END IF;
END $$;

UPDATE merch_order SET payment_state='refunded' WHERE id='30000000-0000-4000-8000-000000000001';
UPDATE merch_order_line SET fulfillment_state='returned'
  WHERE id='40000000-0000-4000-8000-000000000001';
SELECT merch_reputation_submit_review(
  3::BIGINT,'product','30000000-0000-4000-8000-000000000001','40000000-0000-4000-8000-000000000001',5::SMALLINT,FALSE,
  'Synthetic product review updated after return.',
  '{"description_accuracy":5,"product_quality":5}'::jsonb,
  '[]'::jsonb,1,'product-review-return-edit-1','development');

DO $$
DECLARE target_review_id UUID;
BEGIN
  SELECT id INTO target_review_id FROM merch_review
    WHERE order_id='30000000-0000-4000-8000-000000000001' AND review_kind='store';
  IF (SELECT count(*) FROM merch_review WHERE order_id='30000000-0000-4000-8000-000000000001') <> 2 THEN
    RAISE EXCEPTION 'store and product reviews must remain separate';
  END IF;
  IF (SELECT count(*) FROM merch_review_revision WHERE merch_review_revision.review_id=target_review_id) <> 2
    OR (SELECT current_revision FROM merch_review WHERE id=target_review_id) <> 2 THEN
    RAISE EXCEPTION 'review edits did not preserve immutable history';
  END IF;
  IF (SELECT count(*) FROM merch_review WHERE id=target_review_id) <> 1 THEN
    RAISE EXCEPTION 'refund removed review evidence';
  END IF;
  IF NOT merch_review_evidence_is_eligible(
      'product','40000000-0000-4000-8000-000000000001',3) THEN
    RAISE EXCEPTION 'received-and-returned product lost its edit window';
  END IF;
  IF (SELECT count(*) FROM merch_reputation_priority_revision
      WHERE party_id=3 AND subject_kind='store') <> 2
    OR (SELECT current_revision FROM merch_reputation_priority_profile
      WHERE party_id=3 AND subject_kind='store') <> 2 THEN
    RAISE EXCEPTION 'priority edits did not preserve immutable history';
  END IF;
END $$;

DO $$
BEGIN
  UPDATE merch_order SET fraud_state='confirmed'
    WHERE id='30000000-0000-4000-8000-000000000001';
  PERFORM merch_reputation_rebuild_aggregate(
    'store','10000000-0000-4000-8000-000000000001');
  PERFORM merch_reputation_rebuild_aggregate(
    'product','20000000-0000-4000-8000-000000000001');
  IF (SELECT verified_review_count FROM merch_reputation_aggregate
      WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000001') <> 1
    OR (SELECT verified_review_count FROM merch_reputation_aggregate
      WHERE subject_kind='product' AND subject_id='20000000-0000-4000-8000-000000000001') <> 0
    OR NOT EXISTS (SELECT 1 FROM merch_review
      WHERE order_id='30000000-0000-4000-8000-000000000001') THEN
    RAISE EXCEPTION 'fraud correction did not exclude public influence while retaining evidence';
  END IF;
  UPDATE merch_order SET fraud_state='clear'
    WHERE id='30000000-0000-4000-8000-000000000001';
  PERFORM merch_reputation_rebuild_aggregate(
    'store','10000000-0000-4000-8000-000000000001');
  PERFORM merch_reputation_rebuild_aggregate(
    'product','20000000-0000-4000-8000-000000000001');
END $$;

DO $$
BEGIN
  BEGIN
    PERFORM merch_reputation_submit_review(
      4::BIGINT,'store','30000000-0000-4000-8000-000000000090',NULL,3::SMALLINT,FALSE,
      'Should not be accepted.',
      '{"preparation_dispatch":3,"communication":3,"packaging":3}'::jsonb,
      '[]'::jsonb,0,'pre-delivery-reject-1','development');
    RAISE EXCEPTION 'expected pre-delivery rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected pre-delivery rejection' THEN RAISE; END IF;
  END;
  BEGIN
    PERFORM merch_reputation_submit_review(
      1::BIGINT,'store','30000000-0000-4000-8000-000000000091',NULL,5::SMALLINT,FALSE,
      'Owner self review attempt.',
      '{"preparation_dispatch":5,"communication":5,"packaging":5}'::jsonb,
      '[]'::jsonb,0,'owner-self-reject-0001','development');
    RAISE EXCEPTION 'expected owner self-review rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected owner self-review rejection' THEN RAISE; END IF;
  END;
  BEGIN
    PERFORM merch_reputation_submit_review(
      2::BIGINT,'store','30000000-0000-4000-8000-000000000092',NULL,5::SMALLINT,FALSE,
      'Administrator self review attempt.',
      '{"preparation_dispatch":5,"communication":5,"packaging":5}'::jsonb,
      '[]'::jsonb,0,'admin-self-reject-0001','development');
    RAISE EXCEPTION 'expected administrator self-review rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected administrator self-review rejection' THEN RAISE; END IF;
  END;
  BEGIN
    PERFORM merch_reputation_submit_review(
      5::BIGINT,'store','30000000-0000-4000-8000-000000000093',NULL,5::SMALLINT,FALSE,
      'Band member self review attempt.',
      '{"preparation_dispatch":5,"communication":5,"packaging":5}'::jsonb,
      '[]'::jsonb,0,'band-self-reject-00001','development');
    RAISE EXCEPTION 'expected band-member self-review rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected band-member self-review rejection' THEN RAISE; END IF;
  END;
  BEGIN
    INSERT INTO merch_review_media_asset(
      id,uploaded_by,storage_key,content_type,byte_size,sha256,width,height
    ) VALUES (
      '50000000-0000-4000-8000-000000000099',3,'../private/review.webp',
      'image/webp',1024,repeat('b',64),100,100
    );
    RAISE EXCEPTION 'expected unsafe storage key rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected unsafe storage key rejection' THEN RAISE; END IF;
  END;
  INSERT INTO merch_review_media_asset(
    id,uploaded_by,storage_key,content_type,byte_size,sha256,width,height,scan_status,moderation_status
  ) VALUES (
    '50000000-0000-4000-8000-000000000001',7,'synthetic/review-image.webp','image/webp',1024,
    repeat('a',64),100,100,'safe','published'
  );
  BEGIN
    PERFORM merch_reputation_submit_review(
      3::BIGINT,'store','30000000-0000-4000-8000-000000000002',NULL,4::SMALLINT,FALSE,
      'Image ownership rejection attempt.',
      '{"preparation_dispatch":4,"communication":4,"packaging":4}'::jsonb,
      '[{"mediaAssetId":"50000000-0000-4000-8000-000000000001","altText":"Synthetic image"}]'::jsonb,
      0,'image-owner-reject-001','development');
    RAISE EXCEPTION 'expected cross-user image rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected cross-user image rejection' THEN RAISE; END IF;
  END;
END $$;

SELECT merch_reputation_record_operational_signal(
  'courier-late-0001','10000000-0000-4000-8000-000000000001','30000000-0000-4000-8000-000000000001',
  'dispatch_on_time',0,1,'courier','evidence-courier-late-0001','fulfillment','synthetic-courier-1',
  'server','{"handoffOnTime":true,"courierDelayed":true}'::jsonb,NOW()-INTERVAL '1 day');
SELECT merch_reputation_rebuild_aggregate('store','10000000-0000-4000-8000-000000000001');
DO $$ BEGIN
  IF (SELECT operational_average IS NOT NULL FROM merch_reputation_aggregate
      WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000001'
      AND formula_version_id='merch-commercial-bayes-v1') THEN
    RAISE EXCEPTION 'courier-attributable delay affected seller operation score';
  END IF;
END $$;

SELECT merch_reputation_record_operational_signal(
  'seller-dispatch-0001','10000000-0000-4000-8000-000000000001','30000000-0000-4000-8000-000000000001',
  'dispatch_on_time',1,1,'seller','evidence-seller-dispatch-0001','fulfillment','synthetic-fulfillment-1',
  'server','{"handoffOnTime":true}'::jsonb,NOW()-INTERVAL '2 days');
SELECT merch_reputation_process_events(100,'development');
SELECT merch_reputation_process_events(100,'development');

DO $$
DECLARE store_state TEXT; store_score NUMERIC; product_score NUMERIC; formula_id TEXT;
BEGIN
  SELECT publication_state,public_rating,formula_version_id
    INTO store_state,store_score,formula_id FROM merch_reputation_aggregate
    WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000001'
    ORDER BY calculated_through DESC LIMIT 1;
  SELECT public_rating INTO product_score FROM merch_reputation_aggregate
    WHERE subject_kind='product' AND subject_id='20000000-0000-4000-8000-000000000001'
    ORDER BY calculated_through DESC LIMIT 1;
  IF store_state <> 'published' OR store_score IS NULL OR product_score IS NULL THEN
    RAISE EXCEPTION 'eligible synthetic store/product aggregates were not independently published';
  END IF;
  IF formula_id <> 'merch-commercial-bayes-v1' THEN RAISE EXCEPTION 'aggregate lost formula version'; END IF;
  IF (SELECT calculation->>'purchaseValueWeighted' FROM merch_reputation_aggregate
      WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000001'
      ORDER BY calculated_through DESC LIMIT 1) <> 'false' THEN
    RAISE EXCEPTION 'aggregate does not prove purchase value neutrality';
  END IF;
  IF (SELECT count(*) FROM merch_reputation_projection_checkpoint WHERE processed_at IS NULL) <> 0 THEN
    RAISE EXCEPTION 'event projection is not replay-safe';
  END IF;
  IF merch_reputation_search_contribution('10000000-0000-4000-8000-000000000001',1) <> 0 THEN
    RAISE EXCEPTION 'search influence did not fail closed without an explicit environment';
  END IF;
  IF (SELECT count(*) FROM merch_reputation_badge_award
      WHERE store_id='10000000-0000-4000-8000-000000000001'
        AND badge_code='identity_verified' AND status='active') <> 1 THEN
    RAISE EXCEPTION 'reproducible identity badge was not awarded exactly once';
  END IF;
  PERFORM merch_reputation_rebuild_aggregate('store','10000000-0000-4000-8000-000000000002');
  IF (SELECT publication_state FROM merch_reputation_aggregate
      WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000002'
      ORDER BY calculated_through DESC LIMIT 1) <> 'new_store' THEN
    RAISE EXCEPTION 'under-threshold store did not remain neutral';
  END IF;
  IF (SELECT evaluable_order_count FROM merch_reputation_aggregate
      WHERE subject_kind='store' AND subject_id='10000000-0000-4000-8000-000000000001'
      ORDER BY calculated_through DESC LIMIT 1) <> 6 THEN
    RAISE EXCEPTION 'related-account orders changed the evaluable-order threshold';
  END IF;
END $$;

DO $$
BEGIN
  BEGIN
    PERFORM merch_reputation_record_operational_signal(
      'frontend-signal-0001','10000000-0000-4000-8000-000000000001',NULL,
      'response_time',1,1,'seller','frontend-evidence-0001','browser','browser-event','frontend','{}',NOW());
    RAISE EXCEPTION 'expected untrusted signal rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected untrusted signal rejection' THEN RAISE; END IF;
  END;
  BEGIN
    PERFORM merch_reputation_record_operational_signal(
      'evidence-reuse-0001','10000000-0000-4000-8000-000000000001','30000000-0000-4000-8000-000000000001',
      'dispatch_on_time',0,1,'seller','evidence-seller-dispatch-0001','fulfillment','changed-source',
      'server','{"handoffOnTime":false}'::jsonb,NOW());
    RAISE EXCEPTION 'expected evidence replay rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected evidence replay rejection' THEN RAISE; END IF;
  END;
END $$;

DO $$
DECLARE target_review UUID; report_result JSONB; transition_result JSONB; decision_result JSONB;
  appeal_result JSONB; resolution_result JSONB;
BEGIN
  INSERT INTO merch_reputation_notification_preference(
    party_id,moderation_change,appeal_result
  ) VALUES (3,TRUE,TRUE);
  INSERT INTO merch_reputation_notification_preference(party_id,evidence_request)
    VALUES (7,TRUE);
  SELECT id INTO target_review FROM merch_review
    WHERE review_kind='store' AND order_id='30000000-0000-4000-8000-000000000001';
  report_result:=merch_reputation_report_content(
    7::BIGINT,'review',target_review,'offensive','Synthetic authorized report details.',
    '[{"kind":"synthetic-context"}]','report-content-000001','development');
  transition_result:=merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'triage',
    'Synthetic moderator triage with an explicit rationale.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-triage-01');
  IF transition_result->>'state'<>'in_review' THEN
    RAISE EXCEPTION 'triage did not enter in-review state';
  END IF;
  transition_result:=merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'request_evidence',
    'Synthetic request for additional authorized evidence.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-evidence-01');
  PERFORM merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'request_evidence',
    'Synthetic request for additional authorized evidence.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-evidence-01');
  IF transition_result->>'state'<>'awaiting_evidence'
    OR NOT EXISTS (SELECT 1 FROM merch_reputation_notification_outbox
      WHERE notification_key='evidence-request:'||(report_result->>'caseId')||':7') THEN
    RAISE EXCEPTION 'evidence request transition or opt-in notification failed';
  END IF;
  transition_result:=merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'provisionally_hide',
    'Synthetic urgent safety reason for a temporary hide.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-hide-0001');
  IF transition_result->>'state'<>'provisionally_hidden'
    OR (SELECT status FROM merch_review WHERE id=target_review)<>'hidden' THEN
    RAISE EXCEPTION 'provisional hide did not preserve the case workflow';
  END IF;
  transition_result:=merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'resume_review',
    'Synthetic safe restoration while moderation review continues.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-resume-01');
  IF transition_result->>'state'<>'in_review'
    OR (SELECT status FROM merch_review WHERE id=target_review)<>'published' THEN
    RAISE EXCEPTION 'resume review did not restore the prior visibility';
  END IF;
  transition_result:=merch_reputation_transition_moderation_case(
    6::BIGINT,(report_result->>'caseId')::UUID,'provisionally_hide',
    'Synthetic renewed safety reason for a temporary hide.',
    '{"reviewedInAdminPanel":true}','moderation-workflow-hide-0002');
  IF transition_result->>'state'<>'provisionally_hidden'
    OR (SELECT status FROM merch_review WHERE id=target_review)<>'hidden' THEN
    RAISE EXCEPTION 'second provisional hide did not preserve the case workflow';
  END IF;
  decision_result:=merch_reputation_decide_moderation(
    6::BIGINT,(report_result->>'caseId')::UUID,'hide','offensive',
    'Synthetic moderation rationale with sufficient detail.',
    '{"reviewedInAdminPanel":true}','moderation-decision-01');
  appeal_result:=merch_reputation_appeal_decision(
    3::BIGINT,(decision_result->>'decisionId')::UUID,
    'Synthetic appeal grounds with sufficient detail.','moderation-appeal-0001');
  IF appeal_result->>'state' <> 'open' OR (SELECT status FROM merch_review WHERE id=target_review) <> 'hidden' THEN
    RAISE EXCEPTION 'moderation or appeal state transition failed';
  END IF;
  resolution_result:=merch_reputation_resolve_appeal(
    9::BIGINT,(appeal_result->>'appealId')::UUID,'reversed',
    'Synthetic independent appeal reversal rationale.',
    '{"independentReview":true}','appeal-resolution-0001');
  PERFORM merch_reputation_resolve_appeal(
    9::BIGINT,(appeal_result->>'appealId')::UUID,'reversed',
    'Synthetic independent appeal reversal rationale.',
    '{"independentReview":true}','appeal-resolution-0001');
  IF resolution_result->>'state' <> 'reversed'
    OR (SELECT status FROM merch_review WHERE id=target_review) <> 'published' THEN
    RAISE EXCEPTION 'independent appeal reversal did not restore prior visibility';
  END IF;
  IF (SELECT count(*) FROM merch_review_revision WHERE review_id=target_review) <> 2 THEN
    RAISE EXCEPTION 'moderation destroyed original review revisions';
  END IF;
  BEGIN
    PERFORM merch_reputation_respond(
      8::BIGINT,target_review,'Cross-store response should not be accepted.',0,
      'cross-store-response-01','development');
    RAISE EXCEPTION 'expected cross-store response rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected cross-store response rejection' THEN RAISE; END IF;
  END;
END $$;

DO $$
DECLARE risk_case UUID;
BEGIN
  INSERT INTO merch_reputation_risk_case(
    store_id,opened_by,policy_version_id,trigger_type,reason,evidence
  ) VALUES (
    '10000000-0000-4000-8000-000000000001',6,'merch-risk-v1-draft','suspected_fraud',
    'Synthetic risk case created only to verify due process.','{"synthetic":true}'
  ) RETURNING id INTO risk_case;
  BEGIN
    INSERT INTO merch_reputation_risk_measure(case_id,measure_type,imposed_by,reason,evidence)
    VALUES (risk_case,'settlement_hold',6,'Synthetic financial measure should require another reviewer.','{"synthetic":true}');
    RAISE EXCEPTION 'expected independent financial review rejection';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM='expected independent financial review rejection' THEN RAISE; END IF;
  END;
  IF (SELECT count(*) FROM merch_reputation_risk_measure) <> 0 THEN
    RAISE EXCEPTION 'rejected financial measure was persisted';
  END IF;
END $$;
SQL

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_review WHERE review_kind='store' AND order_id='30000000-0000-4000-8000-000000000001';")" \
  "1" \
  "One store review per order"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_review WHERE review_kind='product' AND order_line_id='40000000-0000-4000-8000-000000000001';")" \
  "1" \
  "One product review per eligible line"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_reputation_event;")" \
  "12" \
  "Durable event count"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_reputation_audit_event;")" \
  "9" \
  "Moderation and appeal audit events"

if apply_file tdf-hq/sql/2026-09-08_merch_reputation_rollback.sql 2>/dev/null; then
  echo "Rollback destroyed synthetic durable reputation evidence" >&2
  exit 1
fi
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM merch_review_revision;")" \
  "5" \
  "Rollback refusal retained review history"

echo "Merch reputation migration checks passed"
