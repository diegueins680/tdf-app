#!/bin/sh
set -eu

TDF_REPUTATION_WORKER_CONTAINER="tdf-reputation-worker-migration-$$"
TDF_REPUTATION_WORKER_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_REPUTATION_WORKER_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_REPUTATION_WORKER_CONTAINER" \
  -e POSTGRES_PASSWORD=reputation-worker-test \
  -e POSTGRES_DB=tdf_reputation_worker_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_REPUTATION_WORKER_CONTAINER" \
  psql -v ON_ERROR_STOP=1 -U postgres -d tdf_reputation_worker_test -Atqc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Contextual reputation worker migration database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_REPUTATION_WORKER_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_reputation_worker_test "$@"
}

apply_file() {
  docker exec -i -e "PGOPTIONS=-c statement_timeout=5000" "$TDF_REPUTATION_WORKER_CONTAINER" \
    psql -v ON_ERROR_STOP=1 -U postgres -d tdf_reputation_worker_test \
    < "$TDF_REPUTATION_WORKER_ROOT/$1" >/dev/null
}

assert_equal() {
  actual=$1
  expected=$2
  label=$3
  if [ "$actual" != "$expected" ]; then
    echo "$label: expected '$expected', got '$actual'" >&2
    exit 1
  fi
}

psql_exec -c 'CREATE EXTENSION IF NOT EXISTS pgcrypto;' >/dev/null
psql_exec -c 'CREATE TABLE party (id BIGINT PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party(id) VALUES (101), (102), (103), (104);' >/dev/null

apply_file tdf-hq/sql/2026-09-01_contextual_reputation.sql
apply_file tdf-hq/sql/2026-09-04_contextual_reputation_integrity.sql
apply_file tdf-hq/sql/2026-09-06_contextual_reputation_staging_worker.sql
apply_file tdf-hq/sql/2026-09-06_contextual_reputation_staging_worker.sql

assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text || ':' || simulation_only::text FROM reputation_worker_control WHERE environment='production';")" \
  "false:true" \
  "Production worker gate"

if psql_exec -c "UPDATE reputation_worker_control SET enabled=TRUE WHERE environment='production';" >/dev/null 2>&1; then
  echo "Production reputation worker gate could be enabled" >&2
  exit 1
fi

assert_equal \
  "$(psql_exec -Atc "SELECT (activated_at IS NOT NULL)::text FROM reputation_formula_version WHERE id='public-bayes-roc-v1';")" \
  "true" \
  "Active formula activation timestamp"
if psql_exec -c "UPDATE reputation_formula_version SET public_parameters=public_parameters || '{\"priorMean\":60}'::jsonb WHERE id='public-bayes-roc-v1';" >/dev/null 2>&1; then
  echo "Active reputation formula parameters allowed in-place mutation" >&2
  exit 1
fi
if psql_exec -c "UPDATE reputation_formula_version SET status='draft' WHERE id='public-bayes-roc-v1';" >/dev/null 2>&1; then
  echo "Active reputation formula could return to draft" >&2
  exit 1
fi

psql_exec -c "UPDATE reputation_worker_control SET enabled=TRUE WHERE environment='staging';" >/dev/null
if psql_exec -c "UPDATE reputation_worker_control SET enabled=TRUE WHERE environment='test';" >/dev/null 2>&1; then
  echo "More than one reputation worker environment could be enabled" >&2
  exit 1
fi

category_id=$(psql_exec -Atc "SELECT id FROM reputation_category WHERE slug='quality';")
interaction_id="c6000000-0000-4000-8000-000000000001"
evaluation_id="c6000000-0000-4000-8000-000000000002"
psql_exec -c "
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES (
    '$interaction_id', 'service', 'mix-001', 101, 102,
    '2026-09-01T12:00:00Z', '2026-09-01T12:00:00Z',
    'eligible', 'test_fixture', 'mix-001'
  );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES (
    '$evaluation_id', '$interaction_id', 101, 102, 'a_to_b',
    'draft', 'public-bayes-roc-v1', 1, '2026-10-01T00:00:00Z'
  );
  INSERT INTO reputation_evaluation_category(evaluation_id, category_id, position, weight)
  VALUES ('$evaluation_id', '$category_id', 1, 100);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group, absolute_score
  ) VALUES ('$evaluation_id', '$category_id', 102, 1, 80);
" >/dev/null

assert_equal \
  "$(psql_exec -Atc 'SELECT count(*) FROM reputation_aggregation_outbox;')" \
  "0" \
  "Draft evaluation outbox isolation"

psql_exec -c "
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-09-01T12:00:00Z'
  WHERE id='$evaluation_id';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT event_type || ':' || subject_party_id || ':' || context_key FROM reputation_aggregation_outbox;")" \
  "evaluation.submitted:102:service:mix-001" \
  "Submitted evaluation outbox event"

claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token || '|' || claimed_attempt FROM reputation_claim_aggregation_events('staging', 'worker-test-0001', 10, '2030-09-01T12:01:00Z');")
event_id=$(printf '%s' "$claim" | cut -d '|' -f 1)
claim_token=$(printf '%s' "$claim" | cut -d '|' -f 2)
assert_equal "$(printf '%s' "$claim" | cut -d '|' -f 3)" "1" "Initial claim attempt"

assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$event_id', '$claim_token', '2030-09-01T12:01:01Z');")" \
  "processed" \
  "Staging simulation processing"

assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || verified_interaction_count || ':' || distinct_evaluator_count || ':' || confidence || ':' || publication_state FROM reputation_aggregate_candidate WHERE subject_party_id=102 AND category_id='$category_id';")" \
  "50.9091:1:1:forming:simulation" \
  "Deterministic simulation candidate"
assert_equal \
  "$(psql_exec -Atc 'SELECT count(*) FROM reputation_public_aggregate;')" \
  "0" \
  "Public aggregate isolation"

psql_exec -c "UPDATE reputation_interaction SET status='disputed' WHERE id='$interaction_id';" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='interaction.invalidated' AND context_key='service:mix-001';")" \
  "1" \
  "Interaction invalidation fan-out"
invalidation_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-restore-0001', 1, '2030-09-01T12:01:10Z');")
invalidation_event_id=$(printf '%s' "$invalidation_claim" | cut -d '|' -f 1)
invalidation_claim_token=$(printf '%s' "$invalidation_claim" | cut -d '|' -f 2)
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$invalidation_event_id', '$invalidation_claim_token', '2030-09-01T12:01:11Z');")" \
  "processed" \
  "Invalidated interaction processing"
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count FROM reputation_aggregate_candidate WHERE subject_party_id=102 AND category_id='$category_id' AND context_key='service:mix-001';")" \
  "50.0000:0" \
  "Invalidated interaction evidence removal"

psql_exec -c "UPDATE reputation_interaction SET status='eligible' WHERE id='$interaction_id';" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='recalculation.requested' AND context_key='service:mix-001';")" \
  "1" \
  "Restored interaction recalculation fan-out"
restoration_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-restore-0001', 1, '2030-09-01T12:01:20Z');")
restoration_event_id=$(printf '%s' "$restoration_claim" | cut -d '|' -f 1)
restoration_claim_token=$(printf '%s' "$restoration_claim" | cut -d '|' -f 2)
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$restoration_event_id', '$restoration_claim_token', '2030-09-01T12:01:21Z');")" \
  "processed" \
  "Restored interaction processing"
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count FROM reputation_aggregate_candidate WHERE subject_party_id=102 AND category_id='$category_id' AND context_key='service:mix-001';")" \
  "50.9091:1" \
  "Restored interaction evidence recovery"

ordinal_interaction_a="c6100000-0000-4000-8000-000000000001"
ordinal_evaluation_a="c6100000-0000-4000-8000-000000000002"
ordinal_interaction_b="c6100000-0000-4000-8000-000000000003"
ordinal_evaluation_b="c6100000-0000-4000-8000-000000000004"
psql_exec -c "
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES
    (
      '$ordinal_interaction_a', 'service', 'ordinal-001', 101, 102,
      '2030-09-01T12:02:00Z', '2030-09-01T12:02:00Z',
      'eligible', 'test_fixture', 'ordinal-001-a'
    ),
    (
      '$ordinal_interaction_b', 'service', 'ordinal-001', 101, 102,
      '2030-09-01T12:02:00Z', '2030-09-01T12:02:00Z',
      'eligible', 'test_fixture', 'ordinal-001-b'
    );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES
    (
      '$ordinal_evaluation_a', '$ordinal_interaction_a', 101, 102, 'a_to_b',
      'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
    ),
    (
      '$ordinal_evaluation_b', '$ordinal_interaction_b', 101, 102, 'a_to_b',
      'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
    );
  INSERT INTO reputation_evaluation_category(evaluation_id, category_id, position, weight)
  VALUES
    ('$ordinal_evaluation_a', '$category_id', 1, 100),
    ('$ordinal_evaluation_b', '$category_id', 1, 100);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group
  ) VALUES
    ('$ordinal_evaluation_a', '$category_id', 102, 1),
    ('$ordinal_evaluation_a', '$category_id', 103, 2),
    ('$ordinal_evaluation_b', '$category_id', 103, 1),
    ('$ordinal_evaluation_b', '$category_id', 104, 2);
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-09-01T12:02:00Z'
  WHERE id IN ('$ordinal_evaluation_a', '$ordinal_evaluation_b');
" >/dev/null

ordinal_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-ordinal-0001', 1, '2030-09-01T12:02:30Z');")
ordinal_event_id=$(printf '%s' "$ordinal_claim" | cut -d '|' -f 1)
ordinal_claim_token=$(printf '%s' "$ordinal_claim" | cut -d '|' -f 2)
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$ordinal_event_id', '$ordinal_claim_token', '2030-09-01T12:02:31Z');")" \
  "processed" \
  "Ordinal connected-component processing"
assert_equal \
  "$(psql_exec -Atc "
    SELECT (winner.score > middle.score AND middle.score > loser.score)::text
    FROM reputation_aggregate_candidate winner
    JOIN reputation_aggregate_candidate middle
      ON middle.category_id=winner.category_id
     AND middle.context_key=winner.context_key
     AND middle.formula_version_id=winner.formula_version_id
    JOIN reputation_aggregate_candidate loser
      ON loser.category_id=winner.category_id
     AND loser.context_key=winner.context_key
     AND loser.formula_version_id=winner.formula_version_id
    WHERE winner.subject_party_id=102
      AND middle.subject_party_id=103
      AND loser.subject_party_id=104
      AND winner.category_id='$category_id'
      AND winner.context_key='service:ordinal-001';
  ")" \
  "true" \
  "Bayesian Bradley-Terry ordinal ordering"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE category_id='$category_id' AND context_key='service:ordinal-001';")" \
  "3" \
  "Atomic connected-component candidate refresh"
assert_equal \
  "$(psql_exec -Atc "SELECT metadata->>'componentSubjectCount' FROM reputation_aggregation_event_action WHERE event_id='$ordinal_event_id' AND action='processed';")" \
  "3" \
  "Connected-component audit metadata"

while :; do
  ordinal_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-ordinal-0001', 1, '2030-09-01T12:02:32Z');")
  [ -n "$ordinal_claim" ] || break
  ordinal_event_id=$(printf '%s' "$ordinal_claim" | cut -d '|' -f 1)
  ordinal_claim_token=$(printf '%s' "$ordinal_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$ordinal_event_id', '$ordinal_claim_token', '2030-09-01T12:02:33Z');")" \
    "processed" \
    "Ordinal follow-up event processing"
done

tie_interaction_id="c6200000-0000-4000-8000-000000000001"
tie_evaluation_id="c6200000-0000-4000-8000-000000000002"
psql_exec -c "
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES (
    '$tie_interaction_id', 'service', 'tie-001', 101, 102,
    '2030-09-01T12:02:40Z', '2030-09-01T12:02:40Z',
    'eligible', 'test_fixture', 'tie-001'
  );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES (
    '$tie_evaluation_id', '$tie_interaction_id', 101, 102, 'a_to_b',
    'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
  );
  INSERT INTO reputation_evaluation_category(evaluation_id, category_id, position, weight)
  VALUES ('$tie_evaluation_id', '$category_id', 1, 100);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group
  ) VALUES
    ('$tie_evaluation_id', '$category_id', 102, 1),
    ('$tie_evaluation_id', '$category_id', 103, 1);
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-09-01T12:02:40Z'
  WHERE id='$tie_evaluation_id';
" >/dev/null

tie_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-tie-0001', 1, '2030-09-01T12:02:50Z');")
tie_event_id=$(printf '%s' "$tie_claim" | cut -d '|' -f 1)
tie_claim_token=$(printf '%s' "$tie_claim" | cut -d '|' -f 2)
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$tie_event_id', '$tie_claim_token', '2030-09-01T12:02:51Z');")" \
  "processed" \
  "Ordinal tie processing"
assert_equal \
  "$(psql_exec -Atc "
    SELECT (left_candidate.score = right_candidate.score
            AND left_candidate.score = 50.0000)::text
    FROM reputation_aggregate_candidate left_candidate
    JOIN reputation_aggregate_candidate right_candidate
      ON right_candidate.category_id=left_candidate.category_id
     AND right_candidate.context_key=left_candidate.context_key
     AND right_candidate.formula_version_id=left_candidate.formula_version_id
    WHERE left_candidate.subject_party_id=102
      AND right_candidate.subject_party_id=103
      AND left_candidate.category_id='$category_id'
      AND left_candidate.context_key='service:tie-001';
  ")" \
  "true" \
  "Bradley-Terry tie outcome"

while :; do
  tie_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-tie-0001', 1, '2030-09-01T12:02:52Z');")
  [ -n "$tie_claim" ] || break
  tie_event_id=$(printf '%s' "$tie_claim" | cut -d '|' -f 1)
  tie_claim_token=$(printf '%s' "$tie_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$tie_event_id', '$tie_claim_token', '2030-09-01T12:02:53Z');")" \
    "processed" \
    "Tie follow-up event processing"
done

stable_event_id="c6000000-0000-4000-8000-000000000003"
stable_correlation_id="c6000000-0000-4000-8000-000000000004"
psql_exec -c "
  SELECT reputation_enqueue_aggregation_event(
    '$stable_event_id', 'recalculation.requested', 102, 'service:mix-001',
    '$category_id', 1, 'public-bayes-roc-v1', '$stable_correlation_id', NULL,
    '2026-09-01T12:02:00Z'
  );
  SELECT reputation_enqueue_aggregation_event(
    '$stable_event_id', 'recalculation.requested', 102, 'service:mix-001',
    '$category_id', 1, 'public-bayes-roc-v1', '$stable_correlation_id', NULL,
    '2026-09-01T12:02:00Z'
  );
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE id='$stable_event_id';")" \
  "1" \
  "Stable event idempotency"

if psql_exec -c "SELECT reputation_enqueue_aggregation_event(
  '$stable_event_id', 'recalculation.requested', 102, 'service:other',
  '$category_id', 1, 'public-bayes-roc-v1', '$stable_correlation_id', NULL,
  '2026-09-01T12:02:00Z'
);" >/dev/null 2>&1; then
  echo "Stable event ID accepted conflicting immutable evidence" >&2
  exit 1
fi

if psql_exec -c "UPDATE reputation_aggregation_outbox SET context_key='service:mutated' WHERE id='$stable_event_id';" >/dev/null 2>&1; then
  echo "Reputation event immutable evidence allowed mutation" >&2
  exit 1
fi

run_id="c6000000-0000-4000-8000-000000000005"
run_event_a="c6000000-0000-4000-8000-000000000006"
run_event_b="c6000000-0000-4000-8000-000000000007"
run_correlation_id="c6000000-0000-4000-8000-000000000008"
psql_exec -c "
  INSERT INTO reputation_aggregation_run(
    id, environment, run_kind, formula_version_id, high_water_mark
  ) VALUES (
    '$run_id', 'staging', 'simulation', 'public-bayes-roc-v1',
    '2026-09-01T12:05:00Z'
  );
" >/dev/null
if psql_exec -c "UPDATE reputation_aggregation_run SET high_water_mark='2031-09-01T12:05:00Z' WHERE id='$run_id';" >/dev/null 2>&1; then
  echo "Reputation aggregation run high-water mark allowed mutation" >&2
  exit 1
fi
first_run_event=$(psql_exec -Atc "SELECT reputation_enqueue_aggregation_event(
  '$run_event_a', 'recalculation.requested', 102, 'service:mix-001',
  '$category_id', 1, 'public-bayes-roc-v1', '$run_correlation_id', '$run_id',
  '2026-09-01T12:05:00Z'
);")
second_run_event=$(psql_exec -Atc "SELECT reputation_enqueue_aggregation_event(
  '$run_event_b', 'recalculation.requested', 102, 'service:mix-001',
  '$category_id', 1, 'public-bayes-roc-v1', '$run_correlation_id', '$run_id',
  '2026-09-01T12:05:00Z'
);")
assert_equal "$first_run_event" "$run_event_a" "Run event identity"
assert_equal "$second_run_event" "$run_event_a" "Run/source semantic deduplication"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE run_id='$run_id';")" \
  "1" \
  "Run/source audit uniqueness"

psql_exec -c "
  UPDATE reputation_worker_control SET max_attempts=20 WHERE environment='staging';
  UPDATE reputation_aggregation_outbox SET attempt_count=19 WHERE id='$stable_event_id';
" >/dev/null
retry_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token || '|' || claimed_attempt FROM reputation_claim_aggregation_events('staging', 'worker-test-0001', 1, '2030-09-01T12:03:00Z');")
retry_event_id=$(printf '%s' "$retry_claim" | cut -d '|' -f 1)
retry_token=$(printf '%s' "$retry_claim" | cut -d '|' -f 2)
assert_equal "$(printf '%s' "$retry_claim" | cut -d '|' -f 3)" "20" "Maximum claim attempt"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_fail_aggregation_event('staging', '$retry_event_id', '$retry_token', 'synthetic_failure', '2030-09-01T12:03:01Z');")" \
  "dead_letter" \
  "Bounded retry dead letter"

psql_exec -c "SELECT reputation_requeue_dead_letter_event(
  '$retry_event_id', 103, 'Synthetic staging fault was corrected',
  '2030-09-01T12:04:00Z'
);" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT processing_status || ':' || attempt_count || ':' || (last_error_code IS NULL)::text FROM reputation_aggregation_outbox WHERE id='$retry_event_id';")" \
  "retry:0:true" \
  "Audited dead-letter retry-cycle reset"

run_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-run-0001', 1, '2030-09-01T12:04:01Z');")
run_claim_event_id=$(printf '%s' "$run_claim" | cut -d '|' -f 1)
run_claim_token=$(printf '%s' "$run_claim" | cut -d '|' -f 2)
assert_equal "$run_claim_event_id" "$run_event_a" "Bounded run claim"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$run_claim_event_id', '$run_claim_token', '2030-09-01T12:04:02Z');")" \
  "processed" \
  "Bounded run processing"
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count || ':' || source_event_id FROM reputation_aggregate_candidate WHERE subject_party_id=102 AND category_id='$category_id' AND context_key='service:mix-001';")" \
  "50.0000:0:$run_event_a" \
  "Run high-water evidence cutoff"

requeued_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token || '|' || claimed_attempt FROM reputation_claim_aggregation_events('staging', 'worker-requeue-0001', 1, '2030-09-01T12:04:03Z');")
requeued_event_id=$(printf '%s' "$requeued_claim" | cut -d '|' -f 1)
requeued_claim_token=$(printf '%s' "$requeued_claim" | cut -d '|' -f 2)
assert_equal "$requeued_event_id" "$retry_event_id" "Requeued event claim"
assert_equal "$(printf '%s' "$requeued_claim" | cut -d '|' -f 3)" "1" "Requeued claim attempt"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$requeued_event_id', '$requeued_claim_token', '2030-09-01T12:04:04Z');")" \
  "processed" \
  "Requeued event processing"

if psql_exec -c "UPDATE reputation_aggregation_event_action SET action='processed' WHERE event_id='$retry_event_id';" >/dev/null 2>&1; then
  echo "Reputation event action audit allowed mutation" >&2
  exit 1
fi

before_removed_rank=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE subject_party_id=103 AND category_id='$category_id';")
psql_exec -c "
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group, absolute_score
  ) VALUES ('$evaluation_id', '$category_id', 103, 2, 65);
  DELETE FROM reputation_evaluation_rank
  WHERE evaluation_id='$evaluation_id'
    AND category_id='$category_id'
    AND compared_party_id=103;
" >/dev/null
after_removed_rank=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE subject_party_id=103 AND category_id='$category_id';")
assert_equal "$((after_removed_rank - before_removed_rank))" "2" "Removed ordinal subject fan-out"

psql_exec -c "
  UPDATE reputation_category
  SET applicable_contexts=ARRAY['service'], version=version + 1
  WHERE id='$category_id';
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='category.applicability_changed' AND category_id='$category_id';")" \
  "6" \
  "Category applicability control event"

assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text || ':' || simulation_only::text || ':' || dead_letter_count FROM reputation_worker_health WHERE environment='staging';")" \
  "true:true:0" \
  "Staging worker health view"

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_worker_event_metrics WHERE context_kind='service';")" \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_worker_event_metrics;")" \
  "Non-identifying event metric context"
assert_equal \
  "$(psql_exec -Atc 'SELECT count(*) FROM reputation_worker_processing_metrics WHERE duration_seconds_p95 IS NOT NULL;')" \
  "3" \
  "Processing duration metrics"

psql_exec -c "UPDATE reputation_worker_control SET enabled=FALSE WHERE environment='staging';" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_claim_aggregation_events('staging', 'worker-test-rollback', 10, '2030-09-01T12:10:00Z');")" \
  "0" \
  "Disabled worker rollback gate"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE run_id='$run_id';")" \
  "1" \
  "Rollback gate evidence preservation"

simulation_candidate_count=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE publication_state='simulation';")
apply_file tdf-hq/sql/2026-09-06_contextual_reputation_staging_worker.sql
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE publication_state='simulation';")" \
  "$simulation_candidate_count" \
  "Migration rerun evidence preservation"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_run WHERE id='$run_id';")" \
  "1" \
  "Migration rerun audit-run preservation"

echo "Contextual reputation staging worker migration passed production gating, immutable formula and run cutoffs, run/source idempotency, immutable outbox evidence, leasing, simulation isolation, deterministic absolute and ordinal Bayesian aggregation, connected-component and tie handling, invalidation/restoration and category-control fan-out, bounded recoverable DLQ cycles, audited replay, non-identifying metrics, and rerun checks."
