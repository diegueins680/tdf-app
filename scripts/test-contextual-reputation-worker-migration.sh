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
psql_exec -c 'INSERT INTO party(id) VALUES (101), (102), (103), (104), (105), (106), (107);' >/dev/null
psql_exec -c "
  CREATE TABLE security_role (
    id UUID PRIMARY KEY,
    code TEXT NOT NULL UNIQUE,
    active BOOLEAN NOT NULL DEFAULT TRUE
  );
  CREATE TABLE party_security_role (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
    role_id UUID NOT NULL REFERENCES security_role(id) ON DELETE RESTRICT,
    active BOOLEAN NOT NULL DEFAULT TRUE,
    UNIQUE(party_id, role_id)
  );
  INSERT INTO security_role(id, code) VALUES
    ('c5000000-0000-4000-8000-000000000001', 'customer'),
    ('c5000000-0000-4000-8000-000000000002', 'vendor');
  INSERT INTO party_security_role(party_id, role_id) VALUES
    (104, 'c5000000-0000-4000-8000-000000000001'),
    (105, 'c5000000-0000-4000-8000-000000000002');
" >/dev/null

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

draft_formula_id="draft-run-freeze-test-v1"
draft_run_id="c5100000-0000-4000-8000-000000000001"
psql_exec -c "
  INSERT INTO reputation_formula_version(
    id, public_parameters, preference_parameters, status
  ) VALUES (
    '$draft_formula_id',
    '{\"priorStrength\":8,\"priorMean\":50,\"minimumVerifiedRatings\":3,\"halfLifeDays\":365,\"perEvaluatorCap\":0.25}',
    '{\"method\":\"rank-order-centroid\",\"scale\":100}',
    'draft'
  );
  UPDATE reputation_formula_version
  SET public_parameters=public_parameters || '{\"priorMean\":51}'::jsonb
  WHERE id='$draft_formula_id';
  INSERT INTO reputation_aggregation_run(
    id, environment, run_kind, formula_version_id, high_water_mark
  ) VALUES (
    '$draft_run_id', 'staging', 'simulation', '$draft_formula_id',
    '2030-09-01T00:00:00Z'
  );
" >/dev/null
if psql_exec -c "
  UPDATE reputation_formula_version
  SET public_parameters=public_parameters || '{\"priorMean\":52}'::jsonb
  WHERE id='$draft_formula_id';
" >/dev/null 2>&1; then
  echo "Run-referenced draft formula parameters allowed in-place mutation" >&2
  exit 1
fi

psql_exec -c "UPDATE reputation_worker_control SET enabled=TRUE WHERE environment='staging';" >/dev/null
if psql_exec -c "UPDATE reputation_worker_control SET enabled=TRUE WHERE environment='test';" >/dev/null 2>&1; then
  echo "More than one reputation worker environment could be enabled" >&2
  exit 1
fi

category_id=$(psql_exec -Atc "SELECT id FROM reputation_category WHERE slug='quality';")
role_category_id="c5200000-0000-4000-8000-000000000003"
psql_exec -c "
  INSERT INTO reputation_category(
    id, slug, name_es, name_en, applicable_roles, applicable_contexts,
    default_position
  ) VALUES (
    '$role_category_id', 'role-specific-test', 'Rol específico', 'Role specific',
    ARRAY['vendor'], ARRAY['service'], 9
  );
" >/dev/null
role_interaction_id="c5200000-0000-4000-8000-000000000001"
role_evaluation_id="c5200000-0000-4000-8000-000000000002"
psql_exec -c "
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES (
    '$role_interaction_id', 'service', 'role-001', 101, 104,
    '2030-08-01T12:00:00Z', '2030-08-01T12:00:00Z',
    'eligible', 'test_fixture', 'role-001'
  );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES (
    '$role_evaluation_id', '$role_interaction_id', 101, 104, 'a_to_b',
    'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
  );
  INSERT INTO reputation_evaluation_category(
    evaluation_id, category_id, position, weight
  ) VALUES ('$role_evaluation_id', '$role_category_id', 1, 100);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group, absolute_score
  ) VALUES
    ('$role_evaluation_id', '$role_category_id', 104, 1, 90),
    ('$role_evaluation_id', '$role_category_id', 105, 2, 60);
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-08-01T12:01:00Z'
  WHERE id='$role_evaluation_id';
  DO \$\$
  DECLARE
    claimed RECORD;
  BEGIN
    FOR claimed IN
      SELECT *
      FROM reputation_claim_aggregation_events(
        'staging', 'worker-role-0001', 10, '2030-08-01T12:02:00Z'
      )
    LOOP
      PERFORM reputation_complete_aggregation_event(
        claimed.event_id::uuid, claimed.claim_token::uuid,
        '2030-08-01T12:02:01Z'
      );
    END LOOP;
  END \$\$;
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count FROM reputation_aggregate_candidate WHERE subject_party_id=104 AND category_id='$role_category_id' AND context_key='service:role-001';")" \
  "50.0000:0" \
  "Inapplicable subject role evidence suppression"
assert_equal \
  "$(psql_exec -Atc "SELECT observation_count FROM reputation_aggregate_candidate WHERE subject_party_id=105 AND category_id='$role_category_id' AND context_key='service:role-001';")" \
  "1" \
  "Applicable subject role evidence acceptance"

pair_category_a_id="c5210000-0000-4000-8000-000000000001"
pair_category_b_id="c5210000-0000-4000-8000-000000000002"
pair_interaction_id="c5210000-0000-4000-8000-000000000003"
pair_evaluation_id="c5210000-0000-4000-8000-000000000004"
psql_exec -c "
  INSERT INTO reputation_category(
    id, slug, name_es, name_en, applicable_contexts, default_position
  ) VALUES
    (
      '$pair_category_a_id', 'pair-category-a-test',
      'Categoría A', 'Category A', ARRAY['service'], 10
    ),
    (
      '$pair_category_b_id', 'pair-category-b-test',
      'Categoría B', 'Category B', ARRAY['service'], 11
    );
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES (
    '$pair_interaction_id', 'service', 'pair-001', 101, 102,
    '2030-08-01T12:03:00Z', '2030-08-01T12:03:00Z',
    'eligible', 'test_fixture', 'pair-001'
  );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES (
    '$pair_evaluation_id', '$pair_interaction_id', 101, 102, 'a_to_b',
    'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
  );
  INSERT INTO reputation_evaluation_category(
    evaluation_id, category_id, position, weight
  ) VALUES
    ('$pair_evaluation_id', '$pair_category_a_id', 1, 50),
    ('$pair_evaluation_id', '$pair_category_b_id', 2, 50);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group,
    absolute_score
  ) VALUES
    ('$pair_evaluation_id', '$pair_category_a_id', 103, 1, 80),
    ('$pair_evaluation_id', '$pair_category_b_id', 104, 1, 70);
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-08-01T12:04:00Z'
  WHERE id='$pair_evaluation_id';
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE context_key='service:pair-001';")" \
  "4" \
  "Evaluation subject/category pair fan-out"
assert_equal \
  "$(psql_exec -Atc "
    SELECT count(*)
    FROM reputation_aggregation_outbox
    WHERE context_key='service:pair-001'
      AND (
        (subject_party_id=103 AND category_id='$pair_category_b_id')
        OR (subject_party_id=104 AND category_id='$pair_category_a_id')
      );
  ")" \
  "0" \
  "Compared-subject category cross-product suppression"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE context_key='service:pair-001' AND subject_party_id=102;")" \
  "2" \
  "Evaluation subject selected-category preservation"
while :; do
  pair_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-pair-0001', 1, '2030-08-01T12:05:00Z');")
  [ -n "$pair_claim" ] || break
  pair_event_id=$(printf '%s' "$pair_claim" | cut -d '|' -f 1)
  pair_claim_token=$(printf '%s' "$pair_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$pair_event_id', '$pair_claim_token', '2030-08-01T12:05:01Z');")" \
    "processed" \
    "Subject/category pair follow-up processing"
done

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
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE context_key='service:mix-001';")" \
  "0" \
  "Draft evaluation outbox isolation"

psql_exec -c "
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2030-09-01T12:00:00Z'
  WHERE id='$evaluation_id';
" >/dev/null

assert_equal \
  "$(psql_exec -Atc "SELECT event_type || ':' || subject_party_id || ':' || context_key FROM reputation_aggregation_outbox WHERE context_key='service:mix-001';")" \
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
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='interaction.restored' AND context_key='service:mix-001';")" \
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

psql_exec -c "
  DO \$\$
  DECLARE
    fixture_index INTEGER;
    fixture_evaluator BIGINT;
    fixture_interaction UUID;
    fixture_evaluation UUID;
  BEGIN
    FOR fixture_index IN 1..7 LOOP
      fixture_evaluator := CASE
        WHEN fixture_index <= 4 THEN 101
        ELSE 100 + fixture_index
      END;
      fixture_interaction := gen_random_uuid();
      fixture_evaluation := gen_random_uuid();
      INSERT INTO reputation_interaction(
        id, context_kind, context_id, party_a_id, party_b_id, completed_at,
        verified_at, status, source_kind, source_id
      ) VALUES (
        fixture_interaction, 'service', 'cap-001', fixture_evaluator, 102,
        '2030-09-01T12:01:30Z', '2030-09-01T12:01:30Z',
        'eligible', 'test_fixture', 'cap-001-' || fixture_index::text
      );
      INSERT INTO reputation_evaluation(
        id, interaction_id, evaluator_party_id, subject_party_id, direction,
        status, formula_version_id, revision, edit_deadline
      ) VALUES (
        fixture_evaluation, fixture_interaction, fixture_evaluator, 102, 'a_to_b',
        'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
      );
      INSERT INTO reputation_evaluation_category(
        evaluation_id, category_id, position, weight
      ) VALUES (fixture_evaluation, '$category_id', 1, 100);
      INSERT INTO reputation_evaluation_rank(
        evaluation_id, category_id, compared_party_id, position_group, absolute_score
      ) VALUES (
        fixture_evaluation, '$category_id', 102, 1,
        CASE WHEN fixture_index <= 4 THEN 100 ELSE 0 END
      );
      UPDATE reputation_evaluation
      SET status='submitted', submitted_at='2030-09-01T12:01:30Z'
      WHERE id=fixture_evaluation;
    END LOOP;
  END
  \$\$;
" >/dev/null

cap_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-cap-0001', 1, '2030-09-01T12:01:40Z');")
cap_event_id=$(printf '%s' "$cap_claim" | cut -d '|' -f 1)
cap_claim_token=$(printf '%s' "$cap_claim" | cut -d '|' -f 2)
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$cap_event_id', '$cap_claim_token', '2030-09-01T12:01:41Z');")" \
  "processed" \
  "Adjusted-total evaluator cap processing"
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || verified_interaction_count || ':' || distinct_evaluator_count || ':' || observation_count || ':' || confidence FROM reputation_aggregate_candidate WHERE subject_party_id=102 AND category_id='$category_id' AND context_key='service:cap-001';")" \
  "41.6667:7:4:7:low" \
  "Adjusted-total evaluator cap candidate"
assert_equal \
  "$(psql_exec -Atc "SELECT ((metadata->>'maxEvaluatorShare')::numeric <= 0.250001)::text FROM reputation_aggregation_event_action WHERE event_id='$cap_event_id' AND action='processed';")" \
  "true" \
  "Adjusted evaluator contribution share"

while :; do
  cap_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-cap-0001', 1, '2030-09-01T12:01:42Z');")
  [ -n "$cap_claim" ] || break
  cap_event_id=$(printf '%s' "$cap_claim" | cut -d '|' -f 1)
  cap_claim_token=$(printf '%s' "$cap_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$cap_event_id', '$cap_claim_token', '2030-09-01T12:01:43Z');")" \
    "processed" \
    "Evaluator-cap follow-up event processing"
done

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

psql_exec -c "DELETE FROM reputation_evaluation WHERE id='$tie_evaluation_id';" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='evaluation.erased_or_anonymized' AND context_key='service:tie-001';")" \
  "2" \
  "Pre-cascade evaluation deletion fan-out"
while :; do
  erase_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-erase-0001', 1, '2030-09-01T12:02:54Z');")
  [ -n "$erase_claim" ] || break
  erase_event_id=$(printf '%s' "$erase_claim" | cut -d '|' -f 1)
  erase_claim_token=$(printf '%s' "$erase_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$erase_event_id', '$erase_claim_token', '2030-09-01T12:02:55Z');")" \
    "processed" \
    "Evaluation deletion invalidation processing"
done
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE category_id='$category_id' AND context_key='service:tie-001' AND score=50.0000 AND observation_count=0;")" \
  "2" \
  "Deleted evaluation evidence removal"

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
    '2027-09-01T12:05:00Z'
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

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_claim_aggregation_events('staging', 'worker-run-0001', 1, '2030-09-01T12:03:59Z');")" \
  "0" \
  "Planned run claim suppression"
psql_exec -c "
  UPDATE reputation_aggregation_run
  SET status='running', started_at='2026-09-01T12:05:00Z'
  WHERE id='$run_id';
" >/dev/null
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
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count || ':' || source_event_id FROM reputation_aggregation_run_result WHERE run_id='$run_id' AND subject_party_id=102 AND category_id='$category_id' AND context_key='service:mix-001';")" \
  "50.0000:0:$run_event_a" \
  "Immutable bounded-run result snapshot"
if psql_exec -c "UPDATE reputation_aggregation_run_result SET score=60 WHERE run_id='$run_id';" >/dev/null 2>&1; then
  echo "Reputation aggregation run result allowed mutation" >&2
  exit 1
fi

cancelled_run_id="c6100000-0000-4000-8000-000000000001"
cancelled_event_id="c6100000-0000-4000-8000-000000000002"
cancelled_correlation_id="c6100000-0000-4000-8000-000000000003"
psql_exec -c "
  INSERT INTO reputation_aggregation_run(
    id, environment, run_kind, formula_version_id, status, high_water_mark,
    started_at
  ) VALUES (
    '$cancelled_run_id', 'staging', 'simulation', 'public-bayes-roc-v1',
    'running', '2030-09-01T12:04:04Z', '2030-09-01T12:04:04Z'
  );
  SELECT reputation_enqueue_aggregation_event(
    '$cancelled_event_id', 'recalculation.requested', 103,
    'service:cancelled-run-001', '$category_id', 1,
    'public-bayes-roc-v1', '$cancelled_correlation_id', '$cancelled_run_id',
    '2030-09-01T12:04:04Z'
  );
" >/dev/null
cancelled_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-cancelled-0001', 1, '2030-09-01T12:04:05Z');")
cancelled_claim_event_id=$(printf '%s' "$cancelled_claim" | cut -d '|' -f 1)
cancelled_claim_token=$(printf '%s' "$cancelled_claim" | cut -d '|' -f 2)
assert_equal "$cancelled_claim_event_id" "$cancelled_event_id" "Running run claim before cancellation"
psql_exec -c "
  UPDATE reputation_aggregation_run
  SET status='cancelled', completed_at='2030-09-01T12:04:06Z'
  WHERE id='$cancelled_run_id';
" >/dev/null
if psql_exec -c "SELECT reputation_complete_aggregation_event('$cancelled_claim_event_id', '$cancelled_claim_token', '2030-09-01T12:04:07Z');" >/dev/null 2>&1; then
  echo "Cancelled run allowed an already-claimed event to complete" >&2
  exit 1
fi
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_fail_aggregation_event('staging', '$cancelled_claim_event_id', '$cancelled_claim_token', 'run_cancelled', '2030-09-01T12:04:08Z');")" \
  "retry" \
  "Cancelled run claim recovery"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE source_event_id='$cancelled_event_id';")" \
  "0" \
  "Cancelled run candidate suppression"
if psql_exec -c "
  UPDATE reputation_aggregation_run
  SET status='running', completed_at=NULL
  WHERE id='$cancelled_run_id';
" >/dev/null 2>&1; then
  echo "Terminal reputation aggregation run could be reopened" >&2
  exit 1
fi

expired_event_id="c6300000-0000-4000-8000-000000000001"
expired_correlation_id="c6300000-0000-4000-8000-000000000002"
expired_lease_token="c6300000-0000-4000-8000-000000000003"
psql_exec -c "
  SELECT reputation_enqueue_aggregation_event(
    '$expired_event_id', 'recalculation.requested', 102, 'service:mix-001',
    '$category_id', 2, 'public-bayes-roc-v1', '$expired_correlation_id', NULL,
    '2030-09-01T12:04:05Z'
  );
  UPDATE reputation_aggregation_outbox
  SET processing_status='processing', attempt_count=20,
      lease_token='$expired_lease_token', lease_owner_hash=repeat('a', 64),
      lease_expires_at='2030-09-01T12:04:05Z'
  WHERE id='$expired_event_id';
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_claim_aggregation_events('staging', 'worker-expired-0001', 1, '2030-09-01T12:04:06Z');")" \
  "0" \
  "Expired final lease claim suppression"
assert_equal \
  "$(psql_exec -Atc "SELECT processing_status || ':' || attempt_count || ':' || last_error_code FROM reputation_aggregation_outbox WHERE id='$expired_event_id';")" \
  "dead_letter:20:lease_expired_at_attempt_limit" \
  "Expired final lease dead letter"
psql_exec -c "SELECT reputation_requeue_dead_letter_event(
  '$expired_event_id', 103, 'Expired final lease was inspected and approved',
  '2030-09-01T12:04:07Z'
);" >/dev/null
expired_requeue_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token || '|' || claimed_attempt FROM reputation_claim_aggregation_events('staging', 'worker-expired-0001', 1, '2030-09-01T12:04:08Z');")
expired_requeue_event_id=$(printf '%s' "$expired_requeue_claim" | cut -d '|' -f 1)
expired_requeue_token=$(printf '%s' "$expired_requeue_claim" | cut -d '|' -f 2)
assert_equal "$expired_requeue_event_id" "$expired_event_id" "Expired lease requeue claim"
assert_equal "$(printf '%s' "$expired_requeue_claim" | cut -d '|' -f 3)" "1" "Expired lease new retry cycle"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$expired_requeue_event_id', '$expired_requeue_token', '2030-09-01T12:04:09Z');")" \
  "processed" \
  "Expired lease recovery processing"

fence_interaction_id="c6400000-0000-4000-8000-000000000001"
fence_evaluation_id="c6400000-0000-4000-8000-000000000002"
fence_run_id="c6400000-0000-4000-8000-000000000003"
fence_event_id="c6400000-0000-4000-8000-000000000004"
fence_correlation_id="c6400000-0000-4000-8000-000000000005"
psql_exec -c "
  INSERT INTO reputation_interaction(
    id, context_kind, context_id, party_a_id, party_b_id, completed_at,
    verified_at, status, source_kind, source_id
  ) VALUES (
    '$fence_interaction_id', 'service', 'fence-001', 101, 102,
    '2026-09-04T12:00:00Z', '2026-09-04T12:00:00Z',
    'eligible', 'test_fixture', 'fence-001'
  );
  INSERT INTO reputation_evaluation(
    id, interaction_id, evaluator_party_id, subject_party_id, direction,
    status, formula_version_id, revision, edit_deadline
  ) VALUES (
    '$fence_evaluation_id', '$fence_interaction_id', 101, 102, 'a_to_b',
    'draft', 'public-bayes-roc-v1', 1, '2030-10-01T00:00:00Z'
  );
  INSERT INTO reputation_evaluation_category(evaluation_id, category_id, position, weight)
  VALUES ('$fence_evaluation_id', '$category_id', 1, 100);
  INSERT INTO reputation_evaluation_rank(
    evaluation_id, category_id, compared_party_id, position_group, absolute_score
  ) VALUES ('$fence_evaluation_id', '$category_id', 102, 1, 70);
  UPDATE reputation_evaluation
  SET status='submitted', submitted_at='2026-09-05T12:00:00Z'
  WHERE id='$fence_evaluation_id';
  INSERT INTO reputation_aggregation_run(
    id, environment, run_kind, formula_version_id, status, high_water_mark,
    started_at
  ) VALUES (
    '$fence_run_id', 'staging', 'backfill', 'public-bayes-roc-v1', 'running',
    '2026-09-06T00:00:00Z', '2026-09-06T00:00:00Z'
  );
  SELECT reputation_enqueue_aggregation_event(
    '$fence_event_id', 'recalculation.requested', 102, 'service:fence-001',
    '$category_id', 1, 'public-bayes-roc-v1', '$fence_correlation_id',
    '$fence_run_id', '2026-09-06T00:00:00Z'
  );
  UPDATE reputation_evaluation_rank
  SET absolute_score=90
  WHERE evaluation_id='$fence_evaluation_id'
    AND category_id='$category_id'
    AND compared_party_id=102;
  UPDATE reputation_worker_control SET max_attempts=1 WHERE environment='staging';
" >/dev/null
fence_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-fence-0001', 1, '2030-09-01T12:04:10Z');")
fence_claim_event_id=$(printf '%s' "$fence_claim" | cut -d '|' -f 1)
fence_claim_token=$(printf '%s' "$fence_claim" | cut -d '|' -f 2)
assert_equal "$fence_claim_event_id" "$fence_event_id" "High-water fenced run claim"
if psql_exec -c "SELECT reputation_complete_aggregation_event('$fence_claim_event_id', '$fence_claim_token', '2030-09-01T12:04:11Z');" >/dev/null 2>&1; then
  echo "Bounded run accepted a post-cutoff evidence edit" >&2
  exit 1
fi
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_fail_aggregation_event('staging', '$fence_claim_event_id', '$fence_claim_token', 'source_fence_violated', '2030-09-01T12:04:12Z');")" \
  "dead_letter" \
  "High-water fence failure audit"
psql_exec -c "UPDATE reputation_worker_control SET max_attempts=20 WHERE environment='staging';" >/dev/null

while :; do
  fence_followup_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-fence-0001', 1, '2030-09-01T12:04:13Z');")
  [ -n "$fence_followup_claim" ] || break
  fence_followup_event_id=$(printf '%s' "$fence_followup_claim" | cut -d '|' -f 1)
  fence_followup_token=$(printf '%s' "$fence_followup_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$fence_followup_event_id', '$fence_followup_token', '2030-09-01T12:04:14Z');")" \
    "processed" \
    "Post-fence canonical event processing"
done
assert_equal \
  "$(psql_exec -Atc "SELECT processing_status || ':' || last_error_code FROM reputation_aggregation_outbox WHERE id='$fence_event_id';")" \
  "dead_letter:source_fence_violated" \
  "High-water fenced run dead letter"

role_fence_run_id="c6410000-0000-4000-8000-000000000001"
role_fence_event_id="c6410000-0000-4000-8000-000000000002"
role_fence_correlation_id="c6410000-0000-4000-8000-000000000003"
psql_exec -c "
  INSERT INTO reputation_aggregation_run(
    id, environment, run_kind, formula_version_id, status, high_water_mark,
    started_at
  ) VALUES (
    '$role_fence_run_id', 'staging', 'backfill', 'public-bayes-roc-v1',
    'running', now() - interval '1 minute', now() - interval '1 minute'
  );
  SELECT reputation_enqueue_aggregation_event(
    '$role_fence_event_id', 'recalculation.requested', 105,
    'service:role-001', '$role_category_id', 1,
    'public-bayes-roc-v1', '$role_fence_correlation_id',
    '$role_fence_run_id', now() - interval '1 minute'
  );
  UPDATE party_security_role
  SET active=FALSE
  WHERE party_id=105
    AND role_id='c5000000-0000-4000-8000-000000000002';
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='subject.role_changed' AND subject_party_id=105 AND category_id='$role_category_id' AND context_key='service:role-001';")" \
  "1" \
  "Subject role mutation event"
role_fence_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-role-fence-0001', 1, '2030-09-01T12:04:15Z');")
role_fence_claim_event_id=$(printf '%s' "$role_fence_claim" | cut -d '|' -f 1)
role_fence_claim_token=$(printf '%s' "$role_fence_claim" | cut -d '|' -f 2)
assert_equal "$role_fence_claim_event_id" "$role_fence_event_id" "Role-fenced run claim"
if psql_exec -c "SELECT reputation_complete_aggregation_event('$role_fence_claim_event_id', '$role_fence_claim_token', '2030-09-01T12:04:16Z');" >/dev/null 2>&1; then
  echo "Bounded run accepted a post-cutoff subject role change" >&2
  exit 1
fi
psql_exec -c "
  UPDATE reputation_aggregation_run
  SET status='cancelled', completed_at='2030-09-01T12:04:17Z'
  WHERE id='$role_fence_run_id';
" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_fail_aggregation_event('staging', '$role_fence_claim_event_id', '$role_fence_claim_token', 'source_fence_violated', '2030-09-01T12:04:18Z');")" \
  "retry" \
  "Role-fenced run failure audit"
psql_exec -c "
  UPDATE party_security_role
  SET active=TRUE
  WHERE party_id=105
    AND role_id='c5000000-0000-4000-8000-000000000002';
" >/dev/null
while :; do
  role_change_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-role-change-0001', 1, '2030-09-01T12:04:19Z');")
  [ -n "$role_change_claim" ] || break
  role_change_event_id=$(printf '%s' "$role_change_claim" | cut -d '|' -f 1)
  role_change_token=$(printf '%s' "$role_change_claim" | cut -d '|' -f 2)
  assert_equal \
    "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$role_change_event_id', '$role_change_token', '2030-09-01T12:04:20Z');")" \
    "processed" \
    "Subject role mutation recalculation"
done

metrics_cycle_event_id="c6500000-0000-4000-8000-000000000001"
metrics_cycle_correlation_id="c6500000-0000-4000-8000-000000000002"
metrics_cycle_formula_id="metrics-cycle-test-v1"
psql_exec -c "
  INSERT INTO reputation_formula_version(
    id, public_parameters, preference_parameters, status
  ) VALUES (
    '$metrics_cycle_formula_id',
    '{\"priorStrength\":8,\"priorMean\":50,\"minimumVerifiedRatings\":3,\"halfLifeDays\":365,\"perEvaluatorCap\":0.25}',
    '{\"method\":\"rank-order-centroid\",\"scale\":100}',
    'draft'
  );
  UPDATE reputation_worker_control SET max_attempts=1 WHERE environment='staging';
  SELECT reputation_enqueue_aggregation_event(
    '$metrics_cycle_event_id', 'recalculation.requested', 103,
    'service:metrics-cycle-001', '$category_id', 1, '$metrics_cycle_formula_id',
    '$metrics_cycle_correlation_id', NULL, '2030-09-01T12:04:21Z'
  );
" >/dev/null
metrics_cycle_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-metrics-cycle-0001', 1, '2030-09-01T12:04:22Z');")
metrics_cycle_claim_event_id=$(printf '%s' "$metrics_cycle_claim" | cut -d '|' -f 1)
metrics_cycle_claim_token=$(printf '%s' "$metrics_cycle_claim" | cut -d '|' -f 2)
assert_equal "$metrics_cycle_claim_event_id" "$metrics_cycle_event_id" "Initial metrics retry-cycle claim"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_fail_aggregation_event('staging', '$metrics_cycle_claim_event_id', '$metrics_cycle_claim_token', 'synthetic_cycle_failure', '2030-09-01T12:04:32Z');")" \
  "dead_letter" \
  "Initial metrics retry-cycle dead letter"
psql_exec -c "SELECT reputation_requeue_dead_letter_event(
  '$metrics_cycle_event_id', 103, 'Synthetic metrics retry cycle was approved',
  '2030-09-01T12:05:00Z'
);" >/dev/null
metrics_cycle_claim=$(psql_exec -Atc "SELECT event_id || '|' || claim_token FROM reputation_claim_aggregation_events('staging', 'worker-metrics-cycle-0001', 1, '2030-09-01T12:05:01Z');")
metrics_cycle_claim_event_id=$(printf '%s' "$metrics_cycle_claim" | cut -d '|' -f 1)
metrics_cycle_claim_token=$(printf '%s' "$metrics_cycle_claim" | cut -d '|' -f 2)
assert_equal "$metrics_cycle_claim_event_id" "$metrics_cycle_event_id" "Requeued metrics retry-cycle claim"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_complete_aggregation_event('$metrics_cycle_claim_event_id', '$metrics_cycle_claim_token', '2030-09-01T12:05:11Z');")" \
  "processed" \
  "Requeued metrics retry-cycle processing"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_event_action WHERE event_id='$metrics_cycle_event_id' AND action='claimed' AND attempt_count=1;")" \
  "2" \
  "Repeated attempt number across retry cycles"
assert_equal \
  "$(psql_exec -Atc "SELECT completed_count || ':' || (duration_seconds_p95 = 10)::text FROM reputation_worker_processing_metrics WHERE algorithm_version='$metrics_cycle_formula_id' AND context_kind='service';")" \
  "2:true" \
  "Retry-cycle-separated processing metrics"
psql_exec -c "UPDATE reputation_worker_control SET max_attempts=20 WHERE environment='staging';" >/dev/null

health_queue_before=$(psql_exec -Atc "SELECT queue_depth FROM reputation_worker_health WHERE environment='staging';")
health_planned_event_id="c6510000-0000-4000-8000-000000000001"
health_planned_correlation_id="c6510000-0000-4000-8000-000000000002"
psql_exec -c "SELECT reputation_enqueue_aggregation_event(
  '$health_planned_event_id', 'recalculation.requested', 103,
  'service:planned-health-001', '$category_id', 1, '$draft_formula_id',
  '$health_planned_correlation_id', '$draft_run_id',
  '2030-09-01T12:05:12Z'
);" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT queue_depth FROM reputation_worker_health WHERE environment='staging';")" \
  "$health_queue_before" \
  "Unclaimable planned-run health exclusion"

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
  "9" \
  "Category applicability control event"

assert_equal \
  "$(psql_exec -Atc "SELECT enabled::text || ':' || simulation_only::text || ':' || dead_letter_count FROM reputation_worker_health WHERE environment='staging';")" \
  "true:true:1" \
  "Staging worker health view"

assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_worker_event_metrics WHERE context_kind='service';")" \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_worker_event_metrics;")" \
  "Non-identifying event metric context"
assert_equal \
  "$(psql_exec -Atc 'SELECT (count(*) >= 3)::text FROM reputation_worker_processing_metrics WHERE duration_seconds_p95 IS NOT NULL;')" \
  "true" \
  "Processing duration metrics"

retired_formula_id="retired-decay-test-v1"
retired_context_key="service:retired-decay-001"
psql_exec -c "
  UPDATE reputation_category
  SET status='archived', version=version + 1
  WHERE id='$role_category_id';
  INSERT INTO reputation_formula_version(
    id, public_parameters, preference_parameters, status
  ) VALUES (
    '$retired_formula_id',
    '{\"priorStrength\":8,\"priorMean\":50,\"minimumVerifiedRatings\":3,\"halfLifeDays\":365,\"perEvaluatorCap\":0.25}',
    '{\"method\":\"rank-order-centroid\",\"scale\":100}',
    'retired'
  );
  INSERT INTO reputation_aggregate_candidate(
    subject_party_id, category_id, context_key, formula_version_id,
    score, lower_bound, upper_bound, verified_interaction_count,
    distinct_evaluator_count, observation_count, confidence,
    publication_state, source_event_id, calculated_at
  ) VALUES (
    104, '$category_id', '$retired_context_key', '$retired_formula_id',
    50, 25, 75, 0, 0, 0, 'forming', 'simulation',
    '$stable_event_id', '2029-09-01T00:00:00Z'
  );
" >/dev/null
simulation_candidate_count=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE publication_state='simulation';")
processable_candidate_count=$(psql_exec -Atc "
  SELECT count(*)
  FROM reputation_aggregate_candidate candidate
  JOIN reputation_formula_version formula
    ON formula.id=candidate.formula_version_id
  JOIN reputation_category category
    ON category.id=candidate.category_id
  WHERE candidate.publication_state='simulation'
    AND formula.status IN ('active', 'draft')
    AND category.status='active';
")
decay_event_count_before=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='recalculation.requested';")
decay_scheduled_count=$(psql_exec -Atc "SELECT reputation_schedule_decay_recalculations('staging', 100, '2030-09-02T12:00:00Z');")
assert_equal "$decay_scheduled_count" "$processable_candidate_count" "Daily decay recalculation schedule"
decay_event_count_after=$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='recalculation.requested';")
assert_equal \
  "$((decay_event_count_after - decay_event_count_before))" \
  "$decay_scheduled_count" \
  "Daily decay event fan-out"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='recalculation.requested' AND algorithm_version='$retired_formula_id' AND context_key='$retired_context_key';")" \
  "0" \
  "Retired formula decay suppression"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE event_type='recalculation.requested' AND category_id='$role_category_id' AND context_key='service:role-001' AND occurred_at='2030-09-02T00:00:00Z';")" \
  "0" \
  "Inactive category decay suppression"
assert_equal \
  "$(psql_exec -Atc "SELECT reputation_schedule_decay_recalculations('staging', 100, '2030-09-02T23:59:59Z');")" \
  "0" \
  "Daily decay schedule idempotency"

psql_exec -c "UPDATE reputation_worker_control SET enabled=FALSE WHERE environment='staging';" >/dev/null
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_claim_aggregation_events('staging', 'worker-test-rollback', 10, '2030-09-01T12:10:00Z');")" \
  "0" \
  "Disabled worker rollback gate"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_outbox WHERE run_id='$run_id';")" \
  "1" \
  "Rollback gate evidence preservation"

apply_file tdf-hq/sql/2026-09-06_contextual_reputation_staging_worker.sql
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregate_candidate WHERE publication_state='simulation';")" \
  "$simulation_candidate_count" \
  "Migration rerun evidence preservation"
assert_equal \
  "$(psql_exec -Atc "SELECT count(*) FROM reputation_aggregation_run WHERE id='$run_id';")" \
  "1" \
  "Migration rerun audit-run preservation"
assert_equal \
  "$(psql_exec -Atc "SELECT score || ':' || observation_count || ':' || source_event_id FROM reputation_aggregation_run_result WHERE run_id='$run_id' AND subject_party_id=102 AND category_id='$category_id' AND context_key='service:mix-001';")" \
  "50.0000:0:$run_event_a" \
  "Migration rerun run-result preservation"

echo "Contextual reputation staging worker migration passed production gating, immutable and run-frozen formula parameters, evidence and role-mutation fenced run cutoffs, irreversible run lifecycle and claim/completion gating, run/source idempotency, immutable per-run results and outbox evidence, exact subject/category fan-out, leasing and expired-lease limits, simulation isolation, role-applicable adjusted-share-capped absolute and ordinal Bayesian aggregation, connected-component and tie handling, deletion/invalidation/restoration and category-control fan-out, active-formula/category periodic decay scheduling, bounded recoverable DLQ cycles, audited replay, claimable-work health filtering, retry-cycle-separated non-identifying metrics, and rerun checks."
