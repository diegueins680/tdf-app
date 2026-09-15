#!/usr/bin/env bash
set -euo pipefail
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
test_container="tdf-invitation-concurrency-$$"
cleanup() { docker rm -f "$test_container" >/dev/null 2>&1 || true; }
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=invitation-test -e POSTGRES_DB=invitation_test postgres:16-alpine >/dev/null
for attempt in $(seq 1 30); do
  if docker exec "$test_container" pg_isready -U postgres -d invitation_test >/dev/null 2>&1; then break; fi
  sleep 1
done
test_port=$(docker port "$test_container" 5432/tcp | sed 's/.*://')
export TDF_INVITATION_TEST_DATABASE_URL="host=127.0.0.1 port=$test_port user=postgres password=invitation-test dbname=invitation_test"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --test-arguments='--match=invitation'
