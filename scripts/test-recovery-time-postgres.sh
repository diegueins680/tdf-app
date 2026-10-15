#!/bin/sh
# Disposable Docker database; no production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probe_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-recovery-time.XXXXXXXX")
test_container="tdf-recovery-time-test-$$"
owned=false
cleanup() {
  if [ "$owned" = true ]; then
    docker rm -f "$test_container" >/dev/null 2>&1 || true
  fi
  # Retain SQL on failure for inspection; no workspace files are deleted.
}
trap cleanup EXIT INT TERM
python3 "$repo_root/scripts/recovery-time-boundaries.py" > "$probe_dir/probes.sql"
docker run --rm -d --name "$test_container" \
  -e POSTGRES_PASSWORD=recovery-time-fixture -e POSTGRES_DB=recovery_time_test \
  postgres:16.10-alpine@sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297 >/dev/null
owned=true
attempt=0
# The bootstrap server accepts Unix sockets, then stops before final startup.
# Query the same TCP endpoint used by the probes so bootstrap cannot look ready.
until docker exec -e PGPASSWORD=recovery-time-fixture "$test_container" \
  psql -X -v ON_ERROR_STOP=1 -h 127.0.0.1 -U postgres -d recovery_time_test \
  -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  test "$attempt" -lt 30 || { docker logs "$test_container" >&2; exit 1; }
  sleep 1
done
docker exec -i -e PGPASSWORD=recovery-time-fixture "$test_container" \
  psql -X -v ON_ERROR_STOP=1 -h 127.0.0.1 -U postgres -d recovery_time_test \
  < "$probe_dir/probes.sql"
echo "Finite PostgreSQL recovery-time probes passed; generated SQL: $probe_dir/probes.sql"
