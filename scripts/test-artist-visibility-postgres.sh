#!/bin/sh
# ART-PUBLIC-VISIBILITY-001: artist visibility SQL on a disposable Docker database.
# No production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-artist-visibility-test-$$"
owned=false
cleanup() {
  if [ "$owned" = true ]; then
    docker rm -f "$test_container" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=artist-visibility-fixture -e POSTGRES_DB=artist_visibility_test \
  postgres:16.10-alpine@sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297 >/dev/null
owned=true
port=$(docker port "$test_container" 5432/tcp | head -n 1 | sed 's/.*://')
url="postgresql://postgres:artist-visibility-fixture@127.0.0.1:${port}/artist_visibility_test"
attempt=0
until psql "$url" -X -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  test "$attempt" -lt 30 || { docker logs "$test_container" >&2; exit 1; }
  sleep 1
done
python3 "$repo_root/scripts/test-artist-visibility-sql.py" "$url"
