#!/bin/sh
# COMMUNITY-ARTIST-SIMILARITY-001: the similarity job, the stored read and the
# reference on an isolated PostgreSQL database. Uses, in this order:
#   TDF_ARTIST_SIMILARITY_TEST_DATABASE_URL  an empty database the caller owns
#   Docker                                   a disposable pinned PostgreSQL
#   a local PostgreSQL 16 installation       a disposable cluster in a new directory
# No production connection input is accepted.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-artist-similarity-test-$$"
pg_bin="${TDF_ARTIST_SIMILARITY_PG_BIN:-/usr/local/opt/postgresql@16/bin}"
mode=""
temp_dir=""
cleanup() {
  if [ "$mode" = docker ]; then
    docker rm -f "$test_container" >/dev/null 2>&1 || true
  elif [ "$mode" = local ] && [ -n "$temp_dir" ]; then
    "$pg_bin/pg_ctl" -D "$temp_dir/data" -m immediate stop >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM

if [ -n "${TDF_ARTIST_SIMILARITY_TEST_DATABASE_URL:-}" ]; then
  mode=external
  url="$TDF_ARTIST_SIMILARITY_TEST_DATABASE_URL"
  existing=$(psql "$url" -X -At -v ON_ERROR_STOP=1 -c \
    "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p','v')")
  test "$existing" = 0 || { echo "Artist similarity tests require an empty isolated database" >&2; exit 1; }
elif command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
  mode=docker
  docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
    -e POSTGRES_PASSWORD=artist-similarity-fixture -e POSTGRES_DB=artist_similarity_test \
    postgres:16.10-alpine@sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297 >/dev/null
  port=$(docker port "$test_container" 5432/tcp | head -n 1 | sed 's/.*://')
  url="postgresql://postgres:artist-similarity-fixture@127.0.0.1:${port}/artist_similarity_test"
elif [ -x "$pg_bin/initdb" ] && [ -x "$pg_bin/pg_ctl" ]; then
  mode=local
  temp_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-artist-similarity.XXXXXX")
  port=$((56432 + ($$ % 1000)))
  "$pg_bin/initdb" -D "$temp_dir/data" -U postgres -A trust >/dev/null
  "$pg_bin/pg_ctl" -D "$temp_dir/data" -o "-p $port -c listen_addresses=127.0.0.1 -k $temp_dir" \
    -l "$temp_dir/postgres.log" -w start >/dev/null
  "$pg_bin/createdb" -h 127.0.0.1 -p "$port" -U postgres artist_similarity_test
  url="postgresql://postgres@127.0.0.1:${port}/artist_similarity_test"
else
  echo "No isolated PostgreSQL available (external URL, Docker or local PostgreSQL 16)" >&2
  exit 1
fi

attempt=0
until psql "$url" -X -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  test "$attempt" -lt 45 || { echo "Artist similarity test database did not become ready" >&2; exit 1; }
  sleep 1
done

psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$repo_root/scripts/artist-similarity-fixture.sql"
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$repo_root/tdf-hq/sql/2026-11-21_artist_similarity.sql"
# The migration is safe to apply twice.
psql "$url" -X -q -v ON_ERROR_STOP=1 -f "$repo_root/tdf-hq/sql/2026-11-21_artist_similarity.sql" 2>/dev/null

build_dir="$repo_root/.tmp/artist-similarity-probe"
mkdir -p "$build_dir"
(cd "$repo_root/tdf-hq" && stack exec -- ghc --make -O0 -threaded -isrc test/ArtistSimilarityProbe.hs \
  -outputdir "$build_dir" -o "$build_dir/artist-similarity-probe" >"$build_dir/build.log" 2>&1) \
  || { cat "$build_dir/build.log" >&2; exit 1; }
TDF_ARTIST_SIMILARITY_TEST_DATABASE_URL="$url" "$build_dir/artist-similarity-probe"
