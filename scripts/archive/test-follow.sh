#!/usr/bin/env bash
set -euo pipefail
BASE=${API_BASE:-http://localhost:8080}
ARTIST_ID=${1:-1}
FOLLOWER=${2:-party-test}

echo "Testing follow endpoints against $BASE"

echo "Follow artist $ARTIST_ID as $FOLLOWER"
curl -s -X POST "$BASE/artists/$ARTIST_ID/follow" -H 'Content-Type: application/json' -d "{\"followerPartyId\": \"$FOLLOWER\"}" | jq -C || true

echo
sleep 1

echo "List followers"
curl -s "$BASE/artists/$ARTIST_ID/followers" | jq -C || true

echo
sleep 1

echo "Unfollow"
curl -s -X DELETE "$BASE/artists/$ARTIST_ID/follow?follower=$FOLLOWER" -w "\nHTTP_CODE:%{http_code}\n" -o /dev/stdout || true

echo
sleep 1

echo "List followers after unfollow"
curl -s "$BASE/artists/$ARTIST_ID/followers" | jq -C || true
