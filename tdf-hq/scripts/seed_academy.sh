#!/usr/bin/env bash
set -euo pipefail
: "${APP_BASE_URL:=http://localhost:8080}"
TOKEN_HEADER="X-Seed-Token: ${SEED_TRIGGER_TOKEN:-tdf-bootstrap-seed}"
curl -s -X POST "$APP_BASE_URL/seed" -H "$TOKEN_HEADER" -d '{"academy":true}' | jq .
