#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
APP_DIR="$ROOT_DIR/tdf-hq"
CONFIG_FILE="$ROOT_DIR/fly.toml"

cd "$APP_DIR"

if ! command -v fly >/dev/null 2>&1; then
  echo "fly CLI is required but not found" >&2
  exit 1
fi

COMMIT_SHA=${SOURCE_COMMIT:-$(git rev-parse HEAD)}
export SOURCE_COMMIT="$COMMIT_SHA"
export GIT_SHA="$COMMIT_SHA"

echo "Deploying commit $COMMIT_SHA to Fly..." >&2
exec fly deploy \
  --config "$CONFIG_FILE" \
  --build-arg SOURCE_COMMIT="$COMMIT_SHA" \
  --env SOURCE_COMMIT="$COMMIT_SHA" \
  --env GIT_SHA="$COMMIT_SHA" \
  "$@"
