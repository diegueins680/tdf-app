#!/usr/bin/env bash
set -euo pipefail

# CI-friendly build helper for the web UI.
# Tries npm ci first (fast, reproducible), and falls back to npm install when the lockfile
# is missing or out of sync — useful for Cloudflare/Pages environments that always call npm ci.

cd "$(dirname "${BASH_SOURCE[0]}")/../tdf-hq-ui"

if ! npm ci --include=dev --progress=false --no-audit; then
  echo "npm ci failed, falling back to npm install..."
  npm install --include=dev --progress=false --no-audit
fi

npm run build
