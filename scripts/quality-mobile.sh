#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

run_npm() {
  env \
    -u npm_config__jsr_registry \
    -u npm_config_npm_globalconfig \
    -u npm_config_verify_deps_before_run \
    -u pnpm_config_verify_deps_before_run \
    npm "$@"
}

if node "$ROOT/scripts/mobile-workspace-ready.mjs" --quiet; then
  echo "▶ Linting, type-checking and testing tdf-mobile"
  run_npm --prefix "$ROOT/tdf-mobile" run lint
  run_npm --prefix "$ROOT/tdf-mobile" run typecheck
  run_npm --prefix "$ROOT/tdf-mobile" run test
elif [ "${REQUIRE_MOBILE_WORKSPACE:-0}" = "1" ]; then
  echo "✖ Missing or incomplete tdf-mobile install. Run: git submodule update --init --recursive && (cd tdf-mobile && npm install)" >&2
  exit 1
else
  echo "▶ Skipping mobile checks: workspace missing or install incomplete"
fi
