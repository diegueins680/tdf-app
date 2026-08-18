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

echo "▶ Verifying repository-wide invariants"
run_npm run verify:formal --prefix "$ROOT"
run_npm run test:auto-loop --prefix "$ROOT"
run_npm run test:formal --prefix "$ROOT"
run_npm run test:production-release --prefix "$ROOT"
run_npm run test:ci-pipeline --prefix "$ROOT"
run_npm run test:music-directory-visual-artifacts --prefix "$ROOT"
