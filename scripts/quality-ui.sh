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

echo "▶ Linting, type-checking, testing and building tdf-hq-ui"
run_npm run lint --workspace=tdf-hq-ui --prefix "$ROOT"
run_npm run typecheck --workspace=tdf-hq-ui --prefix "$ROOT"
run_npm run test --workspace=tdf-hq-ui --prefix "$ROOT"
run_npm run build --workspace=tdf-hq-ui --prefix "$ROOT"
