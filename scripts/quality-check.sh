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

echo "▶ Linting, type-checking and testing tdf-hq-ui"
run_npm run lint --workspace=tdf-hq-ui --prefix "$ROOT"
run_npm run typecheck --workspace=tdf-hq-ui --prefix "$ROOT"
run_npm run test --workspace=tdf-hq-ui --prefix "$ROOT"
echo "▶ Building tdf-hq-ui"
run_npm run build --workspace=tdf-hq-ui --prefix "$ROOT"

if [ -f "$ROOT/tdf-mobile/package.json" ]; then
  echo "▶ Linting, type-checking and testing tdf-mobile"
  run_npm run lint --workspace=tdf-mobile --prefix "$ROOT"
  run_npm run typecheck --workspace=tdf-mobile --prefix "$ROOT"
  run_npm run test --workspace=tdf-mobile --prefix "$ROOT"
elif [ "${REQUIRE_MOBILE_WORKSPACE:-0}" = "1" ]; then
  echo "✖ Missing tdf-mobile workspace. Run: git submodule update --init --recursive" >&2
  exit 1
else
  echo "▶ Skipping tdf-mobile checks: tdf-mobile/package.json not found"
fi

BACKEND_DIR="$ROOT/tdf-hq"

if [ -d "$BACKEND_DIR" ] && [ -f "$BACKEND_DIR/stack.yaml" ] && command -v stack >/dev/null 2>&1; then
  echo "▶ Building Haskell executable (stack build tdf-hq:exe:tdf-hq-exe)"
  STACK_ROOT_DIR="${STACK_ROOT:-$ROOT/.stack-root}"
  mkdir -p "$STACK_ROOT_DIR"
  (
    cd "$BACKEND_DIR"
    STACK_ROOT="$STACK_ROOT_DIR" stack build tdf-hq:exe:tdf-hq-exe
  )
  echo "▶ Running Haskell tests (stack test)"
  (
    cd "$BACKEND_DIR"
    STACK_ROOT="$STACK_ROOT_DIR" stack test
  )
elif [ "${REQUIRE_STACK:-0}" = "1" ]; then
  echo "✖ Missing stack or backend project. Install stack and ensure tdf-hq/stack.yaml exists." >&2
  exit 1
else
  echo "▶ Skipping Haskell tests: stack unavailable or backend project missing"
fi

echo "✅ Quality checks completed"
