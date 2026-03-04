#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "▶ Linting, type-checking and testing tdf-hq-ui"
npm run lint --workspace=tdf-hq-ui --prefix "$ROOT"
npm run typecheck --workspace=tdf-hq-ui --prefix "$ROOT"
npm run test --workspace=tdf-hq-ui --prefix "$ROOT"

if [ -f "$ROOT/tdf-mobile/package.json" ]; then
  echo "▶ Linting, type-checking and testing tdf-mobile"
  npm run lint --workspace=tdf-mobile --prefix "$ROOT"
  npm run typecheck --workspace=tdf-mobile --prefix "$ROOT"
  npm run test --workspace=tdf-mobile --prefix "$ROOT"
elif [ "${REQUIRE_MOBILE_WORKSPACE:-0}" = "1" ]; then
  echo "✖ Missing tdf-mobile workspace. Run: git submodule update --init --recursive" >&2
  exit 1
else
  echo "▶ Skipping tdf-mobile checks: tdf-mobile/package.json not found"
fi

if command -v stack >/dev/null 2>&1; then
  echo "▶ Running Haskell tests (stack test)"
  STACK_ROOT_DIR="${STACK_ROOT:-$ROOT/.stack-root}"
  mkdir -p "$STACK_ROOT_DIR"
  (
    cd "$ROOT/tdf-hq"
    STACK_ROOT="$STACK_ROOT_DIR" stack test
  )
elif [ "${REQUIRE_STACK:-0}" = "1" ]; then
  echo "✖ stack command not found. Install stack." >&2
  exit 1
else
  echo "▶ Skipping Haskell tests: stack command not found"
fi

echo "✅ Quality checks completed"
