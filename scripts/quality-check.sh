#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "▶ Linting and type-checking tdf-hq-ui"
npm run lint --workspace=tdf-hq-ui --prefix "$ROOT"
npm run typecheck --workspace=tdf-hq-ui --prefix "$ROOT"

if [ -f "$ROOT/tdf-mobile/package.json" ]; then
  echo "▶ Linting, type-checking and testing tdf-mobile"
  npm run lint --workspace=tdf-mobile --prefix "$ROOT"
  npm run typecheck --workspace=tdf-mobile --prefix "$ROOT"
  npm run test --workspace=tdf-mobile --prefix "$ROOT"
elif [ "${ALLOW_MISSING_MOBILE_WORKSPACE:-0}" = "1" ]; then
  echo "▶ Skipping tdf-mobile checks: tdf-mobile/package.json not found (ALLOW_MISSING_MOBILE_WORKSPACE=1)"
else
  echo "✖ Missing tdf-mobile workspace. Run: git submodule update --init --recursive" >&2
  exit 1
fi

if command -v stack >/dev/null 2>&1; then
  echo "▶ Running Haskell tests (stack test)"
  (
    cd "$ROOT/tdf-hq"
    stack test
  )
elif [ "${ALLOW_MISSING_STACK:-0}" = "1" ]; then
  echo "▶ Skipping Haskell tests: stack command not found (ALLOW_MISSING_STACK=1)"
else
  echo "✖ stack command not found. Install stack or run with ALLOW_MISSING_STACK=1 when skipping intentionally." >&2
  exit 1
fi

echo "✅ Quality checks completed"
