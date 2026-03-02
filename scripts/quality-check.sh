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
else
  echo "▶ Skipping tdf-mobile checks: tdf-mobile/package.json not found"
fi

if command -v stack >/dev/null 2>&1; then
  echo "▶ Running Haskell tests (stack test)"
  (
    cd "$ROOT/tdf-hq"
    stack test
  )
else
  echo "▶ Skipping Haskell tests: stack command not found"
fi

echo "✅ Quality checks completed"
