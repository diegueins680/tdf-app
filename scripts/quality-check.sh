#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "▶ Linting and type-checking tdf-hq-ui"
npm run lint --workspace=tdf-hq-ui --prefix "$ROOT"
npm run typecheck --workspace=tdf-hq-ui --prefix "$ROOT"

echo "▶ Linting, type-checking and testing tdf-mobile"
npm run lint --workspace=tdf-mobile --prefix "$ROOT"
npm run typecheck --workspace=tdf-mobile --prefix "$ROOT"
npm run test --workspace=tdf-mobile --prefix "$ROOT"

echo "▶ Running Haskell tests (stack test)"
(
  cd "$ROOT/tdf-hq"
  stack test
)

echo "✅ Quality checks completed"
