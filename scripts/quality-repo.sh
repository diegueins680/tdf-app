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
run_npm run generate:studio-internship-audit --prefix "$ROOT"
git -C "$ROOT" diff --exit-code -- \
  docs/internships/studio-audit/generated-summary.json \
  docs/internships/studio-audit/studio-feature-inventory.csv \
  docs/internships/studio-audit/test-case-index.csv \
  test/internships/studio-audit/draft-project.json \
  test/internships/studio-audit/draft-stuart-account.json \
  test/internships/studio-audit/studio-feature-inventory.json \
  test/internships/studio-audit/test-cases.json
node --test "$ROOT/scripts/__tests__/studio-internship-audit.test.mjs"
run_npm run verify:formal --prefix "$ROOT"
run_npm run test:auto-loop --prefix "$ROOT"
run_npm run test:formal --prefix "$ROOT"
run_npm run test:production-release --prefix "$ROOT"
run_npm run test:ci-pipeline --prefix "$ROOT"
run_npm run test:music-directory-visual-artifacts --prefix "$ROOT"
run_npm run test:persona-program --prefix "$ROOT"
