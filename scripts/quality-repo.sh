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
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-requirement-declarations.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-new-specification-surfaces.py"
node --test "$ROOT/scripts/__tests__/compiled-api-surface.test.mjs"
node "$ROOT/scripts/check-dependency-security.mjs"
node --test "$ROOT/scripts/__tests__/dependency-security.test.mjs"
node --test "$ROOT/scripts/__tests__/formal-result-summary.test.mjs"
node "$ROOT/scripts/check-build-trust.mjs"
node --test "$ROOT/scripts/__tests__/build-trust.test.mjs"
node --test "$ROOT/scripts/__tests__/disposable-postgres-url.test.mjs"
node --test "$ROOT/scripts/__tests__/merch-runtime-safety.test.mjs"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-hetzner-inspection.py"
node --test "$ROOT/scripts/__tests__/hetzner-inspection.test.mjs"
node --test "$ROOT/scripts/__tests__/hetzner-preparation.test.mjs"
node --test "$ROOT/scripts/__tests__/persistent-uploads.test.mjs"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-hetzner-restore.py"
node --test "$ROOT/scripts/__tests__/hetzner-restore.test.mjs"
node --test "$ROOT/scripts/__tests__/hetzner-migration-rehearsal.test.mjs"
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
node --test "$ROOT/scripts/__tests__/local-api-fixture.test.mjs"
run_npm run verify:formal --prefix "$ROOT"
run_npm run test:auto-loop --prefix "$ROOT"
run_npm run test:formal --prefix "$ROOT"
run_npm run test:production-release --prefix "$ROOT"
run_npm run test:ci-pipeline --prefix "$ROOT"
node --test "$ROOT/scripts/__tests__/generated-api-conformance.test.mjs"
run_npm run test:instagram-token-workflows --prefix "$ROOT"
run_npm run test:music-directory-visual-artifacts --prefix "$ROOT"
run_npm run test:persona-program --prefix "$ROOT"

node --test "$ROOT/scripts/__tests__/artist-import-idempotency.test.mjs"
