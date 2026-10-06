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
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-merge-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-privacy-request-ledger.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-requirement-declarations.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-new-specification-surfaces.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-specification-conformance.py"
node --test "$ROOT/scripts/__tests__/compiled-api-surface.test.mjs"
node "$ROOT/scripts/check-api-availability.mjs"
node "$ROOT/scripts/check-dependency-security.mjs"
node --test "$ROOT/scripts/__tests__/dependency-security.test.mjs"
node --test "$ROOT/scripts/__tests__/formal-result-summary.test.mjs"
node "$ROOT/scripts/check-build-trust.mjs"
node --test "$ROOT/scripts/__tests__/build-trust.test.mjs"
node --test "$ROOT/scripts/__tests__/disposable-postgres-url.test.mjs"
node --test "$ROOT/scripts/__tests__/merch-runtime-safety.test.mjs"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-hetzner-inspection.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-hetzner-backup.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-recovery-files.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-stopped-application-storage.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-coordinated-recovery-bundle.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-coordinated-capture.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-coordinated-encryption.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-coordinated-retrieval.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-recovered-application-content.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-production-recovery-sources.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-production-writer-fence.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-host-scheduler-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-host-process-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-writer-fence-fixture-ownership.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-physical-postgres-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-offline-recovery-capacity.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-release-journal.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-legacy-stop-policy.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-interrupted-release-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-deployment-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-abort-service-journal.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-database-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-application-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-outbound-quarantine.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-ufw-recovery-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-dormant-container-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-bpf-recovery-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-network-recovery-admission.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-edge-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-edge-tls.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-timer-recovery.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-recovery-completion.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-disposable-absence.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-disposable-creation-spec.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-durable-disposable-records.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-coordinated-disposable-creation.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-original-disposable-cleanup.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-recovery-tools.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-recovery-envelope.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-recovery-transfer.py"
node --test "$ROOT/scripts/__tests__/hetzner-inspection.test.mjs"
node --test "$ROOT/scripts/__tests__/hetzner-preparation.test.mjs"
node --test "$ROOT/scripts/__tests__/persistent-uploads.test.mjs"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-hetzner-restore.py"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/scripts/test-isolated-application-canary.py"
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
node --test "$ROOT/scripts/__tests__/event-preview.test.mjs" "$ROOT/scripts/__tests__/ticket-page-preview.test.mjs"
run_npm run verify:formal --prefix "$ROOT"
run_npm run test:auto-loop --prefix "$ROOT"
run_npm run test:formal --prefix "$ROOT"
run_npm run test:artist-enrichment --prefix "$ROOT"
run_npm run test:production-release --prefix "$ROOT"
run_npm run test:ci-pipeline --prefix "$ROOT"
node --test "$ROOT/scripts/__tests__/generated-api-conformance.test.mjs"
node --test "$ROOT/scripts/__tests__/operations-api-schema.test.mjs"
run_npm run test:instagram-token-workflows --prefix "$ROOT"
run_npm run test:music-directory-visual-artifacts --prefix "$ROOT"
run_npm run test:persona-program --prefix "$ROOT"

node --test "$ROOT/scripts/__tests__/artist-import-idempotency.test.mjs"
node --test "$ROOT/scripts/__tests__/diagnose-social.test.mjs"
node --test "$ROOT/scripts/__tests__/payment-readiness-helper.test.mjs"

python3 "$ROOT/scripts/test_production_access.py"
python3 "$ROOT/scripts/test_mail_deliverability_monitor.py"
node --test "$ROOT/scripts/__tests__/production-catalog-inventory.test.mjs"

node --test "$ROOT/scripts/__tests__/retired-messaging-refresher.test.mjs"
