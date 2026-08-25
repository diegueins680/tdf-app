#!/usr/bin/env bash
# Deprecated compatibility helper. Production releases must use the guarded
# release lane in scripts/production-release.mjs via the npm commands below.

set -euo pipefail

environment=${1:-local}
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
project_dir="$(dirname "$script_dir")"

case "$environment" in
  local)
    echo "[deprecated] This helper is retained only for a local backend build." >&2
    echo "[deprecated] It does not apply database migrations or deploy anything." >&2
    if ! command -v stack >/dev/null 2>&1; then
      echo "[error] Stack is required for the local build." >&2
      exit 1
    fi
    cd "$project_dir/tdf-hq"
    stack build --fast
    echo "[ok] Local backend build completed."
    ;;
  staging|production)
    cat >&2 <<'EOF'
[refused] scripts/deploy-stripe-ticketing.sh no longer performs remote releases.

Use the guarded backend release lane from the repository root with a full SHA:

  npm run release:backend:plan -- --sha <full-sha>
  npm run release:backend:preflight -- --sha <full-sha>
  npm run release:backend -- --sha <full-sha> --execute --confirm <full-sha>

The release lane keeps application-startup migrations and event discovery off,
applies required schema changes once, verifies health/version, and rolls out the
pinned backend image.
EOF
    exit 1
    ;;
  *)
    echo "Usage: $0 [local|staging|production]" >&2
    exit 2
    ;;
esac
