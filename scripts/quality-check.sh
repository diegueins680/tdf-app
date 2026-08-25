#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

bash "$ROOT/scripts/quality-repo.sh"
bash "$ROOT/scripts/quality-ui.sh"
bash "$ROOT/scripts/quality-mobile.sh"
bash "$ROOT/scripts/quality-backend.sh"

echo "✅ Quality checks completed"
