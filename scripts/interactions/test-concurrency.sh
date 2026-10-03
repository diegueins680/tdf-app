#!/usr/bin/env bash
set -euo pipefail
interaction_repo=$(cd "$(dirname "$0")/../.." && pwd)
interaction_db="tdf_interaction_concurrency_${$}"
trap 'dropdb --if-exists "$interaction_db"' EXIT
"$interaction_repo/scripts/interactions/prepare-test-database.sh" "$interaction_db" >/dev/null
python3 "$interaction_repo/scripts/interactions/test-concurrency.py" "$interaction_db"
