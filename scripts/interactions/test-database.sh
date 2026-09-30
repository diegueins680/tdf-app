#!/usr/bin/env bash
set -euo pipefail
interaction_repo=$(cd "$(dirname "$0")/../.." && pwd)
interaction_db="tdf_interaction_acceptance_${$}"
# Own only a newly created, uniquely named test database. Do not drop on create failure.
"$interaction_repo/scripts/interactions/prepare-test-database.sh" "$interaction_db" >/dev/null
trap 'dropdb --if-exists "$interaction_db"' EXIT
for interaction_suite in policy command notification navigation legacy moderation entity records-publication; do
  psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/test/integration/interactions/$interaction_suite-properties.sql"
done
python3 "$interaction_repo/scripts/interactions/test-concurrency.py" "$interaction_db"
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/test/integration/interactions/ownerless-policy-migration.sql"
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/test/integration/interactions/report-content-migration.sql"
printf 'PASS isolated interaction database properties and concurrency\n'
