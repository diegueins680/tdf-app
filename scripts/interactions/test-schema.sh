#!/usr/bin/env bash
set -euo pipefail
interaction_repo=$(cd "$(dirname "$0")/../.." && pwd)
# This script creates and owns its isolated database; never accepts a production URL.
interaction_db="tdf_interaction_schema_test_${$}"
createdb "$interaction_db"
trap 'dropdb --if-exists "$interaction_db"' EXIT
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/test/integration/interactions/schema-fixture.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/sql/2026-09-28_universal_interactions.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/sql/2026-09-28_universal_interactions.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$interaction_db" -f "$interaction_repo/tdf-hq/test/integration/interactions/schema-properties.sql"
