#!/usr/bin/env bash
set -euo pipefail
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT="${DIR%/scripts}"
source "$ROOT/config/default.env"
psql "postgres://${DB_USER}:${DB_PASS}@${DB_HOST}:${DB_PORT}/${DB_NAME}" -f "$ROOT/sql/2025-11-04_academy.sql"
echo "OK: academy schema"
