#!/usr/bin/env bash
set -euo pipefail

if [[ -f ./config/default.env ]]; then
  set -a; source ./config/default.env; set +a
fi

DB_HOST="${DB_HOST:-127.0.0.1}"
DB_PORT="${DB_PORT:-5432}"
DB_USER="${DB_USER:-postgres}"
DB_PASS="${DB_PASS:-postgres}"
DB_NAME="${DB_NAME:-tdf_hq}"

export PGPASSWORD="$DB_PASS"

echo "==> Applying packages/lessons/receipts schema to ${DB_USER}@${DB_HOST}:${DB_PORT}/${DB_NAME}"
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -v "ON_ERROR_STOP=1" -f "./sql/2025-10-21_packages_lessons_receipts.sql"
echo "==> Done."

