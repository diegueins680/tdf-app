#!/bin/sh
set -eu

server_bin="${TDF_SERVER_BIN:-/app/tdf-hq-exe}"
migration_sql="${TDF_PRODUCTION_MIGRATIONS_SQL:-/app/production-migrations.sql}"
auto_apply="${AUTO_APPLY_PRODUCTION_MIGRATIONS:-false}"
persistent_migrations="${RUN_MIGRATIONS:-false}"

case "${auto_apply}" in
  true|false) ;;
  *)
    echo "AUTO_APPLY_PRODUCTION_MIGRATIONS must be true or false" >&2
    exit 64
    ;;
esac

case "${persistent_migrations}" in
  true|false) ;;
  *)
    echo "RUN_MIGRATIONS must be true or false" >&2
    exit 64
    ;;
esac

packaged_assets="${TDF_PACKAGED_ASSETS_DIR:-/app/assets}"
served_assets="${HQ_ASSETS_DIR:-}"

if [ -n "${served_assets}" ] && [ "${served_assets}" != "${packaged_assets}" ]; then
  if [ ! -d "${packaged_assets}" ]; then
    echo "Packaged assets directory is missing or unreadable" >&2
    exit 66
  fi
  mkdir -p "${served_assets}"
  cp -R "${packaged_assets}/." "${served_assets}/"
  echo "Packaged assets synchronized to the served asset directory"
fi

if [ "${auto_apply}" = "true" ]; then
  if [ "${persistent_migrations}" = "true" ]; then
    echo "Refusing to combine reviewed production migrations with inferred Persistent migrations" >&2
    exit 64
  fi
  if [ ! -r "${migration_sql}" ]; then
    echo "Reviewed production migration bundle is missing or unreadable" >&2
    exit 66
  fi

  database_url="${DATABASE_URL:-${DATABASE_PRIVATE_URL:-${POSTGRES_URL:-${POSTGRES_PRISMA_URL:-}}}}"
  if [ -n "${database_url}" ]; then
    psql "${database_url}" -X -v ON_ERROR_STOP=1 -f "${migration_sql}"
  else
    export PGHOST="${DB_HOST:-${PGHOST:-127.0.0.1}}"
    export PGPORT="${DB_PORT:-${PGPORT:-5432}}"
    export PGUSER="${DB_USER:-${PGUSER:-postgres}}"
    export PGPASSWORD="${DB_PASS:-${PGPASSWORD:-postgres}}"
    export PGDATABASE="${DB_NAME:-${PGDATABASE:-tdf_hq}}"
    if [ -n "${DB_SSLMODE:-}" ]; then
      export PGSSLMODE="${DB_SSLMODE}"
    fi
    psql -X -v ON_ERROR_STOP=1 -f "${migration_sql}"
  fi
  echo "Reviewed production migrations are applied and schema verification passed"
fi

# The production image never delegates schema authority to Persistent. Direct
# development invocations may still opt into that path without this entrypoint.
export RUN_MIGRATIONS=false
export APP_PORT="${PORT:-${APP_PORT:-8080}}"
exec "${server_bin}"
