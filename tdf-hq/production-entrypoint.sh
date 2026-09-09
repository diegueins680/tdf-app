#!/bin/sh
set -eu

server_bin="${TDF_SERVER_BIN:-/app/tdf-hq-exe}"
migration_sql="${TDF_PRODUCTION_MIGRATIONS_SQL:-/app/production-migrations.sql}"
auto_apply="${AUTO_APPLY_PRODUCTION_MIGRATIONS:-false}"
persistent_migrations="${RUN_MIGRATIONS:-false}"
precheck_only="${TDF_MIGRATION_PRECHECK_ONLY:-false}"

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

case "${precheck_only}" in
  true|false) ;;
  *)
    echo "TDF_MIGRATION_PRECHECK_ONLY must be true or false" >&2
    exit 64
    ;;
esac

if [ "${precheck_only}" = "true" ] && [ "${auto_apply}" != "true" ]; then
  echo "Migration precheck requires AUTO_APPLY_PRODUCTION_MIGRATIONS=true" >&2
  exit 64
fi

packaged_assets="${TDF_PACKAGED_ASSETS_DIR:-/app/assets}"
served_assets="${HQ_ASSETS_DIR:-}"

if [ -n "${served_assets}" ]; then
  if [ ! -d "${packaged_assets}" ]; then
    echo "Packaged assets directory is missing or unreadable" >&2
    exit 66
  fi
  mkdir -p "${served_assets}"
  packaged_assets_canonical="$(CDPATH= cd "${packaged_assets}" && pwd -P)"
  served_assets_canonical="$(CDPATH= cd "${served_assets}" && pwd -P)"
  if [ "${served_assets_canonical}" != "${packaged_assets_canonical}" ]; then
    # The persistent asset volume can contain paths created by an earlier
    # release under a different owner. Preserve existing files and seed what
    # is writable without preventing an otherwise healthy API from starting.
    if cp -R -n "${packaged_assets_canonical}/." "${served_assets_canonical}/"; then
      echo "Packaged assets synchronized to the served asset directory"
    else
      echo "Could not synchronize some packaged assets; preserving the existing served assets" >&2
    fi
  fi
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

if [ "${precheck_only}" = "true" ]; then
  echo "Reviewed production migration precheck completed"
  exit 0
fi

# The production image never delegates schema authority to Persistent. Direct
# development invocations may still opt into that path without this entrypoint.
export RUN_MIGRATIONS=false
export APP_PORT="${PORT:-${APP_PORT:-8080}}"
exec "${server_bin}"
