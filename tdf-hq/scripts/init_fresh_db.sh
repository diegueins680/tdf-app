#!/usr/bin/env bash
# Initialize a fresh database with schema and optionally import data
# Usage: ./scripts/init_fresh_db.sh [data_file.sql]

set -euo pipefail

DATA_FILE="${1:-}"

# Database connection from environment or defaults
DB_HOST="${DB_HOST:-127.0.0.1}"
DB_PORT="${DB_PORT:-5432}"
DB_USER="${DB_USER:-postgres}"
DB_NAME="${DB_NAME:-tdf_hq}"
PGPASSWORD="${DB_PASS:-postgres}"
export PGPASSWORD

echo "==> Initializing fresh database: $DB_NAME@$DB_HOST:$DB_PORT"
echo ""

# Check if database exists
if psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -lqt | cut -d \| -f 1 | grep -qw "$DB_NAME"; then
    echo "WARNING: Database $DB_NAME already exists!"
    read -p "Drop and recreate? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
    
    echo "==> Dropping existing database..."
    psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -c "DROP DATABASE $DB_NAME;"
fi

echo "==> Creating database $DB_NAME..."
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -c "CREATE DATABASE $DB_NAME;"

echo "==> Applying schema from sql/init_schema.sql..."
psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -f sql/init_schema.sql

if [ -n "$DATA_FILE" ]; then
    if [ -f "$DATA_FILE" ]; then
        echo "==> Importing data from $DATA_FILE..."
        psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -f "$DATA_FILE"
        echo "==> Data import complete!"
    else
        echo "ERROR: Data file not found: $DATA_FILE"
        exit 1
    fi
else
    echo "==> No data file provided. Database initialized with empty schema."
fi

echo ""
echo "==> Database ready!"
echo "==> Connection: postgresql://$DB_USER:***@$DB_HOST:$DB_PORT/$DB_NAME"
