#!/usr/bin/env bash
# Docker entrypoint init script for PostgreSQL
# This file is automatically run by postgres:16-alpine on first container start
# Place in: /docker-entrypoint-initdb.d/

set -e

echo "==> Initializing TDF Records database schema..."

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    -- Extensions
    CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
    CREATE EXTENSION IF NOT EXISTS "pgcrypto";
    
    -- Log initialization
    SELECT 'Database initialized: ' || current_database() || ' at ' || now();
EOSQL

# Apply full schema
psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" < /sql/init_schema.sql

echo "==> Schema initialization complete!"
echo "==> Tables created: $(psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -t -c "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public'")"
