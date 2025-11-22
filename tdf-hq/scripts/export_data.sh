#!/usr/bin/env bash
# Export all data from the current database for migration to clean schema
# Usage: ./scripts/export_data.sh [output_file]

set -euo pipefail

# Default output file
OUTPUT_FILE="${1:-sql/data_export_$(date +%Y%m%d_%H%M%S).sql}"

# Database connection from environment or defaults
DB_HOST="${DB_HOST:-127.0.0.1}"
DB_PORT="${DB_PORT:-5432}"
DB_USER="${DB_USER:-postgres}"
DB_NAME="${DB_NAME:-tdf_hq}"
PGPASSWORD="${DB_PASS:-postgres}"
export PGPASSWORD

echo "==> Exporting data from $DB_NAME@$DB_HOST:$DB_PORT"
echo "==> Output file: $OUTPUT_FILE"

# Create output directory if needed
mkdir -p "$(dirname "$OUTPUT_FILE")"

# Export data only (no schema, no ownership)
pg_dump \
  --host="$DB_HOST" \
  --port="$DB_PORT" \
  --username="$DB_USER" \
  --dbname="$DB_NAME" \
  --data-only \
  --no-owner \
  --no-privileges \
  --column-inserts \
  --disable-triggers \
  --file="$OUTPUT_FILE"

echo "==> Export complete!"
echo "==> File size: $(du -h "$OUTPUT_FILE" | cut -f1)"
echo ""
echo "To import into a fresh database:"
echo "  1. Create fresh database: psql -U postgres -c 'DROP DATABASE IF EXISTS tdf_hq_new; CREATE DATABASE tdf_hq_new;'"
echo "  2. Apply schema: psql -U postgres -d tdf_hq_new -f sql/init_schema.sql"
echo "  3. Import data: psql -U postgres -d tdf_hq_new -f $OUTPUT_FILE"
