#!/bin/sh
set -eu

usage() {
  echo "Usage: TDF_OPERATIONS_DATABASE_URL=postgresql://... $0 [--apply] [--run-key KEY] [--batch-size N]" >&2
}

TDF_OPS_MODE="dry-run"
TDF_OPS_RUN_KEY="operations-v1-$(date -u +%Y%m%d)"
TDF_OPS_BATCH_SIZE=500

while [ "$#" -gt 0 ]; do
  case "$1" in
    --apply) TDF_OPS_MODE="apply"; shift ;;
    --run-key) [ "$#" -ge 2 ] || { usage; exit 2; }; TDF_OPS_RUN_KEY=$2; shift 2 ;;
    --batch-size) [ "$#" -ge 2 ] || { usage; exit 2; }; TDF_OPS_BATCH_SIZE=$2; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) usage; exit 2 ;;
  esac
done

case "$TDF_OPS_RUN_KEY" in
  *[!A-Za-z0-9._-]*|'') echo "Invalid --run-key" >&2; exit 2 ;;
esac
case "$TDF_OPS_BATCH_SIZE" in
  *[!0-9]*|'') echo "Invalid --batch-size" >&2; exit 2 ;;
esac
if [ "$TDF_OPS_BATCH_SIZE" -lt 1 ] || [ "$TDF_OPS_BATCH_SIZE" -gt 5000 ]; then
  echo "--batch-size must be between 1 and 5000" >&2
  exit 2
fi

TDF_OPS_DATABASE_URL=${TDF_OPERATIONS_DATABASE_URL:-}
if [ -z "$TDF_OPS_DATABASE_URL" ]; then
  echo "TDF_OPERATIONS_DATABASE_URL is required" >&2
  exit 2
fi
command -v psql >/dev/null 2>&1 || { echo "psql is required" >&2; exit 2; }

TDF_OPS_DRY_RUN=true
[ "$TDF_OPS_MODE" = "apply" ] && TDF_OPS_DRY_RUN=false

echo "operations backfill mode=$TDF_OPS_MODE run_key=$TDF_OPS_RUN_KEY batch_size=$TDF_OPS_BATCH_SIZE"
while :; do
  TDF_OPS_RESULT=$(psql -X -qAt -v ON_ERROR_STOP=1 "$TDF_OPS_DATABASE_URL" \
    -c "SELECT run_id, eligible, inserted, remaining, run_status FROM operations_backfill_batch('$TDF_OPS_RUN_KEY', $TDF_OPS_BATCH_SIZE, $TDF_OPS_DRY_RUN);")
  IFS='|' read -r TDF_OPS_RUN_ID TDF_OPS_ELIGIBLE TDF_OPS_INSERTED TDF_OPS_REMAINING TDF_OPS_STATUS <<EOF
$TDF_OPS_RESULT
EOF
  echo "run_id=$TDF_OPS_RUN_ID eligible=$TDF_OPS_ELIGIBLE inserted=$TDF_OPS_INSERTED remaining=$TDF_OPS_REMAINING status=$TDF_OPS_STATUS"
  [ "$TDF_OPS_DRY_RUN" = true ] && break
  [ "$TDF_OPS_REMAINING" = "0" ] && break
done
