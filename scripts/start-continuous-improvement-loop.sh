#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG="${1:-$ROOT/scripts/continuous-improvement-loop.codex.json}"
LOG_FILE="${CONTINUOUS_LOOP_LOG_FILE:-$ROOT/tmp/continuous-improvement-loop.log}"
PID_FILE="${CONTINUOUS_LOOP_PID_FILE:-$ROOT/tmp/continuous-improvement-loop.pid}"

mkdir -p "$(dirname "$LOG_FILE")"

if [ -f "$PID_FILE" ]; then
  EXISTING_PID="$(cat "$PID_FILE" 2>/dev/null || true)"
  if [ -n "$EXISTING_PID" ] && kill -0 "$EXISTING_PID" 2>/dev/null; then
    echo "Continuous improvement loop is already running with PID $EXISTING_PID" >&2
    exit 1
  fi
fi

nohup node "$ROOT/scripts/continuous-improvement-loop.mjs" \
  --config "$CONFIG" \
  --allow-dirty \
  >> "$LOG_FILE" 2>&1 < /dev/null &

LOOP_PID=$!
echo "$LOOP_PID" > "$PID_FILE"

echo "Started continuous improvement loop."
echo "PID: $LOOP_PID"
echo "Log: $LOG_FILE"
echo "PID file: $PID_FILE"
