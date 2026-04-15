#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ACTION="${1:-start}"
if [ "$ACTION" = "start" ] || [ "$ACTION" = "restart" ] || [ "$ACTION" = "install-launchd" ]; then
  CONFIG="${2:-${CONTINUOUS_LOOP_CONFIG:-$ROOT/scripts/continuous-improvement-loop.codex.json}}"
else
  CONFIG="${CONTINUOUS_LOOP_CONFIG:-$ROOT/scripts/continuous-improvement-loop.codex.json}"
fi
LOG_FILE="${CONTINUOUS_LOOP_LOG_FILE:-$ROOT/tmp/continuous-improvement-loop.log}"
PID_FILE="${CONTINUOUS_LOOP_PID_FILE:-$ROOT/tmp/continuous-improvement-loop.pid}"
STATE_DIR="${CONTINUOUS_LOOP_STATE_DIR:-$ROOT/tmp/continuous-improvement-loop}"
STATUS_FILE="${CONTINUOUS_LOOP_STATUS_FILE:-$STATE_DIR/status.json}"
STOP_FILE="${CONTINUOUS_LOOP_STOP_FILE:-$STATE_DIR/stop}"
SUPERVISOR="$ROOT/scripts/continuous-improvement-loop-supervisor.sh"
LAUNCHD_INSTALLER="$ROOT/scripts/install-continuous-improvement-loop-launchd.sh"

mkdir -p "$STATE_DIR" "$(dirname "$LOG_FILE")"

is_pid_alive() {
  local pid="$1"
  [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null
}

read_pid() {
  cat "$PID_FILE" 2>/dev/null || true
}

status() {
  local pid
  pid="$(read_pid)"
  if is_pid_alive "$pid"; then
    echo "continuous-improvement-loop supervisor: running"
    echo "pid: $pid"
  else
    echo "continuous-improvement-loop supervisor: stopped"
    echo "pid: ${pid:-none}"
  fi
  echo "config: $CONFIG"
  echo "log: $LOG_FILE"
  echo "status file: $STATUS_FILE"
  if [ -f "$STATUS_FILE" ]; then
    echo "status json:"
    cat "$STATUS_FILE"
  fi
}

start() {
  local existing_pid
  existing_pid="$(read_pid)"
  if is_pid_alive "$existing_pid"; then
    echo "Continuous improvement loop supervisor already running with PID $existing_pid"
    status
    return 0
  fi

  node "$ROOT/scripts/continuous-improvement-loop.mjs" --config "$CONFIG" --validate-config-only

  rm -f "$STOP_FILE" "$PID_FILE"
  nohup "$SUPERVISOR" "$CONFIG" >> "$LOG_FILE" 2>&1 < /dev/null &
  local supervisor_pid=$!
  sleep 1
  if ! is_pid_alive "$supervisor_pid"; then
    echo "Failed to start continuous improvement loop supervisor" >&2
    exit 1
  fi
  echo "$supervisor_pid" > "$PID_FILE"
  echo "Started continuous improvement loop supervisor."
  echo "PID: $supervisor_pid"
  echo "Config: $CONFIG"
  echo "Log: $LOG_FILE"
  echo "Status: $STATUS_FILE"
}

stop() {
  local pid
  pid="$(read_pid)"
  if ! is_pid_alive "$pid"; then
    echo "Continuous improvement loop supervisor is not running."
    rm -f "$PID_FILE" "$STOP_FILE"
    return 0
  fi
  : > "$STOP_FILE"
  kill "$pid" 2>/dev/null || true
  for _ in $(seq 1 20); do
    if ! is_pid_alive "$pid"; then
      break
    fi
    sleep 1
  done
  if is_pid_alive "$pid"; then
    kill -9 "$pid" 2>/dev/null || true
  fi
  rm -f "$PID_FILE"
  echo "Stopped continuous improvement loop supervisor."
}

case "$ACTION" in
  start)
    start
    ;;
  stop)
    stop
    ;;
  restart)
    stop
    start
    ;;
  status)
    status
    ;;
  tail)
    touch "$LOG_FILE"
    tail -f "$LOG_FILE"
    ;;
  install-launchd)
    "$LAUNCHD_INSTALLER" "$CONFIG"
    ;;
  *)
    echo "Usage: $0 [start|stop|restart|status|tail|install-launchd] [config-path]" >&2
    exit 1
    ;;
esac
