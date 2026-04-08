#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG="${1:-${CONTINUOUS_LOOP_CONFIG:-$ROOT/scripts/continuous-improvement-loop.codex.json}}"
STATE_DIR="${CONTINUOUS_LOOP_STATE_DIR:-$ROOT/tmp/continuous-improvement-loop}"
LOG_FILE="${CONTINUOUS_LOOP_LOG_FILE:-$ROOT/tmp/continuous-improvement-loop.log}"
PID_FILE="${CONTINUOUS_LOOP_PID_FILE:-$ROOT/tmp/continuous-improvement-loop.pid}"
CHILD_PID_FILE="${CONTINUOUS_LOOP_CHILD_PID_FILE:-$STATE_DIR/child.pid}"
STATUS_FILE="${CONTINUOUS_LOOP_STATUS_FILE:-$STATE_DIR/status.json}"
STOP_FILE="${CONTINUOUS_LOOP_STOP_FILE:-$STATE_DIR/stop}"
HEARTBEAT_FILE="${CONTINUOUS_LOOP_HEARTBEAT_FILE:-$STATE_DIR/heartbeat.txt}"
LOCK_DIR="${CONTINUOUS_LOOP_LOCK_DIR:-$STATE_DIR/lock}"
RESTART_DELAY_SECONDS="${CONTINUOUS_LOOP_RESTART_DELAY_SECONDS:-15}"
HEARTBEAT_TIMEOUT_SECONDS="${CONTINUOUS_LOOP_HEARTBEAT_TIMEOUT_SECONDS:-1800}"
POLL_INTERVAL_SECONDS="${CONTINUOUS_LOOP_SUPERVISOR_POLL_SECONDS:-15}"
CHILD_TIMEOUT_SECONDS="${CONTINUOUS_LOOP_CHILD_TIMEOUT_SECONDS:-7200}"

export PATH="${CONTINUOUS_LOOP_PATH:-/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:${HOME}/.local/bin:${PATH:-}}"
if [ -f "$ROOT/.env" ]; then
  set -a
  # shellcheck disable=SC1090
  source "$ROOT/.env"
  set +a
fi

mkdir -p "$STATE_DIR" "$(dirname "$LOG_FILE")"
export CONTINUOUS_LOOP_LOG_FILE="$LOG_FILE"
export CONTINUOUS_LOOP_PID_FILE="$PID_FILE"
export CONTINUOUS_LOOP_CHILD_PID_FILE="$CHILD_PID_FILE"
export CONTINUOUS_LOOP_STATUS_FILE="$STATUS_FILE"
export CONTINUOUS_LOOP_STOP_FILE="$STOP_FILE"
export CONTINUOUS_LOOP_HEARTBEAT_FILE="$HEARTBEAT_FILE"
export CONTINUOUS_LOOP_HEARTBEAT_TIMEOUT_SECONDS="$HEARTBEAT_TIMEOUT_SECONDS"
export CONTINUOUS_LOOP_RESTART_DELAY_SECONDS="$RESTART_DELAY_SECONDS"
export CONTINUOUS_LOOP_SUPERVISOR_POLL_SECONDS="$POLL_INTERVAL_SECONDS"
export CONTINUOUS_LOOP_CONFIG="$CONFIG"
export CONTINUOUS_LOOP_CHILD_TIMEOUT_SECONDS="$CHILD_TIMEOUT_SECONDS"

log() {
  printf '[%s] %s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$*" >> "$LOG_FILE"
}

json_update() {
  local state="$1"
  local phase="$2"
  local details="${3:-}"
  local child_pid="${4:-}"
  local last_exit_code="${5:-}"
  local restart_count="$6"
  local stale_restarts="$7"
  python3 - "$STATUS_FILE" "$state" "$phase" "$details" "$child_pid" "$last_exit_code" "$restart_count" "$stale_restarts" "$CONFIG" "$$" "$LOG_FILE" "$PID_FILE" "$CHILD_PID_FILE" "$STOP_FILE" "$HEARTBEAT_TIMEOUT_SECONDS" "$POLL_INTERVAL_SECONDS" "$RESTART_DELAY_SECONDS" "$HEARTBEAT_FILE" <<'PY'
import datetime, json, os, sys
(
    status_file,
    state,
    phase,
    details,
    child_pid,
    last_exit_code,
    restart_count,
    stale_restarts,
    config_path,
    supervisor_pid,
    log_file,
    pid_file,
    child_pid_file,
    stop_file,
    heartbeat_timeout,
    poll_interval,
    restart_delay,
    heartbeat_file,
) = sys.argv[1:19]
try:
    with open(status_file, 'r', encoding='utf-8') as fh:
        data = json.load(fh)
except Exception:
    data = {}
now = datetime.datetime.now(datetime.timezone.utc).isoformat()
data.update({
    'updatedAt': now,
    'lastHeartbeat': now,
    'state': state,
    'phase': phase,
    'details': details,
    'lastMessage': details,
    'supervisorPid': int(supervisor_pid),
    'configPath': config_path,
    'logFile': log_file,
    'pidFile': pid_file,
    'childPidFile': child_pid_file,
    'stopFile': stop_file,
    'heartbeatFile': heartbeat_file,
    'heartbeatTimeoutSeconds': int(heartbeat_timeout or '0'),
    'pollIntervalSeconds': int(poll_interval or '0'),
    'restartDelaySeconds': int(restart_delay or '0'),
    'restartCount': int(restart_count or '0'),
    'staleRestartCount': int(stale_restarts or '0'),
})
if child_pid:
    data['childPid'] = int(child_pid)
elif state in {'starting', 'stopping', 'stopped', 'blocked'}:
    data['childPid'] = None

if state == 'blocked':
    data['runnerPid'] = None
    data['currentIteration'] = None
    data['contextDir'] = None
    data['ideaTitle'] = None
if last_exit_code:
    try:
        data['lastExitCode'] = int(last_exit_code)
    except Exception:
        data['lastExitCode'] = last_exit_code
if 'startedAt' not in data:
    data['startedAt'] = now
tmp = status_file + '.tmp'
with open(tmp, 'w', encoding='utf-8') as fh:
    json.dump(data, fh, indent=2)
    fh.write('\n')
os.replace(tmp, status_file)
with open(heartbeat_file, 'w', encoding='utf-8') as fh:
    fh.write(now + '\n')
PY
}

is_pid_alive() {
  local pid="$1"
  [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null
}

heartbeat_age_seconds() {
  python3 - "$STATUS_FILE" <<'PY'
import datetime, json, sys
path = sys.argv[1]
try:
    with open(path, 'r', encoding='utf-8') as fh:
        data = json.load(fh)
    stamp = data.get('lastHeartbeat') or data.get('updatedAt')
    if not stamp:
        print(-1)
        raise SystemExit
    dt = datetime.datetime.fromisoformat(stamp.replace('Z', '+00:00'))
    now = datetime.datetime.now(datetime.timezone.utc)
    print(int((now - dt).total_seconds()))
except Exception:
    print(-1)
PY
}

cleanup() {
  rm -f "$CHILD_PID_FILE"
  rm -rf "$LOCK_DIR"
}

stop_child() {
  local child_pid="${1:-}"
  if ! is_pid_alive "$child_pid"; then
    return 0
  fi
  kill "$child_pid" 2>/dev/null || true
  for _ in $(seq 1 20); do
    if ! is_pid_alive "$child_pid"; then
      return 0
    fi
    sleep 1
  done
  kill -9 "$child_pid" 2>/dev/null || true
}

trap 'cleanup' EXIT

if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "Continuous improvement loop supervisor lock exists at $LOCK_DIR" >&2
  exit 1
fi

echo "$$" > "$PID_FILE"
rm -f "$STOP_FILE"

restart_count=0
stale_restart_count=0
child_pid=""
child_started_epoch=0

json_update "starting" "startup" "Supervisor booting" "" "" "$restart_count" "$stale_restart_count"
log "continuous-improvement-loop supervisor started pid=$$ config=$CONFIG"

preflight_block_reason() {
  if grep -q 'codex-loop-worker\\|\\bcodex\\b' "$CONFIG" 2>/dev/null && ! command -v codex >/dev/null 2>&1; then
    echo 'blocked: Codex CLI is required by the configured worker but is not available in PATH'
    return 0
  fi

  if python3 - "$CONFIG" <<'PY' >/dev/null 2>&1
import json, sys
with open(sys.argv[1], 'r', encoding='utf-8') as fh:
    config = json.load(fh)
raise SystemExit(0 if config.get('pollGitHub', True) else 1)
PY
  then
    if [ -z "${GITHUB_TOKEN:-${GH_TOKEN:-${GITHUB_PAT:-}}}" ] && ! gh auth token >/dev/null 2>&1; then
      echo 'blocked: GitHub polling is enabled but neither token env vars nor `gh auth token` are available'
      return 0
    fi
  fi

  local dirty
  dirty="$(git -C "$ROOT" status --porcelain 2>/dev/null || true)"
  if [ -n "$dirty" ]; then
    local checkpoint_json=""
    if checkpoint_json="$(node "$ROOT/scripts/continuous-improvement-loop-dirty-worktree.mjs" --repo-root "$ROOT" 2>>"$LOG_FILE")"; then
      local summary
      summary="$(printf '%s' "$checkpoint_json" | python3 - <<'PY'
import json, sys
raw = sys.stdin.read().strip()
try:
    payload = json.loads(raw) if raw else {}
except Exception:
    payload = {}
print(payload.get('summary') or 'checkpointed dirty worktree before restarting bounded loop')
PY
)"
      log "$summary"
      return 1
    fi

    local reason
    reason="$(printf '%s' "$checkpoint_json" | python3 - <<'PY'
import json, sys
raw = sys.stdin.read().strip()
try:
    payload = json.loads(raw) if raw else {}
except Exception:
    payload = {}
print(payload.get('reason') or 'dirty worktree checkpoint failed')
PY
)"
    echo "blocked: ${reason}"
    return 0
  fi

  return 1
}

start_child() {
  local blocker
  blocker="$(preflight_block_reason || true)"
  if [ -n "$blocker" ]; then
    child_pid=""
    child_started_epoch=0
    rm -f "$CHILD_PID_FILE"
    json_update "blocked" "preflight-blocked" "$blocker" "" "" "$restart_count" "$stale_restart_count"
    log "$blocker"
    return 1
  fi

  json_update "starting" "launching-child" "Starting bounded loop child" "" "" "$restart_count" "$stale_restart_count"
  CONTINUOUS_LOOP_SUPERVISOR_STATUS_FILE="$STATUS_FILE" \
  CONTINUOUS_LOOP_LOG_FILE="$LOG_FILE" \
  CONTINUOUS_LOOP_PID_FILE="$PID_FILE" \
  CONTINUOUS_LOOP_CHILD_PID_FILE="$CHILD_PID_FILE" \
  CONTINUOUS_LOOP_STOP_FILE="$STOP_FILE" \
  CONTINUOUS_LOOP_HEARTBEAT_TIMEOUT_SECONDS="$HEARTBEAT_TIMEOUT_SECONDS" \
  CONTINUOUS_LOOP_RESTART_DELAY_SECONDS="$RESTART_DELAY_SECONDS" \
  CONTINUOUS_LOOP_SUPERVISOR_POLL_SECONDS="$POLL_INTERVAL_SECONDS" \
  node "$ROOT/scripts/continuous-improvement-loop.mjs" --config "$CONFIG" --max-iterations 1 >> "$LOG_FILE" 2>&1 &
  child_pid=$!
  child_started_epoch="$(date +%s)"
  echo "$child_pid" > "$CHILD_PID_FILE"
  json_update "running" "child-running" "Bounded loop child running" "$child_pid" "" "$restart_count" "$stale_restart_count"
  log "child started pid=$child_pid"
  return 0
}

start_child || true

while true; do
  if [ -f "$STOP_FILE" ]; then
    json_update "stopping" "stop-requested" "Stop requested" "$child_pid" "" "$restart_count" "$stale_restart_count"
    stop_child "$child_pid"
    json_update "stopped" "stopped" "Supervisor stopped by operator" "" "" "$restart_count" "$stale_restart_count"
    log "stop requested; supervisor exiting"
    exit 0
  fi

  if [ -z "$child_pid" ]; then
    blocker="$(preflight_block_reason || true)"
    if [ -z "$blocker" ]; then
      blocker="Supervisor waiting for preflight blockers to clear"
    fi
    json_update "blocked" "preflight-blocked" "$blocker" "" "" "$restart_count" "$stale_restart_count"
    sleep "$POLL_INTERVAL_SECONDS"
    start_child || true
    continue
  fi

  if ! is_pid_alive "$child_pid"; then
    exit_code=0
    wait "$child_pid" || exit_code=$?
    child_pid=""
    child_started_epoch=0
    restart_count=$((restart_count + 1))
    json_update "restarting" "child-exited" "Loop child exited; restarting after delay" "" "$exit_code" "$restart_count" "$stale_restart_count"
    log "child exited code=$exit_code; restarting in ${RESTART_DELAY_SECONDS}s"
    sleep "$RESTART_DELAY_SECONDS"
    start_child || true
    sleep "$POLL_INTERVAL_SECONDS"
    continue
  fi

  now_epoch="$(date +%s)"
  if [ "$child_started_epoch" -gt 0 ] && [ $((now_epoch - child_started_epoch)) -gt "$CHILD_TIMEOUT_SECONDS" ]; then
    stale_restart_count=$((stale_restart_count + 1))
    restart_count=$((restart_count + 1))
    json_update "restarting" "child-timeout" "Loop child exceeded timeout; forcing restart" "$child_pid" "124" "$restart_count" "$stale_restart_count"
    log "child timeout pid=$child_pid age=$((now_epoch - child_started_epoch))s; restarting"
    stop_child "$child_pid"
    child_pid=""
    child_started_epoch=0
    sleep "$RESTART_DELAY_SECONDS"
    start_child || true
  else
    json_update "running" "supervising" "Supervisor healthy" "$child_pid" "" "$restart_count" "$stale_restart_count"
  fi

  sleep "$POLL_INTERVAL_SECONDS"
done
