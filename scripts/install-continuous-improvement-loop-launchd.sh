#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG="${1:-${CONTINUOUS_LOOP_CONFIG:-$ROOT/scripts/continuous-improvement-loop.codex.json}}"
LABEL="${CONTINUOUS_LOOP_LAUNCHD_LABEL:-ai.openclaw.tdf-app.continuous-improvement-loop}"
PLIST_PATH="${CONTINUOUS_LOOP_LAUNCHD_PLIST:-$HOME/Library/LaunchAgents/${LABEL}.plist}"
STATE_DIR="${CONTINUOUS_LOOP_STATE_DIR:-$ROOT/tmp/continuous-improvement-loop}"
LOG_FILE="${CONTINUOUS_LOOP_LOG_FILE:-$ROOT/tmp/continuous-improvement-loop.log}"
PID_FILE="${CONTINUOUS_LOOP_PID_FILE:-$ROOT/tmp/continuous-improvement-loop.pid}"
CHILD_PID_FILE="${CONTINUOUS_LOOP_CHILD_PID_FILE:-$STATE_DIR/child.pid}"
STATUS_FILE="${CONTINUOUS_LOOP_STATUS_FILE:-$STATE_DIR/status.json}"
STOP_FILE="${CONTINUOUS_LOOP_STOP_FILE:-$STATE_DIR/stop}"
HEARTBEAT_FILE="${CONTINUOUS_LOOP_HEARTBEAT_FILE:-$STATE_DIR/heartbeat.txt}"
SUPERVISOR="$ROOT/scripts/continuous-improvement-loop-supervisor.sh"
GUI_DOMAIN="gui/$(id -u)"
PATH_VALUE="${PATH:-/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin}"
NODE_BIN_VALUE="${CONTINUOUS_LOOP_NODE_BIN:-$(command -v node 2>/dev/null || true)}"

mkdir -p "$HOME/Library/LaunchAgents" "$STATE_DIR" "$(dirname "$LOG_FILE")"

cat > "$PLIST_PATH" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>${LABEL}</string>
  <key>ProgramArguments</key>
  <array>
    <string>${SUPERVISOR}</string>
    <string>${CONFIG}</string>
  </array>
  <key>WorkingDirectory</key>
  <string>${ROOT}</string>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <true/>
  <key>StandardOutPath</key>
  <string>${LOG_FILE}</string>
  <key>StandardErrorPath</key>
  <string>${LOG_FILE}</string>
  <key>EnvironmentVariables</key>
  <dict>
    <key>PATH</key>
    <string>${PATH_VALUE}</string>
    <key>CONTINUOUS_LOOP_CONFIG</key>
    <string>${CONFIG}</string>
    <key>CONTINUOUS_LOOP_LOG_FILE</key>
    <string>${LOG_FILE}</string>
    <key>CONTINUOUS_LOOP_PID_FILE</key>
    <string>${PID_FILE}</string>
    <key>CONTINUOUS_LOOP_CHILD_PID_FILE</key>
    <string>${CHILD_PID_FILE}</string>
    <key>CONTINUOUS_LOOP_STATUS_FILE</key>
    <string>${STATUS_FILE}</string>
    <key>CONTINUOUS_LOOP_STOP_FILE</key>
    <string>${STOP_FILE}</string>
    <key>CONTINUOUS_LOOP_HEARTBEAT_FILE</key>
    <string>${HEARTBEAT_FILE}</string>
    <key>CONTINUOUS_LOOP_NODE_BIN</key>
    <string>${NODE_BIN_VALUE}</string>
  </dict>
</dict>
</plist>
EOF

if launchctl print "${GUI_DOMAIN}/${LABEL}" >/dev/null 2>&1; then
  launchctl bootout "$GUI_DOMAIN" "$PLIST_PATH" >/dev/null 2>&1 || true
fi

launchctl bootstrap "$GUI_DOMAIN" "$PLIST_PATH"
launchctl kickstart -k "${GUI_DOMAIN}/${LABEL}"

echo "Installed and started launchd runner."
echo "Label: ${LABEL}"
echo "Plist: ${PLIST_PATH}"
echo "Log: ${LOG_FILE}"
echo "PID file: ${PID_FILE}"
echo "Child PID file: ${CHILD_PID_FILE}"
echo "Status: ${STATUS_FILE}"
echo "Heartbeat: ${HEARTBEAT_FILE}"
