#!/bin/sh
set -eu

poll_seconds=${MUSIC_WORKER_POLL_SECONDS:-5}
case "$poll_seconds" in
  ''|*[!0-9]*) echo "MUSIC_WORKER_POLL_SECONDS must be a positive integer" >&2; exit 2 ;;
esac
if [ "$poll_seconds" -lt 1 ] || [ "$poll_seconds" -gt 300 ]; then
  echo "MUSIC_WORKER_POLL_SECONDS must be between 1 and 300" >&2
  exit 2
fi

worker_once=${MUSIC_WORKER_ONCE_SCRIPT:-"$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/run-music-release-worker-once.sh"}
stop_requested=false
active_pid=''
request_stop() {
  stop_requested=true
  if [ -n "$active_pid" ]; then kill -TERM "$active_pid" 2>/dev/null || true; fi
}
trap request_stop INT TERM

while [ "$stop_requested" = false ]; do
  "$worker_once" &
  active_pid=$!
  result=0
  wait "$active_pid" || result=$?
  if [ "$stop_requested" = true ]; then
    wait "$active_pid" 2>/dev/null || true
    break
  fi
  active_pid=''
  if [ "$result" -ne 0 ]; then
    echo "Music worker iteration failed; retrying after ${poll_seconds}s" >&2
  fi
  [ "$stop_requested" = false ] || break
  sleep "$poll_seconds" &
  active_pid=$!
  wait "$active_pid" || true
  active_pid=''
done
