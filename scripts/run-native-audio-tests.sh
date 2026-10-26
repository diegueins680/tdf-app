#!/usr/bin/env bash
# Isolated Linux CI audio renderer for the generated TDF native-media fixtures.
# No TCP listener, physical audio device, shared PulseAudio daemon, or media mock.
set -euo pipefail
[[ "$(uname -s)" == Linux ]] || { echo 'Linux CI required' >&2; exit 2; }
[[ $# -gt 0 ]] || { echo 'Pass the native test command as arguments' >&2; exit 2; }
command -v pulseaudio >/dev/null
command -v pactl >/dev/null
pulse_runtime=$(mktemp -d /tmp/tdf-native-pulse.XXXXXX)
chmod 700 "$pulse_runtime"
export PULSE_RUNTIME_PATH="$pulse_runtime"
export PULSE_SERVER="unix:$pulse_runtime/native"
export PULSE_SINK=tdf_native
export PULSE_COOKIE="$pulse_runtime/cookie"
head -c 256 /dev/urandom > "$PULSE_COOKIE"
chmod 600 "$PULSE_COOKIE"
mkdir -p artifacts/native-music
pulse_pid=''
cleanup() {
  if [[ -n "$pulse_pid" ]] && kill -0 "$pulse_pid" 2>/dev/null; then
    kill "$pulse_pid"
    wait "$pulse_pid" || true
  fi
  # The runtime directory is intentionally left in the disposable CI container;
  # it contains only this daemon's socket and throwaway authentication cookie.
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
pulseaudio --daemonize=no --exit-idle-time=-1 --use-pid-file=no --realtime=no \
  --log-target=stderr -n \
  --load="module-native-protocol-unix socket=$pulse_runtime/native auth-cookie=$PULSE_COOKIE" \
  --load='module-null-sink sink_name=tdf_native rate=48000 channels=2' \
  > artifacts/native-music/pulse-server.log 2>&1 &
pulse_pid=$!
ready=false
for attempt in {1..50}; do
  if pactl info > artifacts/native-music/pulse-info.txt 2>/dev/null; then ready=true; break; fi
  if ! kill -0 "$pulse_pid" 2>/dev/null; then cat artifacts/native-music/pulse-server.log >&2; exit 1; fi
  sleep 0.1
done
[[ "$ready" == true ]] || { cat artifacts/native-music/pulse-server.log >&2; exit 1; }
pactl set-default-sink tdf_native
pactl list short sinks > artifacts/native-music/pulse-sinks.txt
pactl list short sources > artifacts/native-music/pulse-sources.txt
"$@"
