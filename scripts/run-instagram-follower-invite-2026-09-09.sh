#!/bin/sh
set -eu

cd /Users/diegosaa/GitHub/tdf-app

chrome_bin='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
profile_dir='/Users/diegosaa/.openclaw/browser/openclaw/user-data'
cdp_helper='/Users/diegosaa/GitHub/tdf-app/scripts/instagram-openclaw-cdp.mjs'
chrome_log='/private/tmp/tdf-instagram-preflight-2026-09-09.log'

instagram_authenticated() {
  TDF_INSTAGRAM_CDP_TIMEOUT_MS=4000 \
    /usr/local/bin/node "$cdp_helper" 'true' >/dev/null 2>&1
}

if ! instagram_authenticated; then
  "$chrome_bin" \
    --remote-debugging-port=18800 \
    --user-data-dir="$profile_dir" \
    '--profile-directory=Profile 1' \
    --no-first-run \
    --no-default-browser-check \
    --disable-sync \
    --disable-background-networking \
    --disable-component-update \
    '--disable-features=Translate,MediaRouter' \
    --disable-session-crashed-bubble \
    --hide-crash-restore-bubble \
    --password-store=basic \
    --no-proxy-server \
    https://www.instagram.com/direct/inbox/ \
    >"$chrome_log" 2>&1 &

  preflight_deadline=$(($(date +%s) + 45))
  while [ "$(date +%s)" -lt "$preflight_deadline" ]; do
    if instagram_authenticated; then
      break
    fi
    sleep 1
  done
fi

if instagram_authenticated; then
  echo 'Instagram preflight: authenticated tdf.records.label target is ready.'
else
  echo 'Instagram preflight: authenticated target is unavailable; executor must stop without sending.'
fi

exec /usr/local/bin/codex exec \
  --ephemeral \
  --approve-for-me \
  -C /Users/diegosaa/GitHub/tdf-app \
  - < docs/campaigns/tu-escena-conectada-seguidores-2026-09-09-prompt.md
