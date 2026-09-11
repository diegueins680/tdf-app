#!/bin/sh
set -eu

cd /Users/diegosaa/GitHub/tdf-app
exec /usr/local/bin/codex exec \
  --ephemeral \
  --approve-for-me \
  -C /Users/diegosaa/GitHub/tdf-app \
  - < docs/campaigns/tu-escena-conectada-seguidores-2026-09-08-prompt.md
