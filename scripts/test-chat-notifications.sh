#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
chat_test_db=${TDF_CHAT_NOTIFICATION_DATABASE_URL:?Requires a fully migrated disposable database}
node --input-type=module - "$chat_test_db" "$repo_root/scripts/lib/disposable-postgres-url.mjs" <<'JS'
import { pathToFileURL } from 'node:url';
const { disposablePostgresUrl } = await import(pathToFileURL(process.argv[3]));
disposablePostgresUrl(process.argv[2], { ci: process.env.CI === 'true' });
JS
psql "$chat_test_db" -X -v ON_ERROR_STOP=1 -f "$repo_root/scripts/social/chat-notifications.sql"
psql "$chat_test_db" -X -v ON_ERROR_STOP=1 -f "$repo_root/tdf-hq/test/integration/interactions/notification-properties.sql"
