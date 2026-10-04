#!/bin/sh
set -eu
umask 077

# Root-owned paths and a non-blocking lock prevent concurrent partial archives.
cd /opt/tdf/production
test -f compose.yaml
install -d -m 0700 /opt/tdf/backups
exec 9>/opt/tdf/backups/backup.lock
flock -n 9
stamp=$(date -u +%Y%m%dT%H%M%SZ)
archive="/opt/tdf/backups/tdf-hq-${stamp}.pgdump"
test ! -e "$archive"
test ! -e "$archive.partial"
docker compose exec -T db pg_dump -U postgres -d tdf_hq -Fc > "$archive.partial"
test -s "$archive.partial"
docker compose exec -T db pg_restore --list < "$archive.partial" > /dev/null
mv "$archive.partial" "$archive"
sha256sum "$archive" > "$archive.sha256"
printf 'Completed logical backup: %s\n' "$archive"
# Keep archives until the operator verifies retention and an off-host copy.
