#!/bin/sh
set -eu

: "${DATABASE_URL:?DATABASE_URL is required}"

cursor=${TDF_MUSIC_LEGACY_CURSOR:-0}
batch_size=${TDF_MUSIC_LEGACY_BATCH_SIZE:-100}

case "$cursor" in
  ''|*[!0-9]*) echo "TDF_MUSIC_LEGACY_CURSOR must be a non-negative integer" >&2; exit 2 ;;
esac
case "$batch_size" in
  ''|*[!0-9]*) echo "TDF_MUSIC_LEGACY_BATCH_SIZE must be an integer between 1 and 1000" >&2; exit 2 ;;
esac
if [ "$batch_size" -lt 1 ] || [ "$batch_size" -gt 1000 ]; then
  echo "TDF_MUSIC_LEGACY_BATCH_SIZE must be an integer between 1 and 1000" >&2
  exit 2
fi

while :; do
  result=$(psql "$DATABASE_URL" -X -qAt -v ON_ERROR_STOP=1 \
    -c "SELECT scanned_count || '|' || next_cursor FROM music_scan_legacy_release_sanitation($cursor,$batch_size);")
  scanned=${result%%|*}
  next_cursor=${result#*|}

  case "$scanned:$next_cursor" in
    *[!0-9:]*|:*|*:) echo "Unexpected scanner response: $result" >&2; exit 1 ;;
  esac

  cursor=$next_cursor
  echo "scanned=$scanned resume_cursor=$cursor"
  [ "$scanned" -lt "$batch_size" ] && break
done

echo "Legacy scan complete. Review music_legacy_release_sanitation_queue; no canonical metadata was fabricated."
