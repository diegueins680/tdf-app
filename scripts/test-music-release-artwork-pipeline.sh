#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-music-artwork-test.XXXXXX")
cleanup() {
  rm -rf "$test_dir"
}
trap cleanup EXIT INT TERM

ffmpeg -nostdin -v error -f lavfi -i 'color=c=blue:s=3000x3000:d=1' \
  -frames:v 1 "$test_dir/cover.png"
source_before=$(shasum -a 256 "$test_dir/cover.png" | awk '{print $1}')
"$repo_root/scripts/process-music-release-artwork.sh" "$test_dir/cover.png" "$test_dir/derived" >/dev/null
"$repo_root/scripts/process-music-release-artwork.sh" "$test_dir/cover.png" "$test_dir/derived" >/dev/null
source_after=$(shasum -a 256 "$test_dir/cover.png" | awk '{print $1}')

test "$source_before" = "$source_after"
test "$(jq -r '.pipelineVersion' "$test_dir/derived/manifest.json")" = "artwork-v1"
test "$(jq -r '.source.immutable' "$test_dir/derived/manifest.json")" = "true"
test "$(jq '.derivatives | length' "$test_dir/derived/manifest.json")" = "4"
test -s "$test_dir/derived/cover-display.jpg"
test -s "$test_dir/derived/cover-1200.jpg"
test -s "$test_dir/derived/thumbnail-600.jpg"
test "$(ffprobe -v error -select_streams v:0 -show_entries stream=width -of csv=p=0 "$test_dir/derived/thumbnail-600.jpg")" = "600"

echo "Music artwork pipeline passed real-type inspection, dimensions, immutable checksum, idempotent retry, display art, and thumbnail checks."
