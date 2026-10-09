#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-music-audio-test.XXXXXX")
cleanup() {
  rm -rf "$test_dir"
}
trap cleanup EXIT INT TERM

ffmpeg -nostdin -v error -f lavfi \
  -i 'sine=frequency=440:sample_rate=48000:duration=2' \
  -c:a pcm_s24le "$test_dir/master.wav"
master_before=$(shasum -a 256 "$test_dir/master.wav" | awk '{print $1}')

"$repo_root/scripts/process-music-release-audio.sh" "$test_dir/master.wav" "$test_dir/derived" >/dev/null
"$repo_root/scripts/process-music-release-audio.sh" "$test_dir/master.wav" "$test_dir/derived" >/dev/null

master_after=$(shasum -a 256 "$test_dir/master.wav" | awk '{print $1}')
test "$master_before" = "$master_after"
test "$(jq -r '.pipelineVersion' "$test_dir/derived/manifest.json")" = "audio-v2"
test "$(jq -r '.source.codec' "$test_dir/derived/manifest.json")" = "pcm_s24le"
test "$(jq -r '.source.bitDepth' "$test_dir/derived/manifest.json")" = "24"
test "$(jq -r '.source.immutable' "$test_dir/derived/manifest.json")" = "true"
test "$(jq -r '.normalization.masterModified' "$test_dir/derived/manifest.json")" = "false"
test "$(jq '.derivatives | length' "$test_dir/derived/manifest.json")" = "7"
test -s "$test_dir/derived/stream-low.m4a"
test -s "$test_dir/derived/stream-medium.m4a"
test -s "$test_dir/derived/stream-high.m4a"
test -s "$test_dir/derived/stream-lossless.flac"
test -s "$test_dir/derived/preview.m4a"

echo "Music audio pipeline passed real-type inspection, immutable checksum, loudness measurement, reproducible retry, AAC/FLAC derivatives, preview, and manifest checks."
