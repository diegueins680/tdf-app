#!/bin/sh
set -eu
# Input is the normalized lossless derivative, never the private master.
[ "$#" = 4 ] || { echo 'Usage: preview.sh NORMALIZED_FLAC NEW_OUTPUT START_MS_OR_NULL DURATION_MS_OR_NULL' >&2; exit 2; }
source_file=$1 output_file=$2
[ -f "$source_file" ] && [ ! -e "$output_file" ] || { echo 'Preview requires an existing source and a new output path' >&2; exit 2; }
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
source_duration_ms=$(ffprobe -v error -show_entries format=duration -of json "$source_file" | jq -r '(.format.duration | tonumber) * 1000 | round')
spec=$(jq -nc --argjson sourceDurationMs "$source_duration_ms" --argjson start "$3" --argjson duration "$4" -f "$script_dir/music-preview-spec.jq")
start_seconds=$(printf '%s' "$spec" | jq -r '.startMs / 1000')
duration_seconds=$(printf '%s' "$spec" | jq -r '.durationMs / 1000')
ffmpeg -nostdin -v error -ss "$start_seconds" -i "$source_file" -t "$duration_seconds" \
  -map 0:a:0 -vn -map_metadata -1 -fflags +bitexact -flags:a +bitexact \
  -metadata "comment=tdf-preview-v2:$spec" \
  -ar 48000 -ac 2 -c:a aac -b:a 96k -movflags +faststart "$output_file"
printf '%s\n' "$spec"
