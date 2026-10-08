#!/bin/sh
set -eu

source_file=${1:-}
output_dir=${2:-}
if [ -z "$source_file" ] || [ -z "$output_dir" ]; then
  echo "Usage: $0 PRIVATE_COVER_FILE NEW_DERIVATIVE_DIRECTORY" >&2
  exit 2
fi
if [ ! -f "$source_file" ]; then
  echo "Private cover file does not exist" >&2
  exit 2
fi

source_sha256=$(shasum -a 256 "$source_file" | awk '{print $1}')
if [ -f "$output_dir/manifest.json" ]; then
  if jq -e --arg sourceSha256 "$source_sha256" '.source.sha256 == $sourceSha256' "$output_dir/manifest.json" >/dev/null; then
    echo "Artwork derivatives already exist for $source_sha256"
    exit 0
  fi
  echo "Artwork directory belongs to a different source; use a new versioned directory" >&2
  exit 1
fi
if [ -e "$output_dir" ]; then
  echo "Refusing a pre-existing artwork directory without a valid manifest" >&2
  exit 1
fi

probe_json=$(ffprobe -v error -select_streams v:0 \
  -show_entries stream=codec_name,width,height,pix_fmt,nb_frames:format=format_name,size \
  -of json "$source_file")
stream_count=$(printf '%s' "$probe_json" | jq '.streams | length')
codec=$(printf '%s' "$probe_json" | jq -r '.streams[0].codec_name // ""')
width=$(printf '%s' "$probe_json" | jq -r '.streams[0].width // 0')
height=$(printf '%s' "$probe_json" | jq -r '.streams[0].height // 0')
frames=$(printf '%s' "$probe_json" | jq -r '.streams[0].nb_frames // "1"')
if [ "$stream_count" -ne 1 ]; then
  echo "Cover must contain exactly one decodable image stream" >&2
  exit 1
fi
case "$codec" in
  mjpeg|png) ;;
  *)
    echo "Unsupported cover type: $codec. Use a real JPEG or PNG image." >&2
    exit 1
    ;;
esac
if [ "$width" -ne "$height" ] || [ "$width" -lt 3000 ] || [ "$width" -gt 10000 ]; then
  echo "Cover must be square and between 3000x3000 and 10000x10000 pixels" >&2
  exit 1
fi
if [ "$frames" != "1" ] && [ "$frames" != "N/A" ]; then
  echo "Animated cover art is not supported in the audio-release phase" >&2
  exit 1
fi

output_parent=$(dirname "$output_dir")
mkdir -p "$output_parent"
processing_dir=$(mktemp -d "$output_parent/.music-artwork-processing.XXXXXX")
cleanup() {
  rm -rf "$processing_dir"
}
trap cleanup EXIT INT TERM

printf '%s\n' "$probe_json" | jq '.' > "$processing_dir/inspection.json"
render_size() {
  size=$1
  destination=$2
  ffmpeg -nostdin -v error -i "$source_file" -frames:v 1 -map_metadata -1 \
    -vf "scale=$size:$size:flags=lanczos" -c:v mjpeg -q:v 2 -pix_fmt yuvj420p "$destination"
}
render_size 3000 "$processing_dir/cover-display.jpg"
render_size 1200 "$processing_dir/cover-1200.jpg"
render_size 600 "$processing_dir/thumbnail-600.jpg"

source_sha256_after=$(shasum -a 256 "$source_file" | awk '{print $1}')
if [ "$source_sha256_after" != "$source_sha256" ]; then
  echo "Cover checksum changed while processing; derivatives are quarantined" >&2
  exit 1
fi

for derivative in cover-display.jpg cover-1200.jpg thumbnail-600.jpg inspection.json; do
  derivative_sha=$(shasum -a 256 "$processing_dir/$derivative" | awk '{print $1}')
  derivative_bytes=$(wc -c < "$processing_dir/$derivative" | tr -d ' ')
  jq -nc --arg path "$derivative" --arg sha256 "$derivative_sha" --argjson bytes "$derivative_bytes" \
    '{path:$path,sha256:$sha256,bytes:$bytes}'
done > "$processing_dir/derivatives.ndjson"
derivatives_json=$(jq -s '.' "$processing_dir/derivatives.ndjson")
jq -n --arg sourceSha256 "$source_sha256" --arg sourceCodec "$codec" \
  --argjson width "$width" --argjson height "$height" --argjson derivatives "$derivatives_json" \
  '{manifestVersion:1,pipelineVersion:"artwork-v1",source:{sha256:$sourceSha256,codec:$sourceCodec,width:$width,height:$height,immutable:true},derivatives:$derivatives}' \
  > "$processing_dir/manifest.json"
rm "$processing_dir/derivatives.ndjson"
mv "$processing_dir" "$output_dir"
trap - EXIT INT TERM
echo "Generated verified artwork derivatives in $output_dir; source SHA-256 remains $source_sha256"

