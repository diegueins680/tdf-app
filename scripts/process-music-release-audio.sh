#!/bin/sh
set -eu

source_file=${1:-}
output_dir=${2:-}
preview_start_ms=${3:-null}
preview_duration_ms=${4:-null}
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if [ -z "$source_file" ] || [ -z "$output_dir" ]; then
  echo "Usage: $0 PRIVATE_MASTER_FILE NEW_DERIVATIVE_DIRECTORY [PREVIEW_START_MS_OR_NULL PREVIEW_DURATION_MS_OR_NULL]" >&2
  exit 2
fi
if [ ! -f "$source_file" ]; then
  echo "Private master does not exist" >&2
  exit 2
fi

for command_name in ffmpeg ffprobe jq shasum; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Required command is unavailable: $command_name" >&2
    exit 2
  fi
done

source_sha256=$(shasum -a 256 "$source_file" | awk '{print $1}')
if [ -f "$output_dir/manifest.json" ]; then
  if jq -e --arg sourceSha256 "$source_sha256" --argjson start "$preview_start_ms" --argjson duration "$preview_duration_ms" \
    '.pipelineVersion == "audio-v2" and .source.sha256 == $sourceSha256 and .previewRequest == {startMs:$start,durationMs:$duration}' "$output_dir/manifest.json" >/dev/null; then
    jq -r '.derivatives[].path' "$output_dir/manifest.json" | while IFS= read -r file_name; do
      test -f "$output_dir/$file_name" || exit 1
    done
    echo "Audio derivatives already exist for $source_sha256"
    exit 0
  fi
  echo "Derivative directory belongs to a different source, pipeline or preview range; use a new versioned directory" >&2
  exit 1
fi
if [ -e "$output_dir" ]; then
  echo "Refusing a pre-existing derivative directory without a valid manifest" >&2
  exit 1
fi

file_bytes=$(wc -c < "$source_file" | tr -d ' ')
maximum_bytes=${MUSIC_MASTER_MAX_BYTES:-8589934592}
if [ "$file_bytes" -gt "$maximum_bytes" ]; then
  echo "Master exceeds MUSIC_MASTER_MAX_BYTES ($maximum_bytes)" >&2
  exit 1
fi

probe_json=$(ffprobe -v error -show_entries \
  stream=index,codec_type,codec_name,sample_fmt,sample_rate,channels,bits_per_sample,bits_per_raw_sample,duration \
  -show_entries format=format_name,duration,size -of json "$source_file")
audio_stream_count=$(printf '%s' "$probe_json" | jq '[.streams[] | select(.codec_type == "audio")] | length')
video_stream_count=$(printf '%s' "$probe_json" | jq '[.streams[] | select(.codec_type == "video")] | length')
if [ "$audio_stream_count" -ne 1 ] || [ "$video_stream_count" -ne 0 ]; then
  echo "Master must contain exactly one audio stream and no video streams" >&2
  exit 1
fi

codec=$(printf '%s' "$probe_json" | jq -r '.streams[] | select(.codec_type == "audio") | .codec_name')
sample_rate=$(printf '%s' "$probe_json" | jq -r '.streams[] | select(.codec_type == "audio") | .sample_rate | tonumber')
channels=$(printf '%s' "$probe_json" | jq -r '.streams[] | select(.codec_type == "audio") | .channels')
bit_depth=$(printf '%s' "$probe_json" | jq -r '
  .streams[] | select(.codec_type == "audio")
  | if ((.bits_per_raw_sample // "0") | tonumber) > 0 then (.bits_per_raw_sample | tonumber)
    elif (.bits_per_sample // 0) > 0 then .bits_per_sample
    elif .codec_name == "pcm_s16le" or .codec_name == "pcm_s16be" then 16
    elif .codec_name == "pcm_s24le" or .codec_name == "pcm_s24be" then 24
    elif .codec_name == "pcm_s32le" or .codec_name == "pcm_s32be" then 32
    else 0 end')
duration_seconds=$(printf '%s' "$probe_json" | jq -r '(.format.duration // (.streams[] | select(.codec_type == "audio") | .duration)) | tonumber')
case "$codec" in
  pcm_s16le|pcm_s24le|pcm_s32le|pcm_s16be|pcm_s24be|pcm_s32be|flac|alac) ;;
  *)
    echo "Unsupported master codec: $codec. Use lossless PCM WAV/AIFF, FLAC, or ALAC." >&2
    exit 1
    ;;
esac
if [ "$sample_rate" -lt 44100 ] || [ "$sample_rate" -gt 192000 ]; then
  echo "Sample rate must be between 44.1 kHz and 192 kHz" >&2
  exit 1
fi
if [ "$channels" -lt 1 ] || [ "$channels" -gt 8 ]; then
  echo "Channel count must be between 1 and 8" >&2
  exit 1
fi
case "$bit_depth" in
  16|24|32) ;;
  *)
    echo "Bit depth must be detectable as 16, 24, or 32 bits" >&2
    exit 1
    ;;
esac
if ! printf '%s' "$duration_seconds" | jq -e '. >= 1 and . <= 21600' >/dev/null; then
  echo "Duration must be between 1 second and 6 hours" >&2
  exit 1
fi
preview_spec=$(jq -nc --argjson sourceDurationMs "$(printf '%s' "$duration_seconds" | jq '. * 1000 | round')" \
  --argjson start "$preview_start_ms" --argjson duration "$preview_duration_ms" -f "$script_dir/music-preview-spec.jq")

output_parent=$(dirname "$output_dir")
mkdir -p "$output_parent"
processing_dir=$(mktemp -d "$output_parent/.music-audio-processing.XXXXXX")
cleanup() {
  rm -rf "$processing_dir"
}
trap cleanup EXIT INT TERM

printf '%s\n' "$probe_json" | jq '.' > "$processing_dir/inspection.json"
ffmpeg -nostdin -hide_banner -nostats -i "$source_file" \
  -map 0:a:0 -af 'loudnorm=I=-14:TP=-1:LRA=11:print_format=json' -f null - \
  2> "$processing_dir/loudness-raw.log"
sed -n '/^{/,/^}/p' "$processing_dir/loudness-raw.log" > "$processing_dir/loudness.json"
if ! jq -e '.input_i and .input_tp and .input_lra and .input_thresh and .target_offset' "$processing_dir/loudness.json" >/dev/null; then
  echo "FFmpeg did not produce a complete EBU R128 measurement" >&2
  exit 1
fi

measured_i=$(jq -r '.input_i' "$processing_dir/loudness.json")
measured_tp=$(jq -r '.input_tp' "$processing_dir/loudness.json")
measured_lra=$(jq -r '.input_lra' "$processing_dir/loudness.json")
measured_thresh=$(jq -r '.input_thresh' "$processing_dir/loudness.json")
target_offset=$(jq -r '.target_offset' "$processing_dir/loudness.json")
normalization_filter="loudnorm=I=-14:TP=-1:LRA=11:measured_I=$measured_i:measured_TP=$measured_tp:measured_LRA=$measured_lra:measured_thresh=$measured_thresh:offset=$target_offset:linear=true:print_format=summary"

transcode_aac() {
  bitrate=$1
  destination=$2
  ffmpeg -nostdin -v error -i "$source_file" -map 0:a:0 -vn -map_metadata -1 \
    -fflags +bitexact -flags:a +bitexact -af "$normalization_filter" \
    -ar 48000 -ac 2 -c:a aac -b:a "$bitrate" -movflags +faststart "$destination"
}

transcode_aac 96k "$processing_dir/stream-low.m4a"
transcode_aac 160k "$processing_dir/stream-medium.m4a"
transcode_aac 256k "$processing_dir/stream-high.m4a"
ffmpeg -nostdin -v error -i "$source_file" -map 0:a:0 -vn -map_metadata -1 \
  -fflags +bitexact -flags:a +bitexact -af "$normalization_filter" \
  -c:a flac -compression_level 8 "$processing_dir/stream-lossless.flac"

sh "$script_dir/process-music-release-preview.sh" "$processing_dir/stream-lossless.flac" \
  "$processing_dir/preview.m4a" "$preview_start_ms" "$preview_duration_ms" >/dev/null

source_sha256_after=$(shasum -a 256 "$source_file" | awk '{print $1}')
if [ "$source_sha256_after" != "$source_sha256" ]; then
  echo "Master checksum changed while processing; all derivatives are quarantined" >&2
  exit 1
fi

for derivative in stream-low.m4a stream-medium.m4a stream-high.m4a stream-lossless.flac preview.m4a inspection.json loudness.json; do
  derivative_sha=$(shasum -a 256 "$processing_dir/$derivative" | awk '{print $1}')
  derivative_bytes=$(wc -c < "$processing_dir/$derivative" | tr -d ' ')
  jq -nc --arg path "$derivative" --arg sha256 "$derivative_sha" --argjson bytes "$derivative_bytes" \
    '{path:$path,sha256:$sha256,bytes:$bytes}'
done > "$processing_dir/derivatives.ndjson"

derivatives_json=$(jq -s '.' "$processing_dir/derivatives.ndjson")
jq -n \
  --arg sourceSha256 "$source_sha256" \
  --argjson sourceBytes "$file_bytes" \
  --arg sourceCodec "$codec" \
  --argjson sampleRate "$sample_rate" \
  --argjson channels "$channels" \
  --argjson bitDepth "$bit_depth" \
  --argjson durationSeconds "$duration_seconds" \
  --argjson loudness "$(jq -c '.' "$processing_dir/loudness.json")" \
  --argjson derivatives "$derivatives_json" \
  --argjson preview "$preview_spec" --argjson start "$preview_start_ms" --argjson duration "$preview_duration_ms" \
  '{manifestVersion:1,pipelineVersion:"audio-v2",preview:$preview,previewRequest:{startMs:$start,durationMs:$duration},source:{sha256:$sourceSha256,bytes:$sourceBytes,codec:$sourceCodec,sampleRate:$sampleRate,channels:$channels,bitDepth:$bitDepth,durationSeconds:$durationSeconds,immutable:true},normalization:{targetIntegratedLufs:-14,targetTruePeakDbtp:-1,masterModified:false,measurement:$loudness},derivatives:$derivatives}' \
  > "$processing_dir/manifest.json"
rm "$processing_dir/loudness-raw.log" "$processing_dir/derivatives.ndjson"
mv "$processing_dir" "$output_dir"
trap - EXIT INT TERM
echo "Generated verified audio derivatives in $output_dir; master SHA-256 remains $source_sha256"
