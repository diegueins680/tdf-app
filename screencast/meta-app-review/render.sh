#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./screencast/meta-app-review/render.sh \
#     --desktop screencast/meta-app-review/output/<desktop>.webm \
#     --phone /path/to/android.mp4 \
#     --out screencast/meta-app-review/output/final.mp4

DESKTOP=""
PHONE=""
OUT="screencast/meta-app-review/output/final.mp4"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --desktop) DESKTOP="$2"; shift 2;;
    --phone) PHONE="$2"; shift 2;;
    --out) OUT="$2"; shift 2;;
    *) echo "Unknown arg: $1"; exit 1;;
  esac
done

if [[ -z "$DESKTOP" ]]; then
  echo "Missing --desktop <file>"; exit 1
fi

mkdir -p "$(dirname "$OUT")"

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

# 1) Trim a little head/tail (adjust as needed)
ffmpeg -y -i "$DESKTOP" -ss 00:00:01.0 -to 00:10:00 -c copy "$TMPDIR/desktop_trim.webm" || true
# If copy-trim fails (non-keyframe), re-encode:
if [[ ! -s "$TMPDIR/desktop_trim.webm" ]]; then
  ffmpeg -y -i "$DESKTOP" -ss 00:00:01.0 -to 00:10:00 -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_trim.mp4"
else
  ffmpeg -y -i "$TMPDIR/desktop_trim.webm" -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_trim.mp4"
fi

# 2) Optional: normalize size
ffmpeg -y -i "$TMPDIR/desktop_trim.mp4" -vf "scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" \
  -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_norm.mp4"

# 3) If phone clip provided, concat with a simple hard cut.
if [[ -n "$PHONE" ]]; then
  ffmpeg -y -i "$PHONE" -vf "scale=1080:1920:force_original_aspect_ratio=decrease,pad=1080:1920:(ow-iw)/2:(oh-ih)/2" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/phone_norm.mp4"

  # Put phone clip as picture-in-picture on the right for the last 10 seconds of desktop.
  # (Keeps context while showing "native client".) Adjust timing once we see real durations.
  ffmpeg -y -i "$TMPDIR/desktop_norm.mp4" -i "$TMPDIR/phone_norm.mp4" \
    -filter_complex "[1:v]scale=540:-2[ph];[0:v][ph]overlay=W-w-40:H-h-40:enable='between(t,0,1e9)'" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/with_phone.mp4"

  cp "$TMPDIR/with_phone.mp4" "$OUT"
else
  cp "$TMPDIR/desktop_norm.mp4" "$OUT"
fi

echo "Wrote: $OUT"
