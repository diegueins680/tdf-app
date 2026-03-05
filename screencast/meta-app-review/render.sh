#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ./screencast/meta-app-review/render.sh \
#     --desktop screencast/meta-app-review/output/<desktop>.webm \
#     [--phone /path/to/android.mp4] \
#     [--captions /path/to/captions.srt] \
#     [--autocaptions] \
#     [--narration-text /path/to/narration.txt] \
#     [--narration-audio /path/to/narration.wav] \
#     [--voice "Samantha"] \
#     [--out screencast/meta-app-review/output/final.mp4]

DESKTOP=""
PHONE=""
OUT="screencast/meta-app-review/output/final.mp4"
CAPTIONS=""
AUTO_CAPTIONS=0
NARRATION_TEXT=""
NARRATION_AUDIO=""
VOICE="${TDF_SCREENCAST_VOICE:-Samantha}"
KEEP_TEMP=0

usage() {
  cat <<'USAGE'
Usage:
  ./screencast/meta-app-review/render.sh --desktop <file> [options]

Options:
  --desktop <file>         Desktop recording (.webm/.mp4). Required.
  --phone <file>           Optional phone clip to show as picture-in-picture.
  --captions <file>        Burn in an existing .srt captions file.
  --autocaptions           Auto-generate .srt captions using whisper CLI.
  --narration-text <file>  Generate narration audio from a text file (macOS say).
  --narration-audio <file> Mix pre-recorded narration track into final video.
  --voice <name>           Voice used by macOS say (default: Samantha).
  --out <file>             Output path (default: screencast/meta-app-review/output/final.mp4).
  --keep-temp              Keep temp working files for debugging.
  -h, --help               Show this help.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --desktop) DESKTOP="$2"; shift 2;;
    --phone) PHONE="$2"; shift 2;;
    --captions) CAPTIONS="$2"; shift 2;;
    --autocaptions) AUTO_CAPTIONS=1; shift;;
    --narration-text) NARRATION_TEXT="$2"; shift 2;;
    --narration-audio) NARRATION_AUDIO="$2"; shift 2;;
    --voice) VOICE="$2"; shift 2;;
    --out) OUT="$2"; shift 2;;
    --keep-temp) KEEP_TEMP=1; shift;;
    -h|--help) usage; exit 0;;
    *) echo "Unknown arg: $1"; usage; exit 1;;
  esac
done

if [[ -z "$DESKTOP" ]]; then
  echo "Missing --desktop <file>"
  usage
  exit 1
fi
if [[ ! -f "$DESKTOP" ]]; then
  echo "Desktop file not found: $DESKTOP"
  exit 1
fi
if [[ -n "$PHONE" && ! -f "$PHONE" ]]; then
  echo "Phone file not found: $PHONE"
  exit 1
fi
if [[ -n "$CAPTIONS" && ! -f "$CAPTIONS" ]]; then
  echo "Captions file not found: $CAPTIONS"
  exit 1
fi
if [[ "$AUTO_CAPTIONS" -eq 1 && -n "$CAPTIONS" ]]; then
  echo "Use either --captions or --autocaptions, not both."
  exit 1
fi
if [[ -n "$NARRATION_TEXT" && ! -f "$NARRATION_TEXT" ]]; then
  echo "Narration text file not found: $NARRATION_TEXT"
  exit 1
fi
if [[ -n "$NARRATION_AUDIO" && ! -f "$NARRATION_AUDIO" ]]; then
  echo "Narration audio file not found: $NARRATION_AUDIO"
  exit 1
fi
if [[ -n "$NARRATION_TEXT" && -n "$NARRATION_AUDIO" ]]; then
  echo "Use either --narration-text or --narration-audio, not both."
  exit 1
fi
if ! command -v ffmpeg >/dev/null 2>&1; then
  echo "ffmpeg is required."
  exit 1
fi

mkdir -p "$(dirname "$OUT")"

TMPDIR="$(mktemp -d)"
cleanup() {
  if [[ "$KEEP_TEMP" -eq 1 ]]; then
    echo "Temp files kept at: $TMPDIR"
  else
    rm -rf "$TMPDIR"
  fi
}
trap cleanup EXIT

BASE_VIDEO="$TMPDIR/base.mp4"
WORK_VIDEO="$TMPDIR/work.mp4"

file_size_bytes() {
  local path="$1"
  if stat -f%z "$path" >/dev/null 2>&1; then
    stat -f%z "$path"
  else
    stat -c%s "$path"
  fi
}

# 1) Trim a little head/tail (adjust as needed)
ffmpeg -y -i "$DESKTOP" -ss 00:00:01.0 -to 00:10:00 -c copy "$TMPDIR/desktop_trim.webm" >/dev/null 2>&1 || true
if [[ ! -s "$TMPDIR/desktop_trim.webm" ]]; then
  ffmpeg -y -ss 00:00:01.0 -to 00:10:00 -i "$DESKTOP" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_trim.mp4"
else
  ffmpeg -y -i "$TMPDIR/desktop_trim.webm" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_trim.mp4"
fi

# 2) Normalize desktop size
ffmpeg -y -i "$TMPDIR/desktop_trim.mp4" \
  -vf "scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" \
  -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/desktop_norm.mp4"

# 3) Optional phone picture-in-picture
if [[ -n "$PHONE" ]]; then
  ffmpeg -y -i "$PHONE" \
    -vf "scale=1080:1920:force_original_aspect_ratio=decrease,pad=1080:1920:(ow-iw)/2:(oh-ih)/2" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$TMPDIR/phone_norm.mp4"

  ffmpeg -y -i "$TMPDIR/desktop_norm.mp4" -i "$TMPDIR/phone_norm.mp4" \
    -filter_complex "[1:v]scale=540:-2[ph];[0:v][ph]overlay=W-w-40:H-h-40:enable='between(t,0,1e9)'" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$BASE_VIDEO"
else
  cp "$TMPDIR/desktop_norm.mp4" "$BASE_VIDEO"
fi

# 4) Optional auto-captions via whisper CLI
if [[ "$AUTO_CAPTIONS" -eq 1 ]]; then
  if ! command -v whisper >/dev/null 2>&1; then
    echo "--autocaptions requires 'whisper' on PATH."
    echo "Install openai-whisper or provide --captions <file>."
    exit 1
  fi
  WHISPER_MODEL="${TDF_WHISPER_MODEL:-medium}"
  whisper "$BASE_VIDEO" --language English --task transcribe --model "$WHISPER_MODEL" --output_format srt --output_dir "$TMPDIR" >/dev/null 2>&1
  generated_srt="$TMPDIR/$(basename "${BASE_VIDEO%.*}").srt"
  if [[ ! -f "$generated_srt" ]]; then
    generated_srt="$(find "$TMPDIR" -maxdepth 1 -type f -name '*.srt' | head -n 1 || true)"
  fi
  if [[ -z "$generated_srt" || ! -f "$generated_srt" ]]; then
    echo "Whisper did not generate an .srt file."
    exit 1
  fi
  CAPTIONS="$generated_srt"
fi

# 5) Optional caption burn-in
if [[ -n "$CAPTIONS" ]]; then
  cp "$CAPTIONS" "$TMPDIR/captions.srt"
  ffmpeg -y -i "$BASE_VIDEO" \
    -vf "subtitles=$TMPDIR/captions.srt:force_style='FontName=Arial,FontSize=24,OutlineColour=&H50000000,BorderStyle=3,MarginV=42'" \
    -c:v libx264 -preset veryfast -crf 20 -c:a aac -b:a 160k "$WORK_VIDEO"
  mv "$WORK_VIDEO" "$BASE_VIDEO"
fi

# 6) Optional narration generation (macOS)
if [[ -n "$NARRATION_TEXT" ]]; then
  if ! command -v say >/dev/null 2>&1; then
    echo "--narration-text requires macOS 'say' command."
    echo "Use --narration-audio <file> on non-macOS systems."
    exit 1
  fi
  say -v "$VOICE" -f "$NARRATION_TEXT" -o "$TMPDIR/narration.aiff"
  narration_raw_size="$(file_size_bytes "$TMPDIR/narration.aiff")"
  if [[ "${narration_raw_size:-0}" -le 4096 ]]; then
    echo "Generated narration appears empty for voice '$VOICE'."
    echo "Try another voice or pass --narration-audio <file>."
    exit 1
  fi
  ffmpeg -y -i "$TMPDIR/narration.aiff" -ac 2 -ar 48000 "$TMPDIR/narration.wav"
  narration_wav_size="$(file_size_bytes "$TMPDIR/narration.wav")"
  if [[ "${narration_wav_size:-0}" -le 2048 ]]; then
    echo "Narration conversion produced an empty audio track."
    echo "Provide --narration-audio <file> instead."
    exit 1
  fi
  NARRATION_AUDIO="$TMPDIR/narration.wav"
fi

# 7) Optional narration mix (duck original track if present)
if [[ -n "$NARRATION_AUDIO" ]]; then
  has_audio=0
  if command -v ffprobe >/dev/null 2>&1; then
    if ffprobe -v error -select_streams a:0 -show_entries stream=index -of csv=p=0 "$BASE_VIDEO" | grep -q .; then
      has_audio=1
    fi
  fi

  if [[ "$has_audio" -eq 1 ]]; then
    ffmpeg -y -i "$BASE_VIDEO" -i "$NARRATION_AUDIO" \
      -filter_complex "[0:a]volume=0.30[bed];[1:a]volume=1.0[narr];[bed][narr]amix=inputs=2:duration=first:dropout_transition=2[aout]" \
      -map 0:v -map "[aout]" -c:v copy -c:a aac -b:a 160k "$WORK_VIDEO"
  else
    ffmpeg -y -i "$BASE_VIDEO" -i "$NARRATION_AUDIO" \
      -map 0:v -map 1:a -c:v copy -c:a aac -b:a 160k "$WORK_VIDEO"
  fi
  mv "$WORK_VIDEO" "$BASE_VIDEO"
fi

cp "$BASE_VIDEO" "$OUT"
echo "Wrote: $OUT"
