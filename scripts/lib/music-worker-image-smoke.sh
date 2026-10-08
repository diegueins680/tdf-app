#!/bin/sh
# Runs only inside the built image. No source mounts or substituted executables.
set -eu
test "$(id -u)" = 1000
test "$(uname -s)" = Linux
for tool in bash curl ffmpeg ffprobe jq shasum psql perl xmllint zip unzip; do
  command -v "$tool" >/dev/null
done
ffmpeg -version | sed -n '1p'
curl --version | sed -n '1p'
psql --version
jq --version
/usr/bin/tini --version
sha256sum /app/tdf-ddex-render
perl -c /app/scripts/music-s3-upload.pl
if /app/tdf-ddex-render > /tmp/renderer.log 2>&1; then
  echo 'Renderer unexpectedly accepted missing arguments' >&2
  exit 1
fi
grep -q '^Usage: tdf-ddex-render ' /tmp/renderer.log
echo 'PASS Linux dependencies, non-root user and linked Haskell renderer'

ffmpeg -nostdin -v error -f lavfi \
  -i 'sine=frequency=440:sample_rate=48000:duration=3' \
  -ac 2 -c:a pcm_s24le /tmp/master.wav
master_before=$(shasum -a 256 /tmp/master.wav)
/app/scripts/process-music-release-audio.sh /tmp/master.wav /tmp/audio 500 1750
manifest_before=$(shasum -a 256 /tmp/audio/manifest.json)
/app/scripts/process-music-release-audio.sh /tmp/master.wav /tmp/audio 500 1750
test "$manifest_before" = "$(shasum -a 256 /tmp/audio/manifest.json)"
test "$master_before" = "$(shasum -a 256 /tmp/master.wav)"
jq -e '.pipelineVersion == "audio-v2" and .source.immutable == true
  and .source.bitDepth == 24 and .normalization.masterModified == false
  and .previewRequest == {startMs:500,durationMs:1750}
  and (.derivatives | length) == 7' /tmp/audio/manifest.json >/dev/null
for file in stream-low.m4a stream-medium.m4a stream-high.m4a stream-lossless.flac preview.m4a; do
  test -s "/tmp/audio/$file"
  ffmpeg -nostdin -v error -i "/tmp/audio/$file" -f null -
done
ffprobe -v error -show_entries format=duration -of json /tmp/audio/preview.m4a |
  jq -e '(.format.duration | tonumber) >= 1.7 and (.format.duration | tonumber) <= 1.8' >/dev/null
echo 'PASS real PCM master, AAC/FLAC decode, configured preview and idempotent retry'

ffmpeg -nostdin -v error -f lavfi -i 'color=c=blue:s=3000x3000:d=1' \
  -frames:v 1 -threads 1 /tmp/cover.png
cover_before=$(shasum -a 256 /tmp/cover.png)
/app/scripts/process-music-release-artwork.sh /tmp/cover.png /tmp/artwork
art_manifest_before=$(shasum -a 256 /tmp/artwork/manifest.json)
/app/scripts/process-music-release-artwork.sh /tmp/cover.png /tmp/artwork
test "$art_manifest_before" = "$(shasum -a 256 /tmp/artwork/manifest.json)"
test "$cover_before" = "$(shasum -a 256 /tmp/cover.png)"
jq -e '.pipelineVersion == "artwork-v1" and .source.immutable == true
  and (.derivatives | length) == 4' /tmp/artwork/manifest.json >/dev/null
test "$(ffprobe -v error -select_streams v:0 -show_entries stream=width -of csv=p=0 /tmp/artwork/thumbnail-600.jpg)" = 600
echo 'PASS real cover processing, thumbnail and immutable original'

for kind in audio artwork; do
  jq -r '.derivatives[] | [.sha256,.path] | @tsv' "/tmp/$kind/manifest.json" |
    while IFS="$(printf '\t')" read -r expected path; do
      actual=$(shasum -a 256 "/tmp/$kind/$path" | awk '{print $1}')
      test "$actual" = "$expected"
    done
done
echo 'PASS every generated asset matches its manifest checksum'

printf 'synthetic corrupt fixture; not audio' > /tmp/corrupt.wav
if /app/scripts/process-music-release-audio.sh /tmp/corrupt.wav /tmp/rejected >/tmp/rejected.log 2>&1; then
  echo 'Corrupt master unexpectedly accepted' >&2
  exit 1
fi
test ! -e /tmp/rejected
echo 'PASS corrupt input rejected without published derivatives'
