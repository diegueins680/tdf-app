#!/bin/sh
set -eu

schema_dir=${1:?Usage: test-music-ddex-credits.sh OFFICIAL_SCHEMA_DIR}
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
case "$schema_dir" in /*) ;; *) schema_dir="$(pwd)/$schema_dir" ;; esac
# These are the files from the already-reviewed, pinned official ERN 4.3.2 ZIP.
# The test never downloads schemas or accepts licence terms on the user's behalf.
test "$(shasum -a 256 "$schema_dir/release-notification.xsd" | awk '{print $1}')" = def25b4e72696c9bbc1fed84962acc3a9bae2bc92ef25f8393c99b362aa53a6a
test "$(shasum -a 256 "$schema_dir/allowed-value-sets.xsd" | awk '{print $1}')" = 87e99fe74f57a640dce0d3247d16b3b52358562c1dbefc4617eb8a9b7360d943
output=$(mktemp -d "${TMPDIR:-/tmp}/tdf-ddex-credits.XXXXXX")
# Retain every generated fixture/report for inspection; no private material.
printf 'Synthetic ERN credit fixture evidence: %s\n' "$output"
(
  cd "$root/tdf-hq"
  stack exec -- runhaskell -isrc -itest test/MusicDdexCreditsFixtureMain.hs "$output"
)
for name in single ep album update takedown; do
  xmllint --nonet --noout --schema "$schema_dir/release-notification.xsd" "$output/$name.xml"
done
test "$(xmllint --xpath 'count(//SoundRecording[1]/Contributor[ContributorPartyReference="P_artist"])' "$output/album.xml")" = 1
test "$(xmllint --xpath 'count(//SoundRecording[1]/Contributor[ContributorPartyReference="P_artist"]/Role)' "$output/album.xml")" = 3
test "$(xmllint --xpath 'count(//SoundRecording[2]/Contributor[ContributorPartyReference="P_guest"])' "$output/album.xml")" = 0
test "$(xmllint --xpath 'count(//Release/DisplayArtist[ArtistPartyReference="P_guest"])' "$output/album.xml")" = 0
test "$(xmllint --xpath 'count(//SoundRecording[1]/DisplayArtist[@SequenceNumber="2"][DisplayArtistRole="FeaturedArtist"])' "$output/album.xml")" = 1
test "$(xmllint --xpath 'count(//DealList)' "$output/takedown.xml")" = 0
test "$(xmllint --xpath 'count(//DealList)' "$output/update.xml")" = 1
test "$(xmllint --xpath 'count(//SoundRecording[1]/Contributor[ContributorPartyReference="P_guest"]/Role)' "$output/update.xml")" = 7
# A well-formed XML can still violate the official allowed-value set.
sed 's/<Value>Composer<\//<Value>InventedComposerRole<\//g' "$output/album.xml" > "$output/invalid-role.xml"
if xmllint --nonet --noout --schema "$schema_dir/release-notification.xsd" "$output/invalid-role.xml" > "$output/invalid-role.log" 2>&1; then
  echo 'Invalid role unexpectedly passed official XSD' >&2
  exit 1
fi
shasum -a 256 "$output"/*.xml
echo '5/5 XML fixtures passed official pinned XSD, 8 semantic assertions passed, invalid role rejected.'
