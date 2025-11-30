#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ADMIN_TOKEN=... BASE_URL=https://tdf-hq.fly.dev ./scripts/create_artists_from_list.sh
#
# Requires: curl, jq, bash (4+).

if [[ -z "${ADMIN_TOKEN:-}" ]]; then
  echo "Set ADMIN_TOKEN with an admin bearer token" >&2
  exit 1
fi

BASE_URL="${BASE_URL:-https://tdf-hq.fly.dev}"

ARTISTS=(
  "Federico Molinari"
  "Categal"
  "Los Morrison"
  "Barrelshots"
  "Machaka"
  "Just One Nite"
  "Morex"
  "Diego Saá"
  "Everaldo Vasco"
  "COHEMA"
  "AVR"
  "Owen"
  "MELANIA"
  "ELI LASSO"
  "LYSERGICMAN"
  "Semiazas"
  "Juan Diego"
  "MOOD PATTERN"
  "LE CHU"
  "LABII"
  "ALEJANDRO ROMERO"
  "La Bestia Quiñonez"
  "ESTEBAN MUÑOZ"
  "JULIO DIAZ"
  "Fabro"
  "DATI DICE"
  "Agus"
  "La Clau"
  "JUANO LEDESMA"
  "DANI ALBAN"
  "Liquid Paper Girl"
)

create_party() {
  local name="$1"
  curl -sS -X POST "${BASE_URL}/parties" \
    -H "Authorization: Bearer ${ADMIN_TOKEN}" \
    -H "Content-Type: application/json" \
    --data-raw "$(jq -n --arg name "$name" '{cDisplayName:$name,cIsOrg:false}')" \
    | jq -r '.partyId'
}

slugify() {
  echo "$1" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g;s/^-+//;s/-+$//' | cut -c1-60
}

create_artist_profile() {
  local party_id="$1"
  local name="$2"
  local slug
  slug=$(slugify "$name")
  curl -sS -X POST "${BASE_URL}/admin/artists/profiles" \
    -H "Authorization: Bearer ${ADMIN_TOKEN}" \
    -H "Content-Type: application/json" \
    --data-raw "$(jq -n --argjson pid "$party_id" --arg slug "$slug" --arg name "$name" '{apuArtistId:$pid,apuSlug:$slug,apuBio:$name,apuCity:null,apuHeroImageUrl:null,apuSpotifyArtistId:null,apuSpotifyUrl:null,apuYoutubeChannelId:null,apuYoutubeUrl:null,apuWebsiteUrl:null,apuFeaturedVideoUrl:null,apuGenres:null,apuHighlights:null}')" \
    >/dev/null
}

for artist in "${ARTISTS[@]}"; do
  echo "Creating artist: ${artist}"
  party_id=$(create_party "$artist")
  if [[ "$party_id" == "null" || -z "$party_id" ]]; then
    echo "Failed to create party for ${artist}" >&2
    continue
  fi
  create_artist_profile "$party_id" "$artist" || echo "Failed profile for ${artist}" >&2
done

echo "Done."
