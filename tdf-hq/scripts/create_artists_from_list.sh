#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   ADMIN_TOKEN=... ARTIST_ENTITY_TYPES_FILE=types.json ./scripts/create_artists_from_list.sh
# types.json: one explicit isOrg boolean per source row (true for bands/organizations).
# Keep ARTIST_IMPORT_ID and source row order unchanged when retrying.
# A new namespace denotes an explicitly reviewed new import, not a retry.
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

ARTIST_IMPORT_ID="${ARTIST_IMPORT_ID:-tdf-bundled-artists-v1}"
if [[ ! "$ARTIST_IMPORT_ID" =~ ^[A-Za-z0-9_-]{16,96}$ ]]; then
  echo "ARTIST_IMPORT_ID must be a stable 16-96 character source-operation namespace" >&2
  exit 1
fi
: "${ARTIST_ENTITY_TYPES_FILE:?Provide a reviewed JSON array of isOrg booleans; artist names do not establish entity types}"
jq -e --argjson count "${#ARTISTS[@]}" \
  'type == "array" and length == $count and all(.[]; type == "boolean")' \
  "$ARTIST_ENTITY_TYPES_FILE" >/dev/null

create_party() {
  local name="$1"
  local source_row="$2"
  local is_org
  is_org=$(jq -c --argjson row "$source_row" '.[$row]' "$ARTIST_ENTITY_TYPES_FILE")
  curl --fail-with-body -sS -X POST "${BASE_URL}/parties" \
    -H "Authorization: Bearer ${ADMIN_TOKEN}" \
    -H "Content-Type: application/json" \
    -H "Idempotency-Key: ${ARTIST_IMPORT_ID}-${source_row}" \
    --data-raw "$(jq -n --arg name "$name" --argjson isOrg "$is_org" '{cDisplayName:$name,cIsOrg:$isOrg}')" \
    | jq -r '.partyId'
}

# Profile creation is an explicit administrative review step. The legacy
# profile endpoint overwrites fields on retry and cannot safely implement a
# create-only import. Never send that upsert from this retryable batch.
for source_row in "${!ARTISTS[@]}"; do
  artist="${ARTISTS[$source_row]}"
  echo "Creating artist: ${artist}"
  party_id=$(create_party "$artist" "$source_row")
  if [[ "$party_id" == "null" || -z "$party_id" ]]; then
    echo "Failed to create party for ${artist}" >&2
    exit 1
  fi
  echo "Contact ${party_id} ready; review or create its artist profile in the administrative artist directory."
done

echo "Done."
