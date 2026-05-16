#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

API_BASE="${API_BASE:-https://tdf-hq.fly.dev}"
API_BASE="${API_BASE%/}"
TDF_USERNAME="${TDF_USERNAME:-admin}"
TDF_PASSWORD="${TDF_PASSWORD:-password123}"
EVENT_IMAGE_URL="${EVENT_IMAGE_URL:-}"
EVENT_IMAGE_FILE="${EVENT_IMAGE_FILE:-}"
DRY_RUN="${DRY_RUN:-false}"

EVENT_TITLE="${EVENT_TITLE:-La Cuadra: Nocturna}"
EVENT_START_UTC="${EVENT_START_UTC:-2026-03-27T02:00:00Z}"  # 2026-03-26 21:00 UTC-5
EVENT_END_UTC="${EVENT_END_UTC:-2026-03-27T08:00:00Z}"
EVENT_PRICE_CENTS="${EVENT_PRICE_CENTS:-500}"
EVENT_STATUS="${EVENT_STATUS:-announced}"
EVENT_TYPE="${EVENT_TYPE:-party}"
EVENT_CITY="${EVENT_CITY:-Centro Historico}"
VENUE_NAME="${VENUE_NAME:-La Cuadra - Centro Historico}"

SPONSORS=(
  "INC-SPONS-MUP|Music Unites People"
  "INC-SPONS-RCF|RCF"
  "INC-SPONS-KURO|KURO"
  "INC-SPONS-LA-CUADRA|La Cuadra"
  "INC-SPONS-TDF-RECORDS|TDF Records"
)

log() {
  printf '[import-nocturna] %s\n' "$*" >&2
}

warn() {
  printf '[import-nocturna] WARN: %s\n' "$*" >&2
}

die() {
  printf '[import-nocturna] ERROR: %s\n' "$*" >&2
  exit 1
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"
}

urlencode() {
  jq -rn --arg v "$1" '$v|@uri'
}

dry_run_id() {
  local prefix="$1"
  local raw="${2:-}"
  local normalized
  normalized="$(
    printf '%s' "$raw" \
      | tr '[:upper:]' '[:lower:]' \
      | tr -cs 'a-z0-9' '-' \
      | sed 's/^-*//;s/-*$//'
  )"
  [[ -n "$normalized" ]] || normalized="placeholder"
  printf 'dryrun-%s-%s' "$prefix" "$normalized"
}

api_request() {
  local method="$1"
  local endpoint="$2"
  local payload="${3:-}"
  local url="${API_BASE}${endpoint}"
  local body_file
  local http_code
  local body

  body_file="$(mktemp)"

  if [[ -n "$payload" ]]; then
    http_code="$(
      curl -sS -X "$method" "$url" \
        -H "Authorization: Bearer ${ADMIN_TOKEN}" \
        -H "Content-Type: application/json" \
        --data-raw "$payload" \
        -o "$body_file" \
        -w "%{http_code}"
    )"
  else
    http_code="$(
      curl -sS -X "$method" "$url" \
        -H "Authorization: Bearer ${ADMIN_TOKEN}" \
        -o "$body_file" \
        -w "%{http_code}"
    )"
  fi

  body="$(cat "$body_file")"
  rm -f "$body_file"

  if (( http_code < 200 || http_code >= 300 )); then
    printf '[import-nocturna] API %s %s failed (%s)\n' "$method" "$endpoint" "$http_code" >&2
    printf '%s\n' "$body" >&2
    return 1
  fi

  printf '%s' "$body"
}

create_or_update_request() {
  local method="$1"
  local endpoint="$2"
  local payload="$3"
  if [[ "$DRY_RUN" == "true" ]]; then
    log "DRY_RUN: ${method} ${endpoint}"
    printf '%s' "$payload"
  else
    api_request "$method" "$endpoint" "$payload"
  fi
}

ensure_venue() {
  local query existing_id payload created_id created
  query="$(urlencode "$VENUE_NAME")"
  existing_id="$(
    api_request GET "/social-events/venues?q=${query}&limit=100" \
      | jq -r --arg name "$VENUE_NAME" '
          map(select((.venueName | ascii_downcase) == ($name | ascii_downcase)))
          | first
          | .venueId // empty
        '
  )"

  if [[ -n "$existing_id" ]]; then
    log "Using existing venue: ${VENUE_NAME} (id=${existing_id})"
    printf '%s' "$existing_id"
    return 0
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    local simulated_id
    simulated_id="$(dry_run_id "venue" "$VENUE_NAME")"
    log "DRY_RUN: would create venue '${VENUE_NAME}' (placeholder id=${simulated_id})"
    printf '%s' "$simulated_id"
    return 0
  fi

  payload="$(
    jq -n \
      --arg name "$VENUE_NAME" \
      --arg city "$EVENT_CITY" \
      '{
        venueName: $name,
        venueAddress: $city,
        venueCity: $city,
        venueCountry: null,
        venueLat: null,
        venueLng: null,
        venueCapacity: null,
        venueContact: null,
        venuePhone: null,
        venueWebsite: null,
        venueState: null,
        venueZipCode: null,
        venueImageUrl: null
      }'
  )"

  created="$(create_or_update_request POST "/social-events/venues" "$payload")"
  created_id="$(printf '%s' "$created" | jq -r '.venueId // empty')"
  [[ -n "$created_id" ]] || die "Venue creation did not return venueId"
  log "Created venue: ${VENUE_NAME} (id=${created_id})"
  printf '%s' "$created_id"
}

ensure_artist() {
  local artist_name="$1"
  local artist_bio="$2"
  local query existing_id payload created created_id

  query="$(urlencode "$artist_name")"
  existing_id="$(
    api_request GET "/social-events/artists?name=${query}&limit=200" \
      | jq -r --arg name "$artist_name" '
          map(select((.artistName | ascii_downcase) == ($name | ascii_downcase)))
          | first
          | .artistId // empty
        '
  )"

  if [[ -n "$existing_id" ]]; then
    log "Using existing artist: ${artist_name} (id=${existing_id})"
    printf '%s' "$existing_id"
    return 0
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    local simulated_id
    simulated_id="$(dry_run_id "artist" "$artist_name")"
    log "DRY_RUN: would create artist '${artist_name}' (placeholder id=${simulated_id})"
    printf '%s' "$simulated_id"
    return 0
  fi

  payload="$(
    jq -n \
      --arg name "$artist_name" \
      --arg bio "$artist_bio" \
      '{
        artistName: $name,
        artistGenres: [],
        artistBio: (if $bio == "" then null else $bio end),
        artistAvatarUrl: null,
        artistSocialLinks: null
      }'
  )"

  if ! created="$(create_or_update_request POST "/social-events/artists" "$payload")"; then
    warn "Could not create artist '${artist_name}' through /social-events/artists. Continuing without artistId link."
    printf ''
    return 0
  fi
  created_id="$(printf '%s' "$created" | jq -r '.artistId // empty')"
  if [[ -z "$created_id" ]]; then
    warn "Artist creation returned no artistId for '${artist_name}'. Continuing without artistId link."
    printf ''
    return 0
  fi
  log "Created artist: ${artist_name} (id=${created_id})"
  printf '%s' "$created_id"
}

build_description() {
  cat <<'EOF'
Evento cargado desde afiche oficial.

Fecha local: 26 marzo 2026
Hora local: 21:00 (UTC-5)
Cover: Reserva USD 5
Lugar (afiche): Centro Historico / La Cuadra

Line-up:
- Lord Ethnic (COL)
- Jam Estepez

Auspiciantes / aliados visibles:
- Music Unites People
- RCF
- KURO
- La Cuadra
- TDF Records

Nota arte:
- EVENT_IMAGE_URL puede usarse para guardar la URL final del afiche en eventImageUrl.
- EVENT_IMAGE_FILE puede subirse para alojar el afiche en /assets/serve del backend.
EOF
}

find_existing_event_id() {
  api_request GET "/social-events/events?start_after=2026-03-01T00:00:00Z&limit=500" \
    | jq -r \
      --arg title "$EVENT_TITLE" \
      --arg start "$EVENT_START_UTC" '
        map(
          select(
            (.eventTitle | ascii_downcase) == ($title | ascii_downcase)
            and (.eventStart == $start)
          )
        )
        | first
        | .eventId // empty
      '
}

create_or_update_event() {
  local venue_id="$1"
  local lord_id="$2"
  local jam_id="$3"
  local existing_id payload description endpoint method response event_id

  description="$(build_description)"
  existing_id="$(find_existing_event_id)"

  payload="$(
    jq -n \
      --arg title "$EVENT_TITLE" \
      --arg desc "$description" \
      --arg start "$EVENT_START_UTC" \
      --arg end "$EVENT_END_UTC" \
      --arg venueId "$venue_id" \
      --arg imageUrl "$EVENT_IMAGE_URL" \
      --arg status "$EVENT_STATUS" \
      --arg eventType "$EVENT_TYPE" \
      --arg lordId "$lord_id" \
      --arg jamId "$jam_id" \
      --argjson priceCents "$EVENT_PRICE_CENTS" \
      '{
        eventTitle: $title,
        eventDescription: $desc,
        eventStart: $start,
        eventEnd: $end,
        eventVenueId: $venueId,
        eventPriceCents: $priceCents,
        eventCapacity: null,
        eventTicketUrl: null,
        eventImageUrl: (if $imageUrl == "" then null else $imageUrl end),
        eventIsPublic: true,
        eventType: $eventType,
        eventStatus: $status,
        eventCurrency: "USD",
        eventBudgetCents: null,
        eventArtists: [
          (if $lordId == "" then empty else { artistId: $lordId } end),
          (if $jamId == "" then empty else { artistId: $jamId } end)
        ]
      }'
  )"

  if [[ -n "$existing_id" ]]; then
    method="PUT"
    endpoint="/social-events/events/${existing_id}"
    log "Updating existing event id=${existing_id}"
  else
    method="POST"
    endpoint="/social-events/events"
    log "Creating event: ${EVENT_TITLE}"
  fi

  if [[ "$DRY_RUN" == "true" ]]; then
    local simulated_event_id
    create_or_update_request "$method" "$endpoint" "$payload" >/dev/null
    if [[ -n "$existing_id" ]]; then
      simulated_event_id="$existing_id"
    else
      simulated_event_id="$(dry_run_id "event" "$EVENT_TITLE")"
    fi
    log "DRY_RUN: using event id=${simulated_event_id}"
    printf '%s' "$simulated_event_id"
    return 0
  fi

  response="$(create_or_update_request "$method" "$endpoint" "$payload")"
  event_id="$(printf '%s' "$response" | jq -r '.eventId // empty')"
  if [[ -z "$event_id" ]]; then
    if [[ -n "$existing_id" ]]; then
      event_id="$existing_id"
    else
      die "Event write did not return eventId"
    fi
  fi

  printf '%s' "$event_id"
}

ensure_budget_line() {
  local event_id="$1"
  local code="$2"
  local name="$3"
  local existing_line_id payload

  existing_line_id="$(
    api_request GET "/social-events/events/${event_id}/budget-lines" \
      | jq -r --arg code "$code" '
          map(select(.eblCode == $code))
          | first
          | .eblId // empty
        '
  )"

  if [[ -n "$existing_line_id" ]]; then
    log "Budget line already exists: ${code} (${name})"
    return 0
  fi

  payload="$(
    jq -n \
      --arg code "$code" \
      --arg name "$name" \
      '{
        eblCode: $code,
        eblName: $name,
        eblType: "income",
        eblCategory: "sponsorship",
        eblPlannedCents: 0,
        eblNotes: "Sponsor extracted from event artwork."
      }'
  )"

  create_or_update_request POST "/social-events/events/${event_id}/budget-lines" "$payload" >/dev/null
  log "Created budget line: ${code} (${name})"
}

upload_event_image_file() {
  local event_id="$1"
  local image_file="$2"
  local filename http_code body_file body public_url

  [[ -f "$image_file" ]] || die "EVENT_IMAGE_FILE not found: ${image_file}"
  filename="$(basename "$image_file")"

  if [[ "$DRY_RUN" == "true" ]]; then
    log "DRY_RUN: POST /social-events/events/${event_id}/image (file=${filename})"
    return 0
  fi

  body_file="$(mktemp)"
  http_code="$(
    curl -sS -X POST "${API_BASE}/social-events/events/${event_id}/image" \
      -H "Authorization: Bearer ${ADMIN_TOKEN}" \
      -F "file=@${image_file}" \
      -F "name=${filename}" \
      -o "$body_file" \
      -w "%{http_code}"
  )"
  body="$(cat "$body_file")"
  rm -f "$body_file"

  if (( http_code < 200 || http_code >= 300 )); then
    printf '[import-nocturna] API POST /social-events/events/%s/image failed (%s)\n' "$event_id" "$http_code" >&2
    printf '%s\n' "$body" >&2
    return 1
  fi

  public_url="$(printf '%s' "$body" | jq -r '.eiuPublicUrl // .eiuImageUrl // empty')"
  if [[ -n "$public_url" ]]; then
    log "Uploaded event image: ${public_url}"
  else
    log "Uploaded event image with empty URL response."
  fi
}

main() {
  require_cmd curl
  require_cmd jq

  if [[ "$DRY_RUN" != "true" && "$DRY_RUN" != "false" ]]; then
    die "DRY_RUN must be true or false"
  fi

  ADMIN_TOKEN="$(
    API_BASE="$API_BASE" \
    TDF_USERNAME="$TDF_USERNAME" \
    TDF_PASSWORD="$TDF_PASSWORD" \
    "${ROOT_DIR}/scripts/get-admin-token.sh"
  )"
  export ADMIN_TOKEN

  log "Authenticated against ${API_BASE} as ${TDF_USERNAME}"

  local venue_id lord_id jam_id event_id

  venue_id="$(ensure_venue)"
  lord_id="$(ensure_artist "Lord Ethnic" "Line-up artist from Nocturna poster (COL).")"
  jam_id="$(ensure_artist "Jam Estepez" "Line-up artist from Nocturna poster.")"
  if [[ -z "$lord_id" || -z "$jam_id" ]]; then
    warn "Event will keep lineup names in description; one or more artist IDs could not be linked."
  fi
  event_id="$(create_or_update_event "$venue_id" "$lord_id" "$jam_id")"

  if [[ "$DRY_RUN" == "false" ]]; then
    for item in "${SPONSORS[@]}"; do
      local code="${item%%|*}"
      local name="${item#*|}"
      ensure_budget_line "$event_id" "$code" "$name"
    done
  else
    log "DRY_RUN: skipping sponsor budget line writes"
  fi

  if [[ -n "${EVENT_IMAGE_FILE}" ]]; then
    upload_event_image_file "$event_id" "$EVENT_IMAGE_FILE"
  fi

  log "Done. eventId=${event_id} venueId=${venue_id} lordArtistId=${lord_id} jamArtistId=${jam_id}"
}

main "$@"
