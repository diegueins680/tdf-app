#!/usr/bin/env bash
set -euo pipefail

# Fetch a bearer token via /login.
#
# Usage:
#   ./scripts/get-admin-token.sh
#   API_BASE=https://tdf-hq.fly.dev TDF_USERNAME=admin TDF_PASSWORD=... ./scripts/get-admin-token.sh
#   ADMIN_TOKEN=$(./scripts/get-admin-token.sh)
#
# Options:
#   --bearer        Print "Bearer <token>" instead of the raw token.
#   --export [VAR]  Print "export VAR=<token>" (defaults to API_TOKEN).

usage() {
  cat <<'EOF'
Usage: get-admin-token.sh [--bearer] [--export [VAR]]

Env:
  API_BASE or BASE_URL  Base URL for the API (default: http://localhost:8080)
  TDF_USERNAME          Username for /login (prompted if missing)
  TDF_PASSWORD          Password for /login (prompted if missing)
  ADMIN_USERNAME        Alternate username env (optional)
  ADMIN_PASSWORD        Alternate password env (optional)
EOF
}

output_mode="raw"
export_var=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bearer)
      output_mode="bearer"
      shift
      ;;
    --export)
      output_mode="export"
      if [[ -n "${2:-}" && "${2:0:1}" != "-" ]]; then
        export_var="$2"
        shift 2
      else
        shift
      fi
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if ! command -v curl >/dev/null 2>&1; then
  echo "Missing curl." >&2
  exit 1
fi

json_build() {
  if command -v jq >/dev/null 2>&1; then
    jq -n --arg username "$1" --arg password "$2" '{username:$username,password:$password}'
    return 0
  fi
  if command -v python3 >/dev/null 2>&1; then
    python3 - "$1" "$2" <<'PY'
import json
import sys

username = sys.argv[1]
password = sys.argv[2]
print(json.dumps({"username": username, "password": password}))
PY
    return 0
  fi
  echo "Missing jq or python3 for JSON encoding." >&2
  return 1
}

json_token() {
  if command -v jq >/dev/null 2>&1; then
    jq -r '.token // empty'
    return 0
  fi
  if command -v python3 >/dev/null 2>&1; then
    python3 -c 'import json,sys
try:
    data = json.load(sys.stdin)
except json.JSONDecodeError:
    sys.exit(0)
token = data.get("token") or ""
sys.stdout.write(token)
'
    return 0
  fi
  echo "Missing jq or python3 for JSON parsing." >&2
  return 1
}

API_BASE="${API_BASE:-${BASE_URL:-http://localhost:8080}}"
API_BASE="${API_BASE%/}"
USERNAME="${TDF_USERNAME:-${ADMIN_USERNAME:-}}"
PASSWORD="${TDF_PASSWORD:-${ADMIN_PASSWORD:-}}"

if [[ -z "${USERNAME}" ]]; then
  if [[ -t 0 ]]; then
    read -r -p "Username: " USERNAME
  else
    echo "Missing TDF_USERNAME (no TTY to prompt)." >&2
    exit 1
  fi
fi

if [[ -z "${PASSWORD}" ]]; then
  if [[ -t 0 ]]; then
    read -r -s -p "Password: " PASSWORD
    echo
  else
    echo "Missing TDF_PASSWORD (no TTY to prompt)." >&2
    exit 1
  fi
fi

if [[ -z "${USERNAME}" || -z "${PASSWORD}" ]]; then
  echo "Username and password are required." >&2
  exit 1
fi

payload="$(json_build "$USERNAME" "$PASSWORD")"
response="$(curl -sS -X POST "${API_BASE}/login" \
  -H "Content-Type: application/json" \
  --data-raw "$payload")"

token="$(printf '%s' "$response" | json_token)"

if [[ -z "${token}" ]]; then
  echo "Login failed or token missing." >&2
  printf '%s\n' "$response" >&2
  exit 1
fi

case "$output_mode" in
  bearer)
    printf 'Bearer %s\n' "$token"
    ;;
  export)
    export_var="${export_var:-API_TOKEN}"
    printf 'export %s=%q\n' "$export_var" "$token"
    ;;
  *)
    printf '%s\n' "$token"
    ;;
esac
