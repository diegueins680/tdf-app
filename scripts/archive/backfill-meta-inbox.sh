#!/usr/bin/env bash
set -euo pipefail

# One-time Meta inbox backfill trigger.
#
# Usage examples:
#   API_BASE=https://tdf-hq.fly.dev TDF_USERNAME=admin TDF_PASSWORD=... ./scripts/backfill-meta-inbox.sh
#   API_BASE=https://tdf-hq.fly.dev API_TOKEN=... PLATFORM=all DRY_RUN=true ./scripts/backfill-meta-inbox.sh

API_BASE="${API_BASE:-https://tdf-hq.fly.dev}"
API_BASE="${API_BASE%/}"

PLATFORM="${PLATFORM:-all}" # all | instagram | facebook
CONVERSATION_LIMIT="${CONVERSATION_LIMIT:-50}"
MESSAGES_PER_CONVERSATION="${MESSAGES_PER_CONVERSATION:-50}"
ONLY_UNREAD="${ONLY_UNREAD:-true}"
DRY_RUN="${DRY_RUN:-false}"

if [[ -z "${API_TOKEN:-}" ]]; then
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  API_TOKEN="$("${SCRIPT_DIR}/get-admin-token.sh")"
fi

if [[ -z "${API_TOKEN:-}" ]]; then
  echo "Missing API_TOKEN and unable to obtain one from get-admin-token.sh" >&2
  exit 1
fi

if command -v jq >/dev/null 2>&1; then
  PAYLOAD="$(jq -n \
    --arg platform "$PLATFORM" \
    --argjson conversationLimit "$CONVERSATION_LIMIT" \
    --argjson messagesPerConversation "$MESSAGES_PER_CONVERSATION" \
    --argjson onlyUnread "$ONLY_UNREAD" \
    --argjson dryRun "$DRY_RUN" \
    '{platform:$platform,conversationLimit:$conversationLimit,messagesPerConversation:$messagesPerConversation,onlyUnread:$onlyUnread,dryRun:$dryRun}'
  )"
else
  PAYLOAD="{\"platform\":\"${PLATFORM}\",\"conversationLimit\":${CONVERSATION_LIMIT},\"messagesPerConversation\":${MESSAGES_PER_CONVERSATION},\"onlyUnread\":${ONLY_UNREAD},\"dryRun\":${DRY_RUN}}"
fi

echo "POST ${API_BASE}/meta/backfill"
curl -sS -X POST "${API_BASE}/meta/backfill" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer ${API_TOKEN}" \
  --data-raw "${PAYLOAD}" | if command -v jq >/dev/null 2>&1; then jq .; else cat; fi
