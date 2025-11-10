#!/usr/bin/env bash
set -euo pipefail

BACKEND_URL=${BACKEND_URL:-http://localhost:8080}
LOGO_PATH=${LOGO_PATH:-/app/templates/tdf-logo.pdf}

echo "[smoke] Using backend: $BACKEND_URL"

# 1) Healthcheck
echo "[smoke] Checking /health ..."
code=$(curl -s -o /dev/null -w '%{http_code}' "$BACKEND_URL/health")
if [ "$code" != "200" ]; then
  echo "[smoke][ERROR] /health returned $code"
  exit 1
fi
echo "[smoke] /health OK"

# 2) Check logo presence (if running inside container or mapped path)
if [ -f "$LOGO_PATH" ]; then
  echo "[smoke] Logo found at $LOGO_PATH"
else
  echo "[smoke][WARN] Logo not found at $LOGO_PATH."
  echo "               Place your tdf-logo.pdf at /app/templates/ before generating invoices."
fi

# 3) Generate demo invoice
echo "[smoke] Generating demo invoice ..."
resp=$(curl -s -X POST "$BACKEND_URL/invoices/demo/generate") || { echo "[smoke][ERROR] demo generate failed"; exit 1; }
echo "[smoke] Endpoint responded: $resp"

# 4) Try fetch demo PDF
DEMO_URL="$BACKEND_URL/invoices/demo.pdf"
echo "[smoke] Fetching $DEMO_URL ..."
code=$(curl -s -o /dev/null -w '%{http_code}' "$DEMO_URL")
if [ "$code" != "200" ]; then
  echo "[smoke][ERROR] demo.pdf not found (HTTP $code)."
  exit 1
fi
echo "[smoke] demo.pdf available (HTTP 200)."

# 5) List local invoices folder if present
if [ -d /app/public/invoices ]; then
  echo "[smoke] Listing /app/public/invoices:"
  ls -lah /app/public/invoices || true
fi

echo "[smoke] All good!"
