#!/bin/bash
# Stripe Integration Test Script
# Read-only readiness diagnostic; it does not exercise payment/webhook behavior.

set -e

PROJECT_DIR=${PROJECT_DIR:-"$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"}
API_BASE=${API_BASE:-http://localhost:8080}
FRONTEND_URL=${FRONTEND_URL:-http://localhost:5173}

echo "🧪 Stripe readiness diagnostic"
echo "=========================="
echo "API: $API_BASE"
echo "Frontend: $FRONTEND_URL"
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

pass() { echo -e "${GREEN}✓${NC} $1"; }
fail() { echo -e "${RED}✗${NC} $1"; }
warn() { echo -e "${YELLOW}⚠${NC} $1"; }

# Test 1: Health Check
echo "Test 1: Backend Health"
HEALTH_STATUS=$(curl -s -o /dev/null -w "%{http_code}" "$API_BASE/health" 2>/dev/null || echo "000")
if [ "$HEALTH_STATUS" = "200" ]; then
    pass "Backend is running (HTTP 200)"
else
    fail "Backend not responding at $API_BASE (HTTP $HEALTH_STATUS)"
fi

# Test 2: Check API structure (Social Events API should exist under /social-events)
echo ""
echo "Test 2: API Structure"
# The Stripe endpoints are under /social-events/ which requires auth
# Let's just verify the backend compiles with Stripe support
warn "Stripe support and authenticated payment behavior are not verified by this diagnostic"

# Test 3: Check if Stripe environment variables are configured
echo ""
echo "Test 3: Stripe Environment Configuration"
if [ -f "$PROJECT_DIR/tdf-hq/.env.local" ]; then
    if grep -q "STRIPE_SECRET_KEY" "$PROJECT_DIR/tdf-hq/.env.local" 2>/dev/null; then
        pass "STRIPE_SECRET_KEY found in backend .env.local"
    else
        warn "STRIPE_SECRET_KEY not found in backend .env.local"
    fi
else
    warn "No .env.local file found for backend"
fi

# Test 4: Frontend Environment Check
echo ""
echo "Test 4: Frontend Environment"
if [ -f "$PROJECT_DIR/tdf-hq-ui/.env" ]; then
    if grep -q "VITE_STRIPE_PUBLISHABLE_KEY" "$PROJECT_DIR/tdf-hq-ui/.env" 2>/dev/null; then
        pass "VITE_STRIPE_PUBLISHABLE_KEY found in frontend .env"
    else
        warn "VITE_STRIPE_PUBLISHABLE_KEY not found in frontend .env"
    fi
else
    warn "No .env file found for frontend"
fi

# Test 5: Build Verification
echo ""
echo "Test 5: Backend Build"
if [ -f "$PROJECT_DIR/tdf-hq/.stack-work/install"*/9.6.6/bin/tdf-hq-exe ] || \
   [ -f "$PROJECT_DIR/tdf-hq/.stack-work/dist"*/ghc-9.6.6/build/tdf-hq-exe/tdf-hq-exe ]; then
    pass "Backend binary exists (compiled successfully)"
else
    warn "Backend binary not found - may need to build"
fi

# Summary
echo ""
echo "=========================="
echo "Test Summary"
echo "=========================="
echo ""
echo "✅ Backend Status:"
echo "  - Health endpoint: HTTP $HEALTH_STATUS"
echo "  - Stripe payment/webhook behavior: not exercised"
echo "  - Runtime configuration: not verified by local file checks"
echo ""
echo "📋 Next Steps for Production:"
echo ""
echo "1. Get Stripe API Keys:"
echo "   → https://dashboard.stripe.com/test/apikeys"
echo "   → Copy Publishable key (pk_test_...)"
echo "   → Copy Secret key (sk_test_...)"
echo ""
echo "2. Configure Stripe Webhook:"
echo "   → https://dashboard.stripe.com/test/webhooks"
echo "   → Verify/edit the existing endpoint URL: https://api.tdfrecords.net/social-events/stripe/webhook"
echo "   → Preserve approved events, including payment_intent.succeeded and payment_intent.payment_failed"
echo "   → Preserve the existing endpoint ID/signing secret; do not add a duplicate. Stop if missing."
echo ""
echo "3. Apply reviewed backend configuration:"
echo "   → Use ops/hetzner/README.md; preserve runtime secrets and release/recovery gates"
echo "   → Do not deploy or update secrets on the retired Fly app"
echo ""
echo "4. Deploy Frontend:"
echo "   → Add VITE_STRIPE_PUBLISHABLE_KEY to Cloudflare Pages"
echo "   → Redeploy frontend"
echo ""
echo "5. Test End-to-End:"
echo "   → Create event in app"
echo "   → Buy ticket with card: 4242 4242 4242 4242"
echo "   → Verify webhook received in Stripe Dashboard"
echo "   → Check ticket generated with QR code"
echo ""

# A failed health probe must not be reported as a successful diagnostic.
[ "$HEALTH_STATUS" = "200" ]
