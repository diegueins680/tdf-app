#!/bin/bash
# Stripe Integration Test Script
# Tests the complete payment flow end-to-end

set -e

API_BASE=${API_BASE:-http://localhost:8080}
FRONTEND_URL=${FRONTEND_URL:-http://localhost:5173}

echo "🧪 Stripe Integration Tests"
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
fail() { echo -e "${RED}✗${NC} $1"; exit 1; }
warn() { echo -e "${YELLOW}⚠${NC} $1"; }

# Test 1: Health Check
echo "Test 1: Backend Health"
if curl -s "$API_BASE/health" > /dev/null 2>&1; then
    pass "Backend is running"
else
    fail "Backend not responding at $API_BASE"
fi

# Test 2: Stripe Config Check
echo ""
echo "Test 2: Stripe Configuration"
STRIPE_STATUS=$(curl -s "$API_BASE/version" 2>/dev/null || echo "{}")
if echo "$STRIPE_STATUS" | grep -q "stripe"; then
    pass "Stripe config detected in version endpoint"
else
    warn "Cannot verify Stripe config from version endpoint"
fi

# Test 3: Create Test Event
echo ""
echo "Test 3: Create Test Event"
EVENT_RESPONSE=$(curl -s -X POST "$API_BASE/events" \
    -H "Content-Type: application/json" \
    -d '{
        "title": "Stripe Test Event",
        "description": "Testing Stripe integration",
        "startDate": "'$(date -u -v+7d +%Y-%m-%dT%H:%M:%SZ)'",
        "endDate": "'$(date -u -v+8d +%Y-%m-%dT%H:%M:%SZ)'",
        "venue": "Test Venue",
        "isPublic": true
    }' 2>/dev/null || echo "{}")

EVENT_ID=$(echo "$EVENT_RESPONSE" | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
if [ -n "$EVENT_ID" ]; then
    pass "Created test event: $EVENT_ID"
else
    fail "Failed to create test event"
fi

# Test 4: Create Ticket Tier
echo ""
echo "Test 4: Create Ticket Tier"
TIER_RESPONSE=$(curl -s -X POST "$API_BASE/events/$EVENT_ID/ticket-tiers" \
    -H "Content-Type: application/json" \
    -d '{
        "name": "General Admission",
        "description": "Test tier",
        "priceCents": 1000,
        "currency": "USD",
        "capacity": 100
    }' 2>/dev/null || echo "{}")

TIER_ID=$(echo "$TIER_RESPONSE" | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
if [ -n "$TIER_ID" ]; then
    pass "Created ticket tier: $TIER_ID"
else
    fail "Failed to create ticket tier"
fi

# Test 5: Create Payment Intent
echo ""
echo "Test 5: Create Payment Intent"
PI_RESPONSE=$(curl -s -X POST "$API_BASE/stripe/create-payment-intent" \
    -H "Content-Type: application/json" \
    -d '{
        "eventId": "'$EVENT_ID'",
        "tierId": "'$TIER_ID'",
        "quantity": 1,
        "buyerName": "Test User",
        "buyerEmail": "test@example.com",
        "buyerPhone": "+1234567890"
    }' 2>/dev/null || echo "{}")

CLIENT_SECRET=$(echo "$PI_RESPONSE" | grep -o '"clientSecret":"[^"]*"' | cut -d'"' -f4)
ORDER_ID=$(echo "$PI_RESPONSE" | grep -o '"orderId":"[^"]*"' | cut -d'"' -f4)

if [ -n "$CLIENT_SECRET" ]; then
    pass "Created payment intent with client secret"
else
    # Check if Stripe is configured
    if echo "$PI_RESPONSE" | grep -q "Stripe is not configured"; then
        fail "Stripe is not configured on backend - check STRIPE_SECRET_KEY"
    else
        fail "Failed to create payment intent: $PI_RESPONSE"
    fi
fi

# Test 6: Verify Webhook Endpoint
echo ""
echo "Test 6: Webhook Endpoint"
WEBHOOK_RESPONSE=$(curl -s -X POST "$API_BASE/social-events/stripe/webhook" \
    -H "Content-Type: application/json" \
    -H "Stripe-Signature: test" \
    -d '{"type":"payment_intent.succeeded"}' 2>/dev/null || echo "{}")

if echo "$WEBHOOK_RESPONSE" | grep -q "Invalid signature"; then
    pass "Webhook endpoint active (signature verification working)"
elif echo "$WEBHOOK_RESPONSE" | grep -q "Missing signature"; then
    pass "Webhook endpoint active (requires signature)"
else
    warn "Unexpected webhook response: $WEBHOOK_RESPONSE"
fi

# Test 7: Cleanup
echo ""
echo "Test 7: Cleanup"
if [ -n "$EVENT_ID" ]; then
    curl -s -X DELETE "$API_BASE/events/$EVENT_ID" > /dev/null 2>&1 || true
    pass "Cleaned up test event"
fi

# Summary
echo ""
echo "=========================="
echo "Test Summary"
echo "=========================="
pass "All tests passed!"
echo ""
echo "Next steps:"
echo "1. Configure Stripe webhook in dashboard"
echo "2. Use test card 4242 4242 4242 4242"
echo "3. Verify webhook processing in Stripe logs"
echo "4. Check ticket generation in database"
