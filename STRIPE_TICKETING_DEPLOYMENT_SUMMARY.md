# Stripe Ticketing System - Deployment Summary

## What Was Done

### 1. Backend Configuration Fixed ✅

**File:** `tdf-hq/src/TDF/Config.hs`

**Problem:** Stripe environment variables were hardcoded to `Nothing` (TODO placeholders)

**Solution:** Added proper environment variable loading:
- `STRIPE_SECRET_KEY` - Loaded from env
- `STRIPE_PUBLISHABLE_KEY` - Loaded from env  
- `STRIPE_WEBHOOK_SECRET` - Loaded from env

**Build:** Successfully compiled (90 modules)

### 2. Frontend Environment Updated ✅

**File:** `tdf-hq-ui/.env`

Added:
```
VITE_STRIPE_PUBLISHABLE_KEY=pk_test_your_key_here
```

### 3. Deployment Scripts Created ✅

**Files:**
- `scripts/deploy-stripe-ticketing.sh` - Main deployment script
- `scripts/test-stripe-integration.sh` - Integration testing script
- `STRIPE_DEPLOYMENT_GUIDE.md` - Comprehensive deployment guide

## What You Need To Do

### Step 1: Get Stripe API Keys

1. Go to https://dashboard.stripe.com
2. Sign in to your account
3. Go to **Developers** → **API Keys**
4. Copy:
   - Publishable key (pk_test_... or pk_live_...)
   - Secret key (sk_test_... or sk_live_...)

### Step 2: Configure Stripe Webhook

1. Go to **Developers** → **Webhooks**
2. Click **+ Add endpoint**
3. Enter URL:
   ```
   https://tdf-hq.fly.dev/social-events/stripe/webhook
   ```
4. Select events:
   - ✅ `payment_intent.succeeded`
   - ✅ `payment_intent.payment_failed`
5. Save and copy the **Signing secret** (whsec_...)

### Step 3: Deploy Backend with Stripe Secrets

```bash
# Set secrets in Fly.io
flyctl secrets set STRIPE_SECRET_KEY=sk_live_your_secret --app tdf-hq
flyctl secrets set STRIPE_PUBLISHABLE_KEY=pk_live_your_key --app tdf-hq
flyctl secrets set STRIPE_WEBHOOK_SECRET=whsec_your_secret --app tdf-hq

# Deploy
cd ~/GitHub/tdf-app/tdf-hq
stack build
flyctl deploy --app tdf-hq
```

### Step 4: Deploy Frontend

In Cloudflare Pages dashboard:
1. Go to **Settings** → **Environment variables**
2. Add:
   ```
   VITE_STRIPE_PUBLISHABLE_KEY=pk_live_your_key
   VITE_API_BASE=https://tdf-hq.fly.dev
   ```
3. Redeploy frontend

### Step 5: Test End-to-End

1. Create a test event in the app
2. Click "Buy Tickets"
3. Use test card: `4242 4242 4242 4242`
   - Any future expiry date
   - Any 3-digit CVC
   - Any ZIP code
4. Complete purchase
5. Verify:
   - Payment succeeds
   - Webhook received (check Stripe Dashboard)
   - Ticket generated with QR code
   - Confirmation email sent

## Files Changed

1. `tdf-hq/src/TDF/Config.hs` - Added Stripe env var loading
2. `tdf-hq-ui/.env` - Added Stripe publishable key placeholder
3. `scripts/deploy-stripe-ticketing.sh` - Created deployment script
4. `scripts/test-stripe-integration.sh` - Created test script
5. `STRIPE_DEPLOYMENT_GUIDE.md` - Created deployment guide

## Quick Commands

```bash
# Build backend
cd ~/GitHub/tdf-app/tdf-hq && stack build

# Test integration
cd ~/GitHub/tdf-app && ./scripts/test-stripe-integration.sh

# Deploy everything
cd ~/GitHub/tdf-app && ./scripts/deploy-stripe-ticketing.sh production
```

## Important Notes

- **Test mode first:** Use test keys (sk_test_, pk_test_) for initial testing
- **Webhook secret:** Must be set AFTER creating webhook endpoint in Stripe dashboard
- **HTTPS required:** Stripe webhooks only work with HTTPS (Fly.io provides this)
- **Database migration:** Ticketing tables should already be created from previous work

## Support

If something breaks:
1. Check Fly.io logs: `flyctl logs --app tdf-hq`
2. Check Stripe webhook logs in dashboard
3. Verify secrets are set: `flyctl secrets list --app tdf-hq`
4. Test locally first: `cd tdf-hq && stack exec tdf-hq-exe`
