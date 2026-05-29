# Stripe Ticketing System - Deployment Guide

## Overview

This guide covers deploying the TDF Stripe Ticketing System to production.

## Prerequisites

- Stripe account (test mode for development, live mode for production)
- Fly.io CLI installed and authenticated
- Cloudflare Pages or Vercel account for frontend
- Access to TDF database

## Step 1: Configure Stripe Account

### 1.1 Get API Keys

1. Go to [Stripe Dashboard](https://dashboard.stripe.com)
2. Navigate to **Developers** → **API Keys**
3. Copy the following:
   - **Publishable key** (`pk_test_...` for test, `pk_live_...` for live)
   - **Secret key** (`sk_test_...` for test, `sk_live_...` for live)

### 1.2 Configure Webhook Endpoint

1. Go to **Developers** → **Webhooks**
2. Click **+ Add endpoint**
3. Enter endpoint URL:
   ```
   https://tdf-hq.fly.dev/social-events/stripe/webhook
   ```
4. Select events to listen to:
   - ✅ `payment_intent.succeeded`
   - ✅ `payment_intent.payment_failed`
   - ✅ `charge.refunded` (optional, for refund tracking)
5. Click **Add endpoint**
6. Copy the **Signing secret** (`whsec_...`)

## Step 2: Deploy Backend

### 2.1 Set Fly.io Secrets

```bash
# Set Stripe secrets
flyctl secrets set STRIPE_SECRET_KEY=sk_live_your_secret_key --app tdf-hq
flyctl secrets set STRIPE_PUBLISHABLE_KEY=pk_live_your_publishable_key --app tdf-hq
flyctl secrets set STRIPE_WEBHOOK_SECRET=whsec_your_webhook_secret --app tdf-hq

# Verify secrets are set
flyctl secrets list --app tdf-hq
```

### 2.2 Deploy

```bash
cd ~/GitHub/tdf-app
./scripts/deploy-stripe-ticketing.sh production
```

Or manually:

```bash
cd ~/GitHub/tdf-app/tdf-hq
stack build
flyctl deploy --app tdf-hq
```

## Step 3: Deploy Frontend

### 3.1 Set Environment Variables

In Cloudflare Pages dashboard:
1. Go to your project **Settings** → **Environment variables**
2. Add:
   ```
   VITE_STRIPE_PUBLISHABLE_KEY=pk_live_your_publishable_key
   VITE_API_BASE=https://tdf-hq.fly.dev
   ```

### 3.2 Build and Deploy

```bash
cd ~/GitHub/tdf-app/tdf-hq-ui
npm run build

# Deploy via wrangler
wrangler pages deploy dist --project-name=tdf-app
```

Or push to git for auto-deployment.

## Step 4: Verify Deployment

### 4.1 Health Check

```bash
# Check backend is running
curl https://tdf-hq.fly.dev/health

# Check Stripe config is loaded
curl https://tdf-hq.fly.dev/version
```

### 4.2 Test Payment Flow

1. Open frontend: https://tdf-app.pages.dev
2. Create a test event (or use existing)
3. Click **Buy Tickets**
4. Use test card: `4242 4242 4242 4242`
   - Expiry: Any future date
   - CVC: Any 3 digits
   - ZIP: Any 5 digits
5. Complete purchase
6. Verify:
   - Payment succeeds
   - Webhook is received (check Stripe Dashboard → Webhooks → Logs)
   - Ticket is generated with QR code
   - Confirmation email is sent

### 4.3 Check Webhook Logs

In Stripe Dashboard:
1. Go to **Developers** → **Webhooks**
2. Click your endpoint
3. View **Logs** tab
4. Look for:
   - ✅ HTTP 200 responses
   - ✅ `payment_intent.succeeded` events
   - ❌ No failed deliveries

## Step 5: Production Checklist

### Environment Variables

Backend (Fly.io secrets):
- [ ] `STRIPE_SECRET_KEY` - Secret key (sk_live_...)
- [ ] `STRIPE_PUBLISHABLE_KEY` - Publishable key (pk_live_...)
- [ ] `STRIPE_WEBHOOK_SECRET` - Webhook signing secret (whsec_...)

Frontend (Cloudflare Pages):
- [ ] `VITE_STRIPE_PUBLISHABLE_KEY` - Publishable key (pk_live_...)
- [ ] `VITE_API_BASE` - Backend URL (https://tdf-hq.fly.dev)

### Stripe Configuration
- [ ] Webhook endpoint configured
- [ ] Correct events selected (payment_intent.succeeded, payment_intent.payment_failed)
- [ ] Webhook signing secret copied
- [ ] Test mode vs live mode verified

### Database
- [ ] Migration applied (ticketing tables created)
- [ ] Existing data preserved

### Testing
- [ ] Test card payment successful
- [ ] Webhook received and processed
- [ ] Ticket generated with QR code
- [ ] Email confirmation sent
- [ ] Refund flow tested (optional)

## Troubleshooting

### Webhook Returns 401
- Verify `STRIPE_WEBHOOK_SECRET` is correctly set in Fly.io
- Ensure you're using the signing secret from the correct webhook endpoint
- Check for extra whitespace in secrets

### Webhook Returns 500
- Check Fly.io logs: `flyctl logs --app tdf-hq`
- Verify database connection
- Check Stripe payment intent exists in database

### Payment Fails
- Verify `STRIPE_SECRET_KEY` is set correctly
- Check if using test key with live mode or vice versa
- Review Stripe Dashboard for blocked payments

### Frontend Can't Load Stripe
- Verify `VITE_STRIPE_PUBLISHABLE_KEY` is set in Cloudflare Pages
- Check browser console for Stripe.js errors
- Ensure key starts with `pk_test_` or `pk_live_`

## Security Notes

1. **Never commit API keys** - Use environment variables/secrets
2. **Use separate keys for test/live** - Don't mix them
3. **Webhook signature verification** - Already implemented in backend
4. **HTTPS only** - Stripe requires HTTPS for webhooks
5. **Monitor webhook health** - Set up alerts for failures

## Support

- Stripe docs: https://stripe.com/docs/webhooks
- TDF issues: Check backend logs with `flyctl logs --app tdf-hq`
- Database: Verify webhook events table has records

---

## 2026-05-28 update — Stripe surface beyond ticketing

The deployment story above covers the original ticketing checkout. As of
2026-05-28 the backend exposes four more Stripe-backed surfaces. This section
documents what's new; nothing in the sections above is invalidated.

### Pinned API version

`tdf-hq/src/TDF/Services/Stripe.hs` exports `defaultStripeApiVersion = "2026-04-22.dahlia"`
and every handler reads from this constant — there is no longer any
`"2023-10-16"` literal in the code. Bumping the version is a one-line change
followed by a redeploy.

### Restricted keys (recommended over `sk_`)

`STRIPE_SECRET_KEY` accepts either a secret key (`sk_*`) or a restricted key
(`rk_*`). Production deployments should use a restricted key with the
permissions enumerated in `tdf-hq/.env.example`:

- PaymentIntents: write
- Customers: write
- Refunds: write
- Webhook Endpoints: read
- Accounts (Connect): write (only when tipping is live)
- Account Links (Connect): write (only when tipping is live)
- Checkout Sessions: write (only when course subscriptions are live)

The code path is identical; only the key value changes.

### New backend endpoints

| Method | Path | Purpose | Phase |
|--------|------|---------|-------|
| POST | `/social-events/stripe/create-payment-intent` (extended) | Same ticket-buy flow, now returns optional `spiPaymentSheet` block when the request includes `ticketPurchaseMobileSdkStripeVersion` | 2 |
| POST | `/public/courses/:slug/registrations/:registrationId/payment-intent` | Attendee one-off course payment | 3 |
| POST | `/public/courses/:slug/registrations/:registrationId/checkout-session` | Hosted Stripe Checkout in `subscription` mode for recurring courses | 4 |
| POST | `/artists/:artistId/tips` | Public Connect destination charge that pays out to the artist's connected account, keeping `artistTipPlatformFeeBps` (1000 = 10%) for the platform | 5 |

### New environment variables

- `EXPO_PUBLIC_STRIPE_PUBLISHABLE_KEY` — required on `tdf-mobile` for the
  native PaymentSheet flow. Without it, `StripeProvider` falls through and the
  app still boots but in-app purchases are disabled.
- `EXPO_PUBLIC_STRIPE_MERCHANT_IDENTIFIER` — required only if Apple Pay is
  enabled in PaymentSheet.

### Schema changes (idempotent migrations in `tdf-hq/sql/`)

- `party.stripe_customer_id` — set by `resolveStripeCustomerForBuyer` the first
  time a logged-in party transacts.
- `course_registration.stripe_payment_intent_id`,
  `course_registration.stripe_subscription_id`,
  `course_registration.subscription_status` — webhook lookup keys.
- `course.stripe_subscription_price_id` — when set, the course is sold via
  Checkout Session in subscription mode; when null, the one-off PaymentIntent
  endpoint is the only path.
- `artist_profile.stripe_account_id` — the connected Stripe Express account.
- `artist_tip` table — one row per attempted/completed tip.

### Connect onboarding (Phase 5, partially deferred)

`createConnectExpressAccount` and `createAccountLink` are wired in
`TDF.Services.Stripe`, but no public onboarding endpoint exists yet. For now,
populate `artist_profile.stripe_account_id` directly (admin UI or SQL) with an
already-onboarded `acct_*` id. The artist-facing onboarding flow is a future
PR.

### Verifying the rollout

```sh
# 1. Backend boots and the new endpoints are reachable
curl -X POST https://tdf-hq.fly.dev/public/courses/UNKNOWN/registrations/1/payment-intent \
  -H "Content-Type: application/json" -d '{}'
# expect 404 "Registro no encontrado" (the endpoint is wired)

# 2. Webhook still verifies signatures
flyctl logs --app tdf-hq | grep 'stripe-webhook'

# 3. Customer object created on first authenticated checkout
psql $DATABASE_URL -c "SELECT COUNT(*) FROM party WHERE stripe_customer_id IS NOT NULL;"
```
