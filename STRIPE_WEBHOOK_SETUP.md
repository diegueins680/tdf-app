# Stripe Webhook Configuration Guide

This guide walks you through configuring Stripe webhooks for the TDF ticketing system.

## Prerequisites

- Stripe account (test or live)
- Backend API deployed and accessible via HTTPS
- `STRIPE_SECRET_KEY` and `STRIPE_PUBLISHABLE_KEY` already configured

## Step 1: Locate Your Webhook Endpoint

Your API webhook endpoint is:

```
https://your-api-domain.com/social-events/stripe/webhook
```

**Examples:**
- Production: `https://api.tdf-app.com/social-events/stripe/webhook`
- Staging: `https://staging-api.tdf-app.com/social-events/stripe/webhook`
- Development (using ngrok): `https://abc123.ngrok.io/social-events/stripe/webhook`

## Step 2: Access Stripe Dashboard

1. Log in to [Stripe Dashboard](https://dashboard.stripe.com)
2. Navigate to **Developers** → **Webhooks**
3. Click **Add endpoint** (or **+ Add an endpoint**)

## Step 3: Configure Webhook Endpoint

### Endpoint URL
Enter your webhook endpoint URL:
```
https://your-api-domain.com/social-events/stripe/webhook
```

### Events to Listen To

Select the following events:

#### Payment Events
- ✅ `payment_intent.succeeded` - Triggered when payment completes successfully
- ✅ `payment_intent.payment_failed` - Triggered when payment fails

#### Refund Events
- ✅ `charge.refunded` - Triggered when refund is processed
- ✅ `refund.updated` - Optional: Track refund status changes

### API Version
- Use the **latest API version** or match your Stripe SDK version
- Current recommended: `2024-11-20.acacia` or later

### Description (Optional)
```
TDF Ticketing System - Production Webhooks
```

## Step 4: Save and Get Signing Secret

1. Click **Add endpoint** to save
2. Stripe will show you the **Signing secret** (starts with `whsec_`)
3. **Copy this secret immediately** - you'll need it for environment variables

Example:
```
whsec_1234567890abcdefghijklmnopqrstuvwxyz
```

## Step 5: Configure Environment Variables

### Backend (.env)

Add the webhook secret to your backend `.env` file:

```bash
# In /Users/macpro/GitHub/tdf-app/tdf-hq/.env

STRIPE_SECRET_KEY=sk_live_your_actual_secret_key
STRIPE_PUBLISHABLE_KEY=pk_live_your_actual_publishable_key
STRIPE_WEBHOOK_SECRET=whsec_your_webhook_signing_secret
```

**Important:**
- Use `sk_test_` and `pk_test_` keys for testing
- Use `sk_live_` and `pk_live_` keys for production
- **Never commit** `.env` files to git (already in `.gitignore`)

### Frontend (.env.local or .env)

Update frontend environment variables:

```bash
# In /Users/macpro/GitHub/tdf-app/tdf-hq-ui/.env.local

VITE_API_BASE=https://your-api-domain.com
VITE_STRIPE_PUBLISHABLE_KEY=pk_live_your_actual_publishable_key
```

## Step 6: Restart Your Application

After updating environment variables, restart both backend and frontend:

```bash
# Backend (Haskell)
cd tdf-hq
stack build
stack exec tdf-hq-exe

# Frontend (React + Vite)
cd tdf-hq-ui
npm run build
npm run preview  # or deploy to hosting
```

## Step 7: Test Webhook

### Using Stripe Dashboard

1. Go to **Developers** → **Webhooks**
2. Click on your webhook endpoint
3. Click **Send test webhook**
4. Select `payment_intent.succeeded`
5. Click **Send test webhook**

Check your backend logs for:
```
Received Stripe webhook: payment_intent.succeeded
Processing payment intent: pi_1234567890
```

### Using Stripe CLI (Recommended for Local Development)

Install Stripe CLI:
```bash
# macOS
brew install stripe/stripe-cli/stripe

# Login
stripe login
```

Forward webhooks to local server:
```bash
# This will give you a webhook signing secret for local testing
stripe listen --forward-to localhost:8080/social-events/stripe/webhook

# In another terminal, trigger test events
stripe trigger payment_intent.succeeded
stripe trigger payment_intent.payment_failed
stripe trigger charge.refunded
```

## Step 8: Verify Webhook is Working

### Check Webhook Logs in Stripe Dashboard

1. Go to **Developers** → **Webhooks**
2. Click on your endpoint
3. View the **Logs** tab
4. You should see attempts with HTTP 200 responses

### Check Your Database

After triggering a test `payment_intent.succeeded`, verify:

```sql
-- Check webhook events were recorded
SELECT * FROM stripe_webhook_event ORDER BY created_at DESC LIMIT 10;

-- Check payment intents were created
SELECT * FROM stripe_payment_intent WHERE stripe_payment_intent_id LIKE 'pi_%' ORDER BY created_at DESC LIMIT 5;

-- Check tickets were generated
SELECT * FROM event_ticket WHERE created_at > NOW() - INTERVAL '1 hour';
```

## Troubleshooting

### Webhook Returns 401 Unauthorized

**Cause:** Webhook signature verification failed

**Solution:**
- Verify `STRIPE_WEBHOOK_SECRET` is correctly set
- Ensure you're using the signing secret from the specific webhook endpoint
- Check for extra whitespace in `.env` file

### Webhook Returns 500 Internal Server Error

**Cause:** Application error processing webhook

**Solution:**
- Check backend logs for error details
- Verify database connection is working
- Ensure all Stripe payment intent IDs exist in your database

### Webhook Shows "No response" or Timeout

**Cause:** Backend took too long to respond (>30s)

**Solution:**
- Stripe requires 200 OK response within 30 seconds
- Process webhooks asynchronously if needed
- Return 200 immediately, then process in background

### Duplicate Webhook Processing

**Cause:** Stripe may retry webhooks

**Solution:**
- Already handled! The backend uses `stripe_webhook_event` table for idempotency
- Each `event_id` is only processed once

## Security Best Practices

### 1. Always Verify Webhook Signatures

The backend handler already implements this:

```haskell
-- In TDF/Handlers/SocialEvents.hs
stripeWebhookHandler :: Value -> Maybe Text -> Handler NoContent
stripeWebhookHandler payload sigHeader = do
  config <- asks getStripeConfig
  case sigHeader of
    Nothing -> throwError err401 { errBody = "Missing signature" }
    Just sig -> do
      unless (verifyWebhookSignature config sig payload) $
        throwError err401 { errBody = "Invalid signature" }
  -- ... process webhook
```

### 2. Use Different Keys for Test and Production

- Test mode: `sk_test_...`, `pk_test_...`
- Production: `sk_live_...`, `pk_live_...`
- **Never mix** test and live keys

### 3. Restrict Webhook Access

In production, consider:
- IP whitelisting for Stripe webhook IPs
- Separate webhook endpoint subdomain
- Rate limiting on webhook endpoint

### 4. Monitor Webhook Health

Set up alerts for:
- Webhook failures > 5% in 1 hour
- No webhooks received for > 4 hours (during active sales)
- 500 errors on webhook endpoint

## Development vs Production

### Development Setup (Local Testing)

```bash
# Backend .env
STRIPE_SECRET_KEY=sk_test_your_test_key
STRIPE_PUBLISHABLE_KEY=pk_test_your_test_key
STRIPE_WEBHOOK_SECRET=whsec_from_stripe_cli_listen

# Use Stripe CLI for local webhooks
stripe listen --forward-to localhost:8080/social-events/stripe/webhook
```

### Production Setup

```bash
# Backend .env
STRIPE_SECRET_KEY=sk_live_your_live_key
STRIPE_PUBLISHABLE_KEY=pk_live_your_live_key
STRIPE_WEBHOOK_SECRET=whsec_from_dashboard_endpoint

# Configure webhook in Stripe Dashboard pointing to:
# https://api.tdf-app.com/social-events/stripe/webhook
```

## Webhook Event Flow

### Successful Purchase Flow

1. **User submits payment** → Frontend calls `POST /stripe/create-payment-intent`
2. **Backend creates PaymentIntent** → Stripe returns `client_secret`
3. **Frontend confirms payment** → Stripe processes payment
4. **Stripe sends webhook** → `payment_intent.succeeded` to your endpoint
5. **Backend processes webhook:**
   - Verifies signature
   - Checks for duplicate (idempotency)
   - Updates order status to `completed`
   - Generates tickets with QR codes
   - Sends confirmation email with tickets
   - Returns 200 OK to Stripe

### Failed Payment Flow

1. **Payment fails** → Stripe sends `payment_intent.payment_failed` webhook
2. **Backend processes webhook:**
   - Updates order status to `failed`
   - Releases reserved ticket capacity
   - Logs failure reason
   - Returns 200 OK to Stripe

### Refund Flow

1. **Admin approves refund** → Backend calls Stripe API to create refund
2. **Stripe processes refund** → Sends `charge.refunded` webhook
3. **Backend processes webhook:**
   - Updates refund status to `processed`
   - Marks tickets as `refunded`
   - Sends refund confirmation email
   - Returns 200 OK to Stripe

## FAQ

### Q: Do I need webhooks for the ticketing system to work?

**A:** Yes, webhooks are critical. Without webhooks:
- Tickets won't be generated after payment
- Users won't receive confirmation emails
- Refund status won't update automatically
- Payment failures won't be tracked

### Q: Can I use the same webhook endpoint for test and production?

**A:** No. Use separate Stripe accounts (or switch modes in dashboard) and separate webhook endpoints for test vs production.

### Q: What happens if a webhook fails?

**A:** Stripe automatically retries failed webhooks:
- Immediate retry
- After 1 hour
- After 3 hours
- After 9 hours
- Up to 3 days of retries

Fix the issue in your backend, and Stripe will eventually succeed on retry.

### Q: How do I debug webhook issues?

**A:**
1. Check Stripe Dashboard → Webhooks → Your Endpoint → Logs
2. Check your backend application logs
3. Use Stripe CLI to replay events: `stripe events resend evt_123`
4. Use ngrok for local testing with real Stripe webhooks

## Support

For Stripe-specific issues:
- [Stripe Webhooks Documentation](https://stripe.com/docs/webhooks)
- [Stripe Support](https://support.stripe.com/)

For TDF ticketing system issues:
- Check backend logs: `tail -f tdf-hq/logs/app.log`
- Review database for webhook events: `SELECT * FROM stripe_webhook_event`
- Contact development team

---

## 2026-05-28 update — additional webhook events

The original ticketing flow listened to two events. The course-payment and
artist-tipping work added more. Configure all of these on the same webhook
endpoint (`/social-events/stripe/webhook`) — the dispatcher in
`tdf-hq/src/TDF/Server/SocialEventsHandlers.hs:stripeWebhook` already routes
them.

### Event types to enable

| Event | Source | What the handler does |
|-------|--------|------------------------|
| `payment_intent.succeeded` | ticket purchase, course one-off, artist tip | First tries `event_ticket_order`. If no match → falls through to `course_registration`. If still no match → falls through to `artist_tip`. The matching row's status flips to paid. |
| `payment_intent.payment_failed` | same as above | Same dispatch chain; status flips to cancelled (courses, tickets) or failed (artist tips). |
| `charge.refunded` | ticket refunds | Currently no-op; refund flow is initiated server-side via the refund endpoint, not via webhook. |
| `checkout.session.completed` | course subscription | Records `course_registration.stripe_subscription_id` (from `data.object.subscription`) and flips `subscription_status` to `active` + registration `status` to `paid`. Looks up the registration by `metadata.course_registration_id` which the subscription-checkout handler always sets. |
| `customer.subscription.updated` | course subscription | Mirrors Stripe's subscription `status` onto `course_registration.subscription_status`. Terminal statuses (`canceled`, `unpaid`, `incomplete_expired`) also flip the registration's own `status` to `cancelled`. |
| `customer.subscription.deleted` | course subscription | Always flips the registration to `cancelled` and `subscription_status = canceled`. |
| `invoice.paid` | course subscription renewals | Currently logged and no-op. Hook here when you want to track renewal cadence. |

### Idempotency

Every handler reads the row first and only writes when the current state is
the expected pre-state (`pending`, `pending_payment`). Stripe replays no-op,
so it's safe to leave delivery-retries on the default schedule.

### Dispatch chain (one webhook, many surfaces)

```
payment_intent.succeeded
  ├── event_ticket_order match → mark paid + create EventTicket rows
  ├── course_registration match → flip status to paid
  └── artist_tip match → flip status to paid
```

The fall-through is intentional — adding a new Stripe-paid surface in the
future means another link in this chain, not a new webhook endpoint.

### Metadata convention

Every PaymentIntent the backend creates carries a `purpose` metadata field so
Stripe Dashboard searches can filter by source:

- `purpose=ticket_purchase` (extension of the existing ticketing flow)
- `purpose=course_registration`
- `purpose=course_subscription`
- `purpose=artist_tip`

Plus a stable id (`order_id`, `course_registration_id`, `artist_tip_id`) so
support can join a Stripe row to a TDF row without grepping logs.

### Connect (Phase 5)

When artist Connect onboarding lands, add `account.updated`, `payout.paid`,
and `payout.failed` here. The handlers are not implemented yet — the current
dispatcher silently ignores them, which is the safe default.

---

**Last Updated:** 2026-05-28
**Tested With:** Stripe API Version 2026-04-22.dahlia (pinned by `defaultStripeApiVersion`)
