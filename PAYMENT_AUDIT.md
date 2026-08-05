# TDF Payment Audit & Test Matrix

**Date:** August 4, 2026  
**Scope:** Complete payment lifecycle analysis for TDF platform

---

## 1. Current Payment Architecture

### Payment Methods by Availability

| Method | Ecuador | International | Implementation Status | Notes |
|--------|---------|---------------|----------------------|-------|
| **Cash** | ✅ | ❌ | ✅ Complete | Manual recording in admin |
| **Bank Transfer** | ✅ | ❌ | ✅ Complete | Manual verification |
| **Card POS** | ✅ | ❌ | ✅ Complete | Physical terminal only |
| **Stripe** | ❌ | ✅ | ✅ Complete | NOT available in Ecuador |
| **Datafast** | ✅ | ❌ | ✅ Complete | Local card processing |
| **PayPal** | ✅ | ✅ | ✅ Complete | International + Ecuador |
| **PayPhone** | ✅ | ❌ | ⚠️ Enum only | Not implemented |
| **Wompi** | ❌ | ❌ | ❌ N/A | Not available in Ecuador |
| **Crypto** | ✅ | ✅ | ⚠️ Enum only | Not implemented |

### Critical Finding: Stripe Unavailable in Ecuador

Stripe is **NOT available** in Ecuador. Only Brazil and Mexico are supported in Latin America. The codebase has extensive Stripe integration (PaymentIntents, Connect, webhooks), but it **cannot process Ecuadorian transactions**.

**Implications:**
- Stripe Connect cannot onboard Ecuadorian artists for payouts
- Stripe Checkout cannot process Ecuadorian card payments
- All Stripe-dependent features (tipping, course registration, marketplace) are non-functional for Ecuadorian users

**Required Action:** Use Datafast (local cards) + PayPal (international) as primary payment rails for Ecuador.

### Existing Payment Handlers (Marketplace)

The marketplace already has complete payment handlers:

| Handler | Location | Status |
|---------|----------|--------|
| `createDatafastCheckout` | Server.hs:13564 | ✅ Implemented |
| `createPaypalOrder` | Server.hs:13698 | ✅ Implemented |
| `capturePaypalOrder` | Server.hs | ✅ Implemented |
| `createStripePaymentIntent` | Server.hs | ✅ Implemented (non-functional in EC) |
| Datafast status confirmation | Server.hs | ✅ Implemented |
| PayPal webhook handling | SocialEventsHandlers.hs | ✅ Implemented |
| Stripe webhook handling | SocialEventsHandlers.hs | ✅ Implemented |

### Payment Flow (Marketplace - Working)

```
Customer → Select items → Cart → Checkout → 
  ├─ Datafast: POST /datafast/checkout → Widget URL → Pay → Redirect → GET /datafast/status
  ├─ PayPal: POST /paypal/create → Approval URL → Approve → POST /paypal/capture
  └─ Stripe: POST /stripe/payment-intent → Client secret → Elements → Webhook
```

### Payment Flow (Service Storefront - New)

```
Customer → Select package → Order form → Payment →
  ├─ Datafast: POST /services/storefront/order/:id/datafast/checkout → Widget → Confirm
  ├─ PayPal: POST /services/storefront/order/:id/paypal/create → Approve → Capture
  └─ Stripe: POST /services/storefront/order/:id/stripe/payment-intent → Elements → Webhook
```

---

## 2. Payment Lifecycle Analysis

### Order Status Workflow

```
pending_payment → paid → in_progress → v1_delivered → revisions → approved → delivered → completed
                  ↓
            payment_failed (retry available)
                  ↓
            cancelled (refund possible)
```

### Idempotency Guarantees

| Component | Mechanism | Status |
|-----------|-----------|--------|
| Stripe PaymentIntent | `stripe_idempotency_key` column | ✅ Implemented |
| Datafast checkout | `datafast_checkout_id` column | ✅ Implemented |
| PayPal order | `paypal_order_id` column | ✅ Implemented |
| Order creation | `order_number` unique constraint | ✅ Implemented |
| Webhook processing | `stripe_payment_intent_id` unique | ✅ Implemented |
| Duplicate webhook events | Status check before update | ✅ Implemented |

### Webhook Handling

| Webhook Source | Endpoint | Idempotency | Signature Verification |
|---------------|----------|-------------|----------------------|
| Stripe | `/social-events/stripe/webhook` | ✅ Event ID dedup | ✅ HMAC-SHA256 |
| PayPal | Via capture flow | ✅ Order ID dedup | ⚠️ Needs verification |
| Datafast | Via redirect status | ✅ Checkout ID dedup | ⚠️ Needs verification |

### Refund Capability

| Method | Full Refund | Partial Refund | Implementation |
|--------|------------|----------------|----------------|
| Stripe | ✅ `createRefund` exists | ❌ Not implemented | Server.hs |
| Datafast | ❌ Not implemented | ❌ Not implemented | Needs Datafast API |
| PayPal | ❌ Not implemented | ❌ Not implemented | Needs PayPal API |
| Cash/Bank | ✅ Manual | ✅ Manual | Admin UI |

---

## 3. Test Matrix

### Unit Tests Required

| Test | Priority | Status |
|------|----------|--------|
| Order number generation uniqueness | High | 🔴 Needed |
| Price calculation (cents → display) | High | 🔴 Needed |
| Package filtering by service kind | Medium | 🔴 Needed |
| Form validation (email, required fields) | High | 🔴 Needed |
| Status transition validation | High | 🔴 Needed |
| Revision count enforcement | Medium | 🔴 Needed |

### Integration Tests Required

| Test | Priority | Status |
|------|----------|--------|
| Create order → Datafast checkout → Confirm | High | 🔴 Needed |
| Create order → PayPal create → Capture | High | 🔴 Needed |
| Create order → Stripe intent → Webhook | High | 🔴 Needed (non-functional in EC) |
| Duplicate order submission prevention | High | 🔴 Needed |
| Webhook idempotency (duplicate events) | High | 🔴 Needed |
| Order status transitions | High | 🔴 Needed |
| Revision request workflow | Medium | 🔴 Needed |
| Email notification triggers | Medium | 🔴 Needed |

### End-to-End Tests Required

| Test | Priority | Status |
|------|----------|--------|
| Full purchase flow (Datafast) | High | 🔴 Needed |
| Full purchase flow (PayPal) | High | 🔴 Needed |
| Order tracking page | Medium | 🔴 Needed |
| Revision request flow | Medium | 🔴 Needed |
| Admin order management | Medium | 🔴 Needed |
| Mobile responsive checkout | Medium | 🔴 Needed |

### Payment-Specific Tests

| Scenario | Stripe | Datafast | PayPal | Cash |
|----------|--------|----------|--------|------|
| Successful payment | ✅ | 🔴 | 🔴 | ✅ Manual |
| Failed payment (insufficient funds) | ✅ | 🔴 | 🔴 | N/A |
| Failed payment (expired card) | ✅ | 🔴 | 🔴 | N/A |
| Duplicate webhook event | ✅ | 🔴 | 🔴 | N/A |
| Out-of-order webhook delivery | ✅ | 🔴 | 🔴 | N/A |
| Refund (full) | ✅ Code | 🔴 | 🔴 | 🔴 Manual |
| Refund (partial) | ❌ | 🔴 | 🔴 | 🔴 Manual |
| Currency conversion | ❌ | 🔴 | 🔴 | N/A |
| Idempotent retry | ✅ | 🔴 | 🔴 | N/A |
| Abandoned checkout recovery | ✅ | 🔴 | 🔴 | N/A |
| Payment timeout handling | ✅ | 🔴 | 🔴 | N/A |

---

## 4. Security Audit

### Payment Security Checklist

| Item | Status | Notes |
|------|--------|-------|
| Server-side price calculation | ✅ | Prices from DB, not client |
| Client cannot modify amounts | ✅ | Amounts computed server-side |
| Webhook signature verification | ✅ Stripe, ⚠️ Others | PayPal/Datafast need verification |
| Idempotency keys for all payments | ✅ | Prevents duplicate charges |
| PCI DSS compliance | ✅ | No card data stored |
| HTTPS everywhere | ✅ | Enforced by deployment |
| CSRF protection | ✅ | Servant + CORS config |
| Rate limiting on checkout | ⚠️ | Needs implementation |
| Order number unpredictability | ✅ | UUID-based |
| PII encryption at rest | ⚠️ | Email/phone stored plaintext |

### Recommendations

1. **Add rate limiting** to checkout endpoints (prevent abuse)
2. **Encrypt PII** at rest (buyer email, phone)
3. **Implement PayPal webhook signature verification**
4. **Implement Datafast webhook signature verification**
5. **Add refund support** for Datafast and PayPal
6. **Add partial refund support** for Stripe

---

## 5. Country-Specific Payment Configuration

### Ecuador

| Method | Provider | Fees | Settlement | Notes |
|--------|----------|------|------------|-------|
| Cards (local) | Datafast | ~3.5% + IVA | T+1 | All Ecuadorian cards |
| Cards (intl) | PayPal | 3.4% + $0.30 | Instant | Higher fees but global |
| Bank transfer | Manual | Free | Manual | Requires verification |
| Cash | Manual | Free | Immediate | In-person only |

### International

| Method | Provider | Fees | Settlement | Notes |
|--------|----------|------|------------|-------|
| Cards | Stripe | 2.9% + $0.30 | T+2 | Not available in EC |
| Cards | PayPal | 3.4% + $0.30 | Instant | Global coverage |
| Bank transfer | Wise/Payoneer | Variable | T+3 | For provider payouts |

### Recommended Payment Strategy

**For Ecuadorian customers:**
1. Primary: Datafast (local cards, lower fees)
2. Secondary: PayPal (international cards)
3. Fallback: Bank transfer / Cash

**For international customers:**
1. Primary: PayPal (global coverage)
2. Secondary: Stripe (when available via Stripe Atlas/Atlas)
3. Fallback: Bank transfer

---

## 6. Deployment Requirements

### Environment Variables Needed

```bash
# Datafast (Ecuador card processing)
DATAFAST_MERCHANT_ID=<from Datafast>
DATAFAST_API_KEY=<from Datafast>
DATAFAST_ENVIRONMENT=prod  # or test

# PayPal (International)
PAYPAL_CLIENT_ID=<from PayPal Developer>
PAYPAL_CLIENT_SECRET=<from PayPal Developer>
PAYPAL_WEBHOOK_ID=<from PayPal Dashboard>
PAYPAL_ENVIRONMENT=live  # or sandbox

# Stripe (International - NOT for Ecuador)
STRIPE_SECRET_KEY=sk_live_...
STRIPE_PUBLISHABLE_KEY=pk_live_...
STRIPE_WEBHOOK_SECRET=whsec_...
```

### Webhook Endpoints to Configure

| Provider | URL | Events |
|----------|-----|--------|
| Stripe | `https://tdf-hq.fly.dev/social-events/stripe/webhook` | `payment_intent.succeeded`, `payment_intent.payment_failed`, `charge.refunded` |
| PayPal | `https://tdf-hq.fly.dev/services/storefront/paypal/webhook` | `PAYMENT.CAPTURE.COMPLETED`, `PAYMENT.CAPTURE.DENIED` |
| Datafast | Redirect-based (no webhook needed) | N/A |

---

## 7. Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|------------|
| Stripe unavailable in Ecuador | High | Use Datafast + PayPal |
| Datafast onboarding delay | Medium | Start process immediately |
| PayPal dispute resolution | Medium | Clear terms + delivery proof |
| Payment fraud | Medium | Rate limiting + verification |
| Currency conversion losses | Low | Price in USD, settle in USD |
| Webhook delivery failures | Medium | Retry logic + manual reconciliation |

---

*This audit should be updated after each payment integration is completed and tested.*
