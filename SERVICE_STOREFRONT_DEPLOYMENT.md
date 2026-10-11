# TDF Service Storefront - Deployment Checklist

> Current hosting (2026-09-28): the live API is `https://api.tdfrecords.net`.
> Use [the current guarded deployment/recovery procedure](ops/hetzner/README.md)
> for runtime secrets, releases, backups and logs. Former Fly deployment steps
> below are historical and must not be executed against the retired app/database.
> Preserve existing provider webhook IDs/signing secrets when changing callback
> URLs; inspect existing endpoints before creating a replacement. Existing
> provider/environment restrictions and payment-validation gates still apply.


## Pre-Deployment

### Database
- [ ] Verify the storefront schema and all registered successors against the production migration manifest; do not apply the historical bootstrap SQL directly
- [ ] Apply the registered checkout migrations through `scripts/render-production-migration-batch.mjs`; do not apply an ad hoc subset
- [ ] Verify tables created: `service_storefront_package`, `service_storefront_order`, `service_storefront_order_status_change`, `service_storefront_revision`
- [ ] Verify seed data: 9 packages (3 Mixing, 3 Mastering, 3 Bundle)
- [ ] Backup database before migration

### Backend (Haskell)
- [ ] Add new modules to cabal file:
  - `TDF.API.ServiceStorefront`
  - `TDF.API.ServiceStorefrontTypes`
- [ ] Wire `ServiceStorefrontPublicAPI` into main API
- [ ] Wire `ServiceStorefrontAdminAPI` into admin API
- [ ] Implement server handlers (or use stubs initially)
- [ ] Build and verify: `stack build`
- [ ] Run backend tests: `stack test`

### Frontend (React)
- [ ] Verify TypeScript compilation: `npx tsc --noEmit`
- [ ] Verify all tests pass: `npm run test:ui`
- [ ] Build production bundle: `npm run build:ui`
- [ ] Verify new route accessible: `/mezcla-mastering`

### Payment Configuration
- [ ] Datafast merchant credentials obtained
- [ ] PayPal business account configured
- [ ] Environment variables set:
  ```bash
  # One checkout environment for every enabled provider. Unset defaults to
  # sandbox, and a provider whose own environment differs is rejected (503).
  COMMERCE_CHECKOUT_ENV=production

  # Datafast (names read by tdf-hq; see PAYMENT_AUDIT.md section 6)
  DATAFAST_ENV=production          # must equal COMMERCE_CHECKOUT_ENV
  DATAFAST_BASE_URL=...
  DATAFAST_ENTITY_ID=...
  DATAFAST_BEARER_TOKEN=...
  # Leave DATAFAST_TEST_MODE unset: any value is rejected when
  # DATAFAST_ENV=production. DATAFAST_MID, DATAFAST_TID, DATAFAST_PSERV and
  # DATAFAST_USER_DATA2 belong to the marketplace checkout and are not sent
  # for service orders.

  # PayPal
  PAYPAL_CLIENT_ID=...
  PAYPAL_CLIENT_SECRET=...
  PAYPAL_ENV=production            # must equal COMMERCE_CHECKOUT_ENV
  PAYPAL_MERCHANT_ID=...
  PAYPAL_WEBHOOK_ID=...
  COMMERCE_EVENT_ENCRYPTION_KEY=... # independent 32+ character secret-manager value
  COMMERCE_LOOKUP_TOKEN_SECRET=... # independent 32+ byte guest-capability HMAC key
  ```

### Webhooks
- [ ] PayPal webhook endpoint configured: `https://api.tdfrecords.net/services/storefront/paypal/webhook`
- [ ] Stripe webhook verified (existing): `https://api.tdfrecords.net/social-events/stripe/webhook`
- [ ] Keep Datafast callbacks and refunds disabled until an authenticated merchant contract is verified

### Feature Flags
- [ ] Confirm `commerce.mixing_mastering` remains disabled in production until separate approval
- [ ] PayPal and Datafast webhook/refund gates are approved production capabilities (AUTHORITY-050); confirm their configured state is intentional, not a release default

---

## Deployment Steps

### 1. Database Migration

Use the reviewed manifest and checksum-pinned batch through the canonical
[Hetzner release/recovery procedure](ops/hetzner/README.md). Reconcile the actual
ledger, validate the exact candidate and preserve qualified backups before
applying any pending migration. An incomplete release executor or recovery
qualification blocks production execution; this checklist supplies no ad hoc SQL
or alternate deployment lane.

### 2. Backend Deployment

Stage the reviewed provider credentials and existing webhook ID in the protected
canonical Hetzner environment through that same procedure. Preserve unrelated
configuration and the provider environment; inspect the existing PayPal endpoint
before changing its callback URL and preserve its identity and signing-validation
configuration. Do not create duplicates or rotate credentials merely for a host
correction. Keep unqualified payment/refund switches disabled.

Deploy only the verified immutable candidate through the canonical release lane,
then verify the API identity, database readiness and applicable authenticated
flows. Do not deploy, restart or install secrets on the retired Fly app/database.
Setting environment values alone is not payment or webhook acceptance evidence.

### 3. Frontend Deployment

Use the reviewed main-branch Cloudflare deployment and verify its immutable
identity against `https://www.tdfrecords.net`. A preview build is not production
verification. Local builds use the root `npm run build:ui` command and Node22;
frontend configuration must contain no privileged provider or backend secrets.

### 4. Smoke Tests
```bash
# Check backend health
curl https://api.tdfrecords.net/health

# Check the public package endpoint; this is not payment evidence
curl https://api.tdfrecords.net/services/storefront

# Check frontend
curl -I https://www.tdfrecords.net/mezcla-mastering
```

### 5. Manual Testing
1. Open https://www.tdfrecords.net/mezcla-mastering
2. Verify page loads correctly
3. Test package selection
4. Test order form validation
5. Test payment flow only in the provider sandbox with approved test credentials
6. Verify browser return remains `processing` until server verification or a verified webhook
7. Check order tracking page

---

## Post-Deployment

### Monitoring
- [ ] Set up error tracking (Sentry, LogRocket, etc.)
- [ ] Monitor payment success/failure rates
- [ ] Track conversion metrics (visit → order → payment)
- [ ] Set up alerts for payment failures

### Analytics
- [ ] Verify analytics events firing:
  - `service_page_view`
  - `service_package_selected`
  - `checkout_started`
  - `payment_completed`
  - `order_created`

### Customer Support
- [ ] Prepare support team for new service inquiries
- [ ] Create FAQ document for common questions
- [ ] Set up email templates for order confirmations
- [ ] Define escalation path for payment issues

### Marketing
- [ ] Announce new service via social media
- [ ] Email existing customer base
- [ ] Create landing page promotion
- [ ] Consider paid advertising (Instagram, Facebook)

---

## Rollback Procedure

Follow the canonical recovery procedure using the inspected deployment identity,
qualified database/assets/private-upload backups and a schema-compatible recovery
image. Preserve financial records, idempotency keys, orders and provider event
history. Do not assume an additive migration makes an older binary compatible,
replay a charge, manually settle orders, or discard tables as a rollback shortcut.
Feature containment and any required provider action remain separately reviewed.

---

## Historical Phase 1 backlog (not current implementation status)

The following records the initial roadmap, not a live feature inventory. Current
contracts, feature gates, implemented handlers and release evidence determine
availability; do not infer missing or enabled functionality from this list.

1. **Backend handlers not yet implemented** - Frontend page works but API calls will fail until handlers are added
2. **File upload not implemented** - Customers cannot upload tracks yet (manual process)
3. **Email notifications not implemented** - Order confirmations sent manually
4. **Admin order management not implemented** - Orders visible in DB but no admin UI
5. **Revision workflow not implemented** - Revision requests handled manually

### Historical Phase 2 proposal
- Implement backend server handlers
- Add file upload (Google Drive integration)
- Add email notifications (SendGrid/SES)
- Build admin order management UI
- Implement revision request workflow

---

## Success Criteria

- [ ] Page loads without errors
- [ ] All 9 packages display correctly
- [ ] Order form validates properly
- [ ] Payment flow completes (test mode)
- [ ] Order confirmation displays
- [ ] Order tracking works
- [ ] No TypeScript errors
- [ ] All tests pass
- [ ] Mobile responsive
- [ ] Accessibility compliant (WCAG 2.1 AA)

---

## Contacts

- **Datafast Support:** support@datafast.com.ec
- **PayPal Support:** support@paypal.com
- **Cloudflare Support:** support@cloudflare.com

---

*Last updated: August 4, 2026*
