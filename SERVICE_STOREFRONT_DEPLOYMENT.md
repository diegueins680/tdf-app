# TDF Service Storefront - Deployment Checklist

## Pre-Deployment

### Database
- [ ] Run migration: `tdf-hq/sql/2026-08-04_service_storefront.sql`
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
  # Datafast
  DATAFAST_MERCHANT_ID=...
  DATAFAST_API_KEY=...
  DATAFAST_ENVIRONMENT=prod
  
  # PayPal
  PAYPAL_CLIENT_ID=...
  PAYPAL_CLIENT_SECRET=...
  PAYPAL_WEBHOOK_ID=...
  ```

### Webhooks
- [ ] PayPal webhook endpoint configured: `https://tdf-hq.fly.dev/services/storefront/paypal/webhook`
- [ ] Stripe webhook verified (existing): `https://tdf-hq.fly.dev/social-events/stripe/webhook`
- [ ] Datafast uses redirect-based confirmation (no webhook needed)

### Feature Flags
- [ ] Consider adding feature flag for gradual rollout
- [ ] Default: enabled for all users

---

## Deployment Steps

### 1. Database Migration
```bash
cd tdf-hq
psql -h $DB_HOST -U $DB_USER -d $DB_NAME -f sql/2026-08-04_service_storefront.sql
```

### 2. Backend Deployment
```bash
cd tdf-hq
stack build --copy-bins
flyctl deploy --app tdf-hq
```

### 3. Frontend Deployment
```bash
cd tdf-hq-ui
npm run build
wrangler pages deploy dist --project-name=tdf-app
# Or push to git for auto-deployment via Cloudflare
```

### 4. Smoke Tests
```bash
# Check backend health
curl https://tdf-hq.fly.dev/health

# Check new endpoint (will 404 until handlers implemented)
curl https://tdf-hq.fly.dev/services/storefront

# Check frontend
curl -I https://tdf-app.pages.dev/mezcla-mastering
```

### 5. Manual Testing
1. Open https://tdf-app.pages.dev/mezcla-mastering
2. Verify page loads correctly
3. Test package selection
4. Test order form validation
5. Test payment flow (use test credentials)
6. Verify order confirmation
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

### If Critical Issues Found

1. **Disable feature immediately:**
   - Remove route from `publicRoutes.tsx`
   - Redeploy frontend

2. **If payment issues:**
   - Disable payment endpoints
   - Manually process pending orders

3. **If database issues:**
   - Tables are additive (no destructive changes)
   - Can keep tables, just disable API endpoints

4. **Full rollback:**
   ```bash
   # Revert frontend
   git revert <commit-hash>
   git push
   
   # Backend: tables remain, just don't use them
   # No database rollback needed
   ```

---

## Known Limitations (Phase 1)

1. **Backend handlers not yet implemented** - Frontend page works but API calls will fail until handlers are added
2. **File upload not implemented** - Customers cannot upload tracks yet (manual process)
3. **Email notifications not implemented** - Order confirmations sent manually
4. **Admin order management not implemented** - Orders visible in DB but no admin UI
5. **Revision workflow not implemented** - Revision requests handled manually

### Recommended Phase 2 (Week 2-3)
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
- **Fly.io Support:** support@fly.io
- **Cloudflare Support:** support@cloudflare.com

---

*Last updated: August 4, 2026*
