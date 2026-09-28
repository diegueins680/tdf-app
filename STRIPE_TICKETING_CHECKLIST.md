# Stripe Ticketing Deployment Checklist

> Current hosting (2026-09-28): the live API is `https://api.tdfrecords.net`.
> Use [the current guarded deployment/recovery procedure](ops/hetzner/README.md)
> for runtime secrets, releases, backups and logs. Former Fly deployment steps
> below are historical and must not be executed against the retired app/database.
> Preserve existing provider webhook IDs/signing secrets when changing callback
> URLs; inspect existing endpoints before creating a replacement. Existing
> provider/environment restrictions and payment-validation gates still apply.


## ✅ Completed (by Aria)

- [x] Fixed `Config.hs` to load Stripe env vars from environment
- [x] Backend builds successfully (90 modules compiled)
- [x] Updated frontend `.env` with Stripe placeholder
- [x] Created deployment scripts
- [x] Created comprehensive documentation

## ⬜ Your Action Items

### Step 1: Get Stripe Keys (5 mins)
- [ ] Go to https://dashboard.stripe.com/test/apikeys
- [ ] Copy **Publishable key** (starts with `pk_test_`)
- [ ] Copy **Secret key** (starts with `sk_test_`)

### Step 2: Configure Webhook (5 mins)
- [ ] Go to https://dashboard.stripe.com/test/webhooks
- [ ] Click **+ Add endpoint**
- [ ] Enter URL: `https://api.tdfrecords.net/social-events/stripe/webhook`
- [ ] Select events:
  - [ ] `payment_intent.succeeded`
  - [ ] `payment_intent.payment_failed`
- [ ] Save and copy **Signing secret** (starts with `whsec_`)

### Step 3: Apply the reviewed backend configuration

- [ ] Use [the current guarded deployment runbook](ops/hetzner/README.md) to
  manage Stripe configuration in the protected Hetzner runtime environment.
- [ ] Preserve webhook signing secrets, provider environment and release/recovery
  controls; verify the current release and callback behavior before completion.
- [ ] Do not update or restart the retired Fly app/database.

### Step 4: Deploy Frontend (5 mins)
- [ ] Go to Cloudflare Pages dashboard
- [ ] Add environment variable:
  - Name: `VITE_STRIPE_PUBLISHABLE_KEY`
  - Value: `pk_test_YOUR_KEY`
- [ ] Redeploy site

### Step 5: Test (10 mins)
- [ ] Open app in browser
- [ ] Create test event
- [ ] Click "Buy Tickets"
- [ ] Use test card: `4242 4242 4242 4242`
  - Expiry: Any future date
  - CVC: *** digits
  - ZIP: Any 5 digits
- [ ] Complete purchase
- [ ] Check Stripe Dashboard → Webhooks → Logs (should show 200 OK)
- [ ] Verify ticket appears with QR code

## 🚨 Important Notes

1. **Test mode first** - Use `pk_test_` and `sk_test_` keys initially
2. **Webhook URL** - Must be HTTPS (the current API edge provides TLS)
3. **Signing secret** - Different for each webhook endpoint
4. **Environment variables** - Never commit keys to git

## 🔧 Troubleshooting

| Issue | Solution |
|-------|----------|
| Webhook 401 | Check `STRIPE_WEBHOOK_SECRET` is set correctly |
| Webhook 404 | Backend not deployed or URL is wrong |
| Payment fails | Check `STRIPE_SECRET_KEY` is correct |
| Frontend can't load Stripe | Check `VITE_STRIPE_PUBLISHABLE_KEY` is set |

## 📚 References

- Full guide: `STRIPE_DEPLOYMENT_GUIDE.md`
- Test script: `scripts/test-stripe-integration.sh`
- Deploy script: `scripts/deploy-stripe-ticketing.sh`
