# Verification Guide - PayPal & Fly Configuration

This document provides steps to verify the PayPal client ID configuration and Fly deployment are working correctly.

## 1. Verify Fly Configuration

### Check fly.toml
Confirm that sensitive values are commented out and reference secrets:
```bash
grep -A 2 "VITE_PAYPAL_CLIENT_ID" fly.toml
# Should show: # VITE_PAYPAL_CLIENT_ID is set as a Fly secret...
```

### Check Fly Secrets
List secrets in Fly (requires `flyctl` and authentication):
```bash
flyctl secrets list --app tdf-hq
```

Should include:
- `VITE_PAYPAL_CLIENT_ID`
- `VITE_LIVE_SESSIONS_PUBLIC_TOKEN`

### Set Secrets (if missing)
```bash
# Set PayPal client ID
flyctl secrets set VITE_PAYPAL_CLIENT_ID=your-actual-paypal-client-id --app tdf-hq

# Set live sessions token
flyctl secrets set VITE_LIVE_SESSIONS_PUBLIC_TOKEN=your-actual-token --app tdf-hq
```

## 2. Verify Health Endpoint

### Test Local Health Endpoint
If running locally via Docker:
```bash
cd tdf-hq
make up
make health
```

Expected response:
```json
{
  "status": "ok",
  "db": "ok"
}
```

### Test Production Health Endpoint
```bash
curl https://tdf-hq.fly.dev/health
```

Expected response:
```json
{
  "status": "ok",
  "db": "ok"
}
```

### Check During Boot
The health endpoint returns `{"status":"starting","db":"starting"}` during initialization (Main.hs line 89).

## 3. Verify Port Binding

### Check Haskell Configuration
```bash
grep -A 3 "setHost" tdf-hq/app/Main.hs
```

Should show:
```haskell
Warp.setHost "0.0.0.0" $
```

### Check fly.toml Service Configuration
```bash
grep -A 5 "services" fly.toml
```

Should show:
```toml
[[services]]
  protocol = "tcp"
  internal_port = 8080
  ...
```

### Test Connection
```bash
flyctl status --app tdf-hq
```

Look for:
- Services listening on port 8080 internally
- HTTP/HTTPS ports (80/443) exposed externally
- All instances showing "running" status

## 4. Verify PayPal Integration

### Check UI Environment Variables (Cloudflare Pages)

1. Go to Cloudflare Pages Dashboard
2. Select your project (tdf-app)
3. Go to Settings → Environment variables
4. Verify `VITE_PAYPAL_CLIENT_ID` is set for Production

### Test PayPal Buttons in Marketplace

1. Navigate to the Marketplace page: https://tdf-app.pages.dev/marketplace
2. Add items to cart
3. Proceed to checkout
4. Verify PayPal button appears as a payment option
5. Check browser console for warnings:
   - If PayPal is disabled, you'll see: "PayPal deshabilitado: falta VITE_PAYPAL_CLIENT_ID en build o runtime."

### Verify PayPal Script Loading
Open browser DevTools → Network tab:
```
https://www.paypal.com/sdk/js?client-id=<your-client-id>&currency=USD
```
Should load successfully (200 status).

## 5. Post-Deployment Checklist

After deploying or updating secrets:

- [ ] Health endpoint responds with `{"status":"ok","db":"ok"}`
- [ ] `flyctl status` shows all instances running
- [ ] `flyctl logs` shows no errors about missing environment variables
- [ ] UI marketplace loads without PayPal warnings (if configured)
- [ ] PayPal buttons render in checkout flow (if configured)

## Troubleshooting

### Health Endpoint Not Responding
```bash
# Check Fly logs
flyctl logs --app tdf-hq

# Check app status
flyctl status --app tdf-hq

# SSH into instance (if needed)
flyctl ssh console --app tdf-hq
```

### PayPal Buttons Not Appearing

1. Check browser console for errors
2. Verify environment variable in Cloudflare Pages
3. Check if `paypalEnabled` is true in MarketplacePage component state
4. Verify PayPal SDK loaded in Network tab

### Port Binding Issues

If Fly shows "app not listening on 0.0.0.0:8080":
1. Check APP_PORT environment variable in fly.toml
2. Verify Dockerfile CMD uses APP_PORT
3. Confirm Main.hs binds to 0.0.0.0 (not localhost)
4. Review Fly logs for startup errors

## Security Notes

- Never commit actual PayPal client IDs to git
- Use Fly secrets for backend environment variables
- Use Cloudflare Pages environment variables for frontend build-time vars
- Rotate secrets periodically
- Monitor Fly logs for suspicious activity
