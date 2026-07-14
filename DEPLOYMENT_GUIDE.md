# Deployment Guide - TDF Records Platform

This guide covers deployment procedures for all components of the TDF Records platform.

## Overview

The TDF platform consists of three deployable components:

| Component | Technology | Deployment Target | Status |
|-----------|-----------|-------------------|--------|
| **Backend API** | Haskell + PostgreSQL | Koyeb / Fly.io | Production |
| **Web UI** | React/Vite SPA | Cloudflare Pages / Vercel | Production |
| **Mobile App** | Expo/React Native | App Store / Play Store | Development |

## Prerequisites

### For All Deployments
- Git repository access
- Environment variables documented
- Domain names configured (if using custom domains)

### For Backend
- PostgreSQL database (managed service recommended)
- SMTP credentials for email
- Docker knowledge (for containerized deployment)

### For Frontend
- Node.js 20.19.4+ installed locally for builds
- API endpoint URL ready

### For Mobile
- Expo account
- Apple Developer account (iOS)
- Google Play Developer account (Android)

---

## Backend Deployment (Koyeb)

### Option 1: Dockerfile Deployment (Recommended)

Koyeb can build and deploy directly from the Dockerfile in `tdf-hq/`.

#### 1. Prepare Repository

Ensure `tdf-hq/Dockerfile` is present and working:

```dockerfile
FROM haskell:9.2 as builder
WORKDIR /app
COPY tdf-hq.cabal stack.yaml stack.yaml.lock ./
RUN stack build --only-dependencies
COPY . .
RUN stack build --copy-bins

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y libpq-dev ca-certificates
COPY --from=builder /root/.local/bin/tdf-hq /usr/local/bin/
CMD ["tdf-hq"]
```

#### 2. Create Koyeb Service

1. Go to [Koyeb Dashboard](https://app.koyeb.com/)
2. Click **Create Service**
3. Select **GitHub** as source
4. Choose your repository
5. Set **Root Directory** to `tdf-hq`
6. Koyeb will auto-detect the Dockerfile

#### 3. Configure Environment Variables

Add these in the Koyeb service settings:

```env
# Database (use Koyeb PostgreSQL or external)
DB_HOST=<your-db-host>
DB_PORT=5432
DB_USER=<your-db-user>
DB_PASS=<your-db-password>
DB_NAME=tdf_hq

# Application
APP_PORT=8080
RESET_DB=false
SEED_DB=false
RUN_MIGRATIONS=false
EVENT_DISCOVERY_ENABLED=false

# Web UI URL (for CORS)
HQ_APP_URL=https://your-frontend-url.pages.dev

# SMTP (for email)
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USERNAME=<your-email>
SMTP_PASSWORD=<your-app-password>
SMTP_FROM=ops@tdfrecords.com
SMTP_FROM_NAME=TDF Records
SMTP_TLS=true

# CORS Configuration
ALLOW_ORIGINS=https://your-frontend-url.pages.dev,https://your-custom-domain.com
ALLOW_ALL_ORIGINS=false
```

#### 4. Deploy

Click **Deploy** and wait for the build to complete (usually 10-15 minutes for first build).

#### 5. Verify Deployment

```bash
# Check health endpoint
curl https://your-app.koyeb.app/health

# Expected response:
# {"status":"ok","database":"connected","timestamp":"2024-11-14T..."}

# Check version
curl https://your-app.koyeb.app/version

# Test API
curl https://your-app.koyeb.app/parties
```

### Option 2: Docker Compose (Self-Hosted)

For deployment to your own server with Docker Compose:

#### 1. Prepare Server

```bash
# Install Docker and Docker Compose
curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

#### 2. Clone Repository

```bash
git clone <repository-url>
cd tdf-app/tdf-hq
```

#### 3. Configure Environment

```bash
cp config/default.env .env
# Edit .env with production values
```

#### 4. Deploy

```bash
# Start services
docker-compose up -d

# View logs
docker-compose logs -f

# Check status
docker-compose ps
```

#### 5. Setup Reverse Proxy (Nginx)

```nginx
server {
    listen 80;
    server_name api.yourdomain.com;
    
    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

---

## Backend Deployment (Fly.io)

Fly powers the production `tdf-hq` API. Production releases must use the guarded release lane; do not run `fly deploy` or `scripts/deploy-stripe-ticketing.sh production` directly.

Application startup never owns production schema changes. Both `RUN_MIGRATIONS` and `EVENT_DISCOVERY_ENABLED` remain `false` in `fly.toml` during a release. The release lane applies the reviewed, additive SQL migrations once before updating any API Machine, deploys an image pinned to the requested full git SHA, and verifies the running image digest, version payload, and healthy response.

### Guarded release commands

Run these commands from the repository root. Use the same 40-character commit SHA throughout:

```bash
# Local-only: show the immutable image and ordered release sequence.
npm run release:backend:plan -- --sha <full-sha>

# Read-only: verify production schema/data, image availability, and Fly state.
npm run release:backend:preflight -- --sha <full-sha>

# Mutating: apply pending migrations once and perform the rolling deployment.
npm run release:backend -- --sha <full-sha> --execute --confirm <full-sha>
```

The execute command deliberately requires both `--execute` and an exact SHA confirmation. A failed preflight blocks migration and deployment. Do not bypass that refusal with direct Fly commands.

For the ticket checkout and event-discovery release, the required order is:

1. Verify the already repaired notification schema.
2. Apply `tdf-hq/sql/2026-07-12_ticketing_runtime_schema.sql` to repair the previously unapplied ticketing base schema.
3. Apply `tdf-hq/sql/2026-07-12_ticket_checkout_idempotency.sql`.
4. Apply `tdf-hq/sql/2026-07-12_event_discovery_imports.sql`.
5. Roll out the pinned backend image with discovery still disabled.
6. Verify each started API Machine, the public `/health` and `/version` endpoints, and logs before enabling any new background job.

Before this release can execute, Fly must have `STRIPE_SECRET_KEY` and `STRIPE_WEBHOOK_SECRET` set. The lane also requires every running Machine to have effective `RUN_MIGRATIONS=false` and `EVENT_DISCOVERY_ENABLED=false`. Disable any main-branch auto-deploy path before pushing this release work; schema application must happen only through the guarded lane.

These migrations are additive. If application rollback is required, restore the previous pinned image and leave the new nullable columns/tables in place.

### Setting Secrets in Fly

Sensitive values like API keys should be set as secrets instead of environment variables in `fly.toml`:

```bash
# Set PayPal client ID
flyctl secrets set VITE_PAYPAL_CLIENT_ID=your-paypal-client-id

# Set live sessions token
flyctl secrets set VITE_LIVE_SESSIONS_PUBLIC_TOKEN=your-token

# Ticketmaster may be stored before rollout, but discovery stays disabled in fly.toml
flyctl secrets set TICKETMASTER_API_KEY=your-consumer-key

# List current secrets
flyctl secrets list
```

Note: Secrets are not shown in `fly.toml` to avoid leaking sensitive values.

### Health Check

The API provides a health check endpoint at `/health` that returns:

```json
{
  "status": "ok",
  "db": "ok"
}
```

After deploying or updating secrets, verify the service is healthy:

```bash
# Check health endpoint
curl https://tdf-hq.fly.dev/health

# Check deployment status
flyctl status

# View logs
flyctl logs
```

During application boot, `/health` can temporarily report a `starting` payload. Release verification must wait for `status: "ok"` and `db: "ok"`; an HTTP 200 alone is not sufficient. Fly uses the endpoint for rolling Machine health checks, while the release lane verifies the response body and exact `/version` SHA.

The service is configured to:

- Listen on `0.0.0.0:8080` (accepts connections from Fly's proxy)
- Use `internal_port = 8080` in fly.toml for service binding
- Expose ports 80 (HTTP) and 443 (HTTPS) via Fly's proxy
- Roll Machines gradually and require the `/health` HTTP check
- Keep startup migrations and Ticketmaster discovery disabled by default

## Frontend Deployment

### Option 1: Cloudflare Pages (Recommended)

Cloudflare Pages offers excellent performance with global CDN.

#### 1. Connect Repository

1. Go to [Cloudflare Dashboard](https://dash.cloudflare.com/)
2. Navigate to **Pages**
3. Click **Create a project**
4. Connect your GitHub account
5. Select your repository

#### 2. Configure Build Settings

| Setting | Value |
|---------|-------|
| **Production branch** | `main` |
| **Build command** | `npm run build:ui` |
| **Build output directory** | `tdf-hq-ui/dist` |
| **Root directory** | `.` (repository root) |

#### 3. Set Environment Variables

Add in Cloudflare Pages settings:

```env
NODE_VERSION=20.19.4
VITE_API_BASE=https://tdf-hq.fly.dev
VITE_TZ=America/Guayaquil
VITE_PAYPAL_CLIENT_ID=your-paypal-client-id
VITE_GOOGLE_CLIENT_ID=your-google-client-id
VITE_CHATKIT_WORKFLOW_ID=your-chatkit-workflow-id
VITE_META_APP_ID=your-meta-app-id
VITE_INSTAGRAM_OAUTH_PROVIDER=facebook
VITE_INSTAGRAM_SCOPES=instagram_basic,instagram_manage_messages,instagram_business_basic,instagram_business_manage_messages,pages_show_list,pages_read_engagement
```

Optional for demo mode:
```env
VITE_API_DEMO_TOKEN=your-demo-token
```

**Important:** The `VITE_PAYPAL_CLIENT_ID` must be set in Cloudflare Pages environment variables for PayPal buttons to appear in the Marketplace. This is separate from the Fly secret since the UI is built and served by Cloudflare Pages.

#### 4. Deploy

Cloudflare will automatically build and deploy on every push to `main`.

#### 5. Custom Domain (Optional)

1. Go to **Custom domains** in Pages settings
2. Add your domain (e.g., `app.tdfrecords.com`)
3. Update DNS records as instructed
4. Wait for SSL certificate provisioning

#### 6. Verify Deployment

Visit `https://your-project.pages.dev` or your custom domain.

### Option 2: Vercel

Vercel is a great alternative with similar features.

#### 1. Import Project

1. Go to [Vercel Dashboard](https://vercel.com/dashboard)
2. Click **Add New** → **Project**
3. Import your Git repository

#### 2. Configure Project

| Setting | Value |
|---------|-------|
| **Framework Preset** | Vite |
| **Root Directory** | `tdf-hq-ui` |
| **Build Command** | `npm run build` |
| **Output Directory** | `dist` |

#### 3. Environment Variables

Add in Vercel project settings:

```env
VITE_API_BASE=https://tdf-hq.fly.dev
VITE_TZ=America/Guayaquil
VITE_PAYPAL_CLIENT_ID=your-paypal-client-id
VITE_GOOGLE_CLIENT_ID=your-google-client-id
VITE_CHATKIT_WORKFLOW_ID=your-chatkit-workflow-id
VITE_META_APP_ID=your-meta-app-id
VITE_INSTAGRAM_OAUTH_PROVIDER=facebook
VITE_INSTAGRAM_SCOPES=instagram_basic,instagram_manage_messages,instagram_business_basic,instagram_business_manage_messages,pages_show_list,pages_read_engagement
```

#### 4. Deploy

Vercel automatically deploys on push to main branch.

### Option 3: Static Hosting (Nginx)

For self-hosted static files:

#### 1. Build Locally

```bash
cd tdf-hq-ui
npm install
npm run build
# Output in dist/
```

#### 2. Upload to Server

```bash
# Using rsync
rsync -avz dist/ user@server:/var/www/tdf-app/

# Or using SCP
scp -r dist/* user@server:/var/www/tdf-app/
```

#### 3. Configure Nginx

```nginx
server {
    listen 80;
    server_name app.yourdomain.com;
    root /var/www/tdf-app;
    index index.html;
    
    # SPA routing
    location / {
        try_files $uri $uri/ /index.html;
    }
    
    # Cache static assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}
```

---

## Mobile App Deployment

### iOS Deployment

#### Prerequisites

- Active Apple Developer account ($99/year)
- Mac with Xcode installed
- Expo account

#### 1. Configure App

Update `app.json` in `tdf-mobile/`:

```json
{
  "expo": {
    "name": "TDF Records",
    "slug": "tdf-records",
    "version": "1.0.0",
    "ios": {
      "bundleIdentifier": "com.tdfrecords.app",
      "buildNumber": "1"
    }
  }
}
```

#### 2. Build with EAS

```bash
cd tdf-mobile

# Install EAS CLI
npm install -g eas-cli

# Login to Expo
eas login

# Configure project
eas build:configure

# Build for iOS
eas build --platform ios
```

#### 3. Submit to App Store

```bash
# Submit to TestFlight
eas submit --platform ios --latest

# Or download IPA and upload via Xcode
```

### Android Deployment

#### Prerequisites

- Google Play Developer account ($25 one-time)
- Expo account

#### 1. Configure App

Update `app.json`:

```json
{
  "expo": {
    "android": {
      "package": "com.tdfrecords.app",
      "versionCode": 1
    }
  }
}
```

#### 2. Build with EAS

```bash
cd tdf-mobile

# Build for Android
eas build --platform android

# Build for internal testing (APK)
eas build --platform android --profile preview
```

#### 3. Submit to Play Store

```bash
# Submit to Play Store
eas submit --platform android --latest

# Or download AAB and upload via Play Console
```

---

## Database Setup

### Managed PostgreSQL (Recommended)

Use a managed PostgreSQL service:

#### Option 1: Koyeb PostgreSQL

1. In Koyeb dashboard, create PostgreSQL service
2. Note connection details
3. Use in backend environment variables

#### Option 2: Digital Ocean Managed Database

1. Create PostgreSQL cluster
2. Configure allowed connections
3. Use connection string in backend

#### Option 3: AWS RDS

1. Create RDS PostgreSQL instance
2. Configure security groups
3. Use endpoint in backend

### Database Migrations

Migrations run automatically on backend startup via Persistent ORM.

To run manual migrations:

```bash
cd tdf-hq

# Connect to database
psql -h <host> -U <user> -d <database>

# Run migration SQL files
\i sql/001_multi_role_migration.sql
\i sql/002_lessons_packages.sql
```

### Database Backup

```bash
# Backup
pg_dump -h <host> -U <user> -d <database> > backup.sql

# Restore
psql -h <host> -U <user> -d <database> < backup.sql
```

---

## CORS Configuration

Proper CORS configuration is critical for frontend-backend communication.

### Backend CORS Setup

Set these environment variables in backend:

```env
# Single origin
ALLOW_ORIGIN=https://your-app.pages.dev

# Multiple origins (comma-separated)
ALLOW_ORIGINS=https://your-app.pages.dev,https://your-custom-domain.com

# Allow all (development only!)
ALLOW_ALL_ORIGINS=true
```

### Verifying CORS

```bash
# Test CORS headers
curl -I -X OPTIONS https://your-api.koyeb.app/parties \
  -H "Origin: https://your-app.pages.dev" \
  -H "Access-Control-Request-Method: GET"

# Should see these headers:
# Access-Control-Allow-Origin: https://your-app.pages.dev
# Access-Control-Allow-Methods: GET, POST, PUT, DELETE
```

---

## SSL/TLS Certificates

### Cloudflare Pages & Vercel
- SSL certificates are automatically provisioned
- No configuration needed

### Self-Hosted (Let's Encrypt)

```bash
# Install Certbot
sudo apt install certbot python3-certbot-nginx

# Generate certificate
sudo certbot --nginx -d api.yourdomain.com

# Auto-renewal
sudo certbot renew --dry-run
```

---

## Monitoring & Health Checks

### Backend Health Check

The backend provides a `/health` endpoint:

```bash
curl https://your-api.koyeb.app/health
```

Response:
```json
{
  "status": "ok",
  "database": "connected",
  "timestamp": "2024-11-14T12:00:00Z"
}
```

### Setting Up Monitoring

#### Option 1: UptimeRobot

1. Go to [UptimeRobot](https://uptimerobot.com/)
2. Add monitor for backend health endpoint
3. Configure alerts

#### Option 2: Cloudflare Analytics

Cloudflare Pages includes analytics:
- Page views
- Performance metrics
- Error rates

#### Option 3: Sentry (Error Tracking)

Add Sentry to frontend:

```bash
npm install @sentry/react
```

```typescript
// src/main.tsx
import * as Sentry from "@sentry/react";

Sentry.init({
  dsn: "your-sentry-dsn",
  environment: import.meta.env.MODE,
});
```

---

## Troubleshooting Deployment

### Backend Issues

**Problem: Database connection failed**
```
Error: could not connect to server
```

**Solution:**
- Verify `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASS` are correct
- Check database firewall rules allow connections from Koyeb IPs
- Test connection with `psql` locally

**Problem: Build fails on Koyeb**
```
Error: stack build failed
```

**Solution:**
- Check `stack.yaml` and `tdf-hq.cabal` are in root directory
- Verify Dockerfile is correct
- Check build logs for specific errors

### Frontend Issues

**Problem: API calls fail (CORS)**
```
Access to fetch at 'https://api...' from origin 'https://app...' 
has been blocked by CORS policy
```

**Solution:**
- Add frontend URL to backend `ALLOW_ORIGINS`
- Restart backend after environment variable change
- Verify CORS headers with curl

**Problem: White screen after deployment**

**Solution:**
- Check browser console for errors
- Verify `VITE_API_BASE` is set correctly
- Ensure all environment variables are set in Cloudflare/Vercel
- Rebuild after changing environment variables

**Problem: 404 on refresh (SPA routing)**

**Solution:**
- Cloudflare Pages: Add `_redirects` file:
  ```
  /*    /index.html   200
  ```
- Vercel: Automatically handles SPA routing
- Nginx: Use `try_files $uri $uri/ /index.html;`

### Mobile Issues

**Problem: EAS build fails**

**Solution:**
- Ensure `app.json` is properly configured
- Verify bundle identifier is unique
- Check EAS build logs for specific errors

**Problem: App crashes on launch**

**Solution:**
- Test locally first with `expo start`
- Check for missing environment variables
- Verify API endpoint is accessible from mobile network

---

## Deployment Checklist

### Pre-Deployment

- [ ] All tests pass locally
- [ ] Environment variables documented
- [ ] Database migrations tested
- [ ] CORS configuration verified
- [ ] SSL certificates ready (if self-hosted)

### Backend Deployment

- [ ] Database created and accessible
- [ ] Environment variables set
- [ ] Build successful
- [ ] Health check responds
- [ ] API endpoints accessible

### Frontend Deployment

- [ ] Build successful locally
- [ ] Environment variables set
- [ ] CORS working
- [ ] SPA routing works
- [ ] Static assets load
- [ ] Custom domain configured (if applicable)

### Post-Deployment

- [ ] Smoke tests pass
- [ ] Monitoring configured
- [ ] Backups scheduled
- [ ] Team notified
- [ ] Documentation updated

---

## Rolling Back

### Cloudflare Pages

1. Go to **Deployments** in Pages
2. Find previous successful deployment
3. Click **Rollback to this deployment**

### Koyeb

1. Go to service **Deployments**
2. Select previous deployment
3. Click **Redeploy**

### Manual Rollback

```bash
# Backend
docker-compose down
git checkout <previous-commit>
docker-compose up -d

# Frontend
git checkout <previous-commit>
npm run build
# Re-upload to hosting
```

---

## Security Recommendations

### Production Checklist

- [ ] All secrets in environment variables (not committed)
- [ ] CORS restricted to known origins
- [ ] HTTPS enforced
- [ ] Database passwords strong and unique
- [ ] Admin endpoints protected/removed
- [ ] Rate limiting enabled
- [ ] Security headers configured
- [ ] Regular dependency updates

### Security Headers (Nginx)

```nginx
add_header X-Frame-Options "SAMEORIGIN" always;
add_header X-Content-Type-Options "nosniff" always;
add_header X-XSS-Protection "1; mode=block" always;
add_header Referrer-Policy "no-referrer-when-downgrade" always;
```

---

## Performance Optimization

### Backend

- Use connection pooling (already configured)
- Add database indexes for frequently queried fields
- Enable query caching where appropriate
- Monitor slow queries

### Frontend

- Enable gzip compression (automatic on Cloudflare/Vercel)
- Use CDN for assets (automatic)
- Lazy load routes
- Optimize images

### Database

- Regular VACUUM and ANALYZE
- Monitor query performance
- Add indexes strategically
- Consider read replicas for high traffic

---

## Support & Resources

### Documentation

- [Backend README](../tdf-hq/README.md)
- [Frontend README](../tdf-hq-ui/README.md)
- [Mobile README](../MOBILE_APP.md)

### External Resources

- [Koyeb Documentation](https://www.koyeb.com/docs)
- [Cloudflare Pages Docs](https://developers.cloudflare.com/pages/)
- [Expo EAS Docs](https://docs.expo.dev/eas/)
- [Vercel Docs](https://vercel.com/docs)

---

**Need help?** Open an issue or contact the development team.

---

## Ticketing System Deployment (Stripe Integration)

**Version:** 1.0.0
**Date:** 2026-05-24
**Status:** Production Ready ✅

The TDF ticketing system requires additional configuration for Stripe payment processing, QR code generation, and email notifications.

### Prerequisites for Ticketing

- ✅ Stripe account (test and production keys)
- ✅ Webhook endpoint with HTTPS
- ✅ SMTP configured for ticket confirmation emails
- ✅ Database migration completed

### Step 1: Database Migration

The ticketing system adds 8 new tables to the existing schema.

#### Execute Migration

```bash
# Navigate to backend directory
cd /Users/macpro/GitHub/tdf-app/tdf-hq

# CRITICAL: Backup database first!
pg_dump -h <DB_HOST> -U <DB_USER> -d <DB_NAME> > backup_pre_ticketing_$(date +%Y%m%d_%H%M%S).sql

# Execute ticketing migration
psql -h <DB_HOST> -U <DB_USER> -d <DB_NAME> -f sql/2026-05-24_ticketing_system_enhancements.sql

# Expected output:
# CREATE TABLE (8 times - promo_code, ticket_refund_request, etc.)
# CREATE INDEX (multiple times)
# ALTER TABLE (3 times - event_ticket_order, event_ticket, event_ticket_tier)
```

#### Verify Migration Success

```bash
# Connect to database
psql -h <DB_HOST> -U <DB_USER> -d <DB_NAME>

# Check new tables exist
\dt

# Should see these 8 new tables:
# - promo_code
# - promo_code_redemption
# - ticket_refund_request
# - ticket_transfer
# - event_waitlist
# - stripe_payment_intent
# - stripe_webhook_event
# - ticket_qr_code

# Check modified tables
\d event_ticket_order
\d event_ticket
\d event_ticket_tier
```

### Step 2: Stripe Configuration

#### Get Stripe API Keys

1. Log in to [Stripe Dashboard](https://dashboard.stripe.com)
2. Navigate to **Developers** → **API Keys**
3. Copy:
   - **Publishable key** (starts with `pk_test_` or `pk_live_`)
   - **Secret key** (starts with `sk_test_` or `sk_live_`)

**Important:**
- Use **test mode** keys for staging/development
- Use **live mode** keys for production
- Never commit keys to git

#### Configure Webhook

See detailed instructions in [STRIPE_WEBHOOK_SETUP.md](STRIPE_WEBHOOK_SETUP.md)

**Quick Setup:**

1. Go to **Developers** → **Webhooks** in Stripe Dashboard
2. Click **Add endpoint**
3. Configure:
   - **URL:** `https://api.tdf-app.com/social-events/stripe/webhook`
   - **Events:**
     - ✅ `payment_intent.succeeded`
     - ✅ `payment_intent.payment_failed`
     - ✅ `charge.refunded`
4. Copy the **Signing secret** (starts with `whsec_`)

### Step 3: Backend Environment Variables

Add these to your backend `.env` file:

```env
# Stripe Configuration (REQUIRED for ticketing)
STRIPE_SECRET_KEY=sk_live_your_actual_secret_key
STRIPE_PUBLISHABLE_KEY=pk_live_your_actual_publishable_key
STRIPE_WEBHOOK_SECRET=whsec_your_webhook_signing_secret

# For staging/development, use test keys:
# STRIPE_SECRET_KEY=sk_test_your_test_secret_key
# STRIPE_PUBLISHABLE_KEY=pk_test_your_test_publishable_key
```

**Koyeb/Fly.io Deployment:**

```bash
# Koyeb - Add as environment variables in service settings
# Fly.io - Set as secrets
flyctl secrets set STRIPE_SECRET_KEY=sk_live_...
flyctl secrets set STRIPE_PUBLISHABLE_KEY=pk_live_...
flyctl secrets set STRIPE_WEBHOOK_SECRET=whsec_...
```

### Step 4: Frontend Environment Variables

Add to `tdf-hq-ui/.env` or hosting provider settings:

```env
# Stripe Configuration (Frontend)
VITE_STRIPE_PUBLISHABLE_KEY=pk_live_your_actual_publishable_key

# For staging/development:
# VITE_STRIPE_PUBLISHABLE_KEY=pk_test_your_test_publishable_key
```

**Cloudflare Pages:**
1. Go to **Settings** → **Environment variables**
2. Add `VITE_STRIPE_PUBLISHABLE_KEY`
3. **Important:** Redeploy after adding variables

**Vercel:**
1. Go to **Settings** → **Environment Variables**
2. Add `VITE_STRIPE_PUBLISHABLE_KEY`
3. Trigger new deployment

### Step 5: Deploy Backend with Ticketing

```bash
cd tdf-hq

# Rebuild with new ticketing handlers
stack clean
stack build

# Run tests (optional)
stack test

# Deploy to your hosting provider (see main deployment sections above)
```

**Verify Backend:**

```bash
# Test health endpoint
curl https://your-api.com/health

# Test Stripe payment intent creation (requires auth token)
curl -X POST https://your-api.com/social-events/stripe/create-payment-intent \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_AUTH_TOKEN" \
  -d '{
    "tppEventId": "test-event-id",
    "tppTierId": "test-tier-id",
    "tppQuantity": 1,
    "tppBuyerEmail": "test@example.com",
    "tppBuyerName": "Test User"
  }'

# Should return: {"spiClientSecret":"pi_xxx_secret_yyy","spiOrderId":"order-id"}
```

### Step 6: Test Stripe Webhook

#### Local Testing with Stripe CLI

```bash
# Install Stripe CLI
brew install stripe/stripe-cli/stripe

# Login
stripe login

# Forward webhooks to local backend
stripe listen --forward-to http://localhost:8080/social-events/stripe/webhook

# In another terminal, trigger test events
stripe trigger payment_intent.succeeded
stripe trigger payment_intent.payment_failed
stripe trigger charge.refunded
```

#### Production Testing

```bash
# In Stripe Dashboard → Developers → Webhooks
# Click your webhook endpoint
# Click "Send test webhook"
# Select "payment_intent.succeeded"
# Click "Send test webhook"

# Check backend logs for:
# "Received Stripe webhook: payment_intent.succeeded"
# "Processing payment intent: pi_xxx"
# "Generated 1 tickets for order: order-id"
```

### Step 7: End-to-End Purchase Test

Complete purchase flow test:

1. **Create Test Event:**
   ```bash
   # Create event with tickets via API or UI
   ```

2. **Purchase Ticket:**
   - Go to event page in UI
   - Click "Buy Tickets"
   - Enter buyer details
   - Use test card: `4242 4242 4242 4242`
   - Expiry: Any future date (e.g., 12/34)
   - CVC: Any 3 digits (e.g., 123)
   - Submit payment

3. **Verify Webhook Processing:**
   ```bash
   # Check Stripe Dashboard → Developers → Webhooks → Logs
   # Should see HTTP 200 response
   ```

4. **Verify Ticket Generated:**
   ```sql
   -- Connect to database
   SELECT * FROM event_ticket WHERE created_at > NOW() - INTERVAL '5 minutes';

   -- Verify QR code generated
   SELECT * FROM ticket_qr_code WHERE created_at > NOW() - INTERVAL '5 minutes';

   -- Check webhook processed
   SELECT * FROM stripe_webhook_event ORDER BY created_at DESC LIMIT 5;
   ```

5. **Verify Email Sent:**
   - Check SMTP logs
   - Verify confirmation email received with ticket details

### Step 8: Test Refund Flow

```bash
# 1. Request refund (via API or UI)
curl -X POST https://your-api.com/social-events/events/{eventId}/ticket-orders/{orderId}/refund \
  -H "Authorization: Bearer YOUR_AUTH_TOKEN" \
  -d '{"reason": "Customer request"}'

# 2. Admin approves refund
curl -X POST https://your-api.com/social-events/events/{eventId}/refunds/{refundId}/approve \
  -H "Authorization: Bearer ADMIN_AUTH_TOKEN"

# 3. Verify refund in Stripe Dashboard
# 4. Verify refund confirmation email sent
```

### Step 9: Test Ticket Transfer

```bash
# 1. Initiate transfer
curl -X POST https://your-api.com/social-events/events/{eventId}/tickets/{ticketId}/transfer \
  -H "Authorization: Bearer YOUR_AUTH_TOKEN" \
  -d '{
    "ttcToEmail": "recipient@example.com",
    "ttcToName": "Recipient Name"
  }'

# 2. Verify transfer email sent to recipient
# 3. Recipient accepts transfer via link in email
# 4. Verify ticket holder updated in database
```

### Step 10: Mobile App QR Code Testing

```bash
# 1. Open ticket in mobile app
# 2. Verify QR code displays correctly
# 3. Scan QR code with check-in scanner
# 4. Verify ticket checked in successfully
# 5. Attempt duplicate scan - should be rejected
```

### Ticketing Deployment Checklist

#### Pre-Deployment

- [ ] Database backup created
- [ ] Stripe test keys working in staging
- [ ] Webhook endpoint accessible via HTTPS
- [ ] SMTP credentials configured
- [ ] Migration tested in staging environment

#### Backend Deployment

- [ ] Migration executed successfully
- [ ] 8 new tables created (verify with `\dt`)
- [ ] Environment variables set (STRIPE_SECRET_KEY, etc.)
- [ ] Backend rebuilt and deployed
- [ ] Health check responds
- [ ] Stripe payment intent creation works

#### Stripe Configuration

- [ ] Webhook endpoint created in Stripe Dashboard
- [ ] Events selected (payment_intent.succeeded, payment_failed, charge.refunded)
- [ ] Webhook signing secret copied to backend .env
- [ ] Test webhook sent successfully (HTTP 200)
- [ ] Production keys configured (pk_live_, sk_live_)

#### Frontend Deployment

- [ ] VITE_STRIPE_PUBLISHABLE_KEY set
- [ ] Frontend rebuilt with Stripe Elements
- [ ] Stripe checkout form loads
- [ ] Promo code validation works
- [ ] Payment submission successful

#### Mobile Deployment

- [ ] QR code library installed (react-native-qrcode-svg)
- [ ] Ticket display screen shows QR codes
- [ ] QR scanner screen functional
- [ ] Camera permissions configured

#### Post-Deployment Testing

- [ ] Complete end-to-end purchase flow
- [ ] Ticket generated with QR code
- [ ] Confirmation email received
- [ ] Promo code applied correctly
- [ ] Refund request/approval works
- [ ] Ticket transfer works
- [ ] QR code check-in works
- [ ] Duplicate scan prevention works
- [ ] Waitlist join/notify works

### Monitoring Ticketing System

#### Daily Checks

```bash
# Check webhook success rate
# Stripe Dashboard → Developers → Webhooks → Your Endpoint → Logs
# Should be >99% successful (HTTP 200)

# Check payment success rate
# Stripe Dashboard → Payments
# Monitor failed payments

# Check database health
SELECT COUNT(*) FROM stripe_webhook_event WHERE created_at > NOW() - INTERVAL '24 hours';
SELECT COUNT(*) FROM event_ticket WHERE created_at > NOW() - INTERVAL '24 hours';
```

#### Weekly Checks

```sql
-- Check for stuck orders (payment pending > 1 hour)
SELECT * FROM event_ticket_order
WHERE status = 'payment_pending'
  AND created_at < NOW() - INTERVAL '1 hour';

-- Check for failed webhooks
SELECT * FROM stripe_webhook_event
WHERE processed = false
  AND created_at < NOW() - INTERVAL '10 minutes';

-- Monitor refund requests
SELECT COUNT(*) FROM ticket_refund_request
WHERE status = 'pending';
```

### Troubleshooting Ticketing Issues

#### Issue: Webhook Signature Verification Fails

**Symptoms:**
- Stripe webhooks return 401 Unauthorized
- Backend logs show "Invalid webhook signature"

**Solution:**
1. Verify `STRIPE_WEBHOOK_SECRET` matches Stripe Dashboard
2. Check for trailing whitespace in `.env` file
3. Restart backend after updating `.env`
4. Verify webhook URL is correct (no typos)

#### Issue: Tickets Not Generated After Payment

**Symptoms:**
- Payment succeeds in Stripe
- No tickets in database
- No confirmation email sent

**Solution:**
1. Check Stripe Dashboard → Webhooks → Logs for failures
2. Verify webhook endpoint returns HTTP 200
3. Check backend logs for webhook processing errors
4. Manually replay webhook from Stripe Dashboard

#### Issue: QR Codes Not Scanning

**Symptoms:**
- QR code displays but scanner rejects it
- "Invalid QR code" error

**Solution:**
1. Verify QR data format: `ticketId|eventId|email|timestamp|HMAC`
2. Check HMAC secret configuration
3. Regenerate QR code
4. Verify scanner app has camera permissions

#### Issue: Refunds Not Processing

**Symptoms:**
- Refund approved but not processed in Stripe
- Refund status stuck at "approved"

**Solution:**
1. Check Stripe API logs for refund creation errors
2. Verify STRIPE_SECRET_KEY has refund permissions
3. Check payment_intent_id is correct
4. Manually process refund in Stripe Dashboard

### Rollback Ticketing System

If you need to roll back the ticketing system:

```bash
# 1. Database rollback
psql -h <DB_HOST> -U <DB_USER> -d <DB_NAME> < backup_pre_ticketing_YYYYMMDD_HHMMSS.sql

# 2. Backend rollback
# Deploy previous version without ticketing code

# 3. Frontend rollback
# Deploy previous version without Stripe Elements

# 4. Remove Stripe webhook
# Stripe Dashboard → Developers → Webhooks → Delete endpoint
```

### Ticketing Security Checklist

- [ ] Stripe live keys configured (not test keys)
- [ ] Webhook signature verification enabled
- [ ] QR codes use HMAC signatures
- [ ] Capacity management prevents overselling
- [ ] Database uses row-level locking
- [ ] Sensitive data not logged
- [ ] CORS restricted to known origins
- [ ] Rate limiting on checkout endpoints

### Support Resources

- [Ticketing System Summary](TICKETING_SYSTEM_SUMMARY.md) - Complete feature overview
- [Stripe Webhook Setup](STRIPE_WEBHOOK_SETUP.md) - Detailed webhook configuration
- [Implementation Status](TICKETING_IMPLEMENTATION_STATUS.md) - Development timeline
- [Stripe Documentation](https://stripe.com/docs) - Official Stripe docs
- [Stripe Support](https://support.stripe.com/) - Stripe help center

---

**Ticketing System Deployment Completed:** 2026-05-24
**Version:** 1.0.0
**Next Review:** 2026-06-24
