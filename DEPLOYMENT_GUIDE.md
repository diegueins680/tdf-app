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

Fly powers the production `tdf-hq` API. Deployments must inject the git SHA so `/version` returns the running commit.

The deployment flow is:

1. Build and push `diegueins680/tdf-hq:<commit-sha>` to Docker Hub with the helper script.
2. Tell Fly to deploy that prebuilt image (`fly deploy --image ...`).

Steps:

1. Install Docker (with Buildx) and authenticate to Docker Hub (`docker login`). CI does this via `DOCKERHUB_USERNAME`/`DOCKERHUB_TOKEN` secrets.
2. Install and authenticate Fly CLI (`fly auth login`).
3. Run `scripts/fly-deploy.sh [fly deploy flags]` from the repo root. The script:
   - Captures `git rev-parse HEAD`.
   - Builds and pushes `diegueins680/tdf-hq:<sha>` (override via `DOCKER_IMAGE_REPO`/`DOCKER_IMAGE_TAG`).
   - Calls `fly deploy --image ... --env SOURCE_COMMIT=... --env GIT_SHA=...`.
4. For manual Fly commands, pass the same env vars (`--env SOURCE_COMMIT=$(git rev-parse HEAD) --env GIT_SHA=$(git rev-parse HEAD)`) and reference the pushed Docker Hub image to avoid re-building inside Fly.

### Setting Secrets in Fly

Sensitive values like API keys should be set as secrets instead of environment variables in `fly.toml`:

```bash
# Set PayPal client ID
flyctl secrets set VITE_PAYPAL_CLIENT_ID=your-paypal-client-id

# Set live sessions token
flyctl secrets set VITE_LIVE_SESSIONS_PUBLIC_TOKEN=your-token

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

The service is configured to:
- Listen on `0.0.0.0:8080` (accepts connections from Fly's proxy)
- Use `internal_port = 8080` in fly.toml for service binding
- Expose ports 80 (HTTP) and 443 (HTTPS) via Fly's proxy

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
VITE_API_BASE=https://your-api.koyeb.app
VITE_TZ=America/Guayaquil
```

Optional for demo mode:
```env
VITE_API_DEMO_TOKEN=your-demo-token
```

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
VITE_API_BASE=https://your-api.koyeb.app
VITE_TZ=America/Guayaquil
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
