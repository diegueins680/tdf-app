# Quick Reference - TDF Records Platform

Fast reference for common commands, workflows, and configurations.

## 🚀 Quick Start (1 Minute)

```bash
# Clone with submodules
git clone --recursive https://github.com/diegueins680/tdf-app
cd tdf-app

# Install JS deps (UI + mobile)
npm install

# Backend setup (terminal 1)
cd tdf-hq
cp config/default.env .env
stack build && stack run

# Frontend setup (terminal 2, from repo root)
npm run dev

# Access
# Backend: http://localhost:8080
# Frontend: http://localhost:5173
```

## 📦 Essential Commands

### Backend (Haskell)

```bash
cd tdf-hq

# Build & Run
stack build                    # Build project
stack run                      # Run API server
stack clean                    # Clean build artifacts
stack ghci                     # Interactive REPL

# Docker
make up                        # Start PostgreSQL + API
make down                      # Stop services
make logs                      # View logs
make seed                      # Seed database
make health                    # Check health
make clean                     # Remove volumes

# Database
createdb tdf_hq                # Create database
RESET_DB=true stack run        # Reset database (DANGER!)
SEED_DB=true stack run         # Seed data
```

### Frontend (React)

```bash
cd tdf-hq-ui

# Development
npm install                    # Install dependencies
npm run dev                    # Start dev server
npm run build                  # Build for production
npm run preview               # Preview production build
npm run test                   # Run tests

# Code Quality
npm run lint                   # Run ESLint
npm run typecheck             # TypeScript check
npm run format                # Format with Prettier

# API Client
npm run generate:api          # Regenerate API client
```

### Mobile (React Native)

```bash
cd tdf-mobile

# Submodule
git submodule update --init --recursive

# Development
npm install                    # Install dependencies
npm start                      # Start Expo
npm run ios                    # Run on iOS simulator
npm run android               # Run on Android emulator
npm run web                    # Run in web browser

# API Client
npm run generate:api          # Regenerate API client

# Build
eas build --platform ios      # Build for iOS
eas build --platform android  # Build for Android
```

### Root Level (Workspace)

```bash
# Install all dependencies
npm install

# Generate API clients
npm run generate:api:ui       # Web UI
npm run generate:api:mobile   # Mobile

# Quality checks
npm run quality               # Lint + typecheck + test
npm run lint                  # Lint web UI (alias for lint:ui)
npm run lint:ui              # Lint web UI
npm run typecheck             # TypeScript check web UI (alias for typecheck:ui)
npm run typecheck:ui         # TypeScript check web UI
npm run test                  # Test web UI (alias for test:ui)
npm run test:ui              # Test web UI

# Development servers
npm run dev                  # Start web UI (alias for dev:ui)
npm run dev:ui               # Start web UI
npm run dev:mobile           # Start mobile

# Build
npm run build                # Build web UI (alias for build:ui)
npm run build:ui             # Build web UI
```

## 🔧 Configuration Files

### Backend `.env` (tdf-hq/.env)

```env
# Database
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=postgres
DB_NAME=tdf_hq

# Application
APP_PORT=8080
RESET_DB=false
SEED_DB=true

# Frontend URL (for CORS)
HQ_APP_URL=http://localhost:5173

# SMTP (optional)
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USERNAME=your-email@gmail.com
SMTP_PASSWORD=your-app-password
SMTP_FROM=ops@tdfrecords.com
SMTP_TLS=true

# CORS (comma-separated)
ALLOW_ORIGINS=http://localhost:5173
```

### Frontend `.env` (tdf-hq-ui/.env)

```env
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

### Mobile `.env` (tdf-mobile/.env)

```env
EXPO_PUBLIC_API_BASE=http://localhost:8080
EXPO_PUBLIC_TZ=America/Guayaquil
```

## 🌐 API Endpoints

### Health & Meta

```bash
GET  /health                   # Health check
GET  /version                  # Version info
GET  /meta/roles              # Available roles
GET  /meta/modules            # Available modules
```

### Parties (CRM)

```bash
GET    /parties                # List all parties
POST   /parties                # Create party
GET    /parties/:id            # Get party
PUT    /parties/:id            # Update party
DELETE /parties/:id            # Delete party
GET    /parties/:id/roles      # Get party roles
PUT    /parties/:id/roles      # Update party roles
```

### Bookings

```bash
GET    /bookings               # List bookings
POST   /bookings               # Create booking
GET    /bookings/:id           # Get booking
PUT    /bookings/:id           # Update booking
DELETE /bookings/:id           # Cancel booking
```

### Packages

```bash
GET    /packages               # List package types
POST   /packages               # Create package type
GET    /purchases              # List purchases
POST   /purchases              # Purchase package
GET    /purchases/:id          # Get purchase
```

### Invoices & Payments

```bash
GET    /invoices               # List invoices
POST   /invoices               # Create invoice
GET    /invoices/:id           # Get invoice
POST   /payments               # Record payment
GET    /receipts               # List receipts
GET    /receipts/:id           # Get receipt
```

### Admin

```bash
POST   /admin/seed             # Seed database (dev)
GET    /admin/dropdowns        # Get dropdown data
```

## 🐛 Troubleshooting

### Backend Won't Start

```bash
# Check PostgreSQL
pg_isready -h localhost -p 5432

# Check port
lsof -i :8080

# Test database connection
psql -h localhost -U postgres -d tdf_hq

# Reset database
dropdb tdf_hq && createdb tdf_hq
RESET_DB=true stack run
```

### Frontend Can't Connect

```bash
# Check backend is running
curl http://localhost:8080/health

# Check .env file
cat .env | grep VITE_API_BASE

# Restart with clean cache
rm -rf node_modules dist
npm install && npm run dev
```

### CORS Errors

```bash
# Backend: Add origin to ALLOW_ORIGINS
echo "ALLOW_ORIGINS=http://localhost:5173" >> .env

# Test CORS headers
curl -I -X OPTIONS http://localhost:8080/parties \
  -H "Origin: http://localhost:5173"
```

### Submodule Empty

```bash
# Initialize submodule
git submodule update --init --recursive

# Update to latest
git submodule update --remote
```

### Build Errors

```bash
# Backend
cd tdf-hq
stack clean && stack build

# Frontend
cd tdf-hq-ui
rm -rf node_modules package-lock.json
npm install
```

## 🔍 Diagnostic Commands

```bash
# Check all services
curl http://localhost:8080/health  # Backend
curl http://localhost:5173         # Frontend
pg_isready -h localhost -p 5432    # Database

# Check versions
node --version      # Should be 18+
npm --version       # Should be 9+
stack --version     # Haskell Stack
psql --version      # PostgreSQL

# Check ports
lsof -i :8080       # Backend
lsof -i :5432       # PostgreSQL
lsof -i :5173       # Frontend

# Check processes
ps aux | grep tdf-hq     # Backend
ps aux | grep postgres   # Database
ps aux | grep vite       # Frontend dev server
```

## 📊 Database Commands

```bash
# Connect
psql -h localhost -U postgres -d tdf_hq

# Common queries
SELECT count(*) FROM party;              # Count parties
SELECT * FROM party LIMIT 10;            # List parties
SELECT * FROM booking ORDER BY starts_at DESC LIMIT 10;  # Recent bookings

# Backup
pg_dump -h localhost -U postgres tdf_hq > backup.sql

# Restore
psql -h localhost -U postgres tdf_hq < backup.sql

# Reset (DANGER!)
dropdb tdf_hq && createdb tdf_hq
```

## 🔐 Authentication

### Getting a Token

```bash
# Login (if auth is implemented)
curl -X POST http://localhost:8080/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"admin@tdfrecords.com","password":"password"}'

# Use token
curl http://localhost:8080/parties \
  -H "Authorization: Bearer <your-token>"
```

### Testing Without Auth

```bash
# Some endpoints may work without auth in development
curl http://localhost:8080/parties
curl http://localhost:8080/packages
```

## 🚀 Deployment

### Backend (Koyeb)

```bash
# Environment variables needed:
DB_HOST=<db-host>
DB_USER=<db-user>
DB_PASS=<db-password>
DB_NAME=tdf_hq
APP_PORT=8080
HQ_APP_URL=https://your-frontend.pages.dev
ALLOW_ORIGINS=https://your-frontend.pages.dev
SMTP_HOST=smtp.gmail.com
SMTP_USERNAME=<email>
SMTP_PASSWORD=<app-password>
```

### Frontend (Cloudflare Pages)

```bash
# Build settings
Build command: npm run build:ui
Output directory: tdf-hq-ui/dist
Root directory: .

# Environment variables
NODE_VERSION=20.19.4
VITE_API_BASE=https://your-api.koyeb.app
VITE_TZ=America/Guayaquil
```

### Mobile (EAS)

```bash
# Login
eas login

# Configure
eas build:configure

# Build
eas build --platform ios
eas build --platform android

# Submit
eas submit --platform ios --latest
eas submit --platform android --latest
```

## 📚 Documentation Links

- **[DOCUMENTATION.md](DOCUMENTATION.md)** - Documentation index
- **[README.md](README.md)** - Project overview
- **[QUICKSTART.md](QUICKSTART.md)** - Quick setup guide
- **[DEVELOPMENT.md](DEVELOPMENT.md)** - Development guide
- **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)** - Deployment procedures
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Common issues
- **[MOBILE_APP.md](MOBILE_APP.md)** - Mobile development

## 🎯 Common Workflows

### Adding a New Feature

```bash
# 1. Create feature branch
git checkout -b feature/my-feature

# 2. Backend: Add API endpoint
cd tdf-hq
# Edit src/TDF/API.hs, src/TDF/Server.hs, src/TDF/Models.hs

# 3. Build and test
stack build && stack run

# 4. Update OpenAPI spec
# Edit docs/openapi/*.yaml

# 5. Regenerate frontend clients (from repo root)
cd ..  # back to repo root
npm run generate:api:ui
npm run generate:api:mobile

# 6. Frontend: Use new endpoint
cd tdf-hq-ui/src
# Create components, add API calls

# 7. Test everything
npm run quality

# 8. Commit and push
git add .
git commit -m "feat: add my feature"
git push origin feature/my-feature
```

### Fixing a Bug

```bash
# 1. Create fix branch
git checkout -b fix/bug-description

# 2. Reproduce bug
npm run dev:ui  # or stack run

# 3. Fix code
# Edit relevant files

# 4. Test fix
npm run test:ui  # or stack test

# 5. Commit and push
git commit -m "fix: resolve bug description"
git push origin fix/bug-description
```

### Updating Dependencies

```bash
# Backend
cd tdf-hq
stack update
stack build

# Frontend
cd tdf-hq-ui
npm update
npm audit fix
npm run build

# Test everything
npm run quality
```

## 💡 Tips & Tricks

### Fast Backend Reload

```bash
# Use ghcid for auto-reload during development
stack install ghcid
cd tdf-hq
ghcid --command="stack ghci" --test=":main"
```

### Frontend Hot Reload

```bash
# Vite automatically hot-reloads
# But if it doesn't, try:
npm run dev -- --force
```

### Database Quick Reset

```bash
# One-liner to reset and seed
cd tdf-hq && dropdb tdf_hq && createdb tdf_hq && RESET_DB=true SEED_DB=true stack run
```

### Check All Health

```bash
# Create a simple script
#!/bin/bash
echo "Backend:"; curl -s http://localhost:8080/health | jq
echo "Frontend:"; curl -s http://localhost:5173 > /dev/null && echo "OK" || echo "FAIL"
echo "Database:"; pg_isready -h localhost && echo "OK" || echo "FAIL"
```

### Generate API Documentation

```bash
# View OpenAPI spec in browser
cd tdf-hq/docs/openapi
python3 -m http.server 8000
# Open http://localhost:8000
```

## 🔗 Useful Links

- **GitHub Repo:** https://github.com/diegueins680/tdf-app
- **Backend API:** http://localhost:8080 (local)
- **Frontend UI:** http://localhost:5173 (local)
- **Expo DevTools:** http://localhost:19002 (when running mobile)

---

**Pro Tip:** Bookmark this page for quick reference! 🚀
