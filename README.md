# TDF Records Management Platform

Comprehensive business management system for TDF Records, featuring CRM, scheduling, lesson packages, invoicing, inventory tracking, and trial lesson management.

> **📚 New to the project?** Check out [DOCUMENTATION.md](DOCUMENTATION.md) for a complete guide to all available documentation.

## 🏗️ Architecture

This is a monorepo containing three main applications:

### Backend - `tdf-hq/`
**Tech Stack:** Haskell + Servant + PostgreSQL + Persistent  
**Purpose:** REST API server with JWT authentication, OpenAPI specs, and PDF generation

- CRM & party management with role-based access control
- Resource scheduling (studios, rehearsal rooms, classrooms)
- Package catalog & purchase tracking
- Invoicing & payment processing
- Inventory & equipment management
- Trial lesson workflows
- Pipeline/Kanban management for services

[→ Backend Documentation](tdf-hq/README.md)

### Web UI - `tdf-hq-ui/`
**Tech Stack:** React + Vite + MUI + React Query + TypeScript  
**Purpose:** Admin/management web interface

- Party (customer) management with search & filters
- FullCalendar-based booking interface
- Kanban pipelines for mixing/mastering workflows
- Package & invoice management
- Dark/light theme toggle with persistence
- Type-safe API client generated from OpenAPI specs
- Public Fan Hub (`/fans`) where fans can create accounts, follow artists, and jump directly to their Spotify/YouTube presences

[→ Web UI Documentation](tdf-hq-ui/README.md)

### Mobile App - `tdf-mobile/`
**Tech Stack:** Expo + React Native + React Query + TypeScript  
**Purpose:** Mobile interface for students and staff

- Student lesson scheduling
- Package balance tracking
- Calendar integration
- **Offline support**: Schedule viewing, package balances, and booking mutations work offline with automatic sync

**Note:** This is a Git submodule. Run `git submodule update --init --recursive` after cloning.

[→ Mobile Documentation](MOBILE_APP.md)

## 🚀 Quick Start

### Prerequisites
- **Backend:** Stack (Haskell), PostgreSQL 16
- **Frontend/Mobile:** Node.js 20.19.4+ (LTS), npm 10+
- **Optional:** Docker + Docker Compose

### Development Setup

```bash
# 1. Clone the repository
git clone <repository-url>
cd tdf-app

# 2. Install frontend dependencies
npm install

# 3. Backend setup
cd tdf-hq
cp config/default.env .env
# Edit .env with your database credentials
stack setup
stack build
stack run

# Google Sign-In (optional)
# Set GOOGLE_CLIENT_ID in tdf-hq/.env and VITE_GOOGLE_CLIENT_ID in tdf-hq-ui/.env
# Use a Web OAuth client from Google Cloud; both values should match

# 4. Web UI setup (new terminal, from repo root)
cd tdf-hq-ui
cp .env.example .env
# Set VITE_API_BASE=http://localhost:8080
npm run dev

# 5. Mobile setup (new terminal, from repo root)
cd tdf-mobile
# Set EXPO_PUBLIC_API_BASE=http://localhost:8080
# Set EXPO_PUBLIC_UPLOAD_URL=http://localhost:8080/drive/upload (for inventory photos)
npm run start

# Optional: Radio streaming stack
# See streaming/README.md and set RADIO_INGEST_BASE / RADIO_PUBLIC_BASE / RADIO_WHIP_BASE in backend env.
```

### Inventory photos (Google Drive proxy)
- Backend exposes `POST /drive/upload` (uses Google Drive token). Configure in `tdf-hq/.env`:
  ```
  DRIVE_ACCESS_TOKEN=<Google access token with drive.file scope>
  DRIVE_UPLOAD_FOLDER_ID=<optional target folder>
  ```
- Mobile needs:
  ```
  EXPO_PUBLIC_UPLOAD_URL=http://<api-host>:8080/drive/upload
  EXPO_PUBLIC_API_TOKEN="Bearer <your-api-token>"
  ```
  Inventory will upload camera/galería photos via that endpoint and store the returned URL on assets.

### Google Drive uploads (web UI)
- Needed for the Drive picker flow in the admin UI (uploads via OAuth PKCE).
- Create an OAuth 2.0 Client ID of type **Web application** in Google Cloud Console. Add `http://localhost:5173/oauth/google-drive/callback` for local dev and your deployed domain (e.g. `https://tdf-app.pages.dev/oauth/google-drive/callback`) as authorized redirect URIs.
- Backend exchanges OAuth codes, so set `GOOGLE_CLIENT_SECRET` (or `DRIVE_CLIENT_SECRET`) alongside `GOOGLE_CLIENT_ID`.
- Set in `tdf-hq-ui/.env`:
  ```
  VITE_GOOGLE_DRIVE_CLIENT_ID=<the client ID you created>
  VITE_GOOGLE_DRIVE_REDIRECT_URI=<one of the authorized redirect URIs above>
  VITE_GOOGLE_DRIVE_FOLDER_ID=<optional default Drive folder ID for uploads>
  ```
- The redirect URI must match exactly one of the authorized URIs on the OAuth client.

### Using Docker Compose

```bash
cd tdf-hq
make up      # Start PostgreSQL + API
make seed    # Seed initial data
make logs    # View logs
```

## 📦 Submodules & Backups

- `tdf-mobile/` is tracked as a Git submodule (Expo app). After cloning, run `git submodule update --init --recursive` (or clone with `--recursive`) so `tdf-mobile` pulls the correct commit.
- Local UI snapshots live under `tdf-hq-ui.backup.*` and are ignored by Git. They are useful for experimentation but should never be committed or referenced by CI.
- Any time the root repo is moved to a new machine or CI provider, repeat the submodule init step; otherwise builds that traverse the tree (Cloudflare/Vercel) will fail looking for `tdf-mobile`.

## 📋 Project Structure

```
tdf-app/
├── tdf-hq/              # Haskell backend
│   ├── app/             # Entry point
│   ├── src/TDF/         # Core library
│   ├── docs/            # API documentation & OpenAPI specs
│   ├── scripts/         # Dev utilities, migrations
│   └── sql/             # Raw SQL migrations
├── tdf-hq-ui/           # React web application
│   ├── src/
│   │   ├── api/         # Generated API clients
│   │   ├── features/    # Feature modules
│   │   ├── components/  # Shared components
│   │   └── pages/       # Route pages
│   └── public/
├── tdf-mobile/          # Expo mobile app
│   ├── app/             # Expo Router pages
│   ├── src/
│   │   ├── api/         # Generated API clients
│   │   └── components/  # Shared components
│   └── openapi/         # OpenAPI spec files
├── docs/                # Project-wide documentation
│   └── legacy/          # Archived docs
├── patches/             # Git patches (archived)
├── archives/            # Historical files & backups
└── scripts/             # Shared utility scripts
```

## 🔧 Common Tasks

### Generate API Clients

```bash
# Web UI (from root)
npm run generate:api:ui

# Mobile (from root)
npm run generate:api:mobile

# Or individually
cd tdf-hq-ui && npm run generate:api
cd tdf-mobile && npm run generate:api
```

### Run Tests

```bash
# Web UI
npm run test:ui

# Backend (when implemented)
cd tdf-hq && stack test
```

### Quality Gate (lint + typecheck + backend build/tests)

```bash
npm run quality
```

> Runs ESLint + TypeScript checks for UI/mobile, builds the Haskell executable (`stack build tdf-hq:exe:tdf-hq-exe`), and then runs `stack test` so regressions across TypeScript + Haskell are caught locally before pushing. Configure Stack/DB access first if you run it on a fresh machine.

### Continuous Improvement Loop

The repo now includes a guarded automation harness for the workflow:

1. discover an improvement idea
2. implement it through a configured worker command
3. run a static UI audit
4. fix UI findings
5. run formal verification on the loop state machine
6. fix formal findings
7. commit and push
8. poll GitHub checks and optionally repair failures
9. repeat

Useful commands:

```bash
npm run loop:idea
npm run audit:ui:static
npm run verify:auto-loop
npm run loop:improve -- --config scripts/continuous-improvement-loop.example.json
npm run loop:start
npm run loop:status
npm run loop:tail
npm run loop:stop
npm run loop:install-launchd
```

Forever-runner contract:
- `npm run loop:start` launches a dedicated supervisor (`scripts/continuous-improvement-loop-supervisor.sh`), not just one backgrounded Node process.
- The supervisor enforces a single-instance lock, restarts the loop if the child exits, watches the heartbeat emitted by `scripts/continuous-improvement-loop.mjs`, and restarts stale children after the configured timeout.
- Operator-visible state lives in `tmp/continuous-improvement-loop/status.json` with PID, child PID, phase, heartbeat, restart counters, and last-exit details.
- Logs stay in `tmp/continuous-improvement-loop.log` and remain tail-able with `npm run loop:tail`.
- Stop the runner cleanly with `npm run loop:stop`; a stop file is used so the supervisor exits intentionally instead of immediately respawning.
- `scripts/install-continuous-improvement-loop-launchd.sh` installs a macOS LaunchAgent with `RunAtLoad` + `KeepAlive`, which is the durable contract for always-on execution across crashes and reboots.

Notes:
- When the forever runner encounters a dirty worktree, it now checkpoints that work onto a pushed `continuous-improvement-loop/dirty/<base-branch>/...` branch, returns the loop branch to clean state, and only blocks if that checkpoint recovery fails.
- `loop:improve` is agent-agnostic: wire `implementationCommand`, `uiFixCommand`, `formalFixCommand`, and `ciRepairCommand` to your preferred coding worker.
- `commitMessageTemplate` defaults to `{commit_message}` and can use `{commit_type}`, `{commit_summary}`, `{primary_path}`, and `{files_changed}` if you want a custom format.
- GitHub polling uses `gh api`, so run `gh auth status` first and ensure the repo remote points at GitHub.
- Tuning knobs for the forever runner: `CONTINUOUS_LOOP_HEARTBEAT_TIMEOUT_SECONDS`, `CONTINUOUS_LOOP_RESTART_DELAY_SECONDS`, `CONTINUOUS_LOOP_SUPERVISOR_POLL_SECONDS`, `CONTINUOUS_LOOP_LOG_FILE`, and `CONTINUOUS_LOOP_STATE_DIR`.

### Build for Production

```bash
# Web UI
npm run build:ui

# Backend
cd tdf-hq && stack build --copy-bins
```

## ☁️ Deployments

| Target | Root Directory | Install Command | Build Command | Output | Notes |
| --- | --- | --- | --- | --- | --- |
| **Cloudflare Pages** (`tdf-app.pages.dev`) | `.` | `npm install` | `npm run build:ui` | `tdf-hq-ui/dist` | Add env vars `NODE_VERSION=20.19.4`, `VITE_API_BASE=https://the-dream-factory.koyeb.app`, `VITE_TZ=America/Guayaquil` (optional `VITE_API_DEMO_TOKEN`). |
| **Vercel** | `tdf-hq-ui` | `npm install` | `npm run build` | `dist` | Framework preset: Vite. Same env vars as above. |
| **Koyeb (API)** | `tdf-hq` Docker | `stack build` via Dockerfile | – | – | Configure `DB_*`, `SMTP_*`, `HQ_APP_URL`, and CORS vars (`ALLOW_ORIGINS`, `ALLOW_ALL_ORIGINS`) in the service settings. |

> Tip: when deploying the UI, match the backend URL (`VITE_API_BASE`) with the Koyeb app URL so CORS succeeds. For Cloudflare, the repo root stays `.` and the build script (`npm run build:ui`) emits the UI in `tdf-hq-ui/dist`.

## 🔐 Environment Variables

### Backend (`tdf-hq/.env`)
```env
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=your_password
DB_NAME=tdf_hq
APP_PORT=8080
RESET_DB=false
SEED_DB=true
HQ_APP_URL=http://localhost:5173
SMTP_HOST=smtp.gmail.com
SMTP_PORT=587
SMTP_USERNAME=apikey
SMTP_PASSWORD=secret
SMTP_FROM=ops@tdfrecords.com
SMTP_FROM_NAME=TDF Records
SMTP_TLS=true
# Optional CORS overrides (comma-separated lists accepted)
ALLOW_ORIGINS=https://tdfui.pages.dev,https://your-admin.app
ALLOW_ORIGIN=
ALLOW_ALL_ORIGINS=false
```

### Web UI (`tdf-hq-ui/.env`)
```env
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

### Mobile (`tdf-mobile/.env`)
```env
EXPO_PUBLIC_API_BASE=http://localhost:8080
```

## 📚 Business Features

### CRM & Parties
- Unified party model (customers, teachers, staff)
- Role-based access (Admin, Manager, Teacher, Reception, etc.)
- Instagram/WhatsApp integration
- Emergency contacts & tax ID tracking

### Scheduling
- Multi-resource booking (studios, rehearsal rooms, classrooms)
- 60-minute minimum bookings with 15-minute buffers
- Cancellation policies (24-hour free cancellation)
- Attendance tracking
- Google Calendar two-way sync

### Packages & Billing
- Hour-based packages (e.g., "Guitar 24h" - 24 hours for $500)
- Expiration tracking (120 days)
- Non-transferable, credit-only refunds
- Split payments & installments
- Multi-method payments (cash, bank transfer, POS)

### Inventory
- Serial number tracking
- Location management
- Check-in/check-out system
- QR/barcode support
- Maintenance scheduling (quarterly, annual)
- Consumables & reorder points

### Services & Pipelines
- **Recording:** Inquiry → Quoted → Scheduled → In Session → Editing → Approved → Delivered
- **Mixing:** Brief → Prep → v1 Sent → Revisions → Approved → Delivered
- **Classes:** Enrolled → Scheduled → Attended → Make-up Needed → Completed
- **Event Production:** Lead → Proposal → Confirmed → Pre-Prod → Onsite → Settled

## 🔒 Security Notes

⚠️ **Important:** Never commit sensitive files:
- `.env` files
- API keys or OAuth secrets
- Database credentials
- Private keys

All sensitive files are now in `.gitignore`. Review `archives/` directory for any accidentally committed secrets.

## 📖 Documentation

### 📚 Documentation Hub
**[→ Complete Documentation Index](DOCUMENTATION.md)** - Your starting point for all documentation

### Essential Guides
- **[QUICKSTART.md](QUICKSTART.md)** - Get up and running in minutes
- **[QUICK_START_REFERENCE.md](QUICK_START_REFERENCE.md)** - Command cheat sheet
- **[DEVELOPMENT.md](DEVELOPMENT.md)** - Detailed development workflows
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines
- **[BEST_PRACTICES.md](BEST_PRACTICES.md)** - Code quality and standards
- **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)** - Complete deployment procedures
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Common issues and solutions

### Technical Documentation
- **[Backend (Haskell)](tdf-hq/README.md)** - API architecture and patterns
- **[Frontend (React)](tdf-hq-ui/README.md)** - Web UI structure and components
- **[Mobile App (React Native)](MOBILE_APP.md)** - Mobile development guide
- **[User Roles API Reference](QUICK_REFERENCE.md)** - Multi-role user management endpoints and examples

### Specialized Documentation
- **[UI_VISUAL_GUIDE.md](UI_VISUAL_GUIDE.md)** - UI components and design specs
- **[TESTING_GUIDE.md](TESTING_GUIDE.md)** - Comprehensive testing procedures
- **[ARCHITECTURE_DIAGRAM.md](ARCHITECTURE_DIAGRAM.md)** - System architecture and data flows
- **[FEATURES.md](FEATURES.md)** - Complete feature catalog
- **[SECURITY_NOTICE.md](SECURITY_NOTICE.md)** - Security guidelines

### OpenAPI & Contracts
- **[OpenAPI Specs](tdf-hq/docs/openapi/)** - API specifications
- **[Business Requirements](specs.yaml)** - Business logic and rules

## 🤝 Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

**Quick checklist:**
1. Create feature branches from `main`
2. Follow existing code style and conventions
3. Update tests and documentation
4. Generate API clients after OpenAPI changes (`npm run generate:api:ui`, `npm run generate:api:mobile`)
5. Run quality checks (`npm run quality`)
6. Test locally before pushing

## 📄 License

MIT License - See individual project READMEs for details

---

**TDF Records** - Empowering creativity through technology 🎵
