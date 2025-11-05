# TDF Records Management Platform

Comprehensive business management system for TDF Records, featuring CRM, scheduling, lesson packages, invoicing, inventory tracking, and trial lesson management.

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

[→ Backend Documentation](./tdf-hq/README.md)

### Web UI - `tdf-hq-ui/`
**Tech Stack:** React + Vite + MUI + React Query + TypeScript  
**Purpose:** Admin/management web interface

- Party (customer) management with search & filters
- FullCalendar-based booking interface
- Kanban pipelines for mixing/mastering workflows
- Package & invoice management
- Dark/light theme toggle with persistence
- Type-safe API client generated from OpenAPI specs

[→ Web UI Documentation](./tdf-hq-ui/README.md)

### Mobile App - `tdf-mobile/`
**Tech Stack:** Expo + React Native + React Query + TypeScript  
**Purpose:** Mobile interface for students and staff

- Student lesson scheduling
- Package balance tracking
- Calendar integration
- Offline support (planned)

[→ Mobile Documentation](./tdf-mobile/README.md)

## 🚀 Quick Start

### Prerequisites
- **Backend:** Stack (Haskell), PostgreSQL 16
- **Frontend/Mobile:** Node.js 18+, npm 9+
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

# 4. Web UI setup (new terminal)
cd tdf-hq-ui
cp .env.example .env
# Set VITE_API_BASE=http://localhost:8080
npm run dev

# 5. Mobile setup (new terminal)
cd tdf-mobile
# Set EXPO_PUBLIC_API_BASE=http://localhost:8080
npm run start
```

### Using Docker Compose

```bash
cd tdf-hq
make up      # Start PostgreSQL + API
make seed    # Seed initial data
make logs    # View logs
```

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

### Build for Production

```bash
# Web UI
npm run build:ui

# Backend
cd tdf-hq && stack build --copy-bins
```

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

- [Backend API Reference](./tdf-hq/docs/api.md)
- [OpenAPI Specs](./tdf-hq/docs/openapi/)
- [Business Requirements](./specs.yaml)
- [Legacy Documentation](./docs/legacy/)

## 🤝 Contributing

1. Create feature branches from `main`
2. Follow existing code style and conventions
3. Update tests and documentation
4. Generate API clients after OpenAPI changes
5. Test locally before pushing

## 📄 License

MIT License - See individual project READMEs for details

---

**TDF Records** - Empowering creativity through technology 🎵
