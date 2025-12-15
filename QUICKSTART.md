# Quick Start - TDF Records Platform

Spin up the backend API + web UI for local development.

## Prerequisites

- Node.js 20.19+ and npm 10+
- Backend (pick one):
  - Docker + Docker Compose (recommended for first run), or
  - Stack + PostgreSQL 16+

## 1) Clone with submodules

```bash
git clone --recursive <repository-url>
cd tdf-app
```

If you already cloned without submodules:

```bash
git submodule update --init --recursive
```

## 2) Install JS dependencies (UI + mobile)

```bash
npm install
```

## 3) Configure environment files

```bash
cp tdf-hq/config/default.env tdf-hq/.env
cp tdf-hq-ui/.env.example tdf-hq-ui/.env
```

## 4) Start the backend

### Option A: Docker Compose (recommended)

```bash
cd tdf-hq
make up
```

### Option B: Stack (runs backend locally)

Make sure PostgreSQL is running and matches `tdf-hq/.env` (`DB_HOST`, `DB_USER`, `DB_PASS`, `DB_NAME=tdf_hq`), then:

```bash
cd tdf-hq
stack setup
stack build
stack run
```

Backend: http://localhost:8080

## 5) Start the web UI

In a new terminal from the repo root:

```bash
npm run dev
```

Web UI: http://localhost:5173

## 6) (Optional) Start the mobile app

```bash
npm run dev:mobile
```

See `MOBILE_APP.md` for required `EXPO_PUBLIC_*` environment variables.

## Next steps

- Docs index: `DOCUMENTATION.md`
- Development workflows: `DEVELOPMENT.md`
- Deployment: `DEPLOYMENT_GUIDE.md`
