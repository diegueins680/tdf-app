# TDF HQ

A Haskell (Servant + Persistent) backend that powers TDF's internal HQ app for CRM, scheduling, lesson packages, invoicing, inventory, and trial management. The service exposes a JSON API secured with bearer tokens, generates PDFs for operational checklists, and boots with opinionated seeds so the UI can be exercised immediately.

## Architecture at a Glance

- **Entry point**: [`app/Main.hs`](app/Main.hs) wires configuration, database pooling, migrations, seeding, and the Servant application with CORS middleware.
- **Environment**: [`TDF.Config`](src/TDF/Config.hs) loads `APP_PORT`, database credentials, and flags for resetting/seeding the database, plus the optional seed trigger token.
- **Database**: [`TDF.DB`](src/TDF/DB.hs) creates the PostgreSQL pool and exposes the runtime `Env` consumed across handlers. Persistent models live in [`TDF.Models`](src/TDF/Models.hs), [`TDF.ModelsExtra`](src/TDF/ModelsExtra.hs), and [`TDF.Trials.Models`](src/TDF/Trials/Models.hs) with migrations executed on startup.
- **API surface**: [`TDF.API`](src/TDF/API.hs) defines the main Servant routes. Individual feature areas (CRM parties, bookings, packages, invoices, receipts, bands, inventory, rooms, pipelines, trial lessons, etc.) live in `TDF.API.*` and `TDF.Server*` modules.
- **Server implementation**: [`TDF.Server`](src/TDF/Server.hs) composes all handlers, enforces seed tokens, renders PDF input lists, and hoists the authenticated sub-API. Trials endpoints (`/trials/*`) are mounted alongside the primary API.
- **Authentication**: [`TDF.Auth`](src/TDF/Auth.hs) resolves bearer tokens to roles and module permissions, backing Servant's `AuthProtect` machinery. Admin utilities in [`TDF.ServerAdmin`](src/TDF/ServerAdmin.hs) provide seeded dropdowns and user management.
- **DTOs & contracts**: [`TDF.DTO`](src/TDF/DTO.hs), `TDF.Contracts.*`, and the OpenAPI documents in [`docs/`](docs) define payload shapes shared with the frontend.

### Repository layout

| Path | Purpose |
| --- | --- |
| `app/` | Executable entrypoint and CORS setup. |
| `src/TDF/` | Core library modules (API types, server logic, models, auth, DTOs, seeds, feature servers). |
| `src/TDF/Trials/` | Trial lesson-specific API, models, seeds, and server composition. |
| `scripts/` | Helper scripts for dev (`dev_run.sh`), smoke checks, LaTeX packaging, and DB migrations. |
| `sql/` | Raw SQL migrations for lessons, packages, payments, and receipts. |
| `docs/` | API references, product notes, and OpenAPI specs. |
| `templates/` | LaTeX templates used for generated PDFs (e.g., input list sessions). |

## Getting Started

### Prerequisites

- GHC toolchain via [Stack](https://docs.haskellstack.org/).
- PostgreSQL 16 accessible at the host/port configured in `config/default.env` (or run via Docker Compose).
- `make`, `curl`, and `jq` for convenience targets.
- (Optional) A LaTeX toolchain if you plan to regenerate the PDF template assets locally (`scripts/latex`).

### Configure environment

Copy `config/default.env` to `.env` (or export the variables in your shell):

```env
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=postgres
DB_NAME=tdf_hq
APP_PORT=8080
RESET_DB=false
SEED_DB=true
SEED_TRIGGER_TOKEN=tdf-bootstrap-seed
```

`RESET_DB=true` will drop and recreate the `public` schema on boot; `SEED_DB=false` skips automatic seed data. Setting `SEED_TRIGGER_TOKEN` to a non-empty value enables the unauthenticated `/seed` endpoint; leaving it blank disables it entirely.

## Running the application

### Stack workflow

```bash
# from project root
set -a; source config/default.env; set +a
stack setup         # first run only
stack build
stack run           # runs migrations and optional seeds
```

The API will listen on `http://localhost:8080`. Migrations include the base schema, extra entities, and the trial lesson tables.

### Docker Compose

A `docker-compose.yml` is provided to run PostgreSQL and the app together:

```bash
make up       # build images and start db + app
make logs     # follow combined logs
make health   # hit /health for a quick status
make seed     # POST /admin/seed with the admin token
make down     # stop services (preserve volumes)
make clean    # stop services and remove volumes
```

Override `APP_BASE_URL` when using the `version` Make target, or export environment overrides before `make up` to reconfigure the containerised app.

## Database, migrations, and seeds

- Startup runs `resetSchema` when `RESET_DB=true`, executes migrations from `TDF.Models`, `TDF.ModelsExtra`, and `TDF.Trials.Models`, then optionally `seedAll` for fixtures used by the UI.
- Trial availability helpers live in `scripts/migrate_trial_availability.sh`; lesson/package/receipt migrations live in `scripts/migrate_lessons.sh` and corresponding SQL files.
- Admin-only seed endpoints are exposed under `/admin/seed` and respect the bearer token auth plus `ModuleAdmin` gate.
- The unauthenticated `/seed` endpoint is protected by `X-Seed-Token` and can be disabled via config.

## Feature overview

- **CRM & parties**: Manage parties, roles, and tokens with module-based access control.
- **Scheduling**: Bookings, sessions, trial lesson flows, and PDF input lists (`/input-list/sessions` + PDF rendering).
- **Packages & invoicing**: Package catalog, purchases, invoices, and receipts tied to payments (see `docs/openapi/lessons-and-receipts.yaml`).
- **Inventory & rooms**: Track assets, room setups, and inventory seeding utilities.
- **Pipelines & bands**: Sales/production pipelines and performance band management.
- **Admin tooling**: Dropdown management, user provisioning, role detail enumeration, and global seeding.
- **Metadata endpoints**: `/version`, `/meta/*`, and `/health` for operational visibility.

## Authentication & authorization

- Endpoints under `AuthProtect "bearer-token"` require an `Authorization: Bearer <token>` header.
- Tokens resolve to parties and active roles; modules are derived from roles via `modulesForRoles`.
- Handlers enforce module gates using helpers like `hasModuleAccess` and `ensureModule` (see `TDF.ServerAdmin`).

## Documentation & contracts

- `docs/api.md`, `docs/CalendarAPI.md`, and `docs/CONTRACTS_API.md` describe key integration points.
- REST payloads are defined in `src/TDF/DTO.hs` and `src/TDF/API/Types.hs`, mirroring database entities in `TDF.Models*`.
- The lessons & receipts OpenAPI spec (`docs/openapi/lessons-and-receipts.yaml`) drives the UI integration.
- PDF output uses `templates/invoice.dark.tex`, rendered through helpers in `TDF.Handlers.InputList`.

## Development utilities

- `scripts/dev_run.sh`: export environment variables, build, and run the server in one step.
- `scripts/smoke.sh`: lightweight curl-based smoke tests against local or remote deployments.
- `scripts/latex/*`: build artifacts required for LaTeX/PDF generation if the template changes.

## Testing

A formal test suite is not yet wired up. Add Hspec specs under `test/`, update `tdf-hq.cabal` with a `test-suite`, and run via `stack test` when you start adding automated coverage.

## Troubleshooting

- If you see database connection errors, verify credentials match a running PostgreSQL instance or use Docker Compose.
- To regenerate seeds from scratch, set `RESET_DB=true` and `SEED_DB=true` for a single `stack run` invocation, then revert to defaults.
- CORS origins can be extended via `ALLOW_ORIGINS` (comma-separated list) or `ALLOW_ORIGIN` environment variables without code changes.

---

Happy hacking! If you add new modules or endpoints, remember to update `TDF.API`, wire handlers in `TDF.Server`, extend DTOs as needed, and document contract changes under `docs/` so the frontend stays in sync.
