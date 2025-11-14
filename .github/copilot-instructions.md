# TDF Records Platform - AI Agent Guidelines

## Architecture Overview
This is a **monorepo** with three main applications serving TDF Records' business management platform:
- **`tdf-hq/`** - Haskell backend (Servant API + PostgreSQL + Persistent ORM)
- **`tdf-hq-ui/`** - React web UI (Vite + MUI + React Query + TypeScript)
- **`tdf-mobile/`** - Expo mobile app (React Native + TypeScript - Git submodule)

## Critical Development Workflows

### API Development Pattern
1. **Backend changes**: Update `TDF.API` types → implement handlers in `TDF.Server` → add DTOs in `TDF.DTO` → DB logic in `TDF.DB`/`TDF.Models`
2. **Generate OpenAPI**: Haskell backend auto-generates `docs/openapi/lessons-and-receipts.yaml`
3. **Generate clients**: Run `npm run generate:api:ui` and `npm run generate:api:mobile` from root to update TypeScript clients
4. **Never edit generated files**: All files in `*/api/generated/` are auto-generated from OpenAPI specs

### Build & Development Commands
```bash
# Backend (from tdf-hq/)
make up          # Docker PostgreSQL + API
make seed        # Seed development data
stack ghci       # REPL for interactive development

# Frontend (from root)
npm run dev:ui      # Start web UI dev server
npm run dev:mobile  # Start Expo dev server
npm install         # Install all workspace dependencies
```

### Docker Development Setup
- **Backend**: `tdf-hq/docker-compose.yml` provides PostgreSQL + API container setup
- **Make targets**: `make up|down|logs|restart|seed|health|clean` for container management
- **Environment**: Docker app uses `DB_HOST=db` (container name) vs local `DB_HOST=127.0.0.1`
- **Health checks**: PostgreSQL service has built-in health checks; API waits for healthy DB

### Environment Configuration
- **Backend**: Copy `tdf-hq/config/default.env` to `.env`, then `stack run`
- **Web UI**: Set `VITE_API_BASE=http://localhost:8080` and `VITE_TZ=America/Guayaquil` in `tdf-hq-ui/.env`
- **Mobile**: Set `EXPO_PUBLIC_API_BASE=http://localhost:8080` in `tdf-mobile/.env`

## Business Domain Knowledge

### Booking Rules & Constraints
- **60-minute minimum**: All bookings must be at least 1 hour duration
- **15-minute buffers**: Automatic buffers between consecutive bookings for setup/teardown
- **24-hour cancellation**: Free cancellation policy requires 24+ hours notice
- **Resource constraints**: Multi-resource booking system (studios, rehearsal rooms, classrooms) with conflict detection
- **Status workflow**: `Confirmed|Tentative|Cancelled|Completed` with attendance tracking
- **Time handling**: All times stored as ISO strings, timezone-aware using `VITE_TZ`/`EXPO_PUBLIC_TZ`

### Payment Processing Patterns
- **Multi-method support**: Cash, Bank Transfer, Card/POS with reference tracking
- **Split payments**: `PaymentSplit` model allows multiple payers per payment
- **Installments**: Invoices support partial payments with automatic balance calculation
- **Currency handling**: All amounts stored as `cents` (integer) to avoid floating-point errors
- **Payment workflow**: `pending|paid|void` status with audit trails
- **Receipt generation**: Automatic receipt creation with line items and tax calculation

### Core Entities
- **Parties**: Unified model for customers, teachers, staff with role-based access
- **Packages**: Hour-based (e.g., "Guitar 24h" = 24 hours for $500), 120-day expiration
- **Bookings**: Resource scheduling with attendance tracking and Google Calendar sync
- **Pipelines**: Business workflows (Recording, Mixing, Classes, Events) with Kanban stages

## Project-Specific Conventions

### Haskell Backend Conventions
- **Module hierarchy**: `TDF.*` mirrors directory structure (`src/TDF/Server.hs` → `TDF.Server`)
- **Naming**: Types/Constructors use `UpperCamelCase`, functions use `lowerCamelCase`
- **Database**: Uses Persistent ORM with PostgreSQL; migrations handled automatically
- **Enum patterns**: `data PaymentMethod = CashM | BankTransferM | CardPOSM...` with `derivePersistField`
- **CORS**: Permissive for dev (`app/Main.hs`), restrict `corsOrigins` for production
- **Dev seeding**: `curl -X POST http://localhost:8080/admin/seed` (development only)

### React UI Architecture
- **Feature organization**: `src/features/[domain]/` (lessons, packages, pipelines, etc.)
- **API layer**: Type-safe clients generated from OpenAPI in `src/api/generated/`
- **State management**: React Query for server state, local component state for UI
- **UI framework**: Material-UI (MUI) with theme provider in `src/theme/ColorModeProvider.tsx`
- **Routing**: React Router with pages in `src/pages/`
- **Testing**: Vitest + React Testing Library with `src/setupTests.ts`

### Mobile App Structure
- **Navigation**: Expo Router with file-based routing in `app/`
- **Co-location**: Store feature assets alongside their route modules
- **API clients**: Generated TypeScript clients in `src/api/`
- **Shared utilities**: `src/lib/` for `api`, `queryClient`, `time` utilities
- **Types**: Centralized in `src/types/` to avoid duplication

### Feature Code Patterns
- **Generated API hooks**: `useCreatePaymentMutation`, `usePaymentsQuery` from `src/api/hq/hooks.ts`
- **Form handling**: React Hook Form + Zod validation (see `BookingsPage.tsx`)
- **Currency display**: Use `formatCurrency(cents)` helper, store as integers
- **Date/time**: Luxon for parsing, `VITE_TZ` for timezone handling
- **Error boundaries**: Consistent error handling with React Query error states

## Integration Points & Dependencies

### Calendar Integration
- **FullCalendar.js**: Core calendar component with time grid view
- **Google Calendar sync**: Two-way sync via `ExternalCalendarMapping` model
- **Event format**: `{ id, title, start: ISO, end: ISO }` for calendar events
- **Timezone handling**: `VITE_TZ` environment variable controls display timezone

### Inventory & Equipment
- **Serial tracking**: QR/barcode support with `Asset` model
- **Check-in/out**: Location-based tracking with maintenance schedules
- **Stock transactions**: `StockTxnReason` enum for purchase/consumption/adjustment/return/transfer

### Bar/POS System
- **Register operations**: Cash handling with denomination counting (`USD_DENOMS`)
- **Payment methods**: `PayCash|PayCard|PayTransfer|PayComp` for different transaction types
- **Shift management**: Opening/closing procedures with over/short tracking

## Testing & Quality
- **Web UI**: Vitest + React Testing Library (`npm run test:ui`)
- **Test patterns**: See `tdf-hq-ui/src/pages/bar/__tests__/SellPage.test.tsx` for examples
- **Backend**: No test suite yet; use `stack ghci` for interactive testing
- **Mobile**: Manual testing with Expo Go; no automated suite configured
- **API testing**: Use `curl` commands documented in `tdf-hq/AGENTS.md`

## Security & Configuration
- **Never commit**: `.env` files, API keys, database credentials (see `.gitignore`)
- **Secrets**: Use environment variables; `archives/` may contain historical secrets
- **Development tokens**: Seed endpoint uses `admin-token` for dev convenience
- **Production**: Remove/guard admin endpoints and restrict CORS origins

## Common Pitfalls
- **API changes**: Always regenerate clients after OpenAPI updates
- **Module imports**: Use absolute paths and centralized type definitions
- **Database changes**: Persistent ORM handles migrations; use `RESET_DB=true` for schema changes
- **Workspaces**: Run commands from correct directory (root for npm workspaces, `tdf-hq/` for Stack)
- **Currency precision**: Always use integer cents, never floating-point dollars
- **Time zones**: Respect `VITE_TZ`/`EXPO_PUBLIC_TZ` settings for consistent display
- **Submodules**: Run `git submodule update --init --recursive` after cloning or pulling mobile app changes