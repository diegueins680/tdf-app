# Development Guide

## Getting Started

### First Time Setup

1. **Install dependencies**
   ```bash
   # Root level (installs both UI and Mobile)
   npm install
   
   # Backend (Haskell)
   cd tdf-hq
   stack setup
   stack build
   ```

2. **Database setup**
   ```bash
   # Option 1: Local PostgreSQL
   createdb tdf_hq
   
   # Option 2: Docker
   cd tdf-hq
   docker-compose up -d postgres
   ```

3. **Configure environment**
   ```bash
   cp tdf-hq/config/default.env tdf-hq/.env
   cp tdf-hq-ui/.env.example tdf-hq-ui/.env
   cp tdf-mobile/.env.example tdf-mobile/.env
   
   # Edit each .env file with your settings
   ```

4. **Run migrations & seed data**
   ```bash
   cd tdf-hq
   RESET_DB=true SEED_DB=true stack run
   # After first run, set RESET_DB=false to preserve data
   ```

## Development Workflow

### Running the Stack

**Terminal 1 - Backend:**
```bash
cd tdf-hq
source .env  # or: set -a; source .env; set +a
stack run
# API runs on http://localhost:8080
```

**Terminal 2 - Web UI:**
```bash
npm run dev:ui
# UI runs on http://localhost:5173
```

**Terminal 3 - Mobile (optional):**
```bash
npm run dev:mobile
# Opens Expo DevTools
```

### Making API Changes

When you modify the OpenAPI spec or add endpoints:

```bash
# 1. Update backend code and OpenAPI spec
cd tdf-hq
# Edit docs/openapi/*.yaml

# 2. Regenerate frontend clients
npm run generate:api:ui
npm run generate:api:mobile

# 3. Update frontend code to use new types/endpoints
```

## Code Organization

### Backend (Haskell)

```
tdf-hq/src/TDF/
├── API.hs              # Main API routes
├── API/
│   ├── Admin.hs        # Admin endpoints
│   ├── Bands.hs        # Band management
│   ├── Inventory.hs    # Inventory tracking
│   ├── Pipelines.hs    # Kanban pipelines
│   ├── Rooms.hs        # Room/resource booking
│   └── Sessions.hs     # Session management
├── Server.hs           # Handler implementations
├── Models.hs           # Database schema
├── Auth.hs             # Authentication logic
└── Trials/             # Trial lesson module
```

**Key patterns:**
- Each feature has API + Server + Models modules
- DTOs define JSON payloads (`TDF.DTO`)
- Authentication uses bearer tokens (`TDF.Auth`)
- Database access via Persistent/Esqueleto

### Web UI (React)

```
tdf-hq-ui/src/
├── api/
│   ├── generated/      # Auto-generated from OpenAPI
│   ├── hooks.ts        # React Query hooks
│   └── client.ts       # API client setup
├── features/
│   ├── parties/        # CRM/customer management
│   ├── bookings/       # Calendar & scheduling
│   ├── pipelines/      # Kanban boards
│   └── packages/       # Package management
├── components/         # Reusable UI components
├── pages/             # Route pages
└── theme/             # MUI theme customization
```

**Key patterns:**
- Feature-based organization
- React Query for server state
- React Hook Form + Zod for validation
- MUI components with custom theme

### Mobile (React Native)

```
tdf-mobile/
├── app/               # Expo Router pages (file-based routing)
├── src/
│   ├── api/          # Generated clients + hooks
│   ├── components/   # Shared components
│   └── screens/      # Screen components
└── openapi/          # Local copy of OpenAPI specs
```

## Testing

### Backend Tests
```bash
cd tdf-hq
stack test  # (Not yet implemented - add Hspec specs)
```

### Frontend Tests
```bash
# Web UI
cd tdf-hq-ui
npm run test

# With coverage
npm run test -- --coverage
```

### Manual Testing
```bash
# Smoke tests
cd tdf-hq
./scripts/smoke.sh http://localhost:8080

# API exploration
curl -H "Authorization: Bearer <token>" \
  http://localhost:8080/parties
```

## Common Tasks

### Adding a New Entity

1. **Backend:**
   ```haskell
   -- src/TDF/Models.hs
   share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
   NewEntity
       name Text
       createdAt UTCTime
       deriving Show Eq
   |]
   
   -- src/TDF/API.hs
   type NewEntityAPI = 
     "entities" :> Get '[JSON] [Entity NewEntity]
   
   -- src/TDF/Server.hs
   getEntities :: AppM [Entity NewEntity]
   getEntities = runDB $ selectList [] []
   ```

2. **Update OpenAPI spec:**
   ```yaml
   # docs/openapi/main.yaml
   /entities:
     get:
       summary: List entities
       responses:
         '200':
           content:
             application/json:
               schema:
                 type: array
                 items:
                   $ref: '#/components/schemas/NewEntity'
   ```

3. **Regenerate frontend:**
   ```bash
   npm run generate:api:ui
   ```

4. **Use in frontend:**
   ```typescript
   // tdf-hq-ui/src/api/entities.ts
   import { useQuery } from '@tanstack/react-query';
   
   export function useEntities() {
     return useQuery({
       queryKey: ['entities'],
       queryFn: () => apiClient.GET('/entities')
     });
   }
   ```

### Database Migrations

```bash
# Create new migration
cd tdf-hq/sql
# Create new .sql file with timestamp prefix

# Apply migration
cd tdf-hq
./scripts/migrate_lessons.sh  # or appropriate script
```

### Troubleshooting

**Backend won't start:**
- Check PostgreSQL is running: `pg_isready`
- Verify credentials in `.env`
- Check port 8080 isn't in use: `lsof -i :8080`

**CORS errors:**
- Ensure `ALLOW_ORIGINS` includes your frontend URL
- Check browser console for exact error
- Verify API is running and accessible

**API generation fails:**
- Ensure OpenAPI spec is valid: `npx @stoplight/spectral-cli lint docs/openapi/*.yaml`
- Check path to spec in package.json scripts
- Verify openapi-typescript is installed

**Module not found (Haskell):**
- Run `stack clean && stack build`
- Check `.cabal` file has all modules listed
- Verify import paths match file structure

## Performance Tips

### Backend
- Use connection pooling (already configured)
- Add indexes on frequently queried columns
- Use `selectList` with limits for large tables
- Profile with `stack build --profile`

### Frontend
- Use React Query caching effectively
- Lazy load routes with React.lazy()
- Optimize re-renders with React.memo()
- Use virtual scrolling for long lists

## Git Workflow

```bash
# Create feature branch
git checkout -b feature/your-feature

# Make changes, commit often
git add .
git commit -m "feat: add entity management"

# Before pushing
npm run build:ui  # Ensure it builds
cd tdf-hq && stack build  # Ensure backend compiles

# Push and create PR
git push origin feature/your-feature
```

## Resources

- [Servant Documentation](https://docs.servant.dev/)
- [Persistent/Esqueleto Guide](https://www.yesodweb.com/book/persistent)
- [React Query Docs](https://tanstack.com/query/latest)
- [MUI Components](https://mui.com/material-ui/)
- [Expo Documentation](https://docs.expo.dev/)

---

Need help? Check the main README or ask the team!
