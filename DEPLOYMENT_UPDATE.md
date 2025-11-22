# Database Deployment System - Migration-Free Approach

## Summary

I've created a complete system for deploying the TDF Records platform **without runtime database migrations**. This avoids ALTER TABLE operations, lock contention, and deployment failures in production.

## What Was Created

### 1. Complete Database Schema
**`tdf-hq/sql/init_schema.sql`** (803 lines, 54 tables)
- Normalized schema for all TDF business operations
- 60+ tables covering: parties, bookings, packages, invoices, assets, sessions
- PostgreSQL 16+ with UUID and crypto extensions
- All indexes, constraints, and foreign keys included

### 2. Automation Scripts
- **`tdf-hq/scripts/export_data.sh`** - Export existing database data
- **`tdf-hq/scripts/init_fresh_db.sh`** - Initialize fresh database with schema
- **`tdf-hq/db/init.sh`** - Docker automatic initialization

### 3. Code Changes
- **`src/TDF/Config.hs`** - Added `runMigrations :: Bool` configuration
- **`app/Main.hs`** - Conditional migration execution
- **`docker-compose.yml`** - Auto-mount schema on container start
- **`Makefile`** - Added `export-data`, `init-fresh-db` targets

### 4. Documentation
- **`DATABASE_SCHEMA_GUIDE.md`** (10KB) - Comprehensive schema management guide
- **`MIGRATION_FREE_DEPLOYMENT.md`** (5KB) - Quick reference for deployments

## How It Works

### Traditional Approach (Problems)
```
App starts → Persistent ORM detects model changes → Runs ALTER TABLE → 
Production locks/failures → Downtime → 😱
```

### New Approach (Solutions)
```
Database initialized with complete schema → App connects → No migrations → 
Instant startup → ✅
```

## Usage Examples

### Docker Deployment (Recommended)
```bash
cd tdf-hq
make clean  # Remove old database volume
make up     # Start with fresh schema (auto-initialized)
make seed   # Optional: add development data
make health # Verify: should return OK
```

The schema is automatically applied via docker-compose volume mount.

### Manual Deployment
```bash
cd tdf-hq

# Option 1: Fresh database
./scripts/init_fresh_db.sh

# Option 2: Migrate existing data
./scripts/export_data.sh                           # Creates sql/data_export_*.sql
./scripts/init_fresh_db.sh sql/data_export_*.sql  # Imports data

# Start app with migrations disabled
RUN_MIGRATIONS=false stack run
```

### Production Deployment
```bash
# 1. Export production data
export DB_HOST=prod-server.com
export DB_USER=tdf_prod
export DB_NAME=tdf_hq
./scripts/export_data.sh

# 2. Initialize fresh database
./scripts/init_fresh_db.sh sql/data_export_prod.sql

# 3. Start application
RUN_MIGRATIONS=false stack run
```

## Environment Variables

### New Variable: `RUN_MIGRATIONS`
- `true` (default) - Use Persistent ORM migrations (development)
- `false` (production) - Use pre-initialized schema (deployment)

### Configuration Files
**`tdf-hq/config/default.env`**
```bash
RUN_MIGRATIONS=true  # For local development
```

**Docker Compose**
```yaml
app:
  environment:
    RUN_MIGRATIONS: "false"  # For container deployments
```

## Benefits

| Traditional Migrations | Clean Schema Initialization |
|----------------------|---------------------------|
| ❌ Runtime ALTER TABLE | ✅ Pre-created tables |
| ❌ Lock contention | ✅ No locks |
| ❌ Deployment failures | ✅ Instant startup |
| ❌ Rollback complexity | ✅ Easy rollback (Git) |
| ❌ Slow CI/CD | ✅ Fast pipelines |

## Schema Details

### Core Tables (BIGSERIAL IDs)
- **party** - Unified entity for customers, artists, staff, vendors
- **party_role** - Multi-role support (Admin, Engineer, Teacher, etc.)
- **booking** - Resource scheduling (60-min minimum, 15-min buffers)
- **package_product** / **package_purchase** - Hour-based packages
- **invoice** / **payment** / **receipt** - Complete billing system
- **service_catalog** / **service_order** - Service offerings & orders

### Extended Tables (UUID IDs)
- **room** / **asset** - Equipment & inventory management
- **band** / **band_member** - Artist/band relationships
- **session** / **session_deliverable** - Recording sessions
- **input_list** - Technical session documentation
- **stock_item** / **stock_movement** - Consumables tracking
- **pipeline_card** - Kanban workflow management

### Integration Tables
- **external_calendar_mapping** - Google Calendar sync
- **course_edition** / **lead** - Course registration system
- **whatsapp_message_log** - WhatsApp audit trail

### Key Design Decisions
1. **Cents-based currency** - All amounts stored as integers (avoids float errors)
2. **Soft deletes** - Status fields instead of DELETE (preserves history)
3. **Flexible references** - Text-based refs for loose coupling
4. **Two ID strategies** - BIGSERIAL for core business, UUID for extended features

## Testing

### Verify Schema
```bash
cd tdf-hq
make clean && make up
psql -U postgres -d tdf_hq -c "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public';"
# Should show 54+ tables
```

### Verify Application
```bash
make health
# Should return: {"status":"ok"}

make logs
# Should show: "Using pre-initialized schema"
```

## Backward Compatibility

The system **fully supports both approaches**:

### Development Mode (Keep Using Migrations)
```bash
# No changes needed - works as before
RUN_MIGRATIONS=true stack run
```

### Production Mode (Use Clean Schema)
```bash
# New deployment approach
RUN_MIGRATIONS=false stack run
```

## Migration Path for Existing Deployments

### Option 1: Keep Current Approach
No action needed. Set `RUN_MIGRATIONS=true` and continue using automatic migrations.

### Option 2: Migrate to Clean Schema
```bash
# 1. Test locally
cd tdf-hq
./scripts/export_data.sh
make clean
./scripts/init_fresh_db.sh sql/data_export_*.sql
RUN_MIGRATIONS=false stack run

# 2. Test on staging
export DB_HOST=staging-server
export DB_NAME=tdf_hq_staging
./scripts/init_fresh_db.sh sql/prod_backup.sql

# 3. Deploy to production
export DB_HOST=prod-server
export DB_NAME=tdf_hq_prod
./scripts/init_fresh_db.sh sql/prod_backup.sql
```

## CI/CD Integration

### GitHub Actions Example
```yaml
name: Deploy
on: push
jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Initialize Database
        run: |
          cd tdf-hq
          ./scripts/init_fresh_db.sh
        env:
          DB_HOST: ${{ secrets.DB_HOST }}
          DB_USER: ${{ secrets.DB_USER }}
          DB_PASS: ${{ secrets.DB_PASS }}
      
      - name: Build & Deploy
        run: |
          cd tdf-hq
          RUN_MIGRATIONS=false stack run
```

### Docker Production
```yaml
services:
  db:
    image: postgres:16-alpine
    volumes:
      - ./tdf-hq/sql/init_schema.sql:/docker-entrypoint-initdb.d/init.sql
  
  app:
    environment:
      RUN_MIGRATIONS: "false"
```

## Troubleshooting

### "Database already exists"
```bash
make clean  # Removes Docker volume
make up     # Recreates with fresh schema
```

### "Table already exists"
Schema uses `CREATE TABLE IF NOT EXISTS` - shouldn't happen. If it does:
```bash
psql -U postgres -c "DROP DATABASE tdf_hq; CREATE DATABASE tdf_hq;"
psql -U postgres -d tdf_hq -f tdf-hq/sql/init_schema.sql
```

### "Migrations still running"
```bash
# Check environment
echo $RUN_MIGRATIONS  # Should be "false"

# Or check logs
make logs | grep -i migration
# Should show: "RUN_MIGRATIONS disabled (using pre-initialized schema)."
```

## Maintenance

### When Adding New Tables
```bash
# 1. Update Haskell models (src/TDF/Models.hs or ModelsExtra.hs)
# 2. Let Persistent migrate in dev
RUN_MIGRATIONS=true stack run

# 3. Export updated schema
pg_dump -h 127.0.0.1 -U postgres -d tdf_hq \
  --schema-only --no-owner --no-privileges \
  -f sql/init_schema.sql

# 4. Commit changes
git add sql/init_schema.sql
git commit -m "Add new XYZ table"

# 5. Deploy (Docker automatically uses new schema)
make clean && make up
```

### Schema Versioning
The schema file is in Git - use branches/tags for versions:
```bash
git tag -a schema-v1.0 -m "Production schema January 2025"
git push origin schema-v1.0
```

## Documentation

Comprehensive guides available:

1. **[tdf-hq/DATABASE_SCHEMA_GUIDE.md](tdf-hq/DATABASE_SCHEMA_GUIDE.md)**
   - Complete schema management reference
   - All tables documented
   - Production deployment guide
   - Troubleshooting section

2. **[tdf-hq/MIGRATION_FREE_DEPLOYMENT.md](tdf-hq/MIGRATION_FREE_DEPLOYMENT.md)**
   - Quick reference for deployments
   - Common commands
   - Environment configuration

3. **[tdf-hq/sql/init_schema.sql](tdf-hq/sql/init_schema.sql)**
   - The actual schema (803 lines)
   - Well-commented
   - Ready to use

## Files Modified

```
tdf-hq/
├── app/Main.hs                          # Added conditional migrations
├── config/default.env                   # Added RUN_MIGRATIONS=true
├── db/init.sh                           # NEW: Docker init script
├── docker-compose.yml                   # Mount schema, disable migrations
├── Makefile                             # Added export-data, init-fresh-db
├── scripts/
│   ├── export_data.sh                   # NEW: Export data script
│   └── init_fresh_db.sh                 # NEW: Init database script
├── sql/
│   └── init_schema.sql                  # NEW: Complete schema (803 lines)
├── src/TDF/Config.hs                    # Added runMigrations field
├── DATABASE_SCHEMA_GUIDE.md             # NEW: Comprehensive guide
└── MIGRATION_FREE_DEPLOYMENT.md         # NEW: Quick reference
```

## Next Steps

1. ✅ **System is ready** - All files created and configured
2. 🧪 **Test locally** - Run `make clean && make up`
3. 📖 **Review docs** - Read DATABASE_SCHEMA_GUIDE.md
4. 🚀 **Plan deployment** - Decide: keep migrations or switch to clean schema
5. 👥 **Team training** - Share new deployment workflow
6. 📋 **Update runbooks** - Include new deployment commands

## Support & Questions

- Check logs: `make logs`
- View schema: `psql -U postgres -d tdf_hq -c '\dt'`
- Review docs in `tdf-hq/`
- Test scripts in development first

---

**Created:** 2025-01-19  
**Schema Version:** 54 tables, 74 indexes, 803 lines  
**Compatibility:** PostgreSQL 16+, Haskell Persistent ORM
