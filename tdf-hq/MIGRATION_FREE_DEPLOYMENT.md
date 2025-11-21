# Migration-Free Database Deployment

## Summary

This guide provides a complete solution for deploying TDF Records without runtime database migrations. The schema is pre-initialized using SQL scripts, avoiding ALTER TABLE operations and deployment failures.

## Quick Reference

### For Docker Deployments (Recommended)

```bash
# Fresh start with clean schema
make clean   # Remove old volumes
make up      # Start with init_schema.sql
make seed    # Optional: Add development data
```

**Environment Variables:**
- `RUN_MIGRATIONS=false` - Disable Haskell Persistent migrations
- `SEED_DB=true` - Enable/disable automatic seeding

### For Manual Deployments

```bash
# 1. Initialize fresh database
./scripts/init_fresh_db.sh

# 2. Start application with migrations disabled
RUN_MIGRATIONS=false stack run

# Or with data import
./scripts/init_fresh_db.sh sql/data_export_backup.sql
```

### For Production Deployments

```bash
# 1. Export current production data
make export-data

# 2. Test on staging
export DB_NAME=tdf_hq_staging
./scripts/init_fresh_db.sh sql/data_export_prod.sql

# 3. Deploy to production
export DB_HOST=prod-server
export DB_NAME=tdf_hq_prod
./scripts/init_fresh_db.sh sql/data_export_prod.sql

# 4. Start app with migrations disabled
RUN_MIGRATIONS=false stack run
```

## Files Created

1. **`sql/init_schema.sql`** - Complete normalized database schema (60+ tables)
2. **`scripts/export_data.sh`** - Export data from existing database
3. **`scripts/init_fresh_db.sh`** - Initialize fresh database with schema
4. **`db/init.sh`** - Docker initialization script
5. **`DATABASE_SCHEMA_GUIDE.md`** - Comprehensive documentation

## Configuration Changes

### Environment Variables

New variable: `RUN_MIGRATIONS`
- `true` (default for local dev) - Run Persistent migrations
- `false` (for production) - Skip migrations, use pre-initialized schema

### Docker Compose

```yaml
db:
  volumes:
    - ./sql/init_schema.sql:/docker-entrypoint-initdb.d/01-init_schema.sql:ro
app:
  environment:
    RUN_MIGRATIONS: "false"  # Use pre-initialized schema
```

### Haskell Code Changes

- `src/TDF/Config.hs` - Added `runMigrations :: Bool` field
- `app/Main.hs` - Conditional migration execution

## Workflow

### Development (Local)

```bash
# Traditional approach - let Persistent handle migrations
RUN_MIGRATIONS=true stack run
```

### Staging/Production

```bash
# Use clean schema initialization
RUN_MIGRATIONS=false stack run
```

### Schema Updates

When you modify Haskell models:

```bash
# 1. Update models in src/TDF/Models.hs
# 2. Test locally (let Persistent migrate)
RUN_MIGRATIONS=true stack run

# 3. Export updated schema
pg_dump -h 127.0.0.1 -U postgres -d tdf_hq \
  --schema-only --no-owner -f sql/init_schema.sql

# 4. Commit schema
git add sql/init_schema.sql
git commit -m "Update schema: added XYZ table"

# 5. Deploy uses new schema
make clean && make up
```

## Benefits

✅ **No runtime migrations** - Avoid ALTER TABLE on production  
✅ **Faster deployments** - Pre-initialized schema is instant  
✅ **Version controlled** - Schema is a single SQL file  
✅ **Rollback friendly** - Easy to revert schema changes  
✅ **CI/CD compatible** - Simple to test in pipelines  
✅ **Container optimized** - Works perfectly with Docker  

## Troubleshooting

### "Table already exists"

```bash
# Schema uses IF NOT EXISTS - this shouldn't happen
# If it does, clean and restart:
make clean && make up
```

### "Migrations still running"

Check environment variable:
```bash
echo $RUN_MIGRATIONS  # Should be "false" for production
```

### "Data import failed"

```bash
# Export with proper format
./scripts/export_data.sh

# Import with disable-triggers
psql -U postgres -d tdf_hq -v ON_ERROR_STOP=1 -f sql/data_export.sql
```

## See Also

- **[DATABASE_SCHEMA_GUIDE.md](./DATABASE_SCHEMA_GUIDE.md)** - Complete documentation
- **[docker-compose.yml](./docker-compose.yml)** - Docker configuration
- **[Makefile](./Makefile)** - Convenient commands

## Migration Path

### From Old System

If you're currently using automatic migrations:

**Option 1: Keep migrations enabled for now**
```bash
# In config/default.env or .env
RUN_MIGRATIONS=true
```

**Option 2: Switch to clean schema**
```bash
# 1. Export current data
make export-data

# 2. Test fresh schema
make clean
make init-fresh-db DATA_FILE=sql/data_export_latest.sql

# 3. Update config
RUN_MIGRATIONS=false

# 4. Test application
stack run
```

### For New Deployments

Use clean schema from the start:
```bash
# docker-compose.yml already configured
make up
```

## Support

For questions or issues:
1. Review [DATABASE_SCHEMA_GUIDE.md](./DATABASE_SCHEMA_GUIDE.md)
2. Check application logs: `make logs`
3. Verify database: `psql -U postgres -d tdf_hq -c '\dt'`
4. Contact development team

---

**Last Updated:** 2025-01-19  
**Schema Version:** init_schema.sql (60+ tables, normalized structure)
