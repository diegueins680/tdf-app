# Database Schema Management Guide

## Overview

This guide explains how to deploy the TDF Records platform with a **clean, normalized database schema** that avoids runtime migrations. The approach uses a complete SQL initialization script that creates all tables with their final structure.

## The Problem with Migrations

Traditional ORMs (like Persistent in Haskell) automatically run migrations when the application starts, which can cause:
- **Deployment failures** due to ALTER TABLE operations on production data
- **Downtime** during schema changes
- **Lock contention** on large tables
- **Rollback complexity** when migrations fail

## The Solution: Clean Schema Initialization

Instead of incremental migrations, we:
1. **Export the current schema** to a complete SQL file
2. **Deploy with the final schema** from scratch
3. **Migrate data separately** if needed using data-only exports

This approach is ideal for:
- ✅ New deployments (staging, production)
- ✅ CI/CD pipelines
- ✅ Container-based deployments
- ✅ Database version upgrades

## Files Structure

```
tdf-hq/
├── sql/
│   ├── init_schema.sql           # Complete normalized schema (USE THIS)
│   ├── 001_*.sql                 # Old migration files (historical reference)
│   └── data_export_*.sql         # Data snapshots (generated)
├── scripts/
│   ├── export_data.sh            # Export data from existing DB
│   └── init_fresh_db.sh          # Initialize fresh DB with schema
└── Makefile                      # Convenient make targets
```

## Quick Start

### Option 1: Fresh Deployment (Recommended)

```bash
# Using Make targets (easiest)
make init-fresh-db

# Or using the script directly
./scripts/init_fresh_db.sh
```

This will:
1. Drop existing database if present (with confirmation)
2. Create a fresh database
3. Apply the complete schema from `sql/init_schema.sql`

### Option 2: Migrate Existing Data

```bash
# 1. Export current data
make export-data
# Creates: sql/data_export_YYYYMMDD_HHMMSS.sql

# 2. Initialize fresh DB with exported data
make init-fresh-db DATA_FILE=sql/data_export_20250119_120000.sql
```

### Option 3: Docker Deployment

The Docker setup automatically uses clean schema initialization:

```bash
# Start with fresh database
make clean  # Remove old volumes
make up     # Starts PostgreSQL + API with clean schema
make seed   # Populate with development data
```

## Manual Database Setup

### Local PostgreSQL

```bash
# Create database
psql -U postgres -c "CREATE DATABASE tdf_hq;"

# Apply schema
psql -U postgres -d tdf_hq -f sql/init_schema.sql

# Optional: Import data
psql -U postgres -d tdf_hq -f sql/data_export.sql
```

### Remote Database (Production)

```bash
# Set connection environment variables
export DB_HOST=your-production-host.com
export DB_PORT=5432
export DB_USER=tdf_prod_user
export DB_PASS=your-secure-password
export DB_NAME=tdf_hq_prod

# Initialize with schema
./scripts/init_fresh_db.sh

# Import production data backup if needed
./scripts/init_fresh_db.sh /path/to/production_backup.sql
```

## Schema Details

### Database Schema (`sql/init_schema.sql`)

The schema includes:

#### Core Business Tables (BIGSERIAL IDs)
- **party** - Unified entity for all people/organizations
- **party_role** - Multi-role support (Admin, Engineer, Artist, Student, etc.)
- **service_catalog** - Service offerings
- **service_order** - Customer orders and projects
- **booking** - Resource scheduling with 60-minute minimums
- **attendance** - Lesson/session attendance tracking
- **package_product** - Hour-based packages (e.g., "Guitar 24h")
- **package_purchase** - Customer package purchases
- **invoice** / **payment** / **receipt** - Complete billing system

#### Extended Features (UUID IDs)
- **room** - Bookable spaces with technical specs
- **asset** - Equipment inventory with QR codes
- **band** / **band_member** - Artist/band management
- **session** - Recording/mixing sessions
- **input_list** - Session technical documentation
- **stock_item** - Consumables inventory
- **pipeline_card** - Kanban workflow tracking

#### Integration Tables
- **external_calendar_mapping** - Google Calendar sync
- **course_edition** / **lead** - Course registration
- **whatsapp_message_log** - WhatsApp integration audit

### Key Design Decisions

1. **Two ID Strategies**:
   - `BIGSERIAL` for business entities (party, bookings, invoices)
   - `UUID` for extended features (assets, rooms, sessions)
   - Reason: Core tables need sequential IDs for human reference, extended features benefit from UUIDs for distributed systems

2. **Cents-based Currency**:
   - All monetary values stored as integers in cents
   - Avoids floating-point precision errors
   - Example: $10.50 = 1050 cents

3. **Soft Deletes via Status**:
   - Most entities use status fields instead of DELETE
   - Preserves audit history
   - Example: `booking.status = 'Cancelled'`

4. **Flexible References**:
   - Extended tables use text-based refs to core entities
   - Reduces foreign key coupling
   - Example: `session.client_party_ref` instead of `client_party_id`

## Development Workflow

### Making Schema Changes

When you need to modify the database structure:

1. **Update Haskell Models**:
   ```haskell
   -- In src/TDF/Models.hs or src/TDF/ModelsExtra.hs
   Party
       displayName   Text
       newField      Text Maybe  -- Add new field
   ```

2. **Let Persistent Generate Schema**:
   ```bash
   stack run  # Persistent will auto-migrate in dev
   ```

3. **Export Clean Schema**:
   ```bash
   pg_dump -h 127.0.0.1 -U postgres -d tdf_hq \
     --schema-only --no-owner --no-privileges \
     -f sql/init_schema.sql
   ```

4. **Update Documentation**:
   - Add comments to `sql/init_schema.sql`
   - Update this README if needed

5. **Test Fresh Deployment**:
   ```bash
   make clean
   make init-fresh-db
   stack run
   ```

### Testing Schema Changes

```bash
# 1. Backup current data
make export-data

# 2. Test fresh schema
DB_NAME=tdf_hq_test make init-fresh-db

# 3. Run application tests
stack test

# 4. Rollback if needed
DB_NAME=tdf_hq make init-fresh-db DATA_FILE=sql/data_export_backup.sql
```

## Production Deployment

### Pre-Deployment Checklist

- [ ] Export current production data: `make export-data`
- [ ] Test schema on staging with production data copy
- [ ] Verify all indexes are present
- [ ] Check constraint names don't conflict
- [ ] Review foreign key ON DELETE behaviors
- [ ] Backup production database externally

### Deployment Steps

```bash
# 1. Schedule maintenance window
# 2. Stop application
systemctl stop tdf-hq-api

# 3. Backup production database
pg_dump -h prod-db -U tdf_user -d tdf_hq -Fc -f backup_$(date +%Y%m%d).dump

# 4. Initialize fresh database
export DB_HOST=prod-db
export DB_USER=tdf_user
export DB_NAME=tdf_hq_new
./scripts/init_fresh_db.sh

# 5. Import production data
psql -h prod-db -U tdf_user -d tdf_hq_new -f production_data.sql

# 6. Verify data integrity
psql -h prod-db -U tdf_user -d tdf_hq_new -c "SELECT COUNT(*) FROM party;"

# 7. Update connection string to new database
# 8. Start application
systemctl start tdf-hq-api

# 9. Verify health
curl http://api.tdfrecords.com/health
```

### Rollback Plan

```bash
# If deployment fails, switch back to old database
export DB_NAME=tdf_hq  # Original database
systemctl restart tdf-hq-api
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
        env:
          DB_HOST: ${{ secrets.DB_HOST }}
          DB_USER: ${{ secrets.DB_USER }}
          DB_PASS: ${{ secrets.DB_PASS }}
        run: |
          cd tdf-hq
          ./scripts/init_fresh_db.sh
      
      - name: Deploy Application
        run: |
          # Your deployment steps
```

### Docker Compose Production

```yaml
# docker-compose.prod.yml
services:
  db:
    image: postgres:16-alpine
    volumes:
      - ./sql/init_schema.sql:/docker-entrypoint-initdb.d/01-schema.sql
      - ./sql/seed_data.sql:/docker-entrypoint-initdb.d/02-seed.sql
      - pgdata:/var/lib/postgresql/data
```

## Troubleshooting

### "Database already exists"

```bash
# Drop and recreate
psql -U postgres -c "DROP DATABASE tdf_hq; CREATE DATABASE tdf_hq;"
psql -U postgres -d tdf_hq -f sql/init_schema.sql
```

### "Permission denied"

```bash
# Grant permissions
psql -U postgres -d tdf_hq -c "GRANT ALL ON DATABASE tdf_hq TO your_user;"
psql -U postgres -d tdf_hq -c "GRANT ALL ON ALL TABLES IN SCHEMA public TO your_user;"
```

### "Table already exists"

The schema uses `CREATE TABLE IF NOT EXISTS`, so this shouldn't happen. If it does:
```bash
# Check for partial schema application
psql -U postgres -d tdf_hq -c "\dt"

# Drop all tables and reapply
psql -U postgres -d tdf_hq -c "DROP SCHEMA public CASCADE; CREATE SCHEMA public;"
psql -U postgres -d tdf_hq -f sql/init_schema.sql
```

### Data Import Conflicts

```bash
# Import with constraints disabled (careful!)
psql -U postgres -d tdf_hq << EOF
SET session_replication_role = replica;
\i sql/data_export.sql
SET session_replication_role = DEFAULT;
EOF
```

## Migration from Old System

If you're currently using automatic migrations:

1. **Extract Current Schema**:
   ```bash
   pg_dump -h 127.0.0.1 -U postgres -d tdf_hq_old \
     --schema-only --no-owner -f sql/old_schema.sql
   ```

2. **Compare Schemas**:
   ```bash
   diff sql/old_schema.sql sql/init_schema.sql
   ```

3. **Test Migration**:
   ```bash
   # Export old data
   pg_dump -h 127.0.0.1 -U postgres -d tdf_hq_old \
     --data-only --column-inserts -f sql/migration_data.sql
   
   # Apply to new schema
   make init-fresh-db DATA_FILE=sql/migration_data.sql
   ```

## Best Practices

1. **Version Control Schema**: Always commit `sql/init_schema.sql` changes
2. **Document Changes**: Add comments for complex constraints
3. **Test Locally First**: Use `make clean && make init-fresh-db` frequently
4. **Backup Before Deploy**: Always export data before schema changes
5. **Use Transactions**: Wrap data imports in BEGIN/COMMIT blocks
6. **Monitor Indexes**: Use `EXPLAIN ANALYZE` to verify query performance
7. **Regular Exports**: Schedule daily data exports for disaster recovery

## Additional Resources

- [PostgreSQL Documentation](https://www.postgresql.org/docs/16/)
- [Persistent ORM](https://hackage.haskell.org/package/persistent)
- [TDF Platform Architecture](../ARCHITECTURE_DIAGRAM.md)
- [Development Guide](../DEVELOPMENT.md)

## Support

For issues or questions:
1. Check the [Troubleshooting Guide](../TROUBLESHOOTING.md)
2. Review logs: `make logs`
3. Contact the development team
