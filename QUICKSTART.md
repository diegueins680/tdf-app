# Quick Start Guide - User Role Management System

This guide will help you get the TDF Records User Role Management system up and running.

## Prerequisites

### Backend
- Stack (Haskell build tool) - Install: `curl -sSL https://get.haskellstack.org/ | sh`
- PostgreSQL 13+ database server

### Frontend
- Node.js 18+
- npm (comes with Node.js)

## Setup Steps

### 1. Set up PostgreSQL Database

```bash
# Create database and user
sudo -u postgres psql
CREATE DATABASE tdfhq;
CREATE USER tdf WITH PASSWORD 'tdf';
GRANT ALL PRIVILEGES ON DATABASE tdfhq TO tdf;
\q
```

### 2. Configure Environment Variables

**Backend (`tdf-hq/.env`):**
```bash
DB_HOST=127.0.0.1
DB_PORT=5432
DB_NAME=tdfhq
DB_USER=tdf
DB_PASSWORD=tdf
```

**Frontend (`tdf-hq-ui/.env`):**
```bash
VITE_API_BASE=http://localhost:8080
```

### 3. Start the Backend

```bash
cd tdf-hq

# First time setup (may take several minutes)
stack setup
stack build

# Run the server
stack run
```

Expected output:
```
========================================
  TDF Records API - User Management
========================================

Database: host=127.0.0.1 port=5432 user=tdf password=tdf dbname=tdfhq

[1/3] Running database migrations...
      ✓ Migrations completed

[2/3] Generating OpenAPI specification...
      ✓ OpenAPI spec saved to: openapi.json

[3/3] Starting API server...
      ✓ Server running on http://localhost:8080

API Endpoints:
  GET  /api/users          - List all users
  PUT  /api/users/:id/role - Update user role

========================================
```

### 4. Start the Frontend

In a new terminal:

```bash
cd tdf-hq-ui

# Install dependencies
npm install

# Start development server
npm run dev
```

The UI will be available at: **http://localhost:3000**

### 5. Test the System

#### Option 1: Using the UI
1. Open http://localhost:3000 in your browser
2. You'll see the User Role Management interface
3. Initially, there will be no users (empty table)

#### Option 2: Using curl

**List users:**
```bash
curl http://localhost:8080/api/users
```

**Create sample data** (if you implement the seed endpoint):
```bash
# Add seed endpoint to backend first
curl -X POST http://localhost:8080/admin/seed
```

**Update a user's role:**
```bash
curl -X PUT http://localhost:8080/api/users/1/role \
  -H "Content-Type: application/json" \
  -d '{"urrRole": "AdminRole"}'
```

## Adding Sample Users

Since the initial database is empty, you can add users directly via PostgreSQL:

```sql
-- Connect to database
psql -U tdf -d tdfhq

-- Insert sample party
INSERT INTO party (name, email, phone, role, created_at, updated_at)
VALUES ('John Doe', 'john@example.com', '+593-99-123-4567', 'AdminRole', NOW(), NOW());

-- Get the party ID (adjust based on actual ID returned)
-- Then insert corresponding user
INSERT INTO "user" (party_id, password_hash, is_active, created_at, updated_at)
VALUES (1, '$2b$10$dummyhash', true, NOW(), NOW());
```

Or use the seed data module if implemented.

## Development Workflow

### Making Backend Changes

1. Edit files in `tdf-hq/src/TDF/`
2. Rebuild: `stack build`
3. Restart: `stack run`
4. Regenerate OpenAPI: Automatically done on startup

### Making Frontend Changes

1. Edit files in `tdf-hq-ui/src/`
2. Changes hot-reload automatically
3. Regenerate API client if backend changed:
   ```bash
   npm run generate:api
   ```

### Building for Production

**Backend:**
```bash
cd tdf-hq
stack build --copy-bins
```

**Frontend:**
```bash
cd tdf-hq-ui
npm run build
# Output in dist/ folder
```

## Troubleshooting

### Backend won't start
- Check PostgreSQL is running: `systemctl status postgresql`
- Verify database credentials in `.env`
- Check port 8080 is available: `lsof -i :8080`

### Frontend can't connect to backend
- Verify backend is running on port 8080
- Check `VITE_API_BASE` in `.env`
- Open browser console for detailed error messages

### Build errors
- Backend: Try `stack clean && stack build`
- Frontend: Try `rm -rf node_modules && npm install`

## Next Steps

1. **Add Authentication**: Implement JWT-based auth
2. **Add Seed Data**: Create sample users for testing
3. **Role Restrictions**: Ensure only Admins can change roles
4. **Audit Logging**: Track role changes
5. **Enhanced UI**: Add search, filters, pagination

## API Documentation

Full API documentation is available in:
- `tdf-hq/openapi.json` - OpenAPI 3 specification
- Interactive docs: Use Swagger UI or similar tool

## Support

For issues or questions:
- Check README.md files in each directory
- Review IMPLEMENTATION_SUMMARY.md for architecture details
- Check the specs.yaml for business requirements
