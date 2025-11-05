# TDF HQ Backend

Haskell backend API server for the TDF Records platform with multi-role user management.

## Features

- **Multi-role user management** - Users can have multiple roles simultaneously
- **RESTful API** - Clean Servant-based API
- **PostgreSQL database** - Persistent ORM with automatic migrations
- **Type-safe** - Full type safety from database to API
- **OpenAPI documentation** - Auto-generated API specification

## Prerequisites

- Stack (Haskell build tool)
- PostgreSQL 16+
- GHC 9.4+ (installed via Stack)

## Quick Start

### Database Setup

```bash
# Create database
createdb tdf_hq

# Or using Docker
docker run --name tdf-postgres \
  -e POSTGRES_DB=tdf_hq \
  -e POSTGRES_USER=postgres \
  -e POSTGRES_PASSWORD=postgres \
  -p 5432:5432 \
  -d postgres:16
```

### Build and Run

```bash
# Install dependencies and build
stack setup
stack build

# Run migrations
psql -d tdf_hq -f sql/001_multi_role_migration.sql

# Start the server
stack run
```

The API will be available at `http://localhost:8080`.

## API Endpoints

### Users and Roles

- `GET /api/users` - List all users with their roles
- `GET /api/users/{userId}/roles` - Get roles for a specific user
- `PUT /api/users/{userId}/roles` - Update roles for a user

See `docs/openapi/user-roles.yaml` for full API specification.

## Development

### Interactive Development

```bash
# Start GHCi REPL
stack ghci

# Load modules
:load TDF.Models
:load TDF.Server

# Test functions
getUsersWithRoles
```

### Database Migrations

SQL migrations are in the `sql/` directory. Apply them manually:

```bash
psql -d tdf_hq -f sql/001_multi_role_migration.sql
```

### Adding New Endpoints

1. Define types in `src/TDF/API.hs`
2. Add handler in `src/TDF/Server.hs`
3. Add database logic in `src/TDF/DB.hs` (if needed)
4. Update OpenAPI spec in `docs/openapi/`
5. Regenerate frontend client

## Project Structure

```
tdf-hq/
├── app/
│   └── Main.hs              # Application entry point
├── src/TDF/
│   ├── Models.hs            # Database schema (Persistent)
│   ├── API.hs               # API type definitions (Servant)
│   ├── DTO.hs               # Data transfer objects
│   ├── DB.hs                # Database access layer
│   └── Server.hs            # Request handlers
├── docs/openapi/
│   └── user-roles.yaml      # OpenAPI 3.0 specification
├── sql/
│   └── 001_multi_role_migration.sql  # Database migration
├── package.yaml             # Package configuration
└── stack.yaml               # Stack configuration
```

## Environment Variables

Create a `.env` file (or export variables):

```bash
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=postgres
DB_NAME=tdf_hq
APP_PORT=8080
```

## Testing

### Manual API Testing

```bash
# Get all users
curl http://localhost:8080/api/users

# Get roles for user 1
curl http://localhost:8080/api/users/1/roles

# Update roles
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Teacher", "Artist"]}'
```

### Automated Tests

```bash
stack test
```

## Deployment

```bash
# Build production binary
stack build --copy-bins

# Binary will be in ~/.local/bin/tdf-hq
~/.local/bin/tdf-hq
```

## Troubleshooting

### Stack build fails

```bash
stack clean
stack setup
stack build
```

### Database connection refused

Check PostgreSQL is running:
```bash
pg_isready
```

### Module not found

Ensure all modules are listed in `package.yaml` under `exposed-modules`.

## License

MIT
