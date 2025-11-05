# TDF HQ - Backend API

Haskell backend for TDF Records Management Platform.

## Tech Stack

- **Servant** - Type-safe REST API framework
- **PostgreSQL** - Database
- **Persistent** - Type-safe database ORM
- **OpenAPI 3** - API documentation

## Project Structure

```
tdf-hq/
├── app/
│   └── Main.hs           # Application entry point
├── src/
│   └── TDF/
│       ├── Models.hs     # Database models
│       ├── DTO.hs        # Data Transfer Objects
│       ├── API.hs        # API type definitions
│       ├── DB.hs         # Database operations
│       ├── Server.hs     # Server implementation
│       └── OpenAPI.hs    # OpenAPI spec generation
├── package.yaml          # Package configuration
└── stack.yaml            # Stack configuration
```

## Development

### Prerequisites

- Stack (Haskell build tool)
- PostgreSQL 13+

### Setup

1. Install Stack:
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

2. Set up environment variables:
```bash
export DB_HOST=127.0.0.1
export DB_PORT=5432
export DB_NAME=tdfhq
export DB_USER=tdf
export DB_PASSWORD=tdf
```

3. Build the project:
```bash
stack build
```

4. Run the server:
```bash
stack run
```

The server will start on port 8080 and generate an `openapi.json` file.

## API Endpoints

### User Management

- `GET /api/users` - List all users with their party information and roles
- `PUT /api/users/{userId}/role` - Update a user's role

## Database Schema

### Party
- One-to-one relationship with User
- Contains role information
- Roles: Admin, Manager, Engineer, Teacher, Reception, Accounting, ReadOnly, Customer, Artist, Student

### User
- Associated with exactly one Party
- Contains authentication and login information

## OpenAPI Specification

The OpenAPI specification is automatically generated on server startup and saved to `openapi.json`.
