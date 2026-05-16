# Multi-Role User Management Implementation

This implementation provides a complete multi-role user management system for the TDF Records platform, allowing users (parties) to have multiple roles instead of a single role.

## Overview

The refactoring changes the user role system from a one-to-one relationship (one role per user) to a many-to-many relationship (multiple roles per user) using a join table.

## Architecture

### Database Schema Changes

**Before:**
```sql
CREATE TABLE party (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    role VARCHAR(50),  -- Single role column (REMOVED)
    ...
);
```

**After:**
```sql
CREATE TABLE party (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    -- role column removed
    ...
);

CREATE TABLE party_role_assignment (
    id SERIAL PRIMARY KEY,
    party_id INTEGER NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    role VARCHAR(50) NOT NULL,
    assigned_at TIMESTAMP NOT NULL DEFAULT NOW(),
    assigned_by INTEGER REFERENCES party(id) ON DELETE SET NULL,
    CONSTRAINT unique_party_role UNIQUE (party_id, role)
);
```

### Backend (Haskell)

#### File Structure
```
tdf-hq/
├── src/TDF/
│   ├── Models.hs       - Database schema definitions
│   ├── API.hs          - API endpoint types
│   ├── DTO.hs          - Data Transfer Objects
│   ├── DB.hs           - Database access functions
│   └── Server.hs       - Request handlers
├── app/
│   └── Main.hs         - Application entry point
├── docs/openapi/
│   └── user-roles.yaml - OpenAPI specification
├── sql/
│   └── 001_multi_role_migration.sql - Database migration
├── package.yaml        - Package configuration
└── stack.yaml          - Stack configuration
```

#### Key Changes

**1. Models.hs** - PartyRoleAssignment join table:
```haskell
PartyRoleAssignment
    partyId PartyId
    role PartyRole
    assignedAt UTCTime
    assignedBy PartyId Maybe
    UniquePartyRole partyId role
```

**2. API.hs** - New endpoints:
- `GET /api/users` - Returns users with their roles
- `GET /api/users/{userId}/roles` - Get roles for a specific user
- `PUT /api/users/{userId}/roles` - Update roles for a user (replaces all existing roles)

**3. DB.hs** - Database functions:
- `getUsersWithRoles` - Fetch all users with their assigned roles
- `getUserRoles` - Get roles for a specific user
- `updateUserRoles` - Replace all roles for a user
- `addUserRole` - Add a single role to a user
- `removeUserRole` - Remove a single role from a user
- `userHasRole` - Check if a user has a specific role

### Frontend (React/TypeScript)

#### File Structure
```
tdf-hq-ui/
├── src/
│   ├── components/
│   │   └── UserRoleManagement.tsx  - Main component
│   ├── api/generated/
│   │   ├── types.ts                - TypeScript types
│   │   └── client.ts               - API client
│   ├── App.tsx                     - Root component
│   └── main.tsx                    - Entry point
├── package.json
├── tsconfig.json
├── vite.config.ts
└── index.html
```

#### UserRoleManagement Component

Features:
- **Table view** displaying all users with their roles
- **Multi-select dropdown** for editing roles using MUI Select with `multiple` prop
- **Role chips** display using MUI Chip components with color coding
- **Edit dialog** for managing user roles
- **API integration** using the generated client

Key UI elements:
```tsx
// Multi-select for roles
<Select
  multiple
  value={selectedRoles}
  onChange={handleRoleChange}
  renderValue={(selected) => (
    <Box sx={{ display: 'flex', flexWrap: 'wrap', gap: 0.5 }}>
      {selected.map((role) => (
        <Chip key={role} label={role} size="small" color={ROLE_COLORS[role]} />
      ))}
    </Box>
  )}
>
  {ALL_ROLES.map((role) => (
    <MenuItem key={role} value={role}>{role}</MenuItem>
  ))}
</Select>

// Display roles in table
<Box display="flex" gap={0.5} flexWrap="wrap">
  {user.roles.map((role) => (
    <Chip key={role} label={role} color={ROLE_COLORS[role]} size="small" />
  ))}
</Box>
```

## API Endpoints

### GET /api/users
Returns all users with their roles.

**Response:**
```json
[
  {
    "id": 1,
    "name": "John Doe",
    "email": "john@example.com",
    "phone": "+1234567890",
    "roles": ["Admin", "Manager"],
    "createdAt": "2025-01-01T00:00:00Z"
  }
]
```

### GET /api/users/{userId}/roles
Returns roles for a specific user.

**Response:**
```json
["Admin", "Manager", "Teacher"]
```

### PUT /api/users/{userId}/roles
Updates roles for a user (replaces all existing roles).

**Request:**
```json
{
  "roles": ["Teacher", "Artist"]
}
```

**Response:** `200 OK` (empty body)

## Available Roles

- `Admin` - Full system access
- `Manager` - Management functions
- `Engineer` - Audio engineering tasks
- `Teacher` - Teaching and lesson management
- `Reception` - Front desk operations
- `Accounting` - Financial operations
- `Artist` - Artist-specific features
- `Student` - Student-specific features
- `ReadOnly` - View-only access

## Database Migration

Run the migration script to update the schema:

```bash
cd tdf-hq
psql -d tdf_hq -f sql/001_multi_role_migration.sql
```

The migration:
1. Creates the `party_role_assignment` table
2. Adds indexes for performance
3. Migrates existing single-role data (if present)
4. Removes the old `role` column from the `party` table

## Development Setup

### Backend

```bash
cd tdf-hq

# Install Haskell dependencies
stack setup
stack build

# Run the server
stack run
```

The API will be available at `http://localhost:8080`.

### Frontend

```bash
cd tdf-hq-ui

# Install dependencies
npm install

# Run development server
npm run dev
```

The UI will be available at `http://localhost:5173`.

### Generate API Client

After updating the OpenAPI spec:

```bash
cd tdf-hq-ui
npm run generate:api
```

## Testing

### Manual Testing

1. Start the backend server
2. Start the frontend development server
3. Open `http://localhost:5173` in a browser
4. Use the UI to:
   - View users and their current roles
   - Click the edit icon for a user
   - Select multiple roles from the dropdown
   - Save changes
   - Verify roles are updated in the table

### API Testing with curl

```bash
# Get all users
curl http://localhost:8080/api/users

# Get roles for user ID 1
curl http://localhost:8080/api/users/1/roles

# Update roles for user ID 1
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Teacher", "Artist"]}'
```

## Security Considerations

1. **Authorization**: Add authentication and authorization checks to ensure only admins can modify roles
2. **Audit Trail**: The `assigned_by` field tracks who assigned roles
3. **Validation**: Validate that roles are from the allowed set
4. **Cascading Deletes**: When a user is deleted, their role assignments are automatically removed

## Performance Optimizations

1. **Indexes** on `party_id` and `role` columns for fast lookups
2. **Unique constraint** on `(party_id, role)` prevents duplicate assignments
3. **Connection pooling** in the backend for database connections

## Future Enhancements

- Role hierarchy and permission system
- Role expiration dates
- Role approval workflow
- Activity logging for role changes
- Bulk role assignment
- Role templates
- Mobile app integration

## Troubleshooting

### Backend won't compile
```bash
cd tdf-hq
stack clean
stack build
```

### Frontend build errors
```bash
cd tdf-hq-ui
rm -rf node_modules package-lock.json
npm install
```

### Database connection issues
Check PostgreSQL is running and credentials in `.env` are correct:
```bash
pg_isready
psql -d tdf_hq -U postgres
```

## File Manifest

### Backend Files Created
- `tdf-hq/src/TDF/Models.hs` - Database schema
- `tdf-hq/src/TDF/API.hs` - API types
- `tdf-hq/src/TDF/DTO.hs` - Data transfer objects
- `tdf-hq/src/TDF/DB.hs` - Database functions
- `tdf-hq/src/TDF/Server.hs` - Request handlers
- `tdf-hq/app/Main.hs` - Application entry
- `tdf-hq/docs/openapi/user-roles.yaml` - API spec
- `tdf-hq/sql/001_multi_role_migration.sql` - Migration
- `tdf-hq/package.yaml` - Package config
- `tdf-hq/stack.yaml` - Stack config

### Frontend Files Created
- `tdf-hq-ui/src/components/UserRoleManagement.tsx` - Main component
- `tdf-hq-ui/src/api/generated/types.ts` - TypeScript types
- `tdf-hq-ui/src/api/generated/client.ts` - API client
- `tdf-hq-ui/src/App.tsx` - Root component
- `tdf-hq-ui/src/main.tsx` - Entry point
- `tdf-hq-ui/index.html` - HTML template
- `tdf-hq-ui/package.json` - Package config
- `tdf-hq-ui/tsconfig.json` - TypeScript config
- `tdf-hq-ui/vite.config.ts` - Vite config

## License

MIT License - See individual project READMEs for details.
