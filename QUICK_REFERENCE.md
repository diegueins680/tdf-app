# Multi-Role User Management - Quick Reference

> Scope: This quick reference covers the multi-role user management feature. For overall project setup, see [QUICKSTART.md](QUICKSTART.md) and [QUICK_START_REFERENCE.md](QUICK_START_REFERENCE.md).

## API Quick Reference

### Endpoints

```
GET  /api/users              → List all users with roles
GET  /api/users/:id/roles    → Get roles for specific user
PUT  /api/users/:id/roles    → Update roles for user
```

### Example Requests

**Get all users:**
```bash
curl http://localhost:8080/api/users
```

**Response:**
```json
[
  {
    "id": 1,
    "name": "John Doe",
    "email": "john@tdf.com",
    "phone": "+593987654321",
    "roles": ["Admin", "Manager"],
    "createdAt": "2025-01-01T00:00:00Z"
  },
  {
    "id": 2,
    "name": "Jane Smith",
    "email": "jane@tdf.com",
    "phone": null,
    "roles": ["Teacher", "Artist"],
    "createdAt": "2025-01-15T00:00:00Z"
  }
]
```

**Get user roles:**
```bash
curl http://localhost:8080/api/users/1/roles
```

**Response:**
```json
["Admin", "Manager"]
```

**Update user roles:**
```bash
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Teacher", "Artist", "Student"]}'
```

**Response:** `200 OK`

## Database Schema

### Tables

**party** (existing table, role column removed):
```sql
CREATE TABLE party (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE,
    phone TEXT,
    instagram TEXT,
    whatsapp TEXT,
    tax_id TEXT,
    emergency_contact TEXT,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);
```

**party_role_assignment** (new join table):
```sql
CREATE TABLE party_role_assignment (
    id SERIAL PRIMARY KEY,
    party_id INTEGER NOT NULL REFERENCES party(id) ON DELETE CASCADE,
    role VARCHAR(50) NOT NULL,
    assigned_at TIMESTAMP NOT NULL DEFAULT NOW(),
    assigned_by INTEGER REFERENCES party(id) ON DELETE SET NULL,
    CONSTRAINT unique_party_role UNIQUE (party_id, role)
);

CREATE INDEX idx_party_role_assignment_party_id ON party_role_assignment(party_id);
CREATE INDEX idx_party_role_assignment_role ON party_role_assignment(role);
```

## Role Definitions

| Role        | Description                          | Color (UI) |
|-------------|--------------------------------------|------------|
| Admin       | Full system access                   | Red        |
| Manager     | Management functions                 | Blue       |
| Engineer    | Audio engineering tasks              | Light Blue |
| Teacher     | Teaching and lesson management       | Green      |
| Reception   | Front desk operations                | Purple     |
| Accounting  | Financial operations                 | Orange     |
| Artist      | Artist-specific features             | Blue       |
| Student     | Student-specific features            | Gray       |
| ReadOnly    | View-only access                     | Gray       |

## Common Scenarios

### Assign Multiple Roles to New User

1. Create user in database (or via API)
2. Call `PUT /api/users/{id}/roles` with desired roles:
   ```json
   {"roles": ["Teacher", "Artist"]}
   ```

### Change User Roles

1. Get current roles: `GET /api/users/{id}/roles`
2. Update with new set: `PUT /api/users/{id}/roles`
   ```json
   {"roles": ["Manager", "Reception"]}
   ```

### Remove All Roles from User

```bash
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": []}'
```

### Check if User Has Specific Role

```typescript
const roles = await apiClient.getUserRoles(userId);
const isAdmin = roles.includes('Admin');
```

## Frontend Component Usage

### Basic Usage

```tsx
import UserRoleManagement from './components/UserRoleManagement';

function App() {
  return (
    <div>
      <h1>User Management</h1>
      <UserRoleManagement />
    </div>
  );
}
```

### Custom API Client

```typescript
import { apiClient } from './api/generated/client';

// Get all users
const users = await apiClient.getUsers();

// Update roles
await apiClient.updateUserRoles(userId, ['Teacher', 'Artist']);
```

## Database Queries

### Get users with their roles

```sql
SELECT 
    p.id,
    p.name,
    p.email,
    ARRAY_AGG(pra.role) as roles
FROM party p
LEFT JOIN party_role_assignment pra ON p.id = pra.party_id
GROUP BY p.id, p.name, p.email;
```

### Get all users with a specific role

```sql
SELECT DISTINCT p.*
FROM party p
JOIN party_role_assignment pra ON p.id = pra.party_id
WHERE pra.role = 'Teacher';
```

### Count users by role

```sql
SELECT 
    role,
    COUNT(DISTINCT party_id) as user_count
FROM party_role_assignment
GROUP BY role
ORDER BY user_count DESC;
```

### Get role assignment history

```sql
SELECT 
    p.name,
    pra.role,
    pra.assigned_at,
    assigner.name as assigned_by
FROM party_role_assignment pra
JOIN party p ON pra.party_id = p.id
LEFT JOIN party assigner ON pra.assigned_by = assigner.id
ORDER BY pra.assigned_at DESC;
```

## Environment Setup

### Backend (.env)
```env
DB_HOST=127.0.0.1
DB_PORT=5432
DB_USER=postgres
DB_PASS=postgres
DB_NAME=tdf_hq
APP_PORT=8080
```

### Frontend (.env)
```env
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

## Troubleshooting

### "Unique constraint violation"
- Trying to assign the same role twice to a user
- Solution: Role assignments are idempotent - role already exists

### "Foreign key constraint violation"
- Trying to assign role to non-existent user
- Solution: Verify user ID exists in party table

### "Cannot connect to API"
- Backend not running
- Solution: `cd tdf-hq && stack run`

### "CORS error in browser"
- Frontend and backend on different ports
- Solution: Vite proxy configured in `vite.config.ts`

## Migration Rollback

If you need to rollback the migration:

```sql
BEGIN;

-- Restore single role column
ALTER TABLE party ADD COLUMN role VARCHAR(50);

-- Copy first role from assignments
UPDATE party p SET role = (
    SELECT pra.role 
    FROM party_role_assignment pra 
    WHERE pra.party_id = p.id 
    LIMIT 1
);

-- Drop join table
DROP TABLE party_role_assignment;

COMMIT;
```

## Performance Tips

1. **Use indexes**: Already created on party_id and role
2. **Batch updates**: Update multiple users in a transaction
3. **Cache roles**: Cache user roles in frontend for faster access
4. **Limit queries**: Use pagination for large user lists

## Security Best Practices

1. Add authentication before deployment
2. Validate role names against enum
3. Log all role changes for audit
4. Restrict role updates to admins only
5. Use HTTPS in production

## Next Steps

1. Add JWT authentication
2. Implement role-based permissions
3. Add activity logging
4. Create admin dashboard
5. Deploy to production
