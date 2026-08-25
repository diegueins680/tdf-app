# Multi-Role User Management - Architecture Diagram

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Web Browser                             │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │              React Application (Port 5173)                │  │
│  │                                                           │  │
│  │  ┌─────────────────────────────────────────────────────┐ │  │
│  │  │           UserRoleManagement Component              │ │  │
│  │  │                                                     │ │  │
│  │  │  • User table with role chips                      │ │  │
│  │  │  • Edit dialog with multi-select                   │ │  │
│  │  │  • Color-coded role display                        │ │  │
│  │  │  • Real-time updates                               │ │  │
│  │  └─────────────────────────────────────────────────────┘ │  │
│  │                          │                                │  │
│  │                          ▼                                │  │
│  │  ┌─────────────────────────────────────────────────────┐ │  │
│  │  │              API Client (TypeScript)                │ │  │
│  │  │                                                     │ │  │
│  │  │  • getUsers()                                      │ │  │
│  │  │  • getUserRoles(id)                                │ │  │
│  │  │  • updateUserRoles(id, roles[])                    │ │  │
│  │  └─────────────────────────────────────────────────────┘ │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ HTTP/JSON
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   Haskell Backend (Port 8080)                   │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    Servant API Layer                      │  │
│  │                                                           │  │
│  │  GET  /api/users              → [UserDTO]               │  │
│  │  GET  /api/users/:id/roles    → [PartyRole]             │  │
│  │  PUT  /api/users/:id/roles    → ()                      │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    Server Handlers                        │  │
│  │                                                           │  │
│  │  • getUsers            → Query users + join roles        │  │
│  │  • getUserRoles'       → Query roles for user            │  │
│  │  • updateUserRoles'    → Delete + Insert roles           │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              ▼                                  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                  Database Access Layer                    │  │
│  │                                                           │  │
│  │  • getUsersWithRoles()                                   │  │
│  │  • getUserRoles(partyId)                                 │  │
│  │  • updateUserRoles(partyId, roles[], assignedBy)        │  │
│  │  • addUserRole(partyId, role, assignedBy)               │  │
│  │  • removeUserRole(partyId, role)                         │  │
│  │  • userHasRole(partyId, role)                            │  │
│  └───────────────────────────────────────────────────────────┘  │
│                              │                                  │
│                              │ Persistent ORM                   │
│                              │                                  │
└──────────────────────────────┼──────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   PostgreSQL Database                           │
│                                                                 │
│  ┌──────────────────┐          ┌─────────────────────────────┐ │
│  │      party       │          │  party_role_assignment      │ │
│  ├──────────────────┤          ├─────────────────────────────┤ │
│  │ id (PK)          │◄─────────┤ party_id (FK)              │ │
│  │ name             │          │ role                        │ │
│  │ email            │          │ assigned_at                 │ │
│  │ phone            │          │ assigned_by (FK)            │ │
│  │ instagram        │          │                             │ │
│  │ whatsapp         │          │ UNIQUE(party_id, role)      │ │
│  │ tax_id           │          └─────────────────────────────┘ │
│  │ emergency_contact│                                          │
│  │ created_at       │                                          │
│  │ updated_at       │                                          │
│  └──────────────────┘                                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Data Flow: Updating User Roles

```
User clicks "Edit" on user row
        │
        ▼
Dialog opens with current roles
        │
        ▼
User selects/deselects roles
        │
        ▼
User clicks "Save"
        │
        ▼
React Component
    │
    └─► apiClient.updateUserRoles(userId, selectedRoles)
            │
            ▼
    HTTP PUT /api/users/{userId}/roles
    Body: {"roles": ["Teacher", "Artist"]}
            │
            ▼
    Servant API receives request
            │
            ▼
    Server.updateUserRoles' handler
            │
            ▼
    DB.updateUserRoles(partyId, roles, assignedBy)
            │
            ├─► DELETE FROM party_role_assignment 
            │   WHERE party_id = ?
            │
            └─► INSERT INTO party_role_assignment 
                (party_id, role, assigned_at, assigned_by)
                VALUES (?, ?, NOW(), ?)
                FOR EACH role in roles[]
            │
            ▼
    PostgreSQL commits transaction
            │
            ▼
    Server responds 200 OK
            │
            ▼
    React updates local state
            │
            ▼
    Table re-renders with new role chips
            │
            ▼
    Dialog closes
```

## Component Hierarchy

```
App
 │
 ├─► ThemeProvider (MUI)
 │    │
 │    └─► CssBaseline
 │
 └─► Container
      │
      └─► UserRoleManagement
           │
           ├─► TableContainer
           │    │
           │    └─► Table
           │         │
           │         ├─► TableHead
           │         │
           │         └─► TableBody
           │              │
           │              └─► TableRow (for each user)
           │                   │
           │                   ├─► TableCell (ID)
           │                   ├─► TableCell (Name)
           │                   ├─► TableCell (Email)
           │                   ├─► TableCell (Phone)
           │                   ├─► TableCell (Roles)
           │                   │    │
           │                   │    └─► Chip (for each role)
           │                   │
           │                   └─► TableCell (Actions)
           │                        │
           │                        └─► IconButton (Edit)
           │
           └─► Dialog (Edit Roles)
                │
                ├─► DialogTitle
                │
                ├─► DialogContent
                │    │
                │    └─► FormControl
                │         │
                │         └─► Select (multiple)
                │              │
                │              └─► MenuItem (for each available role)
                │
                └─► DialogActions
                     │
                     ├─► Button (Cancel)
                     └─► Button (Save)
```

## Database Relationships

```
party (1) ────< (many) party_role_assignment
  │                         │
  │                         └─ role: PartyRole enum
  │                         └─ assigned_by ──┐
  │                                          │
  └──────────────────────────────────────────┘
           (self-referential FK)
```

## Role Assignment Flow

```
Before (Single Role):
┌─────────┬─────────┬──────────┐
│ user_id │  name   │   role   │
├─────────┼─────────┼──────────┤
│    1    │  John   │  Admin   │
│    2    │  Jane   │ Teacher  │
└─────────┴─────────┴──────────┘

After (Multiple Roles):
party table:
┌─────────┬─────────┐
│ user_id │  name   │
├─────────┼─────────┤
│    1    │  John   │
│    2    │  Jane   │
└─────────┴─────────┘

party_role_assignment table:
┌─────────┬──────────┬──────────────┐
│party_id │   role   │ assigned_at  │
├─────────┼──────────┼──────────────┤
│    1    │  Admin   │ 2025-01-01   │
│    1    │ Manager  │ 2025-01-01   │
│    2    │ Teacher  │ 2025-01-15   │
│    2    │  Artist  │ 2025-01-15   │
└─────────┴──────────┴──────────────┘

Result when querying users:
┌─────────┬─────────┬────────────────────┐
│ user_id │  name   │       roles        │
├─────────┼─────────┼────────────────────┤
│    1    │  John   │ [Admin, Manager]   │
│    2    │  Jane   │ [Teacher, Artist]  │
└─────────┴─────────┴────────────────────┘
```

## API Request/Response Examples

### Get All Users

```
Request:
  GET /api/users

Response: 200 OK
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

### Get User Roles

```
Request:
  GET /api/users/1/roles

Response: 200 OK
["Admin", "Manager"]
```

### Update User Roles

```
Request:
  PUT /api/users/1/roles
  Content-Type: application/json
  
  {
    "roles": ["Teacher", "Artist", "Student"]
  }

Response: 200 OK
(empty body)
```

## Type Definitions

### Haskell (Backend)

```haskell
-- PartyRole enum
data PartyRole = Admin | Manager | Engineer | Teacher 
               | Reception | Accounting | Artist 
               | Student | ReadOnly

-- Database entity
PartyRoleAssignment
    partyId PartyId
    role PartyRole
    assignedAt UTCTime
    assignedBy PartyId Maybe
    UniquePartyRole partyId role

-- DTO
data UserDTO = UserDTO
    { userId :: Int
    , userName :: Text
    , userEmail :: Maybe Text
    , userPhone :: Maybe Text
    , userRoles :: [PartyRole]
    , userCreatedAt :: UTCTime
    }
```

### TypeScript (Frontend)

```typescript
// Role enum
type PartyRole = 
  | 'Admin' | 'Manager' | 'Engineer' | 'Teacher'
  | 'Reception' | 'Accounting' | 'Artist'
  | 'Student' | 'ReadOnly';

// User interface
interface User {
  id: number;
  name: string;
  email?: string | null;
  phone?: string | null;
  roles: PartyRole[];
  createdAt: string;
}

// Update DTO
interface UserRoleUpdate {
  roles: PartyRole[];
}
```

## Security & Authorization (Future)

```
┌──────────────────────────────────────────┐
│           Authentication Layer           │
│                                          │
│  JWT Token → Verify → Extract User ID   │
└──────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────┐
│          Authorization Layer             │
│                                          │
│  Check if user.roles includes "Admin"   │
│  or has permission to manage roles       │
└──────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────┐
│            Business Logic                │
│                                          │
│  Update roles in database                │
└──────────────────────────────────────────┘
```
