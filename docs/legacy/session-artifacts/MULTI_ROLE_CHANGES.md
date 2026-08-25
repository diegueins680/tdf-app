# Multi-Role Support Changes

## Summary

Updated the user role management system to support **multiple roles per user** as requested in PR comment #3420248296.

## Database Changes

### Before (Single Role)
```
Party Table:
- role: PartyRole (single enum value)
```

### After (Multiple Roles)
```
Party Table:
- (role field removed)

PartyRoleAssignment Table (NEW):
- id: Primary Key
- partyId: Foreign Key to Party
- role: PartyRole enum
- createdAt: UTCTime
- Unique constraint: (partyId, role)
```

## API Changes

### Endpoint
- **Before:** `PUT /api/users/{userId}/role`
- **After:** `PUT /api/users/{userId}/roles`

### Request Body
- **Before:** `{ "urrRole": "AdminRole" }`
- **After:** `{ "urrRoles": ["AdminRole", "ManagerRole"] }`

### Response Format
```typescript
// UserWithParty interface
{
  uwpUserId: number
  uwpEmail?: string
  uwpName: string
  uwpRoles: PartyRole['role'][]  // Changed from uwpRole (single) to uwpRoles (array)
  uwpIsActive: boolean
  uwpLastLoginAt?: string
}
```

## Backend Code Changes

### Files Modified
1. **src/TDF/Models.hs**
   - Removed `role` field from Party table
   - Added `PartyRoleAssignment` table definition
   - Unique constraint prevents duplicate role assignments

2. **src/TDF/DTO.hs**
   - `UserWithParty`: Changed `uwpRole` to `uwpRoles: [PartyRole]`
   - Renamed `UpdateRoleRequest` to `UpdateRolesRequest`
   - Updated to use `urrRoles: [PartyRole]`

3. **src/TDF/API.hs**
   - Updated endpoint from `/role` to `/roles`
   - Changed request body type to `UpdateRolesRequest`

4. **src/TDF/DB.hs**
   - `getAllUsersWithParty`: Now queries `PartyRoleAssignment` table to get all roles
   - Renamed `updateUserRole` to `updateUserRoles`
   - Implements atomic role updates: delete all existing roles, insert new ones

5. **src/TDF/Server.hs**
   - Updated handler to use new function names and types

6. **src/TDF/Seed.hs**
   - Added multi-role examples:
     - John: 1 role (Admin)
     - Maria: 2 roles (Manager, Accounting)
     - Carlos: 1 role (Engineer)
     - Ana: 2 roles (Teacher, Artist)
     - Luis: 3 roles (Reception, Student, Customer)

## Frontend Code Changes

### Files Modified
1. **src/api/client.ts**
   - `UserWithParty`: Changed `uwpRole` to `uwpRoles: PartyRole['role'][]`
   - Renamed `UpdateRoleRequest` to `UpdateRolesRequest`
   - Renamed `updateUserRole` to `updateUserRoles`
   - Updated API call from `/role` to `/roles`

2. **src/components/UserRoleManagement.tsx**
   - Replaced single-select dropdown with **multi-select dropdown**
   - Display multiple role chips in "Current Roles" column
   - Multi-select shows selected roles as chips in the dropdown
   - Support for users with zero roles (displays "No roles assigned")
   - Updated column headers: "Current Role" → "Current Roles", "Change Role" → "Manage Roles"

## UI Improvements

### Multi-Select Dropdown Features
- Visual chips for selected roles inside the dropdown
- Click to select/deselect multiple roles
- Checkboxes for each role option
- Color-coded chips matching role types
- Label: "Roles" for clarity

### Display Features
- Multiple role chips displayed horizontally with wrapping
- Each role has color-coding for quick identification
- Empty state message when user has no roles
- Real-time updates when roles change

## Migration Notes

### Breaking Changes
1. API endpoint path changed (singular to plural)
2. Request/response format changed (single value to array)
3. Database schema changed (Party.role field removed)

### Database Migration Steps
1. Create `PartyRoleAssignment` table
2. Migrate existing `Party.role` values to `PartyRoleAssignment` entries
3. Drop `Party.role` column
4. Run seed data to populate examples

### Client Migration
- Update all API calls from `/role` to `/roles`
- Update request bodies to use arrays
- Handle `uwpRoles` array instead of `uwpRole` string

## Testing Recommendations

1. **Backend:**
   - Test user with 0 roles
   - Test user with 1 role
   - Test user with multiple roles
   - Test updating from single to multiple roles
   - Test updating from multiple to single role
   - Test removing all roles (empty array)

2. **Frontend:**
   - Test multi-select dropdown functionality
   - Test visual display of multiple role chips
   - Test API error handling
   - Test optimistic updates with React Query

## Example Usage

### API Request
```bash
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"urrRoles": ["AdminRole", "ManagerRole", "AccountingRole"]}'
```

### API Response
```json
{
  "urrSuccess": true,
  "urrMessage": "Roles updated successfully",
  "urrUser": {
    "uwpUserId": 1,
    "uwpEmail": "admin@tdfrecords.com",
    "uwpName": "John Admin",
    "uwpRoles": ["AdminRole", "ManagerRole", "AccountingRole"],
    "uwpIsActive": true,
    "uwpLastLoginAt": "2025-11-05T16:00:00Z"
  }
}
```

## Commits

- `7f848c4` - feat: Support multiple roles per user with many-to-many relationship
- `bb9f0a3` - feat: Implement user role management system
