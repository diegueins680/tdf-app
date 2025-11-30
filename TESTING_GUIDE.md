# Testing Guide - Multi-Role User Management

This guide provides manual and automated testing procedures for the multi-role user management system.

## Prerequisites

- PostgreSQL database running with `tdf_hq` database
- Backend server running on port 8080
- Frontend dev server running on port 5173 (for UI testing)

## Frontend QA: Signup with Extended Roles

- Open `/login`, click **Crear cuenta**, select new roles (Fan, Artista, Promotor, Manager, A&R, Producer, etc.) and submit.
- When selecting **Fan**, pick at least one artist/band in the autocomplete; the signup request should include `fanArtistIds` plus `roles`, and follows should be created after login.
- Verify the created session includes the selected roles when the API response omits them (fallback to selected roles).
- Reopen **Roles y permisos** admin screen and confirm the new roles appear on the dropdown; assign a couple of extended roles and save.

## Database Setup for Testing

### Create Test Database

```bash
# Create database
createdb tdf_hq_test

# Run migration
psql -d tdf_hq_test -f tdf-hq/sql/001_multi_role_migration.sql
```

### Insert Test Data

```sql
-- Insert test users
INSERT INTO party (id, name, email, phone, created_at, updated_at)
VALUES 
  (1, 'Alice Admin', 'alice@tdf.com', '+593987654321', NOW(), NOW()),
  (2, 'Bob Teacher', 'bob@tdf.com', '+593987654322', NOW(), NOW()),
  (3, 'Carol Student', 'carol@tdf.com', NULL, NOW(), NOW()),
  (4, 'Dave Manager', 'dave@tdf.com', '+593987654323', NOW(), NOW());

-- Assign roles
INSERT INTO party_role_assignment (party_id, role, assigned_at)
VALUES
  (1, 'Admin', NOW()),
  (1, 'Manager', NOW()),
  (2, 'Teacher', NOW()),
  (2, 'Artist', NOW()),
  (3, 'Student', NOW()),
  (4, 'Manager', NOW()),
  (4, 'Reception', NOW());
```

## Backend API Testing

### Test 1: Get All Users

**Request:**
```bash
curl -X GET http://localhost:8080/api/users
```

**Expected Response:**
```json
[
  {
    "id": 1,
    "name": "Alice Admin",
    "email": "alice@tdf.com",
    "phone": "+593987654321",
    "roles": ["Admin", "Manager"],
    "createdAt": "2025-11-05T08:00:00Z"
  },
  {
    "id": 2,
    "name": "Bob Teacher",
    "email": "bob@tdf.com",
    "phone": "+593987654322",
    "roles": ["Teacher", "Artist"],
    "createdAt": "2025-11-05T08:00:00Z"
  },
  ...
]
```

**Verify:**
- [ ] Status code is 200
- [ ] Response is valid JSON array
- [ ] Each user has `roles` as an array
- [ ] All expected users are returned

### Test 2: Get Roles for Specific User

**Request:**
```bash
curl -X GET http://localhost:8080/api/users/1/roles
```

**Expected Response:**
```json
["Admin", "Manager"]
```

**Verify:**
- [ ] Status code is 200
- [ ] Response is array of role strings
- [ ] Roles match database records

### Test 3: Update User Roles (Add Roles)

**Request:**
```bash
curl -X PUT http://localhost:8080/api/users/3/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Student", "Artist"]}'
```

**Expected Response:**
- Status: 200 OK
- Body: Empty or success message

**Verify in Database:**
```sql
SELECT role FROM party_role_assignment WHERE party_id = 3;
```

Expected result:
```
   role   
----------
 Student
 Artist
```

**Verify:**
- [ ] Status code is 200
- [ ] Database has exactly the specified roles
- [ ] Old roles are removed if not in new list

### Test 4: Update User Roles (Remove All Roles)

**Request:**
```bash
curl -X PUT http://localhost:8080/api/users/3/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": []}'
```

**Expected Response:**
- Status: 200 OK

**Verify in Database:**
```sql
SELECT COUNT(*) FROM party_role_assignment WHERE party_id = 3;
```

Expected result: `0`

**Verify:**
- [ ] Status code is 200
- [ ] All roles removed from database

### Test 5: Update User Roles (Replace Roles)

**Request:**
```bash
curl -X PUT http://localhost:8080/api/users/2/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Teacher", "Engineer", "Reception"]}'
```

**Expected Response:**
- Status: 200 OK

**Verify:**
```bash
curl -X GET http://localhost:8080/api/users/2/roles
```

Expected: `["Teacher", "Engineer", "Reception"]`

**Verify:**
- [ ] Old roles removed
- [ ] New roles added
- [ ] No duplicate roles

### Test 6: Error Handling - Invalid User ID

**Request:**
```bash
curl -X GET http://localhost:8080/api/users/99999/roles
```

**Expected Response:**
- Status: 404 Not Found OR empty array `[]`

**Verify:**
- [ ] Appropriate error response
- [ ] No server crash

### Test 7: Error Handling - Invalid Role Name

**Request:**
```bash
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["InvalidRole"]}'
```

**Expected Response:**
- Status: 400 Bad Request OR server validates and rejects

**Verify:**
- [ ] Invalid roles rejected
- [ ] Database unchanged

### Test 8: Duplicate Role Assignment

**Request:**
```bash
curl -X PUT http://localhost:8080/api/users/1/roles \
  -H "Content-Type: application/json" \
  -d '{"roles": ["Admin", "Admin", "Manager"]}'
```

**Expected Behavior:**
- Deduplication happens (only one "Admin" role saved)
- OR error returned

**Verify in Database:**
```sql
SELECT role, COUNT(*) FROM party_role_assignment 
WHERE party_id = 1 
GROUP BY role 
HAVING COUNT(*) > 1;
```

Expected: No results (unique constraint enforced)

## Frontend UI Testing

### Test 9: Load User List

**Steps:**
1. Open browser to `http://localhost:5173`
2. Wait for page to load

**Verify:**
- [ ] Loading spinner appears initially
- [ ] User table displays after loading
- [ ] All users shown with their roles
- [ ] Roles displayed as colored chips
- [ ] Table has columns: ID, Name, Email, Phone, Roles, Actions

### Test 10: Display Role Chips

**Steps:**
1. View the user table
2. Observe role chips for each user

**Verify:**
- [ ] Each role displays as a separate chip
- [ ] Colors are correct:
  - Admin: Red
  - Manager: Blue
  - Engineer: Light blue
  - Teacher: Green
  - Reception: Purple
  - Accounting: Orange
  - Artist: Blue
  - Student: Gray
  - ReadOnly: Gray
- [ ] Multiple chips wrap properly on small screens

### Test 11: Open Edit Dialog

**Steps:**
1. Click the edit icon (pencil) for a user
2. Dialog should open

**Verify:**
- [ ] Dialog opens
- [ ] Dialog title shows user's name
- [ ] Multi-select dropdown shows current roles selected
- [ ] Current roles display as chips in the select box
- [ ] All available roles appear in dropdown menu

### Test 12: Select Single Role

**Steps:**
1. Open edit dialog for a user with no roles
2. Click on role dropdown
3. Select one role (e.g., "Teacher")
4. Click "Save"

**Verify:**
- [ ] Role appears as chip in dropdown
- [ ] Save button is enabled
- [ ] After save, dialog closes
- [ ] Table updates with new role
- [ ] No page reload occurs

### Test 13: Select Multiple Roles

**Steps:**
1. Open edit dialog
2. Select multiple roles (e.g., "Teacher", "Artist", "Manager")
3. Click "Save"

**Verify:**
- [ ] All selected roles appear as chips
- [ ] Chips are color-coded correctly
- [ ] Save completes successfully
- [ ] Table shows all selected roles

### Test 14: Remove Roles

**Steps:**
1. Open edit dialog for user with multiple roles
2. Deselect one or more roles
3. Click "Save"

**Verify:**
- [ ] Deselected roles removed from chips
- [ ] Save completes successfully
- [ ] Table updates to show remaining roles only

### Test 15: Remove All Roles

**Steps:**
1. Open edit dialog
2. Deselect all roles (clear selection)
3. Click "Save"

**Verify:**
- [ ] No chips shown in dropdown
- [ ] Save completes
- [ ] User row shows "No roles" chip or empty roles cell

### Test 16: Cancel Edit

**Steps:**
1. Open edit dialog
2. Change role selection
3. Click "Cancel"

**Verify:**
- [ ] Dialog closes
- [ ] Changes are NOT saved
- [ ] Table shows original roles

### Test 17: Error Handling - API Failure

**Steps:**
1. Stop the backend server
2. Try to edit roles
3. Click "Save"

**Verify:**
- [ ] Error message displays
- [ ] User can dismiss error
- [ ] Dialog remains open (or closes gracefully)
- [ ] No JavaScript console errors

### Test 18: Loading State

**Steps:**
1. Open edit dialog
2. Select roles
3. Click "Save"
4. Observe save button during API call

**Verify:**
- [ ] Save button shows "Saving..." text
- [ ] Save button is disabled during save
- [ ] Cancel button is disabled during save
- [ ] Dialog cannot be closed during save

### Test 19: Real-time Update

**Steps:**
1. Open two browser tabs to the same page
2. Edit a user's roles in tab 1
3. Refresh tab 2

**Verify:**
- [ ] Changes visible after refresh
- [ ] Data consistency across tabs

### Test 20: Responsive Design

**Steps:**
1. Resize browser window to mobile size (< 600px)
2. Open edit dialog
3. Select roles

**Verify:**
- [ ] Table is scrollable horizontally
- [ ] Dialog is responsive
- [ ] Buttons are accessible
- [ ] Role chips wrap properly

## Database Integrity Testing

### Test 21: Unique Constraint

**SQL:**
```sql
-- This should fail with unique constraint violation
INSERT INTO party_role_assignment (party_id, role, assigned_at)
VALUES (1, 'Admin', NOW());

-- If user 1 already has Admin role
```

**Verify:**
- [ ] Constraint prevents duplicate (party_id, role)
- [ ] Error message is clear

### Test 22: Foreign Key Cascade

**SQL:**
```sql
-- Delete a user
DELETE FROM party WHERE id = 3;

-- Check roles are also deleted
SELECT * FROM party_role_assignment WHERE party_id = 3;
```

**Verify:**
- [ ] Deleting user cascades to role assignments
- [ ] No orphaned role assignments remain

### Test 23: Index Performance

**SQL:**
```sql
-- Explain query to verify index usage
EXPLAIN ANALYZE
SELECT * FROM party_role_assignment WHERE party_id = 1;
```

**Verify:**
- [ ] Index on party_id is used
- [ ] Query is fast (< 1ms for small dataset)

### Test 24: Role Enumeration

**SQL:**
```sql
-- Try to insert invalid role
INSERT INTO party_role_assignment (party_id, role, assigned_at)
VALUES (1, 'SuperAdmin', NOW());
```

**Verify:**
- [ ] Backend validation rejects invalid roles
- [ ] OR database constraint prevents it

## Performance Testing

### Test 25: Load Time - Many Users

**Setup:**
Insert 100 test users with various roles

**Steps:**
1. Open frontend
2. Measure time to load user list

**Verify:**
- [ ] Page loads in < 2 seconds
- [ ] Table renders smoothly
- [ ] No lag when scrolling

### Test 26: Update Time - Multiple Roles

**Steps:**
1. Update user with 5+ roles
2. Measure time from click to UI update

**Verify:**
- [ ] Update completes in < 1 second
- [ ] No blocking UI during update

## Regression Testing Checklist

After any code changes, verify:

- [ ] All backend endpoints still work
- [ ] Frontend loads without errors
- [ ] Edit dialog opens and closes
- [ ] Role selection works
- [ ] Save updates database
- [ ] Cancel discards changes
- [ ] Error handling still functional
- [ ] Type safety maintained (no TypeScript errors)
- [ ] Database constraints enforced

## Automated Testing (Future)

### Backend Tests (Haskell - Hspec)

```haskell
-- Example test structure
describe "User Role Management" $ do
  it "should get all users with roles" $ do
    users <- runDB getUsersWithRoles
    length users `shouldBe` 4
    
  it "should update user roles" $ do
    let userId = toSqlKey 1
    runDB $ updateUserRoles userId [Teacher, Artist] Nothing
    roles <- runDB $ getUserRoles userId
    roles `shouldBe` [Teacher, Artist]
```

### Frontend Tests (TypeScript - Vitest + React Testing Library)

```typescript
// Example test structure
describe('UserRoleManagement', () => {
  it('should render user table', async () => {
    render(<UserRoleManagement />);
    expect(screen.getByText('Alice Admin')).toBeInTheDocument();
  });
  
  it('should open edit dialog on click', async () => {
    render(<UserRoleManagement />);
    const editButton = screen.getAllByLabelText('edit roles')[0];
    fireEvent.click(editButton);
    expect(screen.getByText(/Edit Roles for/)).toBeInTheDocument();
  });
});
```

## Test Coverage Goals

- Backend API: 80%+
- Frontend Components: 70%+
- Database Layer: 90%+
- Integration Tests: Key user flows covered

## Reporting Issues

When reporting test failures, include:

1. Test number and name
2. Expected vs actual behavior
3. Error messages (if any)
4. Browser console logs (for UI tests)
5. Database state (for backend tests)
6. Steps to reproduce

## Sign-off Checklist

Before considering testing complete:

- [ ] All 26 manual tests pass
- [ ] Database integrity verified
- [ ] Error handling tested
- [ ] Performance acceptable
- [ ] Responsive design works
- [ ] Documentation reviewed
- [ ] No console errors
- [ ] No TypeScript errors
- [ ] No SQL errors

## Next Steps

1. Implement automated tests
2. Setup CI/CD pipeline
3. Add load testing
4. Setup monitoring/logging
5. Plan production deployment
