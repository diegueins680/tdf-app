# Party-Based Student/Teacher Refactoring

## Problem
The original schema had separate `teachers` and `students` tables with duplicated contact information (name, email, phone), leading to data inconsistencies and maintenance overhead.

## Solution
Refactor to use the existing `party` table as the single source of truth for contact information, with `teachers` and `students` tables serving only as linking/enrollment records.

## Architecture

### Before (Duplicated Data)
```
party (id, name, email, phone)
  └─ party_role (role: Teacher|Student)

teachers (id, name, email, phone)  ❌ Duplicate data
students (id, name, email, phone)  ❌ Duplicate data
enrollments (student_id → students)
lessons (teacher_id → teachers, student_id → students)
```

### After (Normalized)
```
party (id, name, email, phone)  ✅ Single source of truth
  └─ party_role (role: Teacher|Student)
  └─ teachers (id, party_id → party)
  └─ students (id, party_id → party)

enrollments (student_id → students → party)
lessons (teacher_id → teachers → party, student_id → students → party)
```

## Migration Strategy

### Phase 1: Add References (Non-Breaking)
1. Add `party_id` columns to `teachers` and `students` tables
2. Migrate existing records to create party entries
3. Keep old columns for backward compatibility

### Phase 2: Update Application Code
1. Use views (`v_teachers_with_party`, `v_students_with_party`) for transparent access
2. Update queries to join through party table
3. Modify create/update operations to use party records

### Phase 3: Cleanup (Breaking)
1. Make `party_id` NOT NULL
2. Drop redundant `name`, `email`, `phone` columns
3. Update all references to use party data

## Database Migration

Run the migration script:
```sql
-- Located at: tdf-hq/sql/2025-11-05_refactor_students_teachers_to_parties.sql
\i sql/2025-11-05_refactor_students_teachers_to_parties.sql
```

This script:
- ✅ Adds `party_id` columns with foreign key constraints
- ✅ Migrates existing data to create party records
- ✅ Assigns appropriate roles (Teacher/Student)
- ✅ Creates views for backward compatibility
- ✅ Documents cleanup steps for later execution

## Frontend Helper Functions

### Converting Parties to Students/Teachers

```typescript
import { convertPartyToStudent, convertPartyToTeacher } from '@/utils/partyRoleHelpers';

// Convert a party to a student
const result = await convertPartyToStudent(party, {
  createStudentRecord: true,  // Creates student record in lessons system
  notes: 'Enrolled from trial lesson'
});
// Returns: { partyId: number, studentId?: string }

// Convert a party to a teacher
const result = await convertPartyToTeacher(party, {
  createTeacherRecord: true,  // Creates teacher record in lessons system
  notes: 'Hired as guitar instructor'
});
// Returns: { partyId: number, teacherId?: string }
```

### UI Integration

The `PartiesPage` now includes a "Convert to Student" action button (school icon) that:
1. Adds the `Student` role to the party
2. Creates a student record in the lessons system
3. Links the student record to the party via `party_id`
4. Shows confirmation snackbar with the new student ID

## Benefits

### Data Integrity
- ✅ Single source of truth for contact information
- ✅ Updates to party data automatically reflect in student/teacher views
- ✅ No risk of email/phone mismatches

### Simplified Management
- ✅ One place to update contact details
- ✅ Easy role-based access control via `party_role`
- ✅ Unified CRM view of all contacts

### Reduced Storage
- ✅ Eliminates duplicate name/email/phone fields
- ✅ Teachers and students tables become lean linking tables
- ✅ Better database normalization

## Backward Compatibility

### Using Views
The migration creates views that maintain the old interface:

```sql
-- Old query still works
SELECT * FROM v_teachers_with_party WHERE email = 'teacher@example.com';

-- Returns: teacher_id, party_id, name, email, phone, created_at, updated_at
```

### Gradual Migration
- Phase 1: Both old and new columns exist
- Phase 2: Application uses new structure, old columns deprecated
- Phase 3: Old columns removed after verification

## API Changes

### Before
```typescript
// Separate student creation
POST /api/students { name, email, phone }
```

### After
```typescript
// Create party first
POST /parties { displayName, primaryEmail, primaryPhone, cRoles: ['Student'] }

// Then create student record (links to party)
POST /api/students { party_id }
```

## Testing Checklist

- [ ] Run migration script on dev database
- [ ] Verify existing teachers/students have `party_id` populated
- [ ] Test converting a party to student via UI
- [ ] Verify student appears in lessons system
- [ ] Test updating party email and confirm it reflects in student views
- [ ] Check enrollment and lesson creation with new structure
- [ ] Validate role-based access (students see only their data)

## Rollback Plan

If issues occur:
1. Drop `party_id` columns: `ALTER TABLE students DROP COLUMN party_id;`
2. Restore from backup if data corruption occurs
3. Keep old columns intact during Phase 1 & 2 for safety

## Future Enhancements

- [ ] Add `party_id` index for faster lookups
- [ ] Create trigger to auto-create party when student/teacher created
- [ ] Add validation to prevent orphaned student/teacher records
- [ ] Implement soft deletes to preserve historical data
- [ ] Add audit log for party conversions

## Related Files

- **Migration**: `tdf-hq/sql/2025-11-05_refactor_students_teachers_to_parties.sql`
- **Helpers**: `tdf-hq-ui/src/utils/partyRoleHelpers.ts`
- **UI**: `tdf-hq-ui/src/pages/PartiesPage.tsx` (convert button)
- **Views**: `v_teachers_with_party`, `v_students_with_party`
