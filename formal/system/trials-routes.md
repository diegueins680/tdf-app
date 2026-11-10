# `/trials` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Trials/Server.hs`, `Trials/API.hs`, `Trials/DTO.hs` and `Trials/Models.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Mounting and guards**
- `TrialsAPI = "trials" :> "v1" :> (PublicTrialsAPI :<|> AuthProtect "bearer-token" :> PrivateTrialsAPI)`. Signup, interests and trial requests are public (registered in AUTH-PUBLIC-001 as `public-intake`); everything else is authenticated.
- `ensureSchoolAccess`: ModuleScheduling plus Teacher or school staff (Admin, Manager, StudioManager, Reception). `ensureSchoolStaffAccess` adds the staff role. `ensureTeacherOrStaff`: self or staff. Subject administration needs ModuleAdmin (Admin, StudioManager).
- `withTrialIdentityRequest`: Idempotency-Key (16..128 chars), advisory lock on (scope, key), replay of an identical payload, 409 on a different one, receipt in the same transaction. Used by signup, trial requests and student creation only.
- No handler sends email, WhatsApp or calendar invitations; `TrialThrottle` is unused.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| signup, interests, trial-requests | SCHOOL-PUBLIC-INTAKE-001 | new |
| trial-requests assign, trial-assignments, trial-availability create/delete | SCHOOL-TRIAL-SCHEDULING-001 | new |
| subjects create/update/delete | SCHOOL-SUBJECT-ADMIN-001 | new |
| class-sessions create/update/attend, purchases | SCHOOL-CLASS-SESSIONS-001 | new |
| teachers subjects, teacher students add/remove, students create/update | SCHOOL-TEACHER-STUDENT-001 | new |

`COURSE-SUBJECT-001` specifies only the subject GET routes. `TrialIdentitySpec` (PostgreSQL) covers keyed signup, trial requests and student creation; `PublicLeadSpec` covers validators and lead party resolution.

## Defects fixed on 2026-11-07

1. **Staff could edit any party's contact details.** `PATCH /trials/v1/students/{id}` only required school access for staff and then updated the named party, whatever it was: an administrator's email and phone could be changed through the student route. `validateStudentUpdateTarget` now requires the target to be a student (Student role, a teacher link or a class session) for staff (404 otherwise) and an owned student for teachers (403). Tested in `PublicLeadSpec`.
2. **Duplicate subject names answered 500.** Creation and rename ran into the unique index. Creation now uses `insertUnique` and rename checks the name first; both answer 409.

## Open behaviours for the owner to decide

**Role grants**
- Setting a teacher's subjects grants the Teacher security role to whatever party staff name, including Reception acting on an arbitrary party.
- Adding a student grants the Student role to any existing party; removing the link does not revoke it, and a past class session keeps the teacher's edit rights.

**Concurrency**
- Teacher and room conflict checks for scheduling, availability and class sessions are unlocked reads, so double booking is possible.
- The trial-request availability check can race with scheduling; subject renames can still race into the unique index.

**Data and money**
- Package purchases take the price from the client, accept inactive packages and have no idempotency key; attendance does not consume purchased minutes.
- Re-assigning a Scheduled trial regresses it to Assigned while its assignment row remains.
- Public interests are unkeyed and unthrottled and may carry PII in free text; signup copies contact details into the lead text.

**Permissions**
- Manager and Reception can create availability for others but cannot update or delete it.
