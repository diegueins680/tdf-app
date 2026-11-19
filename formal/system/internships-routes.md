# `/internships` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/ServerInternships.hs`, `ServerInternAudit.hs`, `API/Internships.hs` and `API/InternAudit.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- All 21 routes are behind `AuthProtect "bearer-token"`.
- `ensureAdmin`: Admin, Manager or StudioManager role. `ensureInternAccess`: an admin role or Intern. Both check roles only: no `ModuleInternships`, strict-admin or coherent-grant check, although `ServerFeedback` requires the module for the same operators (PRIV-INTAKE-001).
- `ensurePlanAccess`: an admin, or the intern assigned to the plan's active task (404 otherwise). `ensureActivePlanMutation`: 409 once a plan is completed or cancelled.
- Audit-trail rows and notifications are written in separate transactions after the effect; nothing here sends email.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| profile, todos, clock-in/out, permission requests | OPS-INTERN-SELF-001 | new |
| projects, tasks, permission decisions | OPS-INTERN-ADMIN-001 | new |
| audit plans, cases, executions, daily and final summaries | OPS-INTERN-AUDIT-001 | new (critical) |

No existing requirement specified any of these routes. `scripts/test-studio-internship-audit-api-e2e.mjs` drives the audit workflow end to end (plan races, activation, completion, intern boundaries, execution concurrency); profile, todos, time entries and permissions have no automated coverage.

## Open behaviours for the owner to decide

**Authorization**
- Admin guards are role-only, so an operator whose internships module was revoked keeps full power over projects, tasks and audit plans.
- Permission decisions have no state machine, no lock and no distinct-reviewer rule: an administrator can decide their own request and move it between any states.
- Task creation does not check that the assignee exists or is an intern (plan creation does).

**Concurrency**
- Concurrent clock-ins can leave two open entries, after which both clock-in and clock-out answer 409 until fixed by hand.
- Two concurrent first profile writes race on the unique profile row.
- A plan's completion criteria are evaluated before the plan row lock.

**Workflow semantics**
- Any non-submit save of the final summary clears its submission and approval; every submit re-notifies the whole administrator team.
- Daily summaries have no uniqueness per work date.
