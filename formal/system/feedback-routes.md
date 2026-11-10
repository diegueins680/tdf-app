# `/feedback` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/ServerFeedback.hs`, `API/Feedback.hs` and `tdf-hq/sql/2026-08-21_studio_internship_audit.sql` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Mounting and guards**
- `POST /feedback` and `POST /feedback/account-deletion` are public (`FeedbackAPI`); the eight `/feedback/internal` routes are behind `AuthProtect "bearer-token"`.
- `ensureInternalAccess`: Admin, Manager, StudioManager or Intern role; `isAdminUser` is role-only. Only the deletion resolution (and the legacy queue) re-validate `ModuleInternships` under session, role and module locks.
- `loadAccessibleReport`: strict UUID (400), existing report (404), reporter or administrator (404, not 403).
- Mutations lock the linked task's audit plan `FOR UPDATE` (finalized → 409, environment mismatch → 400); report writes are conditional on an optimistic version; `audit_log` and team notifications are written after the transaction.
- `feedback_reference_integrity` requires published category and severity; `identity_archive_reference_guard` refuses archived parties; execution numbers are unique per test case. No trigger enforces the report state machine.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| account-deletion, internal account-deletion resolution | PRIV-INTAKE-001 | reviewed |
| feedback | FEEDBACK-PUBLIC-INTAKE-001 (MOB-FEEDBACK-001 covers the form rules) | new |
| internal create, update, submit | QA-REPORT-AUTHORING-001 | new (critical) |
| comments, evidence, evidence-links, retests | QA-REPORT-EVIDENCE-001 | new |

`PRIV-INTAKE-001` holds: the marker needs a live matching session with origin proof, an owner advisory lock and pending-receipt reuse, and resolution is an immutable audited row written by an administrator whose role and module are re-validated under locks (`Spec.hs`, "internal feedback administrator module boundary").

## Open behaviours for the owner to decide

**Authorization**
- Report triage (state, priority, assignee, closure) is gated by role only; an operator whose Internships module was revoked keeps it, while the queue and the deletion resolution require the module. The client capability matrix expects the module for every internal action.
- Any reporter or administrator may record a retest; the task's assigned intern is not required.
- Plain comments, evidence and links may be added to closed or discarded reports.

**Storage**
- Internal evidence is written inside the transaction before its row, so a failed insert leaves an orphan file.
- The production internal upload root (`/data/assets/.internal-feedback`) sits under the public assets directory and is hidden only by the asset server's path exclusion.

**Public intake**
- `POST /feedback` accepts anonymous unbounded submissions with a 10 MB attachment and three synchronous emails each, with no throttle or body limit in the application.
- `public-routes.json` records its boundary as `handler-session`; the handler proceeds anonymously when no session is present, so `public-intake` describes it more precisely.

**Idempotency**
- A `PATCH` that repeats the current state bumps the version, writes history and re-notifies the reporter.
- A supplied `executionId` on a retest is validated but not stored; the new execution is linked instead.
