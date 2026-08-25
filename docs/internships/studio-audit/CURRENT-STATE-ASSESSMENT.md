# Current-state assessment

## Baseline and inspection boundary

The assessment uses `origin/main` commit `dac84b099b18b51032fb94f58273120f5375eb85` in an isolated detached worktree. The primary checkout was already dirty and has not been modified. Inspection covered the backend (`tdf-hq`), web client (`tdf-hq-ui`), initialized mobile submodule (`tdf-mobile` at `cff51d…`), authenticated and public routes, database models and SQL migrations, feature registry, generated clients, role/module checks, tests and personas, feedback, internships, audit logs, notifications, CI/deployment files, feature-discoverability reports, and read-only GitHub searches.

## Existing capabilities verified

- Internships already provide projects, tasks, assignees, due dates, manual status/progress, personal todos, time entries, and permission requests.
- The existing public `/feedback` endpoint accepts the catalog category/severity, title, description, consent/contact fields, and one optional attachment. It persists a `Feedback` row and uses the existing email notification path.
- Authentication, canonical party roles, module authorization, audit logs, in-app notifications, catalog data, and provider integrations already exist.
- Studio operation is distributed across CRM, calendar/resources, bookings, sessions, commerce, payments, inventory, reports, Live Sessions, Domo, school, DDEX, notifications, public surfaces, web, and mobile.
- The main-branch generated feature audit contained 137 feature definitions, 156 web routes, 548 backend endpoints, and 3,014 evidence rows. The canonical assignment inventory narrows and expands these into 130 auditable studio-related feature areas.

## Gaps found on the baseline

- Internship projects/tasks have no unambiguous draft/activation state; creation can represent active work immediately.
- Test cases and executions are not normalized, execution history is absent, and task progress is manually editable.
- Critical-case, evidence, report-link, blocker, daily-summary, final-summary, and administrator-approval completion gates are absent.
- Public feedback is intentionally simple but cannot provide authenticated private report ownership, drafts, structured reproduction data, internship/test traceability, multiple evidence items, triage, comments, duplicate links, retests, audit history, exports, or an admin dashboard.
- An intern can propose a task progress value; there is no calculated progress source of truth.
- Public feedback email behavior is unsuitable as the transport for staging audit tests.
- Mobile support is uneven and frequently relies on responsive web fallbacks. The generated inventory records the platform classification for every item.

## Identity finding and runtime gate

Repository-wide and literal production searches found no “Stuart” identity. A bounded read-only production inspection of accounts evidenced by an internship profile, active Intern role, internship assignment, permission request, or time entry identified one contextually exact person: the active account is recorded as **Stewart Moreira**. Verification included his active credential, current Intern profile, recent time activity, studio-oriented skills/areas, and the existing project `Plan de prácticas - Stewart Moreira (#129)`. Therefore:

- the spelling difference is explicit and the identity is not guessed;
- exact party ID and email were verified but are retained only for approved runtime matching, not source control;
- no duplicate account has been created;
- no real draft assignment has been associated with a person;
- no task has been activated and no notification has been sent.

Immediately before draft creation, the preparation script must revalidate those exact runtime-only identifiers to exactly one active account with the `Intern` role. The production account also has existing `Reception`, `Customer`, and `Fan` grants; the implementation did not alter them, and an administrator must confirm the independently justified least-privilege baseline before activation.

## Scope result

The generated inventory contains 130 entries: 125 executable/applicable, four explicit safety exclusions, and one studio-related DDEX import capability classified as documented but not implemented and therefore not executable. Each entry records the scope class, implementation/access/documentation/platform classifications, intended role, route, criticality, and repository evidence. The canonical decision matrix is `studio-feature-inventory.csv`; the JSON form is authoritative for automation.

## GitHub finding and authorized publication

Read-only exact-title searches found no existing matching issue, pull request, or branch immediately before mutation. Diego authorized branch creation, staging, commit, and push on 2026-08-23. The original root feature commit is `d68b794e531501589e18c24dbb60aa00ada0a0f7`; its original mobile generated-client commit was `dac57203f2a55d6f38ecf32953e7449747d25f58`. The completed mobile integration includes PR #30 and the required signup-consent contract follow-up PR #31 at main commit `0c9aeb2d594a46109282f1a82afb7a33a043be80`, which the final root integration references. Draft-PR creation, isolated staging deployment, and creation of an inactive synthetic staging draft were subsequently authorized and exercised. Production deployment, issue creation, activation, assignment to Stewart, and real notification remain explicit unexercised gates.

Relevant history reviewed includes merged work for public feedback (#138), internship payloads (#143), checkout and Domo booking flows (#149 and #150), persona-based quality work (#195), and packages/invoicing/inventory/analytics (#1), plus the open discoverability issue #128. These items informed the inventory but do not duplicate this project.
