# Technical design

## Design principles

The implementation extends the existing internships, `Feedback`, catalog, audit, notification, role, and module infrastructure. Public feedback remains wire-compatible. Internal reports are a normalized extension linked one-to-one to the base feedback row, not a second feedback product.

## Data model

- `intern_project` and `intern_task` gain explicit `activation_status`; a draft task stores a `proposed_assignee` but has no active assignee.
- `intern_audit_plan` connects one project and principal task to staging, effort, duration, midpoint, proposed assignee, and exception approval.
- `intern_test_case` contains the full stable test specification and sort order.
- `intern_test_execution` is append-oriented and retains every initial run and retest. The latest execution drives progress but never erases prior history.
- `intern_daily_summary` records date, minutes, modules, completed cases, reports, blockers, and next step.
- `intern_final_summary` stores a generated structured snapshot plus Stewart's conclusions and submission/approval metadata.
- `internal_feedback_report` extends the existing `feedback` row with structured report, environment, reproduction, test, internship, triage, duplicate, resolution, closure, and GitHub relationships.
- `internal_feedback_evidence`, `internal_feedback_comment`, `internal_feedback_history`, and `internal_feedback_retest` provide authorized evidence, clarification, audit, and verification history.
- `intern_audit_notification_outbox` is a durable immediate/digest delivery handoff whose rows are forced to test-transport behavior outside production. Existing in-app notifications serve the reporter and immediate authorized-team events after activation. A production dispatcher for grouped lower-severity digests and any additional external delivery was not present in the inspected baseline and is an explicit deployment gate.

Foreign keys, enumerated checks, unique constraints, and indexes are defined in the reversible SQL migration. Existing feedback rows remain valid and are exposed read-only to administrators through the legacy view endpoint.

## APIs

Internship endpoints add draft audit plans, cases, executions, daily summaries, final summaries, and an explicit activation action. Internal feedback endpoints support private/admin lists, filters, search, CSV/JSON export, legacy reads, draft create/update/submit, comments, file/link evidence, authorized download, history in detail responses, duplicate candidates, and retests.

The public `POST /feedback` contract and privacy/consent behavior are unchanged. Internal routes are under authenticated `/feedback/internal`; audit routes remain under authenticated internships.

## Authorization matrix

| Capability | Intern/reporter | Assigned intern | Manager/Admin/Studio Manager | Other intern |
| --- | --- | --- | --- | --- |
| View draft plan/task | No | No until activation | Yes | No |
| Activate plan | No | No | Yes, explicit action | No |
| View/update executions | No | Yes | Yes | No |
| Edit protected task/case fields | No | No | Draft cases only / task admin fields | No |
| Edit calculated audit progress | No | No | No; calculated only | No |
| Request temporary permissions | Existing workflow | Yes | Approve if authorized | Own requests only |
| Create/edit report draft | Own only | Own only | Own/admin view | Own only |
| Submit/comment/add evidence | Own only | Own only | Yes | Own only |
| Set proposed severity | Own | Own | Yes | Own only |
| Set authoritative severity/priority/assignee | No | No | Yes | No |
| View another private report | No | No | Yes | No |
| Request info/retest, resolve, close, link GitHub | No | Retest only when requested | Yes | No |

Backend checks are authoritative; hiding a control is never relied upon for security. Direct URL and API access are explicit test cases.

## Workflow

Report states are `draft`, `submitted`, `received`, `needs_info`, `confirmed`, `prioritized`, `in_progress`, `ready_retest`, `verified`, `closed`, `duplicate`, and `discarded`, displayed in Spanish. Submission records both submission and receipt, retains duplicate candidates without merging, and creates history. An information response returns a report to received. A duplicate links to its canonical report. A requested retest appends a new test execution/retest record. Closing requires a reason; reopening remains auditable.

## Calculated progress and completion

Progress is the percentage of applicable cases whose latest execution is terminal. Database triggers recalculate task progress and prevent unqualified completion. Completion requires all applicable cases to have results, critical cases to pass/be verified or justified non-applicable, every failure to link a report, strong evidence where required, no open blockers, daily summaries, a submitted final summary, and administrator approval. Only an authorized recorded exception with justification can override the normal gate.

## Notifications

Stewart receives in-app events after activation for receipt, information requests, material state changes, retest readiness, closure, and reopening. Blocker/critical reports and assignment-blocked, midpoint, final-ready, information-response, and retest events create immediate authorized-team in-app notifications and outbox rows; lower severity creates digest outbox rows. Development and staging use test transports only. No real email, WhatsApp, provider, or customer notification is exercised by the assignment. Connecting, scheduling, retrying, and monitoring digest/external delivery from the outbox requires a separately reviewed operational worker before real activation.

## UI

The responsive intern plan page presents calculated progress, Spanish cases, evidence rules, immutable execution history, daily summaries, final summary, and a safety stop notice. The internal feedback surface provides Stewart's private list and report form/detail, while authorized administrators receive all-report filters, evidence review, triage, duplicate handling, retest, history, export, and legacy feedback access. The public form is unchanged.
