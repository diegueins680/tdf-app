# Draft project preview — approval required

## Identity

**Resolved by a read-only production check on 2026-08-23.** The requested “Stuart” is the active intern recorded as **Stewart Moreira**. The production account, current internship profile, recent time entries, and existing project `Plan de prácticas - Stewart Moreira (#129)` all identify the same person. Exact party ID and email were verified but are deliberately omitted from source control; the draft creation command requires both as runtime-only values and rejects any non-exact match. The synthetic `PER-11` intern remains engineering test data and must never be assigned as Stewart.

No fallback account is needed and no duplicate must be created. The machine-readable `draft-stuart-account.json` records only the non-sensitive verification state and least-privilege review gate. It cannot create or modify an account.

## Project and principal assignment

- Project: **Auditoría funcional y de experiencia del manejo del estudio**
- Assignment: **Ejecutar la auditoría funcional y de experiencia del manejo del estudio**
- State: `draft`; hidden from interns; Stewart is a proposed assignee only
- Environment: dedicated staging tenant with fictional `AUDIT-2026` data
- Duration: 14 calendar days from a later activation
- Expected effort: 20–30 hours (generated cases estimate 23.4 hours)
- Midpoint review: automatically signaled near 50% calculated progress
- Final review: submitted Spanish summary and live demonstration
- Notifications: disabled before activation; test transports in staging

## Scope and cases

- 130 evidence-backed inventory entries
- 125 applicable feature areas
- 174 structured cases across 14 modules
- 14 exploratory charters
- 107 cases requiring strong evidence

Coverage includes authentication/account/navigation, CRM, contacts/leads, calendar/rooms/resources/services, internal/public bookings and conflicts, orders/sessions/input lists/participants, packages/quotes/invoices/manual and sandbox payments/failure/cancellation/refund/reconciliation, inventory/reservations/rentals/scanning/maintenance, operational/financial reports, Live Sessions, notifications, reception/engineer/manager/admin workflows, permissions, localization, accessibility/responsiveness, and only the studio-related portions of Domo, school, and DDEX.

## Schedule

| Day | Focus | Effort |
| ---: | --- | ---: |
| 1 | Onboarding, safety, staging, accounts, clock-in/out | 2–3 h |
| 2 | CRM, customers, leads, studio pipeline | 2–3 h |
| 3 | Calendar, rooms, resources, availability, bookings | 2–3 h |
| 4 | Orders, sessions, participants, input lists, equipment | 2–3 h |
| 5 | Packages, quotes, invoices, sandbox payments; midpoint | 2–3 h |
| 6 | Inventory, maintenance, operations, reports | 2–3 h |
| 7 | Live Sessions and fake/sink integrations | 2–3 h |
| 8 | Studio-related Domo, school, and DDEX boundaries | 2–3 h |
| 9 | Mobile, accessibility, languages, permissions, edges, retests | 2–3 h |
| 10 | Coverage closure, clarifications, final report, demonstration | 2–3 h |

## Permission proposal

The production account currently has active `Intern`, `Reception`, `Customer`, and `Fan` roles. Its effective modules are Internships, CRM, Packages, and Scheduling; this implementation does not add, revoke, or reinterpret those grants. Before activation, an authorized administrator must confirm that `Reception` is still independently justified. The audit itself permanently requires only `Intern` plus Internships. Other test access belongs in staging through approved synthetic role accounts or temporary, least-privilege, time-bound permission requests. Stewart cannot approve a request, grant roles, edit protected task/report fields, set authoritative triage, see other reporters' private data, or activate/complete the assignment without server-enforced authorization.

## Notification behavior

Before activation: none. In staging: in-app/sink/outbox only, synthetic recipients. After a separately approved activation, the implemented in-app channel gives Stewart receipt, clarification, material state, retest, closure/reopen events. Authorized team members receive immediate in-app notices for blocker/critical, assignment-blocked, midpoint, final-ready, information-response, and retest events; all team events also enter the durable outbox, with lower severity grouped for digest delivery. Recipient configuration and a production dispatcher/digest schedule—or an explicit in-app-only decision—must be approved and verified before activation.

## Completion criteria

Every applicable case must have a recorded result; failures require linked reports; critical/failure evidence must be sufficient; blockers/non-applicable cases/retests must be documented; daily time summaries and final summary must exist; no open blockers or unexecuted critical cases may remain; and an authorized Manager/Admin must approve completion or record an auditable exception justification.

## Approval decision still needed

Identity is resolved. The isolated staging API, web app, database, and synthetic personas are deployed and healthy. An inactive in-app staging draft with 174 cases was created using the role-equivalent synthetic Intern; it is hidden, unassigned, has no due date, and produced no notification or outbox row. This gives Diego a safe system preview without storing Stewart's production identifiers in staging.

Activation, assignment to Stewart, production deployment, real notification, external-provider credentials, and issue creation remain separate approval gates. The next approval decision is whether the reviewed implementation and preview may proceed toward production deployment and a production-side inactive association with Stewart; it is not approval to activate or notify him.
