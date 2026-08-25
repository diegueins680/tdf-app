# Test-plan specification

## Inventory and generation

`generate-studio-audit.mjs` deterministically converts the evidence-backed inventory into 174 Spanish cases across 14 modules. It includes 14 exploratory charters and estimates 23.4 execution hours, fitting the requested two-week 20–30-hour assignment. Stable identifiers are never derived from database IDs.

Every case defines module, feature, role, objective, business purpose, preconditions, data, environment, platform/device, language, steps, visible result, persisted state, side effects, cleanup, criticality, result status, evidence, schedule day, and effort. Allowed visible statuses are Pendiente, En progreso, Aprobado, Fallido, Bloqueado, No aplicable, Listo para retest, and Verificado.

## Execution model

- Week 1: onboarding and safe-environment validation, CRM, calendar/rooms/resources/bookings, sessions, quotations/invoices, and sandbox payments.
- Midpoint: review at calculated progress near 50%, including blockers, report quality, permissions, and remaining risk.
- Week 2: inventory/maintenance/reports, Live Sessions, shared Domo/school/DDEX dependencies, mobile, localization, accessibility, edge cases, retests, final summary, and demonstration.
- Each work session starts and ends through existing time tracking and concludes with the required structured daily note.

The plan covers successful and unsuccessful inputs, duplicates/double clicks, concurrency/conflicts, permissions via UI/direct URL/API, expired sessions, interrupted/slow requests, browser history, cancellation, payment failure/abandonment/retry, stale data, unavailable resources, responsive layouts, Spanish/English consistency, keyboard operation, labels/errors, contrast, and text scaling.

## Evidence policy

Ordinary passing cases need a concise observation. Critical workflows, all failures, permission/integrity/payment behavior, blockers, and intermittent results require strong evidence: screenshots or small safe documents plus persisted-state and side-effect observations. Videos are linked from approved external storage. Evidence must never contain real personal data, credentials, payment details, production identifiers, or secrets.

## Reporting rule

Every unrelated finding is a separate internal report. A failed execution cannot qualify for completion without a linked report. Duplicate suggestions are warnings; the reporter still submits and an administrator links the canonical report. Stewart proposes severity honestly; administrators own authoritative severity and priority.

## Exploratory standard

Charters evaluate discovery, understandable labels, operational fit, timing of required information, feedback and recovery, excessive steps, cross-surface consistency, desktop/mobile agreement, accessibility, and improvements. “Could not find it,” “works but is confusing,” and “no issue found after careful execution” are all valid results.

## Final output

The application calculates counts by execution result and report type/severity for the final snapshot. Stewart adds conclusions covering the most important defects, usability problems, ideas, undiscoverable features, web/mobile differences, accessibility, remaining risks, and three priorities. An authorized administrator approves completion.
