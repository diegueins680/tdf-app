# ADR-0111: Course seats are expiring checkout holds, not registrations or payments

Status: Accepted — 2026-08-18

## Context

The public course form created `pending_payment` registrations without expiry. Capacity reporting
counted almost every non-cancelled row, so an unpaid lead could permanently consume a seat. The UI
then described submission as a confirmed registration while payment remained an out-of-band
follow-up. Separate Stripe endpoints could mutate canonical registrations without the shared
checkout verification and provider-event model.

## Decision

Keep `course_registration` as the compatible lead/admin record and link new commerce through
`course_registration_checkout_runtime` to one canonical checkout and one immutable, approved
`course_checkout_policy` version. All money is stored in integer minor units. The policy must match
the authoritative course price and currency, and approved commercial/terms fields are immutable.
Migrated public prices are reviewable drafts only; the migration never activates checkout.

Creating a canonical registration requires terms acceptance and an idempotency key. The server
locks the cohort, recalculates the approved full-payment or explicit deposit amount, checks capacity,
and creates a 15-minute seat hold by default in one transaction. PostgreSQL repeats the course-row
lock and capacity check so competing or non-application writers cannot overbook. Expiry releases the
hold and records an enrollment event. Legacy unpaid leads do not consume seats.

Payment and enrollment remain separate. A course checkout can become `paid` only when a succeeded
attempt has a matching provider binding for checkout, order, environment, merchant, amount, and
currency. Datafast status is checked server to server; PayPal capture is validated server to server.
A browser return is never payment evidence. Verified payment consumes the hold into `enrolled`; it
does not imply attendance or completion. Refund, dispute, chargeback, transfer, and completion
remain distinct states and use append-only evidence.

Guest tracking requires the registration ID plus an unguessable lookup token and returns the same
not-found response for an invalid token or unknown record. When `commerce.courses` is disabled, the
public form may preserve a legacy lead, but the response explicitly says no checkout, payment, or
seat reservation occurred. Legacy Stripe endpoints reject canonical registrations.

## Alternatives

- Count every pending lead as a seat: rejected because abandoned forms create permanent false
  scarcity.
- Trust a checkout return route: rejected because the browser cannot prove capture, merchant,
  amount, currency, or environment.
- Make every course full-payment in code: rejected because higher-value programs need explicitly
  approved deposits or installments without silently undercharging ordinary cohorts.
- Replace all course records with the checkout table: rejected because the domain needs attendance,
  transfer, completion, and historical administration independent of payment.

## Consequences

Ordinary cohorts default to full payment; deposit policies require an explicit approved version.
Production course checkout remains disabled until at least one cohort policy is approved, Datafast
and PayPal sandbox evidence is reconciled, cancellation/refund ownership is assigned, and the
staging checklist passes. Recurring billing remains separately disabled until a merchant capability
is verified. Mobile retains its documented external-web flow while consuming the synchronized
OpenAPI contract.
