# ADR-0110: Marketplace customer requests and rental-deposit settlements are reviewed evidence

Status: Accepted — 2026-08-17

## Context

Sale and rental checkout, holds, custody, and condition reporting are canonical, but a customer had
no safe way to request cancellation, return, extension, or a rental dispute from the public order
tracker. Operations could move domain states directly, which made an inbound request an external
message rather than linked evidence. A non-zero rental deposit could reach a truthful
`refund_due`/`partial_refund_due` state but had no evidence-backed manual settlement path. Treating
an operator click as a provider refund would be false, while recognizing the refundable deposit as
revenue would be incorrect accounting.

## Decision

Persist customer requests against the canonical marketplace order with a scoped lookup capability,
immutable reason/evidence snapshot, request hash, per-order idempotency key, append-only events, and
one open request of each type. Submission changes no payment, fulfillment, custody, date, refund, or
asset state. Database guards stop outbound/closing transitions while a relevant request is pending.
Authorized staff may approve or reject supported requests; approval atomically moves only the
domain state to `cancellation_requested`, `return_requested`, or `disputed`.

A rental extension may move only to `needs_quote` or `rejected`. Direct approval is prohibited in
Haskell and PostgreSQL until an extension implementation can lock the asset date range, create a
versioned price/change-order snapshot, collect any balance, and update the reservation atomically.

Future verified rental payments recognize the rental charge as revenue and the refundable security
deposit as a liability. At `deposit_refund_due`, staff can submit exact server-derived deposit,
deduction, refund, checkout, and currency evidence for bank transfer, cash, POS, or documented full
forfeiture. A different authorized staff member must verify it. Verification posts a balanced
liability/cash/damage-deduction ledger transaction, records returned funds and a credit-note
reference, and moves the deposit to `refunded`, `partially_refunded`, or `forfeited`. It does not
call a provider, create a provider-refund ID, or describe funds as provider-refunded. Historical
paid rentals without the new deposit-liability capture entry remain visible in a read-only
`requires_reclassification` report and are never silently backfilled.

Legacy marketplace orders without a canonical runtime lookup-token hash fail closed at public
tracking and customer-request endpoints. Staff access remains available through authenticated
operations routes.

## Alternatives

- Let the browser update the order: rejected because possession of a tracking link cannot authorize
  fulfillment or payment transitions.
- Approve an extension by editing `end_date`: rejected because it bypasses date contention,
  immutable pricing, terms acceptance, and balance collection.
- Reuse the provider-refund model for every deposit return: rejected because a bank transfer or
  cash return is not evidence that Datafast or PayPal executed a refund.
- Mark a deposit refunded when inspection completes: rejected because a due amount is not funds
  movement.
- Backfill deposit liabilities into posted historical ledgers: rejected because ambiguous history
  requires reconciliation and immutable correcting entries.

## Consequences

Customers have a public, idempotent, honest request channel and operations receive linked review
evidence. Rental deposit liability is separated from revenue for new verified captures, and manual
settlement is available behind `commerce.marketplace_manual_deposit_settlement`. Provider refunds,
automated payouts, automatic extensions, and production money movement remain independently gated.
Operations must preserve evidence in approved private storage and must not self-approve a deposit
settlement.
