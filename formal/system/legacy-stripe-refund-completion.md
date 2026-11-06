# Legacy Stripe refund completion evidence

Requirement: `EVT-TICKET-001`. Narrow invariant:
`PAY-STRIPE-REFUND-COMPLETION-001`.

A refund response identifier alone must not authorize ticket retirement or an
inventory release. Stripe's pinned
[refund object contract](https://docs.stripe.com/api/refunds/object?api-version=2026-04-22.dahlia)
allows outcomes that are pending, require action, fail, or are cancelled. Its
amount uses integer minor units, its currency uses a lowercase three-letter
code, and its PaymentIntent identifies the payment being refunded.

The baseline `7b987d99ea39a03448fdaf7d3c01e5ced3fa23f6` handler accepted any
response with a textual ID, then marked the request approved and all tickets
refunded. The actual PostgreSQL counterexamples include a pending response and
responses with different amounts, currencies, and PaymentIntents. The first
execution-fence correction remains necessary but does not establish completion.

Acceptance criteria:

- Preserve the single committed execution permit from the parent correction.
- Treat a returned ID as observed correlation only. Accept a nonempty visible
  ASCII identifier of at most 128 characters; do not assume a particular prefix.
  These are local bounded-input admission rules, not a claim about every future
  Stripe identifier format. A newly unsupported representation remains held.
- Retain an observed ID while processing even if other response fields are
  invalid. Never overwrite a different previously recorded ID.
- Permit completion only for an object identified as a refund, a succeeded
  outcome, the exact positive requested amount, the expected lowercase currency,
  and the exact expected PaymentIntent string. No expanded PaymentIntent is
  requested by this endpoint, so an unexpected expanded representation is held.
- Missing, malformed, mismatched, or nonterminal completion evidence produces
  generic HTTP 502 without disclosing the response payload or ID. The request
  remains processing, with no completion timestamp or completion projection.
- Recheck the processing request, order identity, amount, and observed ID inside
  the completion transaction. Any projection failure rolls back completion while
  preserving the separately committed execution fence and observed ID.
- An approval retry refuses execution. This change never queries or re-POSTs a
  held refund and never resets it to pending.

Implementation: the private `stripeRefundCompletionMatches` predicate and the
legacy approval branch in `tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`.
Existing extraction-only helper compatibility is retained; it is not a
completion validator. The DTO and existing processing UI remain unchanged.

Verification uses actual routed handlers against the full production migration
manifest with synthetic provider callbacks, plus an independent finite evidence
model in `scripts/__tests__/ticket-admission-model.test.mjs`. The execution
receipt records exact source hashes, positive and negative results, bounds and
commit identities. Model checking is bounded design evidence, not proof of the
Haskell, SQL, authenticated provider connection, or complete workflow.

Assumptions and remaining risks: expected PaymentIntent, amount, and currency
come from the authorized stored order/request snapshot. This correction does
not add a current-actor recheck or an immutability fence for every legacy order
field across provider IO. It does not qualify partial-refund ticket allocations,
admission while processing, accounting reconciliation, or operational recovery.
An observed ID belonging to mismatched evidence is not authority to complete a
refund by ID alone. A later query/reconciliation path must re-establish all
original payment, account, environment, currency, and amount bindings. No real
provider payment or production financial record is exercised by these tests.
