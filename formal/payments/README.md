# Payment proofs

PAY-TIP-001, PAY-CAPTURE-RECON-001, PAY-DOMO-QUOTE-001, PAY-COURSE-PRICE-001,
PAY-PROVIDER-INBOX-001, PAY-ADAPTER-BINDING-001, PAY-INVOICE-ISSUE-001.

## Contract

Stripe `payment_intent.succeeded` / `payment_intent.canceled` webhooks record the
outcome of an artist tip, a course registration or a marketplace order. Deliveries
can be duplicated, reordered and processed concurrently: the event-id dedupe row is
read before dispatch and inserted after it, in separate transactions, so two
concurrent duplicates both dispatch. The recorded outcome must never be replaced.

The handlers (`markArtistTipStatus`, `markCourseRegistrationStatus` and the first
branch of `markMarketplaceOrderStatus` in
`tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`) now write with a conditional
`UPDATE … WHERE status = 'pending'`. Before 2026-10-31 they read the row and then
updated it unconditionally in another transaction.

## Model

`StripeIntentOutcome.tla` abstracts each delivery to one read and one write.

| Variant | Meaning |
|---|---|
| `ConditionalUpdate = TRUE` | current code |
| `ConditionalUpdate = FALSE`, `SingleTerminal = TRUE` | previous code, assuming Stripe never emits both outcomes for one intent |
| `ConditionalUpdate = FALSE`, `SingleTerminal = FALSE` | previous code without that assumption (negative control) |

`Safe` = no terminal outcome is ever replaced, and every terminal outcome is the
outcome of some delivered event. `IndInv` strengthens it.

## Results (`npm run verify:inductive-proofs`)

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration, 3 deliveries, current code | TLC 1.7.2 | no violation |
| Bounded exploration, 3 deliveries, previous code with provider assumption | TLC | no violation |
| Previous code without the assumption, 2 deliveries | TLC | `Safe` violated, as expected: both read `pending`, `paid` is then overwritten by `failed` |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, current code, any `SingleTerminal` | Apalache 0.62.4 | **proved** |
| Same three obligations, previous code with the provider assumption | Apalache | **proved** |
| Previous code without the assumption: reachable violation | Apalache | counterexample found, as expected |

**Scope of the proof.** The proof holds for executions of any length and every
interleaving, with any set of up to six concurrent deliveries (`Deliveries ∈ SUBSET
(1..6)`). The current code's safety needs no assumption about Stripe. The step from
the model to the Haskell code is established by review: one read plus one
conditional write per delivery. It is not mechanically refined.

## Not covered

- Request-level idempotency of `POST /fans/artists/{artistId}/tips` is now provided by the optional `Idempotency-Key` header (`TDF.Commerce.ArtistTip.decideTipReplay`, unique index `uq_artist_tip_idempotency`); the concurrent two-request case is covered by review and the unique index, not by a database test.
- Ticket orders, which have their own path (`EVT-TICKET-001`).
- A fully migrated database for the public-listing query, which is tested on a minimal PostgreSQL fixture with the migrated column types (`scripts/test-artist-visibility-postgres.sh`).

## Captured money is always verified or reconciled (PAY-CAPTURE-RECON-001)

`CaptureAccounting.tla` models one capture handler (Domo, course, event ticket,
marketplace, service storefront) racing a concurrent hold expiry. The handler
loads its context, captures remotely, checks the hold time and admits the payment
under the checkout lock (`recordVerifiedPayment`). Clocks are modelled as perfect.

Before 2026-11-01, a rejected admission only returned an error. TLC finds the race
even with perfect clocks: the time check passes, another request expires the hold,
and the locked admission rejects the payment. The money stays captured with neither
a verified payment nor a reconciliation exception.
`Checkout.recordVerifiedPaymentOrReconcile` now records an open
`verified_payment_not_admitted` exception in the same transaction. Apalache proves
`NoSilentCapture /\ VerifiedIsPaid` inductively for the new code, and finds the
violation for the old one. Twelve handler call sites use the new function. The
provider reconciliation worker, music commerce and the PayPal webhook keep their
own reconciliation paths.

## Domo quote arithmetic (PAY-DOMO-QUOTE-001)

`domo_quote_arithmetic.py` (Z3 4.13.3, pinned) proves over unbounded integers that:

- the half-up basis-point deposit lies within `0..total` for any rate in `1..10000`;
- the balance is never negative;
- tax lies within `0..subtotal`;
- the Int64 guard on line amounts also bounds unit amounts.

Witnesses show the hypotheses are satisfiable. Mutation controls (a rate of 10001
basis points, a negative total) show the bounds are necessary. QuickCheck
properties in `tdf-hq/test/TDF/Commerce/DomoQuotePropertiesSpec.hs` check the real
`calculateDomoQuote`: line sums, `total = subtotal + tax`,
`deposit + balance = total`, and rejection instead of wrapping beyond Int64.

## Provider event lease fencing (PAY-PROVIDER-INBOX-001)

`ProviderInbox.tla` models one inbox event, workers that claim it (incrementing
`attempt_count`), a stale lease that another worker may reclaim after fifteen
minutes, and the outcome write of each worker.

Before 2026-11-01 the outcome writes only required `processing_status =
'processing'`. TLC finds the defect with two workers: A claims attempt 1 and
stalls, B reclaims attempt 2, A finishes and writes `dead_letter` over B's lease
(`TerminalByLatest` violated); in the retry variant, A writes `retry` and B's
later success can no longer be recorded (`NoLostSuccess` violated), so the event
is processed again. Every write now also requires `attempt_count = <claimed
attempt>`. Apalache proves `TerminalByLatest /\ NoLostSuccess` inductively for any
nonempty set of up to three workers and any attempt budget and claim bound in
1..8, and finds the violation for the unfenced protocol.

The side effect a reclaimed worker already performed is outside this model; its
idempotency is PAY-RETRY-001 and PAY-CAPTURE-RECON-001.

## Course price arithmetic (PAY-COURSE-PRICE-001)

`domo_quote_arithmetic.py` also covers `calculateCoursePrice`, which uses the same
half-up basis-point rounding with a deposit rate in 1..9999: the deposit stays
within the total, and the balance is strictly positive whenever the total exceeds
50.00 (5000 minor units). Below that, a deposit can equal the whole total; Z3 gives
the witness. QuickCheck properties on the real function are in
`tdf-hq/test/TDF/Commerce/DomoQuotePropertiesSpec.hs`.

## Hosted adapter binding admission (PAY-ADAPTER-BINDING-001)

`tdf-hq/test/TDF/Commerce/ProviderAdapterBindingSpec.hs` drives the real PlaceToPay
and PayPhone adapters with generated provider responses whose reference, amount
and currency either equal the stored expectation or differ in exactly one field.
The adapters admit a result only on an exact match; PlaceToPay reports success only
for exactly one approved, unreversed transaction; PayPhone callbacks are never
authenticated. Money conversion round-trips every minor amount and rejects
sub-cent precision. These are properties, not proofs: they cover the generated
response shapes.


## Session invoice issuance (PAY-INVOICE-ISSUE-001)

`SessionInvoiceIssue.tla` models requests to `POST /invoices/{sessionId}/generate`
for one session and one request fingerprint. Each request carries an idempotency
key and may carry portal verification. The SRI portal is not idempotent, so an
attempt answered with an error or an unconfirmed status counts as unresolved, as
does a reservation with no answer. Requests may stop at any step.

| Variant | Meaning |
|---|---|
| `Lock = TRUE` | current code: key lookup, sibling check and reservation in one transaction after `pg_advisory_xact_lock` |
| `Lock = FALSE` | check and reservation as separate steps (negative control) |

`Safe` = `OneRunPerKey` (the SRI script starts at most once per key) and
`NoBlindDuplicate` (two unresolved attempts under different keys are never open
together unless one was made after verifying the portal).

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration, 3 requests, 3 keys, current code | TLC 1.7.2 | no violation (12,139 states) |
| Separate check and reservation, 2 requests, 2 keys | TLC | `Safe` violated, as expected: both requests pass the check before either reserves |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, current code | Apalache 0.62.4 | **proved** for any nonempty sets of up to four requests and four keys |
| Separate check and reservation: reachable violation | Apalache | counterexample found, as expected |

The proof covers executions of any length. Equating one transaction under the
advisory lock with one atomic step, and the code with the model, is established
by review and by the PostgreSQL test of the lock and the two-key race
(`scripts/test-session-invoice-issue.sh`), not by mechanical refinement. A
second document issued deliberately after a settled attempt, or by staff after
verifying the portal, is allowed by design.
