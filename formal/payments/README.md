# Payment proofs (PAY-TIP-001, PAY-CAPTURE-RECON-001, PAY-DOMO-QUOTE-001)

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

- Request-level idempotency of `POST /fans/artists/{artistId}/tips`. A retry creates another pending row and PaymentIntent, which charges nothing unless the client confirms it.
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

