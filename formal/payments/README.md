# Payment outcome proofs (PAY-TIP-001)

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
- The SQL public-listing query used for tip admission, which has no database-backed test yet.
