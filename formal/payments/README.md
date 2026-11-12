# Payment proofs

PAY-TIP-001, PAY-CAPTURE-RECON-001, PAY-DOMO-QUOTE-001, PAY-COURSE-PRICE-001,
PAY-STRIPE-WEBHOOK-001, PAY-PROVIDER-INBOX-001, PAY-ADAPTER-BINDING-001, PAY-INVOICE-ISSUE-001, CONTRACT-SEND-001, PAY-CHECKOUT-003, PAY-CHECKOUT-001, PAY-DEPOSIT-POLICY-001, PAY-DOMAIN-BINDING-001.

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

| Obligation | Tool | Owner-reported result; current execution unverified |
|---|---|---|
| Bounded exploration, 3 requests, 3 keys, current code | TLC 1.7.2 | no violation (12,139 states) |
| Separate check and reservation, 2 requests, 2 keys | TLC | `Safe` violated, as expected: both requests pass the check before either reserves |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, current code | Apalache 0.62.4 | **proved** for any nonempty sets of up to four requests and four keys |
| Separate check and reservation: reachable violation | Apalache | counterexample found, as expected |

These are owner-reported abstract-model results, not independently replayed in
the 2026-11-06 consolidation: pinned TLC/Apalache were unavailable. Fresh
hosted execution of both models and their negative controls remains required.
The claimed inductive scope covers executions of any length. Equating one transaction under the
advisory lock with one atomic step, and the code with the model, is established
by review; the PostgreSQL lock/two-key race test
(`scripts/test-session-invoice-issue.sh`) is newly wired into CI and was not
executed during this consolidation. Its positive-example/pending guards were
tested with isolated command stubs, which provide no PostgreSQL execution
evidence. Fresh hosted execution of its two real cases remains required.
There is no mechanical refinement. A
second document issued deliberately after a settled attempt, or by staff after
verifying the portal, is allowed by design.

## Contract delivery by email (CONTRACT-SEND-001)

`ContractSend.tla` models `POST /contracts/{id}/send` (`sendH` in
`TDF.Contracts.Server`): 503 without email configuration, otherwise a
`contract_delivery` row, the SMTP call, and the row marked accepted ("sent") or
failed (502). Requests may stop at any step. It sits here because the proof
runner covers this directory; it is not a payment.

`Safe` = `SentMeansAccepted` ("sent" only after the transport accepted the PDF and
the row says accepted), `NoUnrecordedMail` (every accepted message has a row),
`AcceptedMeansMailed` and `UnavailableIsInert` (503 records and sends nothing).

| Obligation | Tool | Owner-reported result; current execution unverified |
|---|---|---|
| Bounded exploration, 3 requests, configured and unconfigured | TLC 1.7.2 | no violation |
| Row written after SMTP, 1 request | TLC | `Safe` violated, as expected: a stop after SMTP accepted leaves no row |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, current code | Apalache 0.62.4 | **proved** for any nonempty set of up to four requests, configured or not |
| Row written after SMTP: reachable violation | Apalache | counterexample found, as expected |

Not covered: a transport that accepts the message and then raises (the row is
then marked failed although mail went out), and delivery beyond SMTP acceptance.

## Merchandise money (PAY-CHECKOUT-003)

`merch_money_arithmetic.py` (Z3 4.13.3, pinned) assumes exactly the guards of
`TDF.Commerce.Merch.calculateMerchMoney` on Int64 inputs and proves over
unbounded integers that:

- the commission is the floor of `(subtotal - discount) * bps / 10000`, lies within
  `0..subtotal - discount` and never exceeds the exact share;
- commission, seller net and payable total all fit Int64 (total at least 1);
- `commission + processorFee + sellerNet = total`;
- in `checkedMerchSubtotals`, every line amount of an accepted cart lies within the
  accepted sum, so it fits Int64.

Witnesses show an ordinary order and a full-commission order are accepted. Controls
drop one guard each (rate above 10000, discount above subtotal, no seller-net guard,
no total bound) and find the value each lemma excludes. The lemmas are about the
function's arithmetic; that the handler persists these values unchanged is covered
by `tdf-hq/test` and the `2026-10-04_merch_commission_exact` constraints, not here.

## Checkout line products and totals (PAY-CHECKOUT-001)

`checkout_money_arithmetic.py` (Z3 4.13.3, pinned) assumes exactly the guards of
`TDF.Commerce.Money` and proves over unbounded integers that:

- for canonical checkouts of 1 to 6 lines, every accepted line amount is positive,
  at most the parent amount and unchanged by narrowing to Int64, and the narrowed
  lines still sum to the parent; a step lemma extends this to any number of lines;
- legacy cart products and sums of nonnegative amounts that pass the bound narrow
  unchanged (host Int assumed 64-bit), and each amount lies within the total.

Controls show why the code computes before narrowing: with Int64 wrap-around, a line
whose true product overflows can match the parent amount, and three in-range cart
amounts can wrap to a nonnegative wrong total. Atomic rejection of an overflowing
cart (no partial rows) is a property of the calling transaction, tested in
`tdf-hq/test`, not proved here.

## Deposit policy (PAY-DEPOSIT-POLICY-001)

`deposit_policy_arithmetic.py` (Z3 4.13.3, pinned) encodes
`TDF.Commerce.DepositPolicy.applyDepositPolicy` for any policy that
`validateDepositPolicy` accepts and any Int64 total and computed deposit, and
proves that an admitted deposit:

- leaves a balance of at least 1 minor unit (so a deposit is never the whole price);
- is within the approved share (`deposit * 10000 <= total * fullPaymentAboveBps`);
- is never below the minimum or the computed deposit;

that a nonpositive total always charges the total, and that raising the minimum
never turns a full payment into a deposit. Witnesses with the default policy (5.00,
90%) reach a plain deposit, a deposit raised to the minimum and a full payment.
Without the `raised >= total` guard a 100% threshold admits a zero-balance deposit;
without the share guard a deposit exceeds the share. Which policy version the
callers load is covered by the course and Domo tests and triggers, not here.

## PlaceToPay query admission (PAY-ADAPTER-BINDING-001)

`placetopay_query_admission.py` (Z3 4.13.3, pinned) encodes `parseQuery`,
`parseRequestPayment`, `validateTransaction`, `validateBindingParser` and
`placeToPayStatus` from `tdf-hq/src/TDF/Commerce/ProviderAdapter/PlaceToPay.hs`
over one session response with up to three transactions, and `decimalToMinor`
/ `minorToDecimal` from `ProviderAdapter.hs` over the reals. It refuses to run
when either file's sha256 changes. It proves that a successful parse binds the
response to the locator and the stored expectation; that `Succeeded` needs an
APPROVED session with exactly one present APPROVED, explicitly unreversed
transaction whose original and settled amounts equal the expectation and every
other present transaction REJECTED; that no-charge certainty needs every
transaction REJECTED; that minor amounts round-trip exactly and sub-cent
decimals are never admitted. Dropping the one-approved rule, the reversal
evidence, the settled-amount check or the request-id check each breaks its
proof (load-bearing). The QuickCheck properties in
`ProviderAdapterBindingSpec` remain the evidence over the real parser; this
proof is the pinned translation of the same rules.

## Provider binding admission (PAY-DOMAIN-BINDING-001)

`provider_binding_admission.py` (Z3 4.13.3, pinned) encodes
`commerce_checkout_has_verified_payment` from
`tdf-hq/sql/2026-10-16_provider_domain_payment_binding.sql` conjunct by conjunct,
over one checkout, attempt, binding and, for hosted providers, create operation and
intent. It refuses to run if that migration's sha256 changes or another forward
migration redefines the predicate, so the hand encoding cannot silently drift.

- **Hosted (PlaceToPay session, PayPhone sale):** admitted evidence agrees on
  checkout, provider, account, environment, provider reference, provider resource,
  currency and amount, as the requirement states; the provider reference may differ
  from the internal order id.
- **Direct (Datafast, PayPal, Stripe):** the binding's reference is the checkout's
  domain order id, the resource type is the provider's, and environment, amount and
  currency agree.
- A non-succeeded attempt, or one of another environment, amount or currency, is
  never admitted.
- **Necessity:** dropping any one of the 25 hosted-branch conjuncts breaks the
  agreement, so none is redundant; the result is pinned so a change is reviewed.

The manual-evidence branch (bank transfer, cash, POS with two different reviewers)
and the Domo date-hold guard are not encoded here; they are covered by the
PostgreSQL matrix in `scripts/test-provider-domain-binding.py`.

## Execution evidence for the 2026-11-06 consolidation

The four new Z3 4.13.3 programs (`checkout_money_arithmetic.py`,
`merch_money_arithmetic.py`, `deposit_policy_arithmetic.py`, and
`provider_binding_admission.py`) actually exited successfully on clean source
`743c5c995538420f6702c6df1ae002792f7e1eaa`; their source files and financial
implementation inputs are unchanged by the subsequent evidence-wording update.
This is local solver execution, not hosted CI or mechanical implementation
refinement. The two new TLA models and the two real PostgreSQL invoice cases
were not executed in this consolidation. No prior immutable PostgreSQL job/log
receipt was found for the new two-case claim; owner prose is retained as a
reported result, not substituted for execution evidence. Existing inherited
SQLite/migration evidence remains separately scoped to the earlier audit.

## Legacy Stripe ticket capture (`PAY-STRIPE-WEBHOOK-001`, `PAY-CAPTURE-RECON-001`)

`stripe_ticket_capture.py` (Z3 4.13.3, pinned) encodes what a signed
`payment_intent.succeeded` event does to a matched ticket order:
`handleStripePaymentIntentSucceeded` and `validateStripeTicketPaymentEvidence`
of `SocialEventsHandlers.hs`, and `parseStripeTicketCapture` and
`stripeTicketClosedOrderNeedsReview` of `Server/StripeTicketCapture.hs`. It
refuses to run when the text of any of those regions changes.

| Obligation | Result |
|---|---|
| Tickets are issued only with the full binding (succeeded, same PaymentIntent, order, event, amount, currency) on a pending order | proved (UNSAT of the negation) |
| The answer is 400 exactly when the binding is not admitted | proved |
| A succeeded positive capture on an existing order ends in tickets, a paid redelivery or an open exception | proved |
| An exception is written only for a capture, never together with tickets, with the order total as expected and the received amount as observed | proved |
| A paid redelivery changes nothing and records nothing; only a pending order becomes paid | proved |
| Issue, redelivery, both exception types and a refusal without capture are reachable | witnesses found |
| Order row deleted between lookup and read-back while the stale copy says pending | reachable and silent (limit; no application code deletes ticket orders) |
| Dropping any of the six comparisons, the pending fence or either exception write | each breaks at least one proof |

Not modelled: signature check and event-id dedupe, JSON decoding, the separate
transaction of the not-admitted exception, ticket codes, the confirmation
e-mail, and events that match no ticket order.
