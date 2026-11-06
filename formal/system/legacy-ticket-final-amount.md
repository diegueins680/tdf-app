# Fee-inclusive admission envelope for new legacy checkouts

Parent requirement: `EVT-TICKET-001`.

`EVT-LEGACY-FINAL-AMOUNT-001`: a new legacy Stripe checkout must validate its
computed total, including the buyer platform fee, against the existing internal
0..99,999,999 amount envelope before stock, order, ticket or promo writes. A
zero total remains a free checkout. Existing checkout retries retain their
stored total and existing settlement/recovery behavior.

Authority is the existing `validateTicketCheckoutAmount` contract, its amount
validation regressions, the conservative Stripe service amount validator and
the unchanged platform fee policy. Face value was previously checked before
adding the buyer fee. Three actual-handler failures from seven baseline cases
instead created cancelled orders with amounts 100,000,000 or 101,999,999 and
returned 503 through missing-configuration rollback. No provider was contacted;
these observations establish incorrect internal admission and unnecessary hold
history, not an incorrect charge or a universal provider rejection.

The correction invokes the existing amount validator with quantity 1 and the
already computed final total only in the new-order branch, before its hold
transaction. Multiplying that total by the purchase quantity again would be
incorrect. Base amount validation, promo calculation, fee rounding, terms,
currency, lock order and provider initialization remain unchanged.

The existing limit is deliberately retained. Stripe's pinned PaymentIntent
reference describes eight digits, while its current currency guide documents
wider limits for some payment methods. Increasing TDF's supported envelope or
adding payment-method/currency/account rules is a separate product decision.

- [Stripe PaymentIntent reference](https://docs.stripe.com/api/payment_intents/create?api-version=2026-04-22.dahlia)
- [Stripe currency guide](https://docs.stripe.com/currencies#maximum-charge-amounts)

With a nonnegative discounted face no greater than 99,999,999 and the current
400 bps formula, totalFee=floor(face*400/10000), buyerFee=floor((totalFee+1)/2),
and final=face+buyerFee. The exact supported face boundary is 98,039,215; its
final total is 99,999,999. Face 98,039,216 yields 100,000,000. Integer arithmetic
claims and current 64-bit intermediate safety can be checked with the scoped
Z3 artifact. Those claims assume this formula and do not prove Haskell/SQL
refinement, provider success or future configurable fee policy.

Actual-handler regression obligations cover the original three failures,
quantity 1 and 2 boundaries, a fixed discount rescuing the boundary, an
insufficient discount, a fully discounted free checkout and idempotent replay,
and historical pending replay. The positive-amount cases deliberately reach the
existing missing-configuration 503 and retain one cancelled order with exact
fee metadata and net-zero stock/promo effects; they do not establish payment
success. Invalid new totals return 400 and leave no order/stock/ticket/promo
effects. A historical pending amount 100,000,000 remains pending on its existing
503 path, without a new refusal, order or rollback. It is synthetic compatibility
evidence, not proof that such a provider charge occurred.

Historical financial data is not rewritten. Existing initial route/base amount
checks may still refuse a retry for their own reasons; this correction does not
guarantee every retry succeeds. Price/owner snapshots, tiny positive minimum
amounts, currency-specific rules, capacity races, uncertain provider execution
and later reconciliation remain separate contracts.
