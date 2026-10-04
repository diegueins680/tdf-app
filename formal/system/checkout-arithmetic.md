# Checkout and marketplace amount correspondence

`PAY-CHECKOUT-001` requires exact integer products and sums before narrowing to
storage/DTO types. Canonical checkouts have a positive `Int64` parent amount,
at least one line, positive unit amounts and quantities fitting PostgreSQL
`INTEGER`. Each persisted subtotal equals its exact quantity × unit amount and
the exact sum equals the parent. Rejection occurs before any canonical checkout
insert. The private insertion function receives validated subtotals; it must not
recalculate them with unchecked fixed-width multiplication.

Marketplace cart previews retain nonnegative prices and zero/empty totals; payment
admission separately requires a positive payable total. Legacy `Int` fields must
also reject products and aggregates beyond the host's representable range. Invalid
persisted quantities/prices and overflowing previews return an HTTP conflict,
not a successful wrapped total. A cart mutation whose resulting receipt cannot
represent its total rolls back the mutation, including its timestamp. Existing
quantity and rental-selection business guards remain in force.

The counterexample uses three quantity-one lines with amounts `M`, `M`, and `3`,
where `M = 9223372036854775807`. The original Haskell aggregate wraps to `1`.
The fully migrated test database accepted a parent total of `1` and the three
lines, whose exact sum is `18446744073709551617`. This is a real adapter/schema
counterexample, not evidence of a real-money charge. The synthetic SQL transaction
was rolled back. A SELECT-only production PG17 check on 2026-10-04 found one
canonical checkout, no missing lines, no subtotal mismatch and no overflow.

`TDF.Commerce.Money` uses unbounded `Integer` until it has validated storage
bounds. `CheckoutMoneySpec` exercises boundary cases plus three 1000-case properties:
arbitrary signed multi-line inputs against an unbounded oracle, and generated
valid positive snapshots. `verify-checkout-money.py` runs the actual source and
nine controlled mutations: wrapped canonical sum, unbounded storage quantity,
hidden zero quantity, wrapped legacy sum and wrapped legacy product, plus four merchandise product/aggregate/commission/payable mutations. A compiler
error does not count as detecting an invalid model; every control must reach a
failing Hspec assertion. The seed, source/test hashes and output are retained.

The shared real-HTTP fixture checks invalid stored data, exact ordinary totals
and rollback of an overflowing cart update. It uses no provider endpoint.
This is property and boundary execution evidence, not a proof over all cart sizes,
all currencies or the whole checkout lifecycle. The compiler, Integer semantics,
PostgreSQL transaction adapter and finite fixture environment are trusted. Memory
exhaustion and arbitrary external SQL are excluded. No liveness/fairness claim is
made for these terminating local arithmetic operations.

`PAY-CHECKOUT-002` adds a database boundary in
`2026-10-04_checkout_amount_correspondence.sql`: every committed checkout has
nonempty lines whose exact payable total equals its parent payable total.
Currency and subtotal/discount/tax/fee/total are immutable after insertion;
payment/refund counters and lifecycle state can still advance. Both current
writers construct the parent and lines in one transaction. Merchandise keeps
shipping in the header fee and a separate line, so component-by-component
subtotal equality would incorrectly reject its supported representation.

Deferred constraint triggers check at commit (or explicit SET CONSTRAINTS).
The migration locks both tables and rejects pre-existing discrepancies without
rewriting financial evidence. Historical migrations remain unchanged. Recovery
retains the additive constraints; there is no down migration. A failed preflight
rolls back all migration effects and requires separately reviewed reconciliation.

The concurrency argument is deliberately narrow: preflight-valid parent amounts
are immutable, existing lines cannot be updated/deleted, and appended line totals
are nonnegative. A positive append cannot individually pass even with an older
repeatable-read snapshot; zero-total appends preserve equality. Uncommitted new
parents are fenced by the existing foreign key. This is not a general proof for
mutable aggregates. Privileged DDL, TRUNCATE, disabled triggers, corruption and
resource exhaustion are excluded. There is no liveness claim.

`checkout-amount-postgres.mjs` applies the actual SQL to a fully migrated isolated
database, rejects invalid historical state, checks real commit failures/rollback,
accepts shipping and Int64 maximum, and verifies immutable terms/lines. Its five
transactionally rolled-back negative controls remove preflight, parent checking,
append checking, currency immutability, or the pre-migration snapshot visibility fence. Six concurrent append transactions
reach an observed advisory-lock barrier before commit across Read Committed,
Repeatable Read and Serializable; all must reject and leave the original total.
A dedicated old-snapshot schedule starts before preflight, observes an incomplete snapshot, then attempts to append after another transaction and migration establish consistent current state. The new boundary row is invisible to that old repeatable-read snapshot, forcing a retry with SQLSTATE 40001. Removing the check admits the invalid append; the negative-control transaction rolls back.
Local PostgreSQL 16 execution is not evidence of production deployment or PG17
execution; the hosted backend job repeats the test on its service.

General tax, discount, rounding, JavaScript numeric precision, all other monetary
writers and provider finality remain separate obligations. `PAY-CHECKOUT-003` validates merchandise quantities (1..100), nonnegative prices,
exact products/sum, positive payable total and Int64 bounds before shipping rules
and persistence. The same validated line values feed order and checkout rows.
Commission uses exact Integer multiplication and floor division by 10000; seller
net and total are checked before narrowing. A separate additive migration replaces
the old BIGINT-intermediate commission CHECK with numeric/trunc, preserving its
rounding rule. A real maximal-commission insert and rejected-overflow HTTP checkout
exercise storage correspondence without payments. Refund/settlement SQL arithmetic
and client numeric precision remain separate obligations.
The unused legacy Stripe writer remains disabled at its public handler.
