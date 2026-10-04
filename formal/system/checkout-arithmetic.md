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
bounds. `CheckoutMoneySpec` exercises boundary cases plus two 1000-case properties:
arbitrary signed multi-line inputs against an unbounded oracle, and generated
valid positive snapshots. `verify-checkout-money.py` runs the actual source and
five controlled mutations: wrapped canonical sum, unbounded storage quantity,
hidden zero quantity, wrapped legacy sum and wrapped legacy product. A compiler
error does not count as detecting an invalid model; every control must reach a
failing Hspec assertion. The seed, source/test hashes and output are retained.

The shared real-HTTP fixture checks invalid stored data, exact ordinary totals
and rollback of an overflowing cart update. It uses no provider endpoint.
This is property and boundary execution evidence, not a proof over all cart sizes,
all currencies or the whole checkout lifecycle. The compiler, Integer semantics,
PostgreSQL transaction adapter and finite fixture environment are trusted. Memory
exhaustion and arbitrary external SQL are excluded. No liveness/fairness claim is
made for these terminating local arithmetic operations.

Open database obligation: the schema checks each line but does not enforce the
aggregate across lines and parent. Application repair is not that database
constraint. An additive constraint migration and migration-era/fixture review
remain required; applied historical migrations must not be edited. General tax,
discount, rounding, JavaScript numeric precision, all other monetary writers and
provider finality remain separate obligations. The unused legacy Stripe writer
is still disabled at its public handler; this repair must not reactivate it.
