# Financial snapshots at new legacy ticket admission

Parent requirement: `EVT-TICKET-001`.

`EVT-LEGACY-PRICE-SNAPSHOT-001`: the tier price and case-insensitive currency
used to calculate a new legacy order must still match the current tier under
its lock before reservation/order/ticket effects. A changed quote returns409
and commits no hold effects. Existing order snapshots remain valid after later
tier edits; replay retains the stored financial snapshot.

Authority is the existing direct-purchase free-tier pricing restriction and
ADR0112, normal organizer `updateTicketTier`, and the canonical new-runtime
price/currency correspondence check in
`tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql`. The latter compares
the current tier's price and uppercased currency with the new immutable snapshot.
It does not require historical orders to match every later tier edit.

Both legacy routes previously computed their order from a tier loaded before
waiting for Event/Tier locks. Four actual-handler counterexamples hold an
unchanged Event, start an ordinary buyer and observe it waiting, then commit a
normal organizer tier-price0→1 or currencyUSD→EUR edit. On release both routes
return200 and persist one paid0/USD order, sold3 and three issued tickets. No
test-side price/currency mutation, provider credentials or email configuration
produces these failures.

The shared `lockTicketPurchaseTierPricing` checks current event binding, exact
unit price and uppercased currency under the Tier lock. Direct creation invokes
it after current Event eligibility. Stripe invokes it after the current terms
share lock, before the existing inventory reservation. Lock order remains
Event→terms when applicable→Tier, held through order commit. Refusals have no
reservation/order/promo/ticket effects; no provider or email IO is under locks.

Only the new-order branch uses this fence. It does not reprice existing holds
or move financial policy into `TDF.Ticketing.Inventory`. Name, position, stock,
capacity and updatedAt are not financial identity; harmless edits and current
stock validation retain their existing authorities. Initial route checks may
still refuse an existing retry for their own reasons; universal retry success
is not claimed.

Regression obligations cover the four actual failures, successful admission
after an unrelated name/position edit, earlier purchase followed by a normal
price/currency edit, and free idempotent replay retaining the old order/currency.
The latter is historical preservation, not a second newly priced purchase.
Two additional Tier-barrier schedules queue the real editor first and buyer
second. Removing only the shared financial read's FOR UPDATE, while retaining
Inventory's lock, makes both admit paid0 orders after the price edit. The new
assertions reject this controlled implementation fault; the correct source is
restored and hash-checked before final qualification. A fresh read followed only
by a later inventory lock is insufficient to bind the financial snapshot.
Both buyer/organizer are ordinary fixture principals. These are TESTED handler
and database properties, not HTTP-authentication or provider-payment proof.

The BOUNDED_VERIFIED design model has one buyer and one tier editor, two prices
and two currencies, twelve distinct initial/target pairs,216 states and216
transitions. FinancialSnapshotMatchesAtAdmission counts mismatch at admission,
not a later legitimate historical difference. RefusalHasNoEffects concerns
durable effects, not writes rolled back by an exception. Twelve retained
historical states are reached. Four controls omit price, currency, Tier locking
or commit prepared effects on refusal; each must yield its intended counterexample.

Assumptions: valid initial financial fields, participating tier writers and
PostgreSQL transactions. Default READ COMMITTED is qualified; alternate-isolation
availability, mixed old binaries, out-of-protocol writes, current database-time
sale cutoffs, promo/policy snapshot governance, provider recovery and full
Haskell/SQL refinement remain separate obligations. Historical financial data
is not rewritten by this correction.
