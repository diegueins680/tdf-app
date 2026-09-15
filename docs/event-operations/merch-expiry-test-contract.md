# Canonical merch expiry: executable test contract

This contract repairs verification of an existing dependency, not event-specific commerce or new
financial behavior. It precedes the test change on `test/merch-expiry-checkout-contract`, based on
PR 351 (`192f9578665109e52d28a96c772e3307bdc684e5`). Production SQL, worker, manifest and flags stay
unchanged. Existing source evidence:

- `2026-09-07_artist_merch_storefronts.sql:935`: expiry updates eligible canonical checkouts and
  returns the number of updated **checkouts**, including an eligible checkout without a stock hold.
- `merch_apply_checkout_status`: the checkout update triggers release of active inventory and
  canonical holds in the same transaction; consumed reservations are not released.
- `TDF.Commerce.MerchReservationWorker`: comments describe checkout expiry; its metric is explicitly
  `expiredCheckouts`. No consumer interprets the result as a released-item quantity.
- `docs/artist-merch/DESIGN.md` and `OPERATIONS.md` describe the same checkout-first operation.

## Operation and fixture bounds

`merch_release_expired_reservations(at)` selects `domain_type='merch_order'`, status in
`holding/awaiting_payment/processing/failed`, and `expires_at <= at`. Its result is the cardinality
of that selected set, not the number of active holds or the sum of held units. PostgreSQL owns the
transaction and trigger effects. This test invokes SQL only, in the owning disposable database;
it does not start a worker, call a provider or change production.

At this point the existing fixture has one paid checkout with four consumed units and two holding
checkouts sharing one deadline. The preceding competing-reservation test left one of those with
one reserved unit; the other has no reservation. Therefore expiry returns **2**, releases **1**
unit, and leaves **4** sold units. Assert this fixture shape explicitly so later fixture changes
cannot silently alter the meaning of the expected count.

| ID | Preconditions / operation | Required postcondition |
|---|---|---|
| MX-01 | Tick one microsecond before the two pending checkouts' common deadline | Return 0; all captured checkout/order/inventory/hold/payment/audit rows unchanged |
| MX-02 | Tick exactly at the common deadline | Return 2; both checkouts expired, both pending orders failed; exactly one held unit released in both inventory and canonical hold records |
| MX-03 | Existing paid checkout and consumed reservation | Payment/order/consumed evidence unchanged; reserved stock 0, sold stock 4 |
| MX-04 | Retry at the same deadline, then a later instant | Return 0 each time; complete captured projection/history unchanged, including versions/timestamps |
| MX-05 | Try to reserve again using an expired checkout and its otherwise valid deadline | Reject at the canonical checkout eligibility guard; no reservation, stock or history mutation |

An assertion failure aborts the SQL transaction and fails the owning migration suite. There are no
catch-all success paths, relaxed production constraints, fake ledger entries or provider callbacks.
The paid checkout must survive these checks so the existing downstream refund/settlement/rollback
tests can continue unchanged.

## Formal traceability and limitations

EO-033–036/EO-041–044/EO-055–058 map to the existing `ReservationRace`, `ContractPayment` and
`OperationalLiveness` finite abstractions, and to these concrete SQL assertions for the existing
checkout projection. Those bounded models do not prove SQL row counts, timestamp precision,
transaction isolation, or every legacy commerce transition. MX-01–05 are executable refinements,
not new mathematical proof claims. No feature implementation changes in this test-only increment.

This fixture covers two holding checkouts, one paid checkout and one finite variant. It does not
prove every status/domain, worker scheduling fairness, multiple expiry replicas, or a race between
late verified payment and expiration. In particular, a late **verified** payment requires separate
stock/financial reconciliation analysis; the existing paid-write trigger's evidence check is not
itself a proof that an expired allocation can safely be consumed. Keep providers disabled until
that owning-domain review and sandbox verification are complete. These assertions are not a
claim that all event hiring, booking or settlement requirements are finished.
