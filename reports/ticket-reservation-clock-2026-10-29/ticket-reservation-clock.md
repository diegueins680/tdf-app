# Sale and promotion windows at new ticket reservation

Parent requirement: `EVT-TICKET-001`.

`EVT-TICKET-RESERVATION-CLOCK-001`: a new direct, legacy Stripe or public
canonical ticket reservation must use a fresh PostgreSQL wall-clock sample taken
in a separate statement after its final admission row-lock wait. Before creating
inventory, redemption, order or ticket effects, the locked tier sale window and
any locked promotion validity window must admit that same sample. Existing
inclusive boundaries remain: start <= admissionTime <= end; absent endpoints
impose no bound. A newly elapsed window refuses with409 and no new-purchase
effects. This does not require commit or provider completion before the end.

Authority: on2026-10-29 the owner explicitly selected “Require validity when
inventory is reserved” after being shown the request-arrival alternative.
Existing `isTicketTierSaleOpen` and `validatePromoCodeDateWindow` establish the
inclusive boundaries. The former request-time behavior is not preserved as a
commercial entitlement; no durable reservation existed before admission.

The unmodified b827 baseline has seven actual-handler counterexamples against
the complete227-migration schema. An unchanged Event, Tier or Promo is locked,
the buyer is observed waiting in PostgreSQL, and the database clock is observed
strictly beyond the unchanged deadline before release. All five legacy cases
return200 and create paid-zero orders/sold3/tickets3; the expired-promo case
also consumes one redemption. Both public canonical cases create an order and
stock1, including a redemption under an expired promotion. No provider is called.

The shared clock reader uses a separate `SELECT clock_timestamp()` after the
already ordered admission locks. Request time, transaction-start time (`now()`)
and a wall-clock call inside a locking SELECT can predate waits. Direct creation
samples after Event/Tier; legacy Stripe after Event/terms/Tier/Promo; public
canonical after advisory/Event/hold cleanup/Tier/policy/Promo. Callers already
hold the Event/Tier locks which Inventory reacquires, so those participating
accesses do not introduce another writer wait after the sample. The same sample
checks promo dates and the existing inventory authority's current tier window.

Request/audit timestamps and existing hold duration remain unchanged. Canonical
hold cleanup remains before Tier/Promo locks to retain its existing deadlock
order. New date refusal occurs before stock mutation. Existing reuse exits and
stored orders are preserved. Canonical's pre-existing outer sale/promo checks
still precede reuse; this correction does not guarantee arbitrary expired
canonical replays. Legacy same-key promo replay after expiry retains its prior
order and redemption, as the history regression checks.

Verification obligations: real expired wait schedules must refuse without new
effects; equivalent future-window schedules must succeed; removing database
wall-time freshness must expose real failures. Inclusive endpoint unit tests,
existing inventory/snapshot/refund checks and complete schema regressions remain
required. Current execution receipts determine TESTED status. No mathematical
proof, SQL/Haskell refinement, model completeness or production guarantee is
inferred from passing tests.

Assumptions: valid initial rows, participating PostgreSQL READ COMMITTED writers,
all admission locks acquired before sampling, and database wall time as authority.
The sample is the logical admission point immediately before reservation effects;
it is not transaction commit time. Hostile DDL, arbitrary direct writers, old
binaries, clock manipulation, policy/terms interval changes, hold-duration
refresh, alternate isolation availability, HTTP authentication, external payment
and production rollout remain separate obligations. Current-head review,
current-main composition, hosted CI and protected FIFO integration are required.
