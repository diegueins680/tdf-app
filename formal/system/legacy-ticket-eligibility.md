# Current event eligibility for new legacy ticket holds

Parent requirement: `EVT-TICKET-001`.

`EVT-LEGACY-HOLD-ELIGIBILITY-001`: a new direct order or legacy checkout hold
must satisfy the existing event purchase predicate under the Event lock, before
stock, order or ticket effects. Ordinary event lifecycle/privacy updates use the
same lock. If closure wins, admission refuses. If a free purchase wins, the
ordinary event edit does not erase its earlier admitted order/tickets; later
payment authorization and hold rollback retain their separate contracts.

Authority is the existing `validateTicketPurchaseEventEligibility` contract
(private metadata refuses403; missing purchase capability refuses409), normal
organizer lifecycle/privacy editing, and canonical checkout's existing locked
eligibility check. This is enforcement of established behavior, not a new event
publication policy or an organizer/admin override.

Both legacy paths previously checked eligibility before their inventory
transaction. Six actual-handler counterexamples move an initially on-sale event
to postponed, cancelled or private through the normal organizer update endpoint,
then let a waiting buyer proceed. Each persists one paid free order, three issued
tickets and sold3 after that update commits. A barrier only locks an unchanged
Event row; it does not mutate lifecycle, privacy or stock. Both legacy purchase
entry points are exercised without provider or SMTP credentials.

The local `lockTicketPurchaseEvent` wrapper now reads the locked current event
and invokes the existing capability/metadata predicate. Direct creation returns
its refusal from the SQL transaction and maps it outside; there are no order
writes to commit on this path. The Stripe new-hold transaction throws the same
ServerError through its existing rollback/catch path before terms, promo, stock
or order effects. Current404/403/409/500 classifications are retained where
applicable. Stock reservation remains inside order creation through the shared
inventory authority.

Lock order remains Event, then terms policy for Stripe, then Tier. No provider or
email IO occurs under these locks. The outer fast checks remain. This new guard
only applies to new holds; existing checkout reuse, settlement and recovery keep
their own contracts. In particular, existing initial route validation may still
refuse a closed-event retry; no newly successful closed-event retry is claimed.
Canonical checkout and `TDF.Ticketing.Inventory` are unchanged: inventory does
not become an authority for publication eligibility.

Assumptions: PostgreSQL transactions and the ordinary event writers share the
Event row lock; current workflow capability definitions remain stable through
the admission transaction. Those catalog rows are re-read, not universally
locked or frozen. Direct SQL/out-of-protocol writes, concurrent capability-catalog
governance, all workflow states, price/owner snapshots and database-time sale
cutoffs are outside this correction. Default READ COMMITTED is tested; alternate
isolation availability is not established.

Actual-handler tests cover both routes, each of postponed/cancelled/private,
and both lock orderings. Closure-first must return409/403 with zero effects;
purchase-first must retain its valid order and tickets after the normal update.
These are TESTED implementation obligations, not an HTTP-authentication or
provider-payment end-to-end proof.

The BOUNDED_VERIFIED model abstracts eligibility as open/closed/private, with
six initial/mutation combinations, one buyer and one editor. It checks
NoNewHoldAfterClosure and RefusalHasNoEffects, while reaching retained earlier
orders. Cached eligibility, omitted admission locking and a returned refusal
that commits prepared effects must each produce counterexamples. Only durable
hold effects are counted; temporary writes rolled back by the Stripe exception
path are not modeled as committed violations. The complete workflow graph,
capability-catalog mutation and Haskell/SQL refinement are not proven.
