# Legacy refund request creation transaction

Requirement: `EVT-TICKET-001`.

- `PAY-LEGACY-REFUND-REQUEST-001`: participating legacy request submissions for
  one order cannot create more than one request after observing absence.
- `AUTH-LEGACY-REFUND-REQUEST-001`: current organizer or buyer authority remains
  valid through insertion; unrelated administrators do not acquire an override.
- `PAY-LEGACY-REFUND-REQUEST-SNAPSHOT-001`: insertion uses the locked current paid
  order, matching event, valid amount, currency and a rechecked routing classification.

The previous handler checked for an existing request and inserted in separate
transactions. Two submissions could both observe absence. The actual PostgreSQL
counterexample creates two pending requests for one order, each independently
eligible for the later per-request execution fence. Additional controlled cases
commit a paid-state, buyer, amount or organizer change after the initial reads
but before the old INSERT; the stale snapshot still authorizes insertion.

The legacy branch now uses one explicit READ COMMITTED transaction, locking
event then order as the canonical ticket operations do. It validates the locked
current rows, rechecks routing, reads existing requests and inserts at most one.
The lock is held through commit. The returned entity and currency come from this
transaction; no provider IO occurs while locks are held.

[PostgreSQL 16 isolation semantics](https://www.postgresql.org/docs/16/transaction-iso.html)
give each READ COMMITTED statement a new committed snapshot. The existence query
after a lock wait therefore sees the preceding request's committed insert.
Merely locking an unchanged row inside a REPEATABLE READ transaction would not
refresh that transaction's snapshot. The implementation chooses the isolation
level with the pinned
[Persistent runner](https://hackage-content.haskell.org/package/persistent-2.17.1.0/docs/Database-Persist-Sql.html#v:runSqlPoolWithIsolation),
instead of depending on the database session default.

Existing behavior is preserved: a paid order with any prior request is refused,
including a rejected request. The missing amount defaults to the current order
total; an explicit positive partial amount is still accepted up to that total.
The canonical PayPal/bank-transfer wrapper keeps its successive allocated-refund
behavior. No global unique constraint is added to the shared request table.

Routing classification means canonical runtime presence and succeeded-provider
codes. A change from the initial classification returns a conflict and requires
reloading; the shared loader avoids two independently maintained definitions.
Commerce runtime/attempt rows are re-observed, not locked by this change; no
universal freeze of provider state through commit is claimed. This does not
verify original payment authenticity, merchant identity or every possible
concurrent financial writer. The stored paid order remains the legacy
money authority. Writers that bypass the locking protocol and pre-existing
duplicate requests remain outside this correction; no history is deleted.

Actual-handler regressions use the full production migration manifest. A SHARE
table lock forces both old existence checks to finish before either INSERT can
commit; the repaired waiter instead waits on the event/order lock. Controlled
mutations independently distinguish buyer and organizer authority. Compatibility
cases retain full/default/partial amounts, amount refusals, organizer support and
duplicate refusal. The finite two-requester model checks current eligibility and
singular creation, including omitted-lock, stale-snapshot and stale-eligibility
controls. Implementation evidence is TESTED; the design is BOUNDED_VERIFIED,
without a Haskell/SQL refinement or complete refund workflow proof.

Stripe partial-money-to-ticket allocation, admission during uncertain processing,
provider execution/recovery and migration of old duplicate requests remain
separate obligations. Creating a pending request does not establish a payment,
refund, inventory release or accounting entry.

The existing SQLite visibility harness retains its assertions and uses the
backend-aware query form without unsupported FOR UPDATE syntax, as existing
handlers do. It is compatibility evidence, not evidence of PostgreSQL row-lock
concurrency. The explicit locking guarantee above is scoped to PostgreSQL.
