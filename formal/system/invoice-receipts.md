# Invoice receipt authority — PAY-INVOICE-001

This is the canonical boundary for `POST /receipts` and the receipt created by
`POST /invoices` with `ciGenerateReceipt=true`. A receipt records an invoice
snapshot. It is neither proof of payment nor SRI authorization. The historical
`lessons-and-receipts.yaml` API fragment is not this mounted API.

One invoice admits one receipt. Invoice ID is the idempotency domain across
currently authorized invoicing staff. On a replay, omitted/null overrides retain
the issued snapshot. Explicit buyer name, buyer email or nonblank notes must
match the normalized issued value; otherwise reject409. Blank notes normalize
to omission. An explicit currency must match the invoice, including on replay;
reject422 on disagreement. There is no foreign-exchange operation here.

Before issuance, validate each stored quantity >0, unit amount >=0, tax basis
points in0..10000, and line total = quantity*unit + floor(quantity*unit*bps/10000).
Convert to unbounded Integer before multiplication and aggregation. Header
subtotal, tax and total must equal the exact line aggregates, all within the
nonnegative Haskell Int range. Reject invalid historical data422; do not rewrite
financial history to make validation pass. Zero-price lines remain valid.

Current session and canonical role/module capability are required. Hold the
actual token, active role-assignment UUIDs and the active permission chain under
shared row locks through the transaction. Permission lookup uses the captured
assignment IDs. Lock the invoice FOR UPDATE before checking for an issued
receipt; lock existing invoice lines FOR SHARE. Receipt, lines and annual number
allocation commit or roll back together. The combined invoice/receipt path also
rolls back its new invoice and lines. Typed denial is raised within the SQL
transaction; HTTP conversion happens only after rollback. SQL constraint,
serialization, deadlock and amount overflow conflicts return fixed409. Unexpected
failures receive the fixed request500 boundary; asynchronous cancellation
propagates. Never expose SQL diagnostics or customer data in error bodies.

Annual `R-YYYY-NNNN` numbering uses one atomic counter upsert returning its new
value. Minimum display width is four digits; gaps are permitted. The additive
migration locks invoice then receipt writers before seeding from the maximum
existing issued numeric suffix, preserving larger retained counter values. It
runs in one explicit transaction. Unique invoice/number constraints and a
composite foreign key bind receipt currency and header amounts to its invoice.
Incompatible existing receipts stop migration; operator reconciliation and a
reviewed forward repair are required, never automatic deletion/relabeling.
Production must drain old writers before migration. Recovery code must honor the
counter and snapshot constraints; do not resume count-based legacy issuance.

The source state machine is recorded in `requirements.json`: absent -> issued
under current authorization and a valid invoice; issued -> same issued snapshot
on compatible replay. Changed replay, invalid snapshot and revoked authorization
have no effect. An interrupted transaction has no local effect. A committed but
unacknowledged receipt is recovered by replaying its invoice ID with current
authorization. Invoice creation itself has no external idempotency key; a blind
retry can create a second invoice and is not authorized by this receipt contract.

`InvoiceReceipt.tla` abstracts two actors, two invoices, two payload values, one
issuance/replay attempt per actor and one possible revocation. Counters are bounded
by those attempts. PostgreSQL transactions/locks are abstracted as serialized
admission and atomic commit; the model does not prove their implementation.
No fairness or liveness claim is made; denied attempts may stutter indefinitely.
Safety: unique receipt per invoice, unique allocated number, payload-bound replay,
and current authorization at the commit/response boundary. Four controlled
mutations independently remove invoice serialization, use a stale counter,
ignore replay binding, and ignore authorization; each must violate its named
invariant. This is bounded model checking, not a universal proof.

Actual HTTP/PostgreSQL checks cover admission, same/distinct-invoice races,
revocation after witnessed waits, failed-line rollback, invalid amounts, database
constraints and a legacy-writer migration race with an unlocked negative control.
QuickCheck covers defined finite valid amount domains; explicit Int upper-bound
and overflow examples exercise exact arithmetic. Exact candidate evidence must
be retained before delivery; source mapping alone is not conformance evidence.

Exclusions/open debt: complete fiscal/provider issuance lifecycle, session invoice
link atomicity, arbitrary database-superuser mutation, future invoice-line edit
APIs, payment capture/refund, universal receipt retention and production rollout.
No endpoint in this repair deletes issued receipts. Personal receipt fields stay
behind invoicing capability; financial retention policy remains separately open.

Primary rationale: PostgreSQL17 [row/table locks](https://www.postgresql.org/docs/17/explicit-locking.html)
and [ON CONFLICT](https://www.postgresql.org/docs/17/sql-insert.html) support local
transaction serialization. TDF adopts these local primitives without inferring
exactly-once provider effects. See `RES-RECEIPT-001` in the research register.
