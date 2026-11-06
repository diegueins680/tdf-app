# Legacy refund approval with historical duplicate identities

Parent requirement: `EVT-TICKET-001`.

`PAY-LEGACY-REFUND-HISTORY-001`: an unbound legacy approval may claim a pending
request only when the order's complete refund-request history contains exactly
that identity. Ambiguous history returns a conflict before claiming or calling
Stripe and preserves every request, observed provider ID, ticket and inventory row.

The existing legacy creation rule refuses any previous request, including a
rejected one. Canonical allocated refunds intentionally support successive
partial requests. Thus counting only pending requests, picking a historical
winner, or imposing a global unique order constraint would change established
behavior. This correction applies only to the unbound legacy approval branch.

The preceding per-request execution fence does not prevent two historical request
IDs from each claiming and making a refund POST for one order. Actual-handler
counterexamples make two sequential or concurrent callbacks return an uncertain
response: both requests remain processing, both POSTs have already happened, and
the order remains paid. Other rejected, processing or approved history is also
ignored by the previous implementation. A later inventory guard cannot undo a POST.

Approval now takes the same PostgreSQL event then order locks as creation. Within
an explicit READ COMMITTED transaction it reads all request keys, requires exactly
the target, rechecks that the target is unbound, checks the current event/order
relationship, and executes the existing pending-only conditional claim. The
transaction commits before provider IO. A claim loser remains a conflict.
A binding appearing after initial routing is refused and requires reloading.
Canonical-bound requests route before this guard; their allocated lifecycle is
preserved. No request history is deleted, rejected, reset or automatically chosen.

READ COMMITTED is explicit so a request-history query after a lock wait sees a
competing transaction's committed history even if the session default is REPEATABLE
READ. Request-history reads do not acquire request-row locks. In particular, do
not introduce an unconditional request FOR UPDATE after the order lock without
reconciling the completion path's request-before-order updates. The pending-only
CAS retains the existing execution boundary.

Assumptions: PostgreSQL and transactional rollback operate as specified; new
legacy creation participates in the event/order lock protocol; historical
identities are not concurrently deleted or reparented by direct writers. Legacy
completion, rejection and approval preserve identity. Mixed old binaries or
out-of-protocol writers can bypass this guarantee. The test-only SQLite query
fallback is compatibility support, not PostgreSQL concurrency evidence.

Regression cases exercise the actual handlers against the full migration schema:
sequential and concurrent duplicates, every existing historical status, a duplicate
committed during a lock wait, and canonical successive partial replay. Existing
single-identity concurrency, uncertain-provider, exact-completion and inventory
cases remain required. Synthetic provider callbacks never contact Stripe.

The finite model enumerates single and double histories over pending, rejected,
processing and approved, two approvers, committed claims, uncertain POST outcomes
and retries. It checks ambiguous history never posts and at most one modeled POST.
Omitted-history and pending-only mutants must produce counterexamples. It abstracts
SQL isolation and locking into an atomic claim and does not prove their refinement.
Implementation evidence is TESTED; the finite design is BOUNDED_VERIFIED. No new
full-handler mathematical proof or external payment authenticity is claimed.

Historical duplicate reconciliation, original-payment verification, partial-money
ticket allocation, admission during uncertain processing, stored authorization
changes after the initial manager check, provider recovery, and all direct database
writers remain separate obligations. This fail-closed guard can require manual
reconciliation of legitimate historical legacy activity; it never supplies a new
execution identity to resolve an uncertain outcome.
