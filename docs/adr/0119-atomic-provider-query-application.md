# ADR 0119: Caller-owned provider reconciliation transactions

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. Status: implemented in a dependent
draft after #353. No provider activation or deployment is authorized here.

## Problem and scope

PlaceToPay/PayPhone reconciliation was invoked from the durable callback inbox.
Its financial application called `transactionSave` at entry and on successful
branches, and `transactionUndo` on domain errors. These Persistent operations
commit/roll back the enclosing PostgreSQL transaction, not a nested transaction.
That boundary is unsuitable for composition with a future worker lease: a
pre-existing lock could be released before the caller commits its job outcome,
and a later caller failure could not undo already committed payment records.

This is a demonstrated transaction-composition defect, not evidence of a live
duplicate charge. The existing standalone callback path still used immutable
bindings and terminal-operation checks. This increment repairs the shared
application boundary before adding unattended missed-callback recovery.

## Decision

The caller owns the SQL transaction. An authenticated, adapter-parsed query is
applied under a local savepoint:

```text
Caller transaction (and any locks acquired before application)
  └─ revalidate immutable binding and exact query money/identity/certainty
     └─ savepoint: provider query application
        ├─ provider operation
        ├─ payment attempt + canonical intent/history
        ├─ checkout payment state
        ├─ balanced capture ledger + receipt
        └─ audit / unknown-status exception
     domain rejection → roll back only this savepoint
     success/pending  → release savepoint, NOT commit
  caller commits or rolls back all its work
SQL exception / interruption → escape to caller; outer transaction rolls back
```

`applyQueryResult` is an internal Haskell composition boundary, not a public API.
Its input must come from an authenticated query parsed against the stored
locator. It reloads the immutable binding and rejects changed identifiers or
money. It also rejects absent/different amount or currency, a different resource
ID, or certainty inconsistent with the typed status. These checks are defense in
depth; assembling an `AdapterResult` is not authentication and must never be
used to turn callback financial fields into evidence.

The wrapper no longer commits or rolls back the caller's transaction. Domain
errors return `Left` after rolling back local partial writes. Database errors
are not caught and converted into successful outcomes inside an aborted SQL
transaction. Pre-existing caller locks remain held through its commit/rollback.
Locks acquired inside a failed savepoint may be released; a future job lease
must therefore be locked **before** invoking this helper.

`processProviderEventIO` remains the production entry point. Its injectable
transport/clock variant allows contract tests without bypassing stored callback
trust validation, runtime configuration, immutable lookup or adapter parsing.
Production still uses the shared bounded/no-implicit-retry HTTP transport. Query
application and mismatch timestamps now use the response observation time, not
the earlier callback/query-start time. This is local observation time, not a
claim about provider settlement time.

## Official evidence and confidence

Accessed 2026-09-15 UTC. High confidence in documented contracts; no TDF merchant
account, provider sandbox or staging transaction was qualified by these reads.

- [PostgreSQL 16: rollback to savepoint](https://www.postgresql.org/docs/16/sql-rollback-to.html)
  and [release savepoint](https://www.postgresql.org/docs/16/sql-release-savepoint.html):
  savepoints isolate part of a transaction; releasing one does not commit the
  outer transaction. This supplies the SQL mechanism, not a certification of
  the application.
- [PostgreSQL 16: explicit locking](https://www.postgresql.org/docs/16/explicit-locking.html):
  locks acquired after a savepoint can be released on rollback to that savepoint;
  a caller's lease lock must predate the application savepoint.
- [PlaceToPay session query](https://docs.placetopay.dev/en/checkout/api/reference/session):
  authenticated `POST /api/session/{requestId}` returns session and transaction
  details. Synthetic tests preserve that structure, with independently generated
  identifiers, USD minor-unit money and no customer/card payload.
- [PlaceToPay notification](https://docs.placetopay.dev/en/checkout/notification/):
  the documented callback is not retried and should be acknowledged promptly.
  Missing a callback can therefore require independent session reconciliation.
- [PayPhone API Sale](https://docs.payphone.app/api-sale): authenticated sale
  lookup supports transaction/client references; its documented status-query
  limit is 30 per minute. Synthetic tests use the existing sale-query schema.

## Threat controls and explicit limits

- Caller failure, SQL error or thread interruption cannot leave this helper's
  payment, ledger, receipt or history committed independently.
- Concurrent successes converge on one posted capture ledger, one receipt and
  one paid audit. A late pending/declined/unknown result cannot downgrade a
  successful operation; conflicting success cannot overwrite confirmed no-charge
  evidence. These conflicts remain operator/retry cases, not a fallback signal.
- Expired checkout rejection rolls back the operation update too. It does not
  fulfill a released booking/ticket/stock hold or establish a no-charge outcome.
- Transport errors stay ambiguous/retryable. Mismatched provider money is
  dead-lettered with a redacted reconciliation exception. Callbacks cannot
  change payment status without the subsequent query.
- No new provider request, capture, refund, void, payout or recurring operation
  is introduced. Existing internal create-operation transaction boundaries are
  not generalized or silently changed by this patch.
- This is **not** a job-lease implementation. The existing inbox claim/result
  updates are still separate from financial application; terminal financial
  idempotency remains essential after a crash. A future worker must fence stale
  results and atomically commit its lease outcome with financial application.

## Next implementation gate: missed-callback recovery

The independent reconciliation worker is **not implemented or enabled** by this
ADR. It needs an additive durable job/lease schema, explicit false-by-default
environment flags, account/runtime qualification, bounded backoff/dead-lettering,
shared provider/account query budgets, and tests of lease expiry/revocation and
multi-process concurrency. Budget all callback and scheduled query consumers
together; a per-thread sleep does not enforce the documented PayPhone limit.
Confirm the provider's enforcement scope and conservatively coordinate all
consumers sharing credentials; the public page does not specify that scope.

Only known immutable create-operation resource bindings may be enqueued. Query
outside the database transaction; then lock/revalidate the current lease and
configuration before calling this helper inside the same transaction as the job
outcome. A delayed response from a superseded lease must have no financial effect.
Do not manufacture a verified webhook to schedule a query. Unknown-resource
ambiguous creates require a separately specified recovery path; elapsed time or
retry exhaustion never authorizes another charge. Late paid/expired checkouts
need explicit operator and fulfillment policy, not automatic stock reallocation.

An additional compatibility finding from the current PayPhone source review:
its Sale documentation labels status code `2` as canceled. The inherited adapter
maps it to `AdapterDeclined`, so new observations currently produce a failed
canonical intent with confirmed no-charge evidence. This patch preserves that
mapping, and the tests name the existing internal state, not an issuer-decline
guarantee. Correcting the classification requires a separate compatibility fix:
new cancellations should be labeled correctly while replays of previously failed
intents must not rewrite history or become permanent retries. This remains open.

## Compatibility, rollout and rollback

No schema, flag seed, historical backfill, wire contract or generated client is
changed. The existing migration chain and original provider/payment references
remain intact. Web/mobile/admin continue consuming the same canonical states.

For an authorized future rollout, keep providers disabled until the dependent
stack, callback replay tests and real sandbox qualification pass. Before/after
deployment compare pending operations, paid checkout/intent agreement, posted
capture totals, receipts and open exceptions without exporting provider payloads.
If rollback is necessary, disable dependent recovery work and restore the prior
code without deleting records. The prior version restores the composition defect;
do not run a lease-based caller on it. No production action is part of this work.

See [verification and remaining blockers](../payments/reconciliation-atomicity-2026-09-14.md).
