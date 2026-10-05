# ADR 0129: keep verified refunds and canonical accounting atomic

Date: 2026-09-16 UTC. Status: proposed implementation; activation remains blocked.
Depends on [ADR 0128](0128-refund-execution-fence.md) and draft PR #396.

## Context

Before completing held-refund recovery, the existing completion path must agree
with the canonical payment domain. `RefundStore.recordVerifiedRefund` previously
updated the refund, checkout, ledger, credit-note record and audit, but not the
payment intent. The attempt-to-intent trigger only synchronizes positive payment
progress, not refunds. An exact successful refund therefore left the intent
`captured` with zero refunded money. This was reproduced with synthetic records
in real PostgreSQL; it is not evidence of an incident in a merchant account.

The canonical refund transition also added two `Int64` values before checking
the captured limit. A counterexample returned a negative refunded balance after
overflow. The request/reservation guard repaired in #396 did not cover this
separate state-machine entry point.

## Decision

Keep completion in the existing caller-owned SQL transaction:

```text
lock original refund; exact succeeded replay returns without writes
  -> lock original checkout and attempt; verify immutable binding and payable state
  -> read committed successful-refund totals after acquiring those locks
  -> check checkout totals and the original intent's capture/refund totals
  -> apply PaymentRefundVerified to that intent and append its state history
  -> complete refund + checkout + ledger + credit-note record + refund audit
  -> commit all, or roll back all on SQL failure
```

No recoverable `Left` is returned after a successful intent transition. The
remaining writes either succeed or raise an SQL exception that escapes the
transaction. Callers must not catch that exception inside the transaction and
commit partial work.

The checkout lock serializes distinct partial refunds. Aggregate checks use a
fresh statement after acquiring the lock: a scalar subquery in a waiting
`READ COMMITTED` locking statement can retain the earlier statement snapshot.
The intent check is scoped to the original attempt, not a replacement payment.
Refund replay remains idempotent at the locked refund record; a numeric refund
event is never treated as generically replay-idempotent by the intent store.

Compare sums using unbounded `Integer` before materializing an `Int64` result.
Reject negative stored refunded balances. Full/partial classification and all
external currency/minor-unit contracts stay unchanged.

## Compatibility and operational consequences

Do not silently repair old drift. Inconsistent checkout or canonical totals, an
unmatched binding, a dispute, or an unsupported state holds the new completion
for reconciliation. This can reduce automatic completion availability; it must
not trigger a replacement refund POST or release a reservation.

Already-succeeded refunds are acknowledged without rewriting historical intent
totals. Legacy attempts without an intent remain unbound; no intent or fabricated
history is created. This increment does not backfill or reclassify old refunds.
It does not add a migration, API field, generated client, UI control or provider
HTTP call. Existing readers benefit from corrected *new* intent transitions.

The PayPal refund flag, independent approval, merchant/environment checks and
non-expiring execution fence remain unchanged. Keep affected refund commands
disabled until the full provider/account and recovery qualification is complete.
Drain older writers before rollout; rolling back the binary can restore drift.
Disable refund commands before rollback and preserve all newly appended evidence.

This is the accounting prerequisite, not the provider GET adapter or an operator
resolution command. Unknown-ID recovery, legacy failed resolution, canonical
backfill, dispute/refund coordination, seller/tax allocations and actual SRI credit
note issuance remain separate work. Do not advertise this as complete refund
reconciliation or certified accounting.

## Verification

See [the dated evidence record](../payments/refund-accounting-2026-09-16.md) for
exact commits, commands, red/green results, unchanged surfaces and pending gates.
