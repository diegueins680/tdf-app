# ADR 0121: Preserve terminal no-charge history on reconciliation

Date: 2026-09-15 UTC. Status: implemented; local database verification after #363. No provider
activation, live transaction or deployment is authorized by this ADR.

## Findings and evidence

PayPhone's [API Sale response table](https://docs.payphone.app/api-sale) labels
status `2` canceled, while a later paragraph calls the same code rejected/not
effective. Its [notification reference](https://docs.payphone.app/notificacion-externa)
also labels `2` canceled. Sources accessed 2026-09-15 UTC: high confidence in the
published numeric mapping and no-completed-charge meaning, but the exact reason
or actor is not established by this code. Merchant sandbox qualification and
provider confirmation of reason semantics remain required. Do not infer issuer
decline, voluntary buyer cancellation, a refund, or a void from status `2`.

The inherited adapter maps `2` to `AdapterDeclined`. Simply changing that to
`AdapterCancelled` would allow the shared intent state machine to transition a
historical failed intent to canceled, rewriting its recorded classification.
The generic customer cancellation transition is not itself changed here.

A second defect exists in shared PlaceToPay/PayPhone reconciliation: repeated
no-charge results reuse `recordPaymentFailure`, updating the old attempt's error
and timestamps, appending another failure audit, and setting an awaiting or
processing checkout failed. That checkout may already belong to a legitimate
new attempt created after the first confirmed no-charge outcome. Financial
operation idempotency alone therefore does not make this path idempotent.

## Decision

Use `AdapterCancelled` for newly queried PayPhone status `2`, retaining
`ProviderConfirmedNoCharge` only after authenticated server-side lookup with
exact immutable resource, merchant reference, amount and currency matching.
Callbacks remain hints; no callback status or browser parameter is charge
evidence. Unknown codes remain ambiguous and cannot release fallback.

After the operation outcome is validated and locked, inspect the bound intent
and attempt under the existing caller-owned SQL transaction/savepoint:

| Existing financial state | Result of a confirmed no-charge query |
|---|---|
| Coherent failed/canceled intent and failed/canceled attempt, zero authorized/captured/refunded money, no posted ledger for that attempt | Return processed without changing intent, attempt, checkout, financial history or its existing failure label. |
| Coherent unresolved intent/attempt with zero money and no posted ledger | Apply the first terminal outcome once. New cancellations use canceled intent and attempt states. |
| Money/ledger evidence or incoherent/unsupported terminal state | Reject for reconciliation; never silently relabel or clear money. |

The query still validates current provider evidence. This is not accepting a
failed database row as proof of no charge. A previously ambiguous operation can
be resolved by authenticated matching evidence; any existing coherent terminal
financial label remains unchanged. Success/conflicting no-charge operation
guards remain intact.

Canceling a payment attempt does not cancel its order. A first verified
cancellation leaves an active checkout retryable (`failed`), while the intent
and attempt are `cancelled`. A closed/paid/expired checkout is not reopened.
Replays never write checkout state, so a newer processing/paid attempt remains
untouched. Historical canceled intents with legacy failed attempts remain
compatible; no bulk data migration or backfill changes them.

Existing public session state remains `confirmed_no_charge`, and customer copy
continues to say no charge was completed. It does not assert who canceled or
that an issuer declined. No new wire enum, generated client or payment method
activation is necessary for this classification repair.

## Transaction, security and rollout constraints

Reuse [ADR 0119](0119-atomic-provider-query-application.md)'s savepoint ownership
and [ADR 0120](0120-durable-provider-query-recovery.md)'s job fencing/budget gates.
Row locks and caller rollback behavior follow the
[PostgreSQL 16 locking contract](https://www.postgresql.org/docs/16/explicit-locking.html),
accessed 2026-09-15 UTC. No secret, raw provider response or card data is persisted
by this change. All test credentials/responses are synthetic.

No new migration, provider mutation, cancellation endpoint or automatic retry
is introduced. Existing audited schema and immutable references remain intact.
Retain the full reviewed migration chain, including the budget schema required
by callback queries. Drain old no-charge consumers before an authorized rollout:
an old binary can still relabel a new canceled attempt as failed or alter a
newer checkout. Rolling back to that binary reintroduces this replay defect;
pause the affected consumers and prefer a forward repair, preserving all rows.

Real provider cancellation/refund/void execution, unknown-resource recovery,
late-paid fulfillment, safe dead-letter administration, settlements and seller
payouts remain separate work. This ADR makes no legal/accounting/PCI claim.
See the [verification and rollout record](../payments/no-charge-replay-2026-09-15.md)
for exact source commits, executed tests, interrupted runs and activation blockers.
