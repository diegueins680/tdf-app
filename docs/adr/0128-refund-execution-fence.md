# ADR 0128: fence refund execution and preserve uncertain reservations

Date: 2026-09-16 UTC. Status: proposed implementation; production activation blocked.

## Context and threat

`RefundStore.approveRefundForProcessing` previously granted an execution permit
for an already-processing refund. `approveServiceRefundHandler` uses that permit
to POST to the original PayPal capture. The persistent refund UUID remains the
provider idempotency key, but provider retention is not an indefinite local lock.
Concurrent or sufficiently delayed approval retries must not issue another POST.

The two callers of `recordRefundFailure` are verification mismatch and an
unrecognized/non-completed PayPal refund status. Neither call proves that funds
were never returned. Previously the write set `failed`, excluding the amount
from checkout and line reservations and allowing cancellation/re-execution.
Historical `failed` records therefore cannot safely be interpreted as no-refund.
This is a code finding, not evidence of a real duplicate refund or incident.

## Decision

Use the existing transaction and locked refund row as a durable, non-expiring
execution fence. Only `requested` or `approved` may acquire the first permit,
under the existing independent-approver rule. `processing`/`succeeded` replays
return the stored record without another permit. `failed` requires reconciliation;
it cannot be reissued or cancelled by these application commands. Pre-execution
`requested`/`approved` cancellation remains available.

```text
requested -> approved -> processing -> succeeded
    |            |           |
    +------------+           +-- timeout / mismatch / unknown -> processing (held)
    v
cancelled                 historical failed -> held for reconciliation
```

The legacy function name `recordRefundFailure` is retained for source
compatibility, but means a failure to verify completion: record a diagnostic,
not a terminal financial conclusion. Keep the amount reserved at checkout and
line level. Do not overwrite existing provider references. Do not store arbitrary
provider status text as a diagnostic code. Exact verified completion retains
the existing ledger/credit-note path and duplicate-evidence protection.

Perform balance comparisons using unbounded `Integer` arithmetic before accepting
an `Int64` minor-unit request. Public amounts and persisted types are unchanged.
Overcommitted or negative stored balances fail closed; no rounding or float is used.

## Operational and compatibility consequences

This is a conservative execution/reservation repair, not automatic refund
reconciliation. A crash after claiming but before sending can leave a hold even
if no remote refund happened. Do not clear the hold or click another provider to
recover it. Query the original provider resource under the original merchant and
environment; if its identity cannot be established, escalate using the original
capture/refund request UUID. No unattended POST retry is permitted by this change.

Historical `failed` statuses and all references remain unchanged. They count as
reserved until an evidence-backed resolution is implemented and reviewed. Do not
mass-relabel them, infer failure from age/404, or use direct SQL cancellation.
Genuine no-refund outcomes also require review for now. Liveness, unknown-ID
discovery, verified failure release and an operator resolution command remain
explicit follow-up work, not a successful sandbox test.

No schema migration/backfill is introduced. Existing SQL checks still allow some
legacy transitions; application commands are narrower. Restrict database writes
to approved operations. Drain old refund handlers before rollout: an old binary
can still reissue a `processing` record. Roll back by disabling affected refund
commands and preserving holds; do not restore old execution behavior while enabled.

Merch administration follows the same restriction: no cancel/approve controls for
`failed` or `processing`, with an explicit reconciliation hold in English/Spanish.
An execution-disabled flag is not evidence that no money moved; the UI now directs
staff to original refund evidence. Requested/approved pre-execution review remains.
These displays do not enable the unqualified merch refund adapter or add a native
admin workflow.

Keep `checkout.paypal.refunds` disabled unless all independent merchant, sandbox,
reconciliation, legal/accounting and activation gates have been satisfied. This
increment does not qualify event/merchandise/marketplace refund settlement,
seller-liability allocation, SRI credit notes or canonical-intent refund projection.
An application credit-note record is not proof of SRI issuance.

## Primary sources

Accessed 2026-09-16 UTC. High confidence in documented API mechanics; no account,
Ecuador merchant availability, live fees or sandbox outcome inferred.

- [PayPal idempotency](https://developer.paypal.com/api/rest/reference/idempotency/):
  retention depends on the API, and concurrent requests with the same key may
  not both succeed. Local policy therefore must not assume indefinite replay safety.
- [Refund captured payment](https://developer.paypal.com/api/payments/v2/captures-refund):
  original capture endpoint, amount for partial refunds, request idempotency and
  represented responses; in-progress request conflicts are documented.
- [Show refund details](https://developer.paypal.com/api/payments/v2/refunds-get):
  a read-only lookup exists for a known refund ID. This ADR does not claim it is
  already wired into the application or that it resolves an unknown ID.
- [Official Payments v2 specification](https://github.com/paypal/paypal-rest-api-specifications/blob/main/openapi/payments_payment_v2.json):
  refund statuses include `CANCELLED`, `FAILED`, `PENDING` and `COMPLETED`.
  An unfamiliar value or inconsistent money is not proof of no-refund.
