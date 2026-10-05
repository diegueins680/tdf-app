# ADR 0123: Preserve capture evidence and resource-binding replay

Date: 2026-09-15 UTC. Status: implemented; local regression verification passed.
Dependent on #374. No provider activation or deployment is authorized.

## Findings

The shared `CheckoutStore` is used by Datafast/PayPal product handlers and
PlaceToPay/PayPhone reconciliation. Its capture replay currently updates an
already successful attempt and paid checkout with the new observation timestamp.
Its receipt check compares only amount/currency, allowing a same-value receipt
with another adapter or external reference. Missing historical evidence can be
silently reconstructed through the first-capture path. Separately, an exact
resource-binding replay writes the supplied pending stage even when the attempt
has already succeeded. Thus the binding path can invalidate a valid paid attempt
before capture verification sees it.

These are internal replay defects, independent of remote idempotency support.
The late-payment trace also confirmed that a first success after checkout expiry
is rejected; changing that requires separate inventory and financial-review
decisions, not automatic reopening of released seats or bookings.

## Decision

Preserve the existing caller-owned transaction and immutable binding validation.
Treat a coherent, already-posted capture as an acknowledgement only: return
`Right False` (not newly paid) without any financial update, receipt issuance,
audit/history append, or fulfillment transition. Require the original successful
attempt, full recorded checkout payment, posted attempt capture ledger and an
exact matching payment receipt. Post-capture refund/dispute checkout states may
acknowledge that original capture without returning the checkout to paid.

Check receipt amount, currency, canonical receipt number, adapter and external
resource together. A voided original receipt is still historical capture evidence,
not permission to issue a replacement or represent it as a valid fiscal document.
Missing, conflicting or partial evidence must require reconciliation instead of
being backfilled by a repeated provider result. First captures retain all
authoritative-provider, binding, currency and amount gates.

Binding replay must not rewrite a terminal attempt or its timestamps. A binding
first validates and locks the exact checkout/attempt pair before any insertion.
It can advance an unresolved attempt monotonically; a stage observation is not
authority to resurrect failed/canceled/expired/review states or regress a
processing attempt. Update the checkout only for a real permitted attempt-stage
change. Existing immutable binding fields and stored references are never changed.

## Evidence and limits

Official sources accessed 2026-09-15 UTC:

- [PayPal REST idempotency](https://developer.paypal.com/api/rest/reference/idempotency/):
  supported endpoints correlate retries with the same request ID; support and
  retention depend on the endpoint. This does not make TDF's database effects
  idempotent automatically (engineering inference).
- [PlaceToPay session query](https://docs.placetopay.dev/en/checkout/api/reference/session/)
  and [PayPhone API Sale](https://docs.payphone.app/api-sale): authenticated
  queries expose provider transaction state; neither establishes TDF inventory
  availability or authorizes a replacement charge.
- [PostgreSQL 16 explicit locking](https://www.postgresql.org/docs/16/explicit-locking.html):
  row locks persist until transaction completion, subject to savepoint rollback.

High confidence in the documented endpoint/locking semantics and inspected local
code; no merchant onboarding or sandbox qualification is inferred. An attempted
Open Payment Platform server-to-server documentation URL was unavailable through
the browser; no new Datafast-specific API behavior is based on that attempt.

No new migration, historical rewrite, wire enum, checkout method, refund execution
or native UI is intended. Regression fixtures simulate financial states; they are
not real provider refunds, chargebacks, settlement evidence or regulatory approval.
External notifications and complete domain fulfillment require their own tests;
`Right False` only establishes that this core did not newly mark a checkout paid.

## Rollout and rollback

Deploy only after the full dependent stack is reviewed and authorized staging is
qualified. Drain old callback/verification writers before cutover: an old binary
can still downgrade an attempt during binding replay. Preserve mismatched legacy
receipts and incomplete captures for explicit reconciliation, not automatic repair.
Rollback reintroduces those defects, so pause affected consumers and prefer a
forward repair. Never clear history, replace an ambiguous attempt or reissue a
receipt to make replay succeed.

See the [verification record](../payments/capture-replay-integrity-2026-09-15.md)
for reproduced failures, final source/test evidence and staging blockers.
