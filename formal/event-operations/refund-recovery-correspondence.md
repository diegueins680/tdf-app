# Bounded refund recovery correspondence

Authority: ADRs 0128–0130 and the preservation of held refunds, exact provider binding, single execution and atomic canonical accounting described in `docs/payments/held-refund-query-2026-09-16.md` and `docs/payments/refund-accounting-2026-09-16.md`.

`RefundRecovery.tla` models two refunds of one minor unit against two captured units, two concurrent query workers, held/verified provider results, and authority revocation/restoration between query admission and application. All reachable interleavings in that finite configuration are checked. This is bounded safety analysis, not whole-program refinement or a claim about all provider behavior. No liveness or eventual provider response is assumed.

Implementation mapping:

- `Execute`: execution admission and held-balance fencing in `TDF.Commerce.RefundStore`; an ambiguous processing refund is queried rather than executed again.
- `Query`: `reconcileKnownRefund` admission/quota reservation before external HTTP in `TDF.Commerce.RefundReconciliation`.
- `Apply`: the second locked transaction in `reconcileKnownRefund`, which reloads the target, rechecks runtime/database authority and binding, returns `already_completed` for a succeeded record, keeps held results processing, and calls `recordVerifiedRefund` only for current verified completion.
- Atomic `effects/refunded/state` updates: `recordVerifiedRefund` plus the reviewed canonical synchronization trigger in `2026-09-16_payment_intent_refund_sync.sql`. The model abstracts their transaction as one step; the real PostgreSQL provider-retry suite checks duplicate accounting and concurrent application against the composed schema.
- `ToggleAuthority`: changes to the conjunction of current provider/runtime/feature eligibility. The model asserts authority at the instant of application, not that subsequent revocation invalidates immutable historical receipts.

The model assumes exact amount/currency/merchant/resource validation has produced `verified`; it does not prove parsing, signatures, HTTP authenticity, session authorization, quota fairness, crash recovery or database isolation. Those boundaries are exercised by `RefundRecoverySpec`, `RefundSafetySpec`, `ProviderRetrySpec`, and their real PostgreSQL harness. The separate source-derived arithmetic verifier covers the recognized Int64 capture/refund arithmetic fragments.

Three negative controls independently permit another execution of a held refund, apply a stale verified result after another worker has completed accounting, or skip the post-HTTP authority check. The existing runner requires each mutated configuration to produce the named invariant violation with TLC exit 12; a parse/tool failure is not accepted as a counterexample.

## Hosted service payment completion

`HostedServicePayment.tla` extends this transaction-boundary verification to hosted mixing/mastering payments: two callback/query workers, both valid and invalid domain bindings, and an operator advancing fulfillment after payment. `Apply` maps to the savepoint in `ProviderReconciliation.applyQueryResultCorrelated`, canonical `Checkout.recordVerifiedPayment`, and the locked `synchronizeServiceOrder` update/audit. PostgreSQL `hostedServiceOrderSpec` exercises actual concurrent callbacks, caller rollback, mismatched order/amount, and fulfillment-preserving replay against the real service-storefront migrations. This finite model abstracts successful SQL transactions as atomic steps; it does not establish SQL isolation or provider authenticity. Three negative controls remove order synchronization, duplicate the paid audit, or regress fulfillment on replay. Browser pending recovery is separately covered by `HostedProviderCheckout.test.tsx`, including an unrelated active session coexisting with the original transmitted request and unchanged idempotency key.
