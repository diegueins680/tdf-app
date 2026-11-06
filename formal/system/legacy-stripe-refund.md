# Legacy Stripe ticket refund execution

Requirement: `EVT-TICKET-001`. Narrow invariant:
`PAY-STRIPE-REFUND-EXECUTION-001`.

The existing ticket contract requires uncertain refund POST outcomes to retain a
non-expiring execution fence. Its non-PayPal request path includes legacy Stripe
orders with no canonical refund binding. It is therefore subject to that rule.

Before correction, the handler read `pending`, called Stripe, then wrote
`approved`. Concurrent callers could both read `pending` and submit a refund.
A response lost after provider acceptance left `pending`, allowing another POST.
The rejection handler also used a pre-transaction status read and could overwrite
an approval in progress.

Acceptance criteria:

- A conditional database update from `pending` to `processing` commits before
  the provider callback. Only the caller that changes one row may execute it.
- Every later approval of that request is refused, including after an uncertain
  provider response, parsing failure, exception or process restart. There is no
  timeout-based reopening and no automatic provider retry.
- Rejection changes only a currently pending request. A stale rejection rolls
  back all changes in its transaction, including canonical cancellation effects.
- Missing Stripe configuration does not consume the execution permit.
- The existing successful full-refund projection remains available and releases
  inventory once for the same request identity.

Implementation: `socialEventsServerWithStripeRefund` in
`tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`. The normal server supplies the
existing `Stripe.createRefund`; injection substitutes only provider IO in tests.
No database migration or provider credential change is needed: `processing` is
already an admitted request state and has an existing UI presentation.

Regression protection: `legacyStripeRefundSpec` in
`tdf-hq/test/TDF/Commerce/ProviderRetrySpec.hs`, executed by the existing
`scripts/test-provider-retry-runtime.sh` backend CI lane against the complete
production migration manifest. Synthetic callbacks model provider acceptance
followed by a lost response; they perform no real financial operation.
`scripts/__tests__/ticket-admission-model.test.mjs` separately explores a finite
design abstraction and checks deliberately unsafe variants.

Verification levels are reported separately in the execution receipt. Model
checking is bounded verification of the abstraction, not proof of Haskell/SQL
refinement. Actual PostgreSQL handler cases are TESTED evidence. The safety
argument assumes normal writers preserve the processing barrier, a committed
database is not rolled back behind an executed external operation, and the
provider transport does not itself retry POSTs. Privileged SQL changes, restoring
an older database, old binaries serving traffic and manual provider operations
are outside this guarantee.

Completion admission is specified separately in
[legacy-stripe-refund-completion.md](legacy-stripe-refund-completion.md): a
provider ID is retained as correlation while successful status and exact request
binding are required for completion. Its receipt supplies the new verification
scope; the original execution-fence receipts keep their historical identity.

Remaining risks: legacy partial refunds still use the historical whole-order
ticket/inventory projection; legacy tickets remain issued while an outcome is
uncertain. No query/reconciliation operation for held legacy Stripe
refunds is introduced here. These are recorded as separate unresolved issues in
`reports/correctness-cycle-2026-10-28.json`. An uncertain request must not be reset
to pending to recover it. Historical uncertain attempts still marked pending
cannot be reconstructed by this patch, so the guarantee begins with requests
claimed by the corrected handler.
