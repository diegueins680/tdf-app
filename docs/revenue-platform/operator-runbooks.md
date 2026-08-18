# Revenue and distribution operator runbooks

These procedures preserve evidence and fail closed. They do not authorize a production charge,
capture, refund, takedown, DSP delivery, or payout.

## Payment webhook or callback failure

1. Disable only the affected provider/domain initiation flag if integrity is uncertain.
2. Keep orders in `awaiting_payment` or `processing`; never infer success from a browser return.
3. Inspect redacted correlation ID, event-inbox state, signature result, timestamp, event ID and retry
   count. Never paste full payloads or credentials into a ticket.
4. Query provider status server-to-server and compare merchant, resource, order, amount and currency.
5. Where the environment-specific worker flag is enabled, allow it to process `retry` events. For
   `dead_letter`, repair the documented root cause first, then use
   `/admin/commerce/provider-events` and enter a specific remediation reason.
   The audited requeue preserves the original attempt count and encrypted payload; it does not mark
   the order paid. Never reset the counter or edit the event directly.
6. Reconcile before re-enabling. Contact affected customers with the honest current state.

## Provider outage

Disable new provider attempts, preserve already-created attempts, keep alternate rails visible only
if independently healthy, and show a recoverable unavailable state. Do not create a manual-paid
status. Watch latency/error/decline baselines, obtain provider incident evidence, and re-enable in a
small staging/canary window after reconciliation.

## Reconciliation mismatch

Freeze destructive corrections. Group exceptions by missing internal/provider/ledger/refund record,
currency/amount mismatch, duplicate reference, or timing window. Assign an owner and attach only
redacted evidence. Resolve through an immutable compensating entry or linked evidence; obtain a
second reviewer for cash-impacting changes. Do not close until provider and ledger totals agree or
the accepted write-off is documented.

## PayPal refund pending, failed, or ambiguous

Do not create a second refund request. Keep the immutable refund UUID and retry the approval action
with the same provider request ID after checking provider health. A requester may not approve their
own refund. For `PENDING`, query PayPal server-to-server and compare capture, refund ID, amount,
currency, environment and merchant before recording completion. For a mismatch or an externally
observed refund/reversal, keep the internal refund unchanged and resolve the reconciliation
exception with a second reviewer. Never edit allocations, ledger entries, or credit notes in place.

## Marketplace sale hold or custody mismatch

Disable `commerce.marketplace_sales` for new production checkouts without disabling unrelated
domains. Do not edit the order to `paid` or the asset to `Sold`. Compare the immutable checkout
lines, active/consumed hold, provider binding, marketplace runtime row, fulfillment history, asset,
and listing under one correlation/order ID. A checkout start should have one expiring active hold;
verified payment should consume it and stop at `ready_to_fulfill`; only documented pickup or
delivery should produce `delivered` and `Sold`.

If payment exists without a fulfillment transition, keep custody with TDF and assign operations. If
delivery evidence exists without verified payment, stop handoff and open a reconciliation incident.
For a return, record the authorized transition and condition evidence; `returned` restores asset
custody but must not silently reactivate the listing. Use compensating financial records for any
refund. Never repurpose the sale lifecycle for a rental.

## Marketplace rental hold, custody, or deposit mismatch

Disable `commerce.marketplace_rentals` to stop new rental checkout while leaving sales and other
domains available. Compare the immutable checkout line, rental runtime, date-exclusion hold,
accepted terms version/history, provider binding, payment evidence, asset state, outbound/inbound
condition reports, rental events, and deposit state under one order ID.

Verified payment may produce `confirmed`, never `checked_out`. Do not hand off without the outbound
report. A return must record the inbound report before inspection. `deduction_proposed`,
`refund_due`, and `partial_refund_due` are work queues—not evidence that money moved. For a non-zero
deposit, do not close the rental until a separately verified refund, partial refund, forfeiture, or
dispute record exists. Never edit the deposit amount or historical terms in place; use a new terms
version and compensating financial records.

For a date conflict, do not delete the competing row. Identify the earlier accepted hold/payment,
release only an expired or validly cancelled hold, and contact the affected customer. For loss or
damage, preserve photos/documents in approved private evidence storage, follow the dispute window,
and require the configured operator approvals before any financial settlement.

### Marketplace customer request review

Open the request through the authenticated `customer-requests` route and compare its immutable
order/type/reason/date/evidence snapshot with the current sale or rental state. Do not edit the
order because a customer submitted a request. Approval may only open the supported cancellation,
return, or operational-dispute state; payment remains unchanged. Reject stale or inapplicable
requests with customer-safe notes. Mark a rental extension `needs_quote`; never approve it or edit
the return date until a versioned extension quote, atomic date check, accepted terms, and any
payable balance are implemented.

### Rental deposit manual settlement

Use this path only after inspection has reached `deposit_refund_due` and the canonical checkout is
fully paid with no prior returned-funds total. The submitter records the actual bank/cash/POS
reference or full-forfeiture evidence in approved private storage. A different invoicing-authorized
operator compares the evidence, deposit, approved deduction, refund, currency, checkout, and rental
state. Choose `requires_reconciliation` instead of verification for ambiguity. Verification is an
accounting liability settlement, not a Datafast/PayPal refund; never add a provider ID or tell the
customer a provider refund occurred. Confirm the ledger balances to zero, the credit-note reference
matches the evidence, and the terminal deposit state is correct before closing the rental. Disable
`commerce.marketplace_manual_deposit_settlement` if evidence storage, separation of duties, or
ledger reconciliation is unavailable.

## Public event ticket hold payment or issuance mismatch

Disable `commerce.event_tickets` for the affected environment to stop new guest holds without
changing existing orders. If one provider is unhealthy, disable only that provider rail as well.
Do not edit `quantity_sold`, promotion counters, the order status, or the ticket runtime by hand.

For every incident compare the event and tier, approved policy/version, immutable checkout line,
buyer/organizer fee snapshot, hold expiry, provider attempt and binding, payment evidence, runtime
fulfillment history, issued ticket rows, promotion redemption, receipt, ledger transaction and
reconciliation exception under one correlation/order ID.

- A browser return or PayPal approval with no verified provider evidence remains `processing` or
  `awaiting_payment`. Do not issue tickets.
- An expired unpaid hold must release event/tier capacity and its reserved promotion claim exactly
  once. Re-run the idempotent expiry function rather than editing counters.
- A payment first verified after expiry is a reconciliation incident. Do not silently recreate the
  hold or issue over capacity; assign customer support and finance to refund or rebook through an
  approved compensating workflow.
- A paid runtime with no tickets is an issuance incident. Confirm the runtime-row lock is clear and
  that no `issued` audit or ticket batch already exists before using an audited retry path. Never
  delete the uniqueness evidence to force another issue.
- Ticket rows without verified canonical payment are a severity-one integrity incident: stop
  check-in for the affected order and preserve all evidence.
- Organizer proceeds posted to `liability.event_organizer_payable` are not settlement. Keep
  `commerce.event_ticket_settlements` disabled until the approved dual-control evidence workflow is
  available; never mark a payable settled from a bank promise or spreadsheet note.

Before a TDF-owned staging pilot, approve exactly one event policy, verify capacity and sale windows,
exercise Datafast and PayPal sandbox create/return/capture paths, prove replay and late-payment
handling, reconcile checkout/receipt/ledger/provider totals to zero variance, run the rollback gate,
name the webhook and customer-support owners, and verify both domain/provider kill switches. This
does not authorize a production charge or organizer settlement.

## Distribution dead letter or release-date risk

Stop retries if the package/profile/checksum may be wrong. Verify immutable release version,
recipient profile, message ID, operation/prior delivery, package manifest and transport receipt.
Never call upload `sent` without a receipt or `acknowledged` without recipient evidence. Escalate the
release-date risk to operations/customer support, agree a corrected schedule, and retry idempotently
only through the named adapter. A changed package requires a new version/message.

## Rights dispute

Restrict release changes and new delivery while preserving evidence. Notify authorized legal/rights
operators, identify territories/recipients and the exact disputed scope, retain declarations,
contracts, acceptance versions and correspondence, and follow reviewed complaint/counter-notice
policy. Do not make a legal ownership determination in software or silently change splits.

## Emergency takedown

An emergency request still requires an authorized principal, reason/evidence, affected release
version and exact recipient/territory scope. Obtain the required second approval, link every request
to its prior delivery, preserve outgoing message/manifest/checksum/receipt, and report
`takedown_requested` until recipient evidence supports `takedown_completed`. Production submission
requires separate authorization.

## Usage-report variance or correction

Quarantine malformed reports; preserve raw bytes/checksum and import version. Compare reported
control totals by currency/period to normalized totals. Import a correction as a child of the
original and add reversing/replacement allocation events; never overwrite source lines or issued
statements. Reissue as a corrected statement with traceability and notify affected beneficiaries.

## Payout failure or beneficiary-account change

Automatic payout is disabled. For a manual settlement, stop the item, verify statement/payable
balance and beneficiary/KYC/tax/bank status, and require a distinct approver. An account change needs
step-up verification, out-of-band notice and cooling period. Preserve failed attempt/reference and
use a new idempotent attempt; never edit the original to `paid`.

## Minimum incident record

Record environment, time range, affected capability/domain, correlation/provider/recipient IDs,
non-sensitive evidence hashes, customer impact, money/status impact, flag actions, operator and
reviewer, provider/partner case, reconciliation result, notifications, root cause, remediation and
re-enable approval. Rotate any suspected secret through the secret manager and validate that logs
contain no raw secret/card/KYC/payout data.
