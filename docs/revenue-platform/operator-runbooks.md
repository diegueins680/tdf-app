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
