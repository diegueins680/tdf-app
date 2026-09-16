# ADR 0130: recover a held refund by querying its original provider ID

Date: 2026-09-16 UTC. Status: proposed implementation; not activated.
Depends on [ADR 0128](0128-refund-execution-fence.md),
[ADR 0129](0129-atomic-canonical-refund-accounting.md), and draft PR #401.

## Context and official specification

A refund POST that returned pending or lost its response must not be submitted
again. The execution fence preserves that restriction, but an operator needs a
way to obtain positive evidence for an already-known refund. This increment
covers only the existing mixing/mastering PayPal refund flow, not discovery of
an unknown refund ID or marketplace refunds.

PayPal documents an OAuth-authenticated GET by refund ID and a response containing
the amount, status and resource links. Its merchant-on-behalf-of assertion requires
prior consent; this implementation does not use that partner mechanism.
Source: [official Payments v2 refund lookup](https://developer.paypal.com/api/payments/v2/refunds-get),
accessed 2026-09-16; high confidence for the published API, no merchant qualification
implied. Examples are sanitized fixtures, not executed sandbox responses.

## Decision and sequence

```text
strict-admin request + immutable internal refund UUID
  -> load known refund/capture/merchant/environment/amount/currency binding
  -> validate process switch
  -> transaction: lock refund, recheck account/flag/capabilities, reserve shared quota,
                  append requested audit, commit
  -> OAuth token acquisition, then GET only the original refund resource
  -> validate exact IDs, amount, USD, and environment-pinned self/up links
  -> transaction: lock refund, recheck binding and authority
       exact COMPLETED -> existing atomic RefundStore completion + outcome audit
       other status    -> retain processing/reservation + held outcome audit
       error/conflict  -> retain reservation; no replacement execution permit
```

GET of our admin endpoint reads local readiness only. POST of our admin endpoint
initiates the lookup and may change local accounting, but never posts a refund or
capture to PayPal. Neither operation follows provider-supplied links. The existing
bounded transport disables redirects and redacts credentials and response errors.

The adapter rejects malformed, duplicate or mismatched binding evidence. The
optional response merchant identifier must match when present; its absence is
permitted by the documented response shape. Merchant authority additionally comes
from exact configured account/environment credentials, immutable local capture
ownership and OAuth access to the resource. This is not connected-account support.

Money is parsed through `Integer`, checked against positive `Int64` bounds and
compared in minor units. The new API represents minor units as decimal strings,
including the maximum `Int64`, to avoid JavaScript precision loss. Existing API
numeric fields are unchanged. Browser responses are checked against the requested
UUID, environment and amount; they are never financial evidence for the backend.

## Authority, concurrency and failure semantics

Both endpoints require the existing strict-admin role policy before lookup.
Success responses carry `Cache-Control: no-store`. The UI issues no automatic
provider lookup, prevents concurrent submissions, clears stale results on input
changes/errors and ignores late responses after unmounting.

Admission and completion require the exact environment's
`checkout.paypal.refund_reconciliation` flag, enabled ready USD merchant account,
approved contract and validated credential status. `server_verification`,
`full_refund` and `partial_refund` evidence must be `sandbox_verified` or
`production_verified` for that same environment. There is no sandbox bypass.
The process switch `PAYPAL_REFUND_RECONCILIATION_ENABLED` must be exactly `true`.
Absent flags or configuration disable the action. This does not turn on any
existing charge/refund execution flag.

Refund-row locks serialize operators and the existing durable provider/environment
query budget limits admission across operations. The admission audit commits
before network access. Account, flag and capability evidence rows are share-locked
during financial application, preventing revocation from interleaving with commit.
Database statements and lock waits have a three-second local timeout. No DB
transaction spans the provider HTTP request.

An older pending response cannot overwrite a concurrent completion. A completed
replay does not append another credit note or ledger refund. A failed final audit
rolls back the intent, checkout, refund, ledger and receipt writes together. The
earlier requested audit remains durable. A crash after admission can leave only
that audit: inspect the still-held refund and safely query its same ID later.

`FAILED`, `CANCELLED`, unknown status, timeout, malformed response and 404 never
release reserved funds or authorize another refund. Positive evidence may still
be blocked by canonical drift or dispute state. Accounting review is required;
do not bypass it by overwriting balances or creating a new refund.

## Compatibility and rollout

No new table, column, backfill, destructive migration or reinterpretation of
historical records. Existing migrations supply the audit, binding, capability and
query-budget tables. The new flag is intentionally absent by default; an operator
may register it disabled without changing any existing flag. Roll back by disabling
this flag and process switch before replacing the application. Keep all query
audits and financial history; never roll back a used ledger or queue.

Known-ID recovery is not unknown-ID discovery, legacy-failed resolution, provider
settlement reconciliation, dispute resolution, automatic polling, merchant account
approval, or SRI credit-note issuance. No new card, seller, fee or tax data is stored.
The inherited ledger allocation is only qualified here for the mixing/mastering
storefront. Other domains and seller liabilities require separate accounting work.

Verification and activation checklist:
[implementation report and runbook](../payments/held-refund-query-2026-09-16.md).
