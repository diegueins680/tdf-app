# ADR 0124: Retain verified approvals on closed checkouts

Date: 2026-09-15. Status: implemented on a dependent draft branch; not activated.

## Context

An authenticated PlaceToPay or PayPhone query can reveal an approval after the
local checkout expires or is canceled. The local closure does not establish the
provider's financial outcome. The previous application rejected the capture and
rolled back its changes; the query worker retained only a generic exception with
no observed amount. Reopening the checkout could consume released inventory or
create an invalid fulfillment. Treating the rejection as no charge is unsafe.

## Decision

After adapter parsing, exact amount/currency/resource validation and a fresh
immutable-binding lookup, lock the create operation and then the checkout/attempt.
For an approval on `expired` or `cancelled`, append a
`verified_payment_on_closed_checkout` reconciliation exception containing the
provider, environment, merchant alias, checkout/resource references, exact expected
and observed minor units, currency and local observation time. Append a correlated
audit event linking the exception, attempt and create operation in the same
caller-owned transaction. Do not persist raw query responses or payer data.

The exception UUID is UUIDv5 in the URL namespace over the versioned name
`urn:tdf:payment-review:closed-checkout:v1:` plus the canonical create-operation
UUID. It is an identity key, not a secret or an authentication mechanism. Existing
primary-key uniqueness and operation serialization deduplicate callbacks, leases
and concurrent queries without a schema migration. A retained row must match the
immutable scope and money; conflicting evidence fails closed without rewriting it.

The capture ledger, receipt, checkout, attempt, intent and operation remain
unchanged. This record is an unresolved observation, **not booked cash, recognized
revenue, a suspense-ledger posting, a settlement or a refund**. The processor
returns its existing dead-letter disposition. A scheduled job does not add a
second generic review entry for the same recorded approval.

Once this review identity exists, a subsequent matched query cannot automatically
release it, including after an exception workflow-label change. The exact-replay
and authenticated status readers mask that operation as `ambiguous`, hide its
encrypted redirect and disallow fallback using the existing API contract. Stored
provider status and references are preserved. This conservative projection means
the local payment application needs reconciliation, not that the observed
provider approval disappeared. It makes no claim that a worker is currently live.

```text
authenticated query + immutable binding + exact money
                    |
         closed checkout approval
                    |
       exception + audit, one transaction
                    |
      hold / no redirect / no fallback
                    |
       authorized finance investigation
       (no automated release or refund)
```

## Boundaries and invariants

- Untrusted callbacks and malformed/mismatched typed results cannot create or
  acknowledge this evidence. Existing signature, query-budget and runtime gates
  remain required; no provider is enabled by this change.
- Replays neither change observation time nor overwrite assignment/resolution
  notes. Workflow status `resolved`/`ignored` is not financial-release authority.
- Audit failure, SQL failure or caller rollback rolls back the new exception.
  There is no hidden commit in this application boundary.
- The existing closed-checkout payment/fulfillment guards remain in force.
  Operators must not manually reopen the order, delete the exception or create a
  replacement payment to escape the hold. A compliant remediation/release command
  and accounting treatment require a separate implementation and review.
- This applies to the shared PlaceToPay/PayPhone query lane only. Datafast/PayPal
  keep their existing shared capture guard; their first-late-capture operational
  handling is not newly implemented here. Other mismatched or unapplicable query
  results retain their existing fail-closed behavior.
- Admin reconciliation aggregates can count these observations and their amounts;
  they are not a settlement export or a complete cash ledger. Existing generic
  historical exceptions are retained, not silently deduplicated/backfilled.

## Rollout and rollback

No SQL migration, feature-flag activation, generated-schema change or backfill.
Existing query-recovery migrations are prerequisites inherited from the parent
stack. Drain old callback/query consumers before deploying this behavior: old
binaries do not understand the review hold and can expose old recovery redirects.
Roll back the binary only with these consumers disabled and affected orders held;
retain all review and audit rows. Do not drop tables or clear history for rollback.

Provider sandbox, staging, SRI credit-note, Ecuadorian legal/accounting and any
refund/remediation decisions remain separate gates. This ADR is not certification.
