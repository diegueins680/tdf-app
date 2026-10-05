# ADR 0125: Read-only, redacted reconciliation evidence

Date: 2026-09-15. Status: implemented on a dependent draft branch; not activated.

## Context

[ADR 0124](0124-closed-checkout-approval-evidence.md) retains verified approvals
on closed checkouts without reopening inventory or applying a capture. Existing
admin aggregates do not expose individual exceptions and their exact amounts.
Operators need a safe way to find a record without raw provider payloads or a
database console. An observation must remain distinct from booked cash and from
authority to release a financial hold.

## Decision

Append `GET /admin/commerce/reconciliation-exceptions` to the existing commerce
operations API. Apply the existing strict-Admin policy before validating filters
or accessing a database. Default to sandbox; permit explicit production reads
under that same authorization. Accept four review statuses, an optional canonical
checkout UUID, limit 1–100 (default 25) and offset 0–10000 (default 0). Echo every
applied filter. Never accept a customer lookup token in this endpoint.

Read inside a read-only SQL transaction with a three-second per-statement timeout.
Require the exception, binding, attempt and checkout tables. Missing tables are
reported as `crrSchemaReady=false`, not as a verified empty result. SQL failures,
including locks/timeouts or incompatible columns, return a fixed 503 without
diagnostics. Successful reports and database-error responses use `no-store`.

Select one bounded exception page before linking it. Return exception UUID,
allowlisted provider/status/reason classifications, nullable exact expected and
observed minor units, validated currency text and timestamps. Unknown stored
classifications become `unrecognized`. Omit raw exception text, merchant aliases,
external references, notes, assignment identities and provider payloads entirely.
Amounts are nullable signed-64-bit **decimal strings** in this additive contract;
null is unknown, never zero. Existing numeric DTOs are not silently reinterpreted.

Return internal checkout/attempt UUIDs only when there is exactly one matching
attempt and its stored binding agrees on provider, environment, merchant and
resource; checkout identity/environment and attempt/checkout money must agree with
the binding. The exception's legacy text reference is compared as text, never
cast to UUID or exposed. These links identify records; they do not authenticate
the exception amount or prove a capture/settlement. Missing links are not evidence
of no payment. Multiple eligible attempts yield no link rather than a guessed one.

```text
strict Admin → validate bounded filters → read-only exception page
                                               |
                                  unique immutable-binding lookup
                                               |
                           redacted DTO / nullable exact-money strings
                                     /                     \
                            web admin view           generated mobile types
                            (read only)               (no native admin screen)
```

Web defaults to sandbox/open, has no mutation controls and uses the generated
contract. Hide previous evidence during refresh, request failure, filter-response
mismatch or invalid submitted checkout input. Use `BigInt` for exact USD display;
other currencies retain explicit integer minor units without assuming a scale.
Spanish/English labels, accessible filter controls, responsive cards and neutral
review-status chips preserve the distinction between review and payment state.
There is no durable browser storage for this report and inactive query data has
zero retention time. Live offset pagination is not an immutable accounting export.

## Threats, compatibility and limitations

| Threat | Control / boundary |
|---|---|
| Authorization bypass or customer IDOR | Existing strict-Admin gate before filters/database; customer identifiers do not grant access. |
| Injection / cross-environment lookup | Validated bounded filters and parameterized SQL; required environment match. |
| Sensitive logging or response disclosure | Fixed projection, omitted private columns, static user-facing errors; synthetic redaction regressions. |
| Mislinked legacy evidence | Exact stored binding and unique attempt; ambiguous/malformed legacy references remain unlinked. |
| Amount corruption through JavaScript precision | Exact nullable strings and signed-range-checked `BigInt`; no floating-point money in the new view. |
| Stale or misleading success | Echo validation, hide cached rows, explicit missing schema, neutral resolved status and hold warning. |
| Reporting load / lock contention | Bounded page/offset, page-first link query, read-only transaction and statement timeout. No production-volume benchmark yet. |
| Refund abuse / payout fraud / duplicate charge | This GET never reserves work, contacts a provider, changes payment state or authorizes financial action. |

No migration, backfill, data deletion, new index, provider activation or secret is
required. Historical records and existing routes remain unchanged. Application
rollback may remove this additive reader/UI without altering evidence. **Do not
roll back ADR 0124's held-payment safety behavior** or delete evidence to regain a
checkout redirect. Deploy backend before the new UI; an older backend returns a
safe unavailable state, not fabricated empty evidence.

This is operational exception visibility, not a ledger export, settlement
reconciliation engine, refund tool, release command, audit-history browser or
seller-payout implementation. Privileged database edits remain outside this
control. Finance/fulfillment remediation and Ecuadorian legal/accounting decisions
remain separate approved workflows. Provider and staging qualification are not
established by local tests.

## Primary references

Accessed 2026-09-15; high confidence in the documented technical principles,
not a claim of certification or completed production qualification:

- [RFC 8259, section 6](https://www.rfc-editor.org/rfc/rfc8259): interoperable
  JSON integer precision motivates exact strings beyond binary64's safe range.
- [OWASP Authorization Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authorization_Cheat_Sheet.html):
  deny by default and validate authorization on each request.
- [OWASP Logging Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html):
  avoid exposing credentials, tokens, sensitive payment data and personal data.
- [PostgreSQL 16 client defaults](https://www.postgresql.org/docs/16/runtime-config-client.html):
  read-only transactions and statement timeout are independent controls.

Executed evidence and operational procedure:
[reconciliation evidence handoff](../payments/reconciliation-evidence-2026-09-15.md).
