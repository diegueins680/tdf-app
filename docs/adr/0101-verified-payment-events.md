# ADR-0101: Verified payment events and separate fulfillment

Status: Accepted — 2026-08-13; runtime refinement accepted — 2026-08-14

## Decision

Only a signature-verified provider event or authenticated server-to-server verification may post a
successful payment. Provider events enter a deduplicated inbox before processing. Browser returns
can request verification but cannot declare success. Payment and fulfillment state machines are
separate and connected by idempotent events.

Verified event payloads are retained encrypted, with an immutable SHA-256 fingerprint and provider,
environment, merchant, event and resource bindings. Delivery timestamps have a bounded replay
window; workers use a claim/retry/dead-letter lifecycle. A duplicate event may resume a due retry but
cannot replace the original payload. Provider mocks and sandbox evidence cannot transition
production records.

Refunds use a separate two-person state machine and immutable line allocation. A provider refund
becomes successful only after exact server-side provider ID, amount and currency verification; the
result is represented by compensating ledger entries and a credit note, not destructive edits.

Direct browser success, mutable `paid` flags, and order creation as payment proof were rejected
because they permit tampering, replay, duplicate capture, and false commercial claims.
