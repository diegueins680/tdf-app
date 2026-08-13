# ADR-0101: Verified payment events and separate fulfillment

Status: Accepted — 2026-08-13

## Decision

Only a signature-verified provider event or authenticated server-to-server verification may post a
successful payment. Provider events enter a deduplicated inbox before processing. Browser returns
can request verification but cannot declare success. Payment and fulfillment state machines are
separate and connected by idempotent events.

Direct browser success, mutable `paid` flags, and order creation as payment proof were rejected
because they permit tampering, replay, duplicate capture, and false commercial claims.
