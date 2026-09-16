# ADR 0127: Keep payment worker diagnostics independent of exception contents

Date: 2026-09-15 (Ecuador); verification continues 2026-09-16 UTC.
Status: implemented and locally regression-tested on a dependent draft branch;
not deployed or provider-qualified.

## Finding

`ProviderEventWorker` and `MerchReservationWorker` passed `displayException` to
`redactLogValue`, which only truncated text and replaced four characters. It did
not remove secrets, request headers, SQL parameters, connection strings, customer
data or provider payloads. Other control characters and backslashes could also
break the hand-built JSON event. This is a source-level exposure risk, not proof
that a real credential was leaked. No production logs were inspected or copied.
`TDF.DB.makePool` already filters SQL debug logging; exception rendering in these
worker handlers was a separate path, not protection provided by that filter.

The query-recovery worker already used a fixed failure message. All three loops
could nevertheless terminate when writing a diagnostic raised a synchronous
exception. A payment tick can have committed work before logging fails, so
rerunning that tick inside a logging-error handler would be unsafe.

## Decision

Use a tested single-iteration boundary for each existing worker loop. Production
passes its unchanged tick action, stderr error sink and, where applicable,
stdout information sink. Tests replace only actions/sinks, not payment policy.

- Error events contain only fixed component, level and message fields. Never
  render, inspect, truncate, hash or append a caught exception. The existing
  `error` detail field is intentionally removed from the two vulnerable events.
- Keep event-processing and expiry counters as integer-only information events;
  retain the existing silent idle behavior. These counters are not charges,
  settlements, revenue, worker heartbeats or evidence of merchant qualification.
- Catch synchronous diagnostic-write failures without logging their exceptions
  elsewhere, repeating the tick, changing a lease or altering financial state.
  The next iteration remains subject to the existing delay and qualification,
  locking, idempotency and stale-lease rules.
- Preserve `safe-exceptions` cancellation semantics for the tick and log sink.
  Cancellation/shutdown is not a recoverable provider failure and must not be
  swallowed or relabeled as one.
- Make the provider-event startup warning fixed as well; invalid encryption
  configuration remains disabled and its value never enters a diagnostic.

This changes diagnostics only. It does not acknowledge/retry provider events,
clear dead letters, enable a worker or provider, mark an order paid, release
inventory directly, issue a refund, change an API/client or modify a database.
Existing persistent payment audit/history and reconciliation evidence remain
authoritative and unchanged.

## Operational consequences

Alert on the fixed component/severity/message and use hosting timestamps and
authorized queue/reconciliation views for investigation. Do not turn raw
exception logging back on or dump credentials/payloads to diagnose a failure.
Check database connectivity, approved secret-name presence, deployed SHA,
environment/worker flags, queue age and lease recovery in an authorized
environment. No automatic provider retry or financial repair is authorized by
a diagnostic event.

A failed logging destination can suppress a diagnostic; this repair provides no
new durable diagnostic sink, heartbeat or health guarantee. Monitor log delivery
and queue age independently. Existing transaction audit writes are not made
best-effort by this change. Synchronous business failures still follow the
existing worker retry/lease path.

No migration or backfill is needed. Binary rollback reintroduces the diagnostic
exposure/worker-exit risk: prefer a forward repair or pause affected workers while
preserving their durable queues and the operator's original payment holds.
Historical logs were not searched, modified or deleted; any exposure assessment,
retention or credential rotation requires an authorized incident process.

## Sources and confidence

Accessed 2026-09-16 UTC; high confidence in the documented engineering guidance,
not certification or successful production/sandbox execution:

- [OWASP Logging Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html):
  exclude sensitive authentication/payment/personal data, encode output correctly
  and test logging failures. Fixed failure events and separate transaction audit
  are this application's design choices; no PCI/legal certification is implied.
- [safe-exceptions maintainer documentation](https://github.com/fpco/safe-exceptions#readme):
  recovery helpers distinguish synchronous exceptions from cancellation using
  the exception hierarchy. Tests deliberately inject an async exception type to
  prove propagation; they do not claim deployed process-shutdown qualification.

Evidence, commands and remaining limits:
[worker diagnostic verification](../payments/worker-log-safety-2026-09-15.md).
