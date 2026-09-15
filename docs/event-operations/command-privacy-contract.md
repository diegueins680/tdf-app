# Command target privacy contract

Refines EO-003/EO-009/EO-045/EO-051/EO-055 before changing the disabled event-operations API.
The caller is authenticated and the command has passed typed HTTP parsing. The feature is enabled.
The event state lock and current-time contextual authorization remain the existing boundary.

## Observation policy and operation

For an absent event and an existing event without current read access, a POST transition must return
the identical HTTP 404 JSON body `{"code":"not_found"}`. The SQL command returns exactly
`{"error":"not_found"}` in both cases. Neither path discloses an event ID, version, lifecycle,
receipt, replay flag, correlation ID, nor whether an idempotency key is occupied. Check this for new
keys, accepted/rejected receipts, changed content and other-actor bindings, including after access
revocation/expiry. Authorization precedes replay/conflict response selection.

A caller with current read access but insufficient transition authority continues to receive 403
`forbidden` for a fresh, otherwise valid command. Historical responses remain immutable and can be
replayed only under current read authority and the existing exact actor/event/content binding.
Existing version, lifecycle, separation-of-duties and feature-disable rules are unchanged.

No denial may cause a lifecycle transition. Existing inaccessible-event rejection audit/receipt
records remain durable and private: retain the internal `forbidden` diagnostic in a new rejection
receipt and its audit reason, but do not return that receipt to an unreadable caller. A later grant
of read access may reveal the caller's own historical rejection. Replays append denial audit without
overwriting history. Missing targets cannot have event-linked audit/receipt rows (foreign keys).

## Model and validation gate

`CommandPrivacy.tla` uses paired absent/existing worlds with the same abstract request. It explores
one request with 4 receipt classes, 2 version classes and 3 grants, including grant changes while
waiting. `Observe` abstracts the existing locked current-authorization decision; this is not a new
proof of database locking or session freshness. `OpaqueTarget` compares complete abstract status/body
observations when read access is absent. `ReadOnlyDenial` prevents incorrectly replacing all 403s.
Other invariants check types and the abstract no-unauthorized-effect gate. Both negative controls
must produce TLC exit 12 for `OpaqueTarget`: returning a distinct existence error, and checking
receipts before visibility. No feature implementation begins until the positive model passes and
both negative controls are detected. Run `scripts/verify-event-operations-formal.sh` with the pinned
tool paths documented in the formal README; retain its existing TLC and Alloy checks.

Executable verification must compare SQL JSON exactly and HTTP status, raw body, content type and
absence of resource-specific headers, not merely assert denial. Preserve audit/receipt/state counts
and read-only 403 regressions. Re-run revocation/concurrency, migration reapplication and rollback.

## Limits and compatibility

This is response-envelope non-disclosure, not constant-time behavior: row locks, audit writes,
latency, availability, connection failure and operator-only logs/metrics are outside the paired
observation model. Generic server Date headers are not resource metadata. Rate limiting and broader
side-channel mitigation remain required before private-resource activation. Feature-disabled and
malformed/unauthenticated outcomes depend on configuration/input, not private target existence.
Token revocation between authentication and command remains a separate open issue.

No public request/DTO/schema signature changes. Unreadable POST errors deliberately change from
403 to 404; clients must treat both as access denial and must not infer existence. The SQL migration
is still an unmerged, disabled-default, non-production-manifest increment and is corrected in place.
No deployed migration checksum is replaced. Use its existing rollback to disable/drop entry points,
preserving immutable history; do not restore the leaking behavior as a production rollback.
