# Enabled-only inbound Operations capture — OPS-INBOUND-CAPTURE-001


## Current authority (2026-10-13)

The user explicitly chose **capture only when Operations is enabled; define
retention before rollout**. AUTHORITY-OPS-CAPTURE-POLICY-20261013 supersedes the
unconditional collection assumption in the historical direction repair below.
The additive `2026-10-13_operations_inbound_capture_policy.sql` requires the default
Operations organization to be enabled and its default branch active. The guard
uses the same IDs as `operations_record_event`; missing context denies capture.
It holds SHARE locks through the message transaction. If disabling commits first,
a waiting message is stored without capture. If capture is admitted first,
disabling waits for that transaction to commit. No flag is enabled by migration.
Messages stored while disabled are not captured retroactively on redelivery.

Provider message storage remains available. Guard/capture errors retain the
existing contained-failure behavior; this is not exactly-once successful delivery.
Restoring old unguarded trigger functions is not a supported policy rollback.
Disable Operations to stop new capture; already recorded events are unchanged.

**Activation is blocked pending retention policy.** Raw sender identifiers still
enter append-only event/audit records when capture is enabled. No retention period,
erasure/anonymization mechanism, exception or backup expiry is approved here.
The canonical release preparation returns a machine-readable policy blocker;
production capture cannot be activated merely because the guard merged. A rollout
that keeps capture disabled must verify the disabled flag and installed guard
under the deployment writer fence. Normal inbox storage has its separate policy.

### Current checks and model boundary

`OperationsInboundCaptureSpec` explicitly enables only the synthetic fixture for
existing capture cases, restores the prior state, and tests disabled storage on
all three providers plus no capture on redelivery after re-enabling. The HTTP/PG
fixture witnesses six blocked inserts (three channels × organization/branch
switch), then verifies no capture after disabling commits. Its reverse-order
case proves disablement waits for an already admitted message to commit.

`InboundCapture.tla` now includes a toggling enablement state and ghost admission
at first insertion. Two message IDs, four message kinds, three deliveries and a
boolean switch give 372 distinct states in the development run. Organization and
branch authority are abstracted together as that switch. Capture and its held-lock
transaction are atomic in the model. Capture failures, worker projection,
retention, arbitrary SQL refinement, process crashes and unknown numbers of actors
are excluded. There is no fairness or liveness claim. The new disabled-guard
mutation violates `CaptureRespectsEnablement`; the three existing direction,
non-request and replay controls still fail their named properties. Exact candidate
receipts must supersede development counts before delivery.

The lock design follows [PostgreSQL17 row-lock semantics](https://www.postgresql.org/docs/17/explicit-locking.html#LOCKING-ROWS):
`FOR KEY SHARE` would not fence a non-key boolean update, whereas `FOR SHARE`
does. The concrete races exercise READ COMMITTED behavior; error rollback can
release the capture block's locks because its event effects were also rolled back.
The [OWASP logging guidance](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html#disposal-of-logs)
supports explicit disposal rules for records and copies, but supplies no approved
TDF duration. `research.json` records both applicability decisions separately from
the test evidence.

## Historical direction-repair rationale and effects

The following description records why migration 2026-10-11 was introduced. Its
unconditional wording and old three-control results are historical provenance;
the enabled-only rule above governs the current specification.

The Operations control center turns an inbound WhatsApp, Instagram or Facebook
message into a domain event (`communication.<channel>.received`) through a trigger
on the message table. The outbox worker projects those events into one work item
per sender and channel ("message needs a response", priority high).

## Rule

- Subject to current enabled-only admission, a message stored as a request produces one
  Operations event, in the transaction that stores it.
- Storing the message comes first: if the event cannot be recorded, the message is
  stored without one and PostgreSQL logs a warning naming the message id.
- The application's own outgoing messages produce none.
- For Instagram and Facebook, two kinds of row are stored like an inbound message
  without being a new request, and produce none: the record of a deletion that
  arrived before its message, and a message imported from history (a row whose
  metadata carries the backfill's `"backfilled": true`, whatever its age, or one
  stored more than a day after it was sent).
- A message delivered again finds its row, inserts nothing, and produces nothing.
  That is a property of how the application stores messages, not of the trigger.

## Defect repaired (AUTHORITY-062)

`operations_whatsapp_inbound_event` and `operations_social_inbound_event`
(installed on 2026-08-09) returned unless the row's direction was `inbound` or
`received`. The application has always stored inbound messages with the direction
`incoming`. No inbound message has therefore produced an Operations event or work
item, on any channel, since the control center was installed. The conformance test
that "exercises the installed WhatsApp capture trigger" inserted its rows with the
direction `inbound`, which the application never writes, so it passed.

Migration `2026-10-11_operations_inbound_capture_direction` replaces both functions
so that they accept `incoming` as well, skip the two kinds of row above, and
contain a capture failure so that it cannot undo the message. It changes no table
and captures nothing retroactively. A rollback file restores the
previous functions. The conformance test now inserts `incoming`.

## What changes when this is applied in production

None of the following has ever run in production, because no inbound message was
ever captured.

- **Rows.** Each new inbound message appends about five rows: a domain event, an
  outbox entry and, once the worker projects it, a work-item event, a stream event
  and an audit row. Growth is proportional to the number of inbound messages.
- **Work items.** The worker opens one high-priority item per sender and channel in
  the reception queue ("message needs a response") and updates it on each further
  message. A new message reopens an item that was resolved or archived.
- **Never closed automatically.** An automatic reply, a manual reply or a later
  deletion of the message does not resolve the item.
- **Service-level timers.** Each item gets acknowledgement, mitigation and
  resolution timers at priority high, in business hours. Items nobody handles show
  as breached. A reopened item keeps its original timers.
- **Telephone numbers kept permanently.** The sender identifier, which on WhatsApp
  is the telephone number, is written to the event's aggregate id and correlation
  key and to the audit row's correlation id. The event and audit tables are
  append-only by trigger and have no erasure path.
- **Visibility.** The Operations API is served only when
  `operations_organization.operations_enabled` is true; it is seeded false. The
  production value was not checked. If it is false, events, items and breaches
  accumulate where nobody sees them.
- **Nothing is sent.** No code sends a message, e-mail or notification to staff or
  customers because of these items.
- **Turning it off.** Applying the rollback file by hand restores the previous
  functions. The migration runner has no rollback step.

## Evidence

- `formal/event-operations/InboundCapture.tla`: two messages of any kind (request,
  outgoing, deletion record, history import), three deliveries. 144 distinct states,
  three invariants: `RequestsCaptured`, `OnlyRequestsCaptured`, `CapturedOnce`.
  Three controls each change one guard and are each caught by a named invariant:
  the application's direction is refused (`RequestsCaptured`, the defect), every
  row stored as incoming is captured (`OnlyRequestsCaptured`), every delivery is
  captured (`CapturedOnce`).
- `tdf-hq/test/TDF/OperationsInboundCaptureSpec.hs`
  (`scripts/test-operations-inbound-capture.sh`, PostgreSQL, fully migrated schema):
  inbound messages are stored through the functions the webhooks call; the
  outgoing and history rows are inserted directly. One event for a WhatsApp message
  and none more when it is delivered again; one for an Instagram and one for a
  Facebook message; none for an outgoing message, a deletion that arrives first, or
  a row three days old, or a minute-old row carrying the backfill marker (a row
  whose metadata says `"backfilled": false` is still captured); a message whose
  capture fails is stored without an event.
  With the rollback applied the first three cases fail (run locally on 2026-10-11).
  Not covered: a WhatsApp sender matched to a party, the backfill HTTP route itself
  (its rows are represented by the metadata it writes), the
  one-day boundary, outgoing WhatsApp messages, and the projection into work items
  (the existing HTTP test below covers the WhatsApp projection).
- `scripts/lib/operations_conformance.py`: the existing HTTP test of the WhatsApp
  thread (event, projection, identity binding) now uses the application's
  direction. It runs in CI; it was not run locally.

## Limits

- The model is bounded and checks the capture decision only. It does not cover the
  outbox worker, the projection into work items, or service-level clocks.
- "Imported from history" is decided by origin and by age: a row the backfill marked
  in its metadata is never captured, and neither is a row whose time is more than
  one day before its insertion. Webhook rows carry the server's time and are always
  captured, however late Meta delivers them. The model treats history as a kind of
  row and does not model the one-day boundary.
- A capture failure is contained with a savepoint per stored message, and the
  event is then missing with only a log line to show for it.
- Two simultaneous Instagram or Facebook payloads naming the same two senders in
  opposite order can deadlock on the per-sender sequence row; with the containment
  above the loser stores its message without an event.
- The release tooling's schema verification does not check that these functions
  accept `incoming`.
- A sender correlated to another identity later is supported by the projection only
  for WhatsApp.
- A WhatsApp message is captured whatever its text, including opt-out keywords and
  consent confirmations.
- Messages stored before the migration are not captured.
- Whether reception is staffed and configured to handle these items in production
  was not examined.
