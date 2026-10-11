# Inbound messages reach Operations — OPS-INBOUND-CAPTURE-001

The Operations control center turns an inbound WhatsApp, Instagram or Facebook
message into a domain event (`communication.<channel>.received`) through a trigger
on the message table. The outbox worker projects those events into one work item
per sender and channel ("message needs a response", priority high).

## Rule

- A message the application stores as a request from a customer produces exactly
  one Operations event, in the transaction that stores it.
- The application's own outgoing messages produce none.
- For Instagram and Facebook, two kinds of row are stored like an inbound message
  without being a new request, and produce none: the record of a deletion that
  arrived before its message, and a message imported from history (stored more than
  a day after it was sent, which only the backfill does).
- A message delivered again finds its row, inserts nothing, and produces nothing.

## Defect repaired (AUTHORITY-062)

`operations_whatsapp_inbound_event` and `operations_social_inbound_event`
(installed on 2026-08-09) returned unless the row's direction was `inbound` or
`received`. The application has always stored inbound messages with the direction
`incoming`. No inbound message has therefore produced an Operations event or work
item, on any channel, since the control center was installed. The conformance test
that "exercises the installed WhatsApp capture trigger" inserted its rows with the
direction `inbound`, which the application never writes, so it passed.

Migration `2026-10-11_operations_inbound_capture_direction` replaces both functions
so that they accept `incoming` as well and skip the two kinds of row above. It
changes no table and captures nothing retroactively. A rollback file restores the
previous functions. The conformance test now inserts `incoming`.

## What changes when this is applied in production

From the moment the migration is applied, every new inbound message creates an
event, and the outbox worker opens or updates a high-priority work item for its
sender in the reception queue. That is what the control center was built to do, and
it has never happened in production. Automatic replies do not close these items.
Reception should expect the items to start appearing; the volume equals the number
of distinct people who write on each channel.

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
  messages are stored through the functions the webhooks call. One event for a
  WhatsApp message and none more when it is delivered again; one for an Instagram
  and one for a Facebook message; none for an outgoing message, a deletion that
  arrives first, or a history import. With the rollback applied the first three
  cases fail (run locally on 2026-10-11).
- `scripts/lib/operations_conformance.py`: the existing HTTP test of the WhatsApp
  thread (event, projection, identity binding) now uses the application's
  direction. It runs in CI; it was not run locally.

## Limits

- The model is bounded and checks the capture decision only. It does not cover the
  outbox worker, the projection into work items, or service-level clocks.
- "Imported from history" is decided by age (more than one day between the
  message's time and its insertion). A webhook delivery delayed by more than a day
  is not captured; a backfill of messages less than a day old is.
- A WhatsApp message is captured whatever its text, including opt-out keywords and
  consent confirmations.
- Messages stored before the migration are not captured.
- The sender identifier (a telephone number on WhatsApp) is the aggregate id of the
  event, as designed in the control center; this change adds no new field.
- Whether reception is staffed and configured to handle these items in production
  was not examined.
