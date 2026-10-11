# Redelivered Instagram and Facebook messages — MSG-META-REDELIVERY-001

Meta delivers an inbound Instagram or Facebook message to its webhook again when an
earlier delivery was slow or failed, and an administrator can backfill a
conversation from the Graph API, which reads messages that webhooks already
delivered. `instagram_message.external_id` and `facebook_message.external_id` are
unique, so a message has one row. This rule covers what a repeat may do to it.

## Rule

- A stored message keeps its text, its direction, its deletion and its reply state
  (pending, answered, on hold for a person, failed) however often it is delivered or
  backfilled again.
- A repeat may only fill a sender name or advertising attribution that was missing.
- A deletion delivered again keeps its first time. A message whose deletion arrived
  first never gains text.
- The reply worker therefore takes up a message once.

## Defect repaired (AUTHORITY-061)

Both webhooks and the backfill wrote messages with an upsert whose update set the
text, set the direction to `incoming` and set the reply status to `pending`:

- A message the reply worker had put on hold for a person, or had failed on, became
  `pending` again when Meta redelivered it or an administrator ran a backfill, and
  the worker took it up again and could answer it automatically. An answered message
  was protected only because the worker also requires that no reply time is set.
- A redelivery carrying other text replaced the stored text, including on a message
  the sender had deleted; the existing unit test expected exactly that replacement.
- A stored outgoing message with the same id was turned into an incoming one.
- A deletion delivered again moved the deletion time forward.

`storeInstagramInboundOnce` and `storeFacebookInboundOnce` now insert the message
when it is absent and otherwise leave it, filling only a missing sender name or
attribution. The webhooks and the backfill use them. The deletion path sets the
deletion time only when none is stored.

## Evidence

- `formal/event-operations/MetaRedelivery.tla`: one message, up to three deliveries
  with either of two texts, up to two deletions in any order, a clock, and a worker
  that answers, holds or fails. 411 distinct states, four invariants:
  `ConsideredOnce`, `TextKept`, `NoTextAfterDeletion`, `DeletionTimeKept`. Three
  controls each change one guard and are each caught by a named invariant: a repeat
  resets the reply state (`ConsideredOnce`), a repeat rewrites the text
  (`TextKept`), a repeated deletion moves its time (`DeletionTimeKept`).
- `tdf-hq/test/TDF/MetaRedeliverySpec.hs` (`scripts/test-meta-redelivery.sh`,
  PostgreSQL, fully migrated schema, nothing sent): a message on hold, an answered
  message and a failed one keep their state when delivered again; other text does
  not replace the stored text; an outgoing message is not turned into an incoming
  one; a deleted message keeps its text and first deletion time; a deletion that
  arrives first leaves no text; a missing sender name is filled and a stored one
  kept; the same holds for Facebook; eight simultaneous first deliveries leave one
  row.
- `tdf-hq/test/TDF/ServerExtraSpec.hs`: the existing deletion case now expects the
  original text after a late repeat.

## Limits

- The model is bounded, covers one message and checks safety only. It treats a
  delivery as one atomic step.
- Two simultaneous first deliveries are decided by the unique index: the loser's
  transaction fails, its whole webhook batch is answered with an error, and Meta
  delivers it again. No lock avoids that failure.
- A message's text is kept after its sender deletes it; only the deletion time is
  recorded and the row is hidden from conversation views. Whether deleted text
  should be erased is undecided.
- A real edit of a message by its sender, if Meta ever delivers one under the same
  id, is not applied. No edit event is handled today.
- The backfill reports a message already stored as imported, as before.
- The reply worker itself was read, not changed or tested. The capture triggers
  `operations_instagram_inbound_capture`, `operations_facebook_inbound_capture` and
  `operations_whatsapp_inbound_capture` test for the directions `inbound` and
  `received` while the application stores `incoming`, so they record nothing; that
  is a separate defect, not addressed here.
- No test goes through HTTP with a signed body or through the backfill route; the
  tests call the function both webhooks call. The backfill's use of the same
  functions is covered by reading only.
