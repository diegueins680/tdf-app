# Redelivered WhatsApp messages — MSG-WHATSAPP-REDELIVERY-001

Meta delivers an inbound WhatsApp message to the webhook again when an earlier
delivery was slow or failed, and the signed body of a delivery stays valid, so the
same message can reach `POST /webhooks/whatsapp` (and its `/hooks/whatsapp` alias)
any number of times. `whats_app_message.external_id` is unique, so the message is
stored once. This rule covers what the message asks for.

## Rule

- The delivery that stores an inbound message applies what it asks for; a delivery
  that finds the message already stored changes nothing.
- Storing the message, withdrawing consent for an opt-out keyword (`STOP`,
  `SALIR`, `CANCELAR`, `BAJA`) and confirming a pending consent request happen in
  one transaction: a stored opt-out has been applied.
- The automatic reply to an enrolment message (`inscribirme`) and the consent
  confirmation notice are sent only by the delivery that stored the message.
- Deliveries of one message are handled one at a time (advisory lock on the
  provider message id), so a simultaneous delivery waits and then finds the message
  stored.

## Defect repaired (AUTHORITY-060)

The handler stored the message idempotently and then ran every effect on every
delivery:

- A redelivered `STOP` withdrew consent again, with the time of the redelivery.
  A number that had opted out and later subscribed again was opted out by the
  redelivery of its old message.
- A redelivered enrolment message was answered again. The limit of three per hour
  counts course registrations, and a redelivery creates none (the registration is
  keyed by the stored message), so the limit never stopped the repeated replies.
  The reply is sent synchronously inside the webhook request, which is the
  situation in which Meta redelivers.
- The message and its opt-out were separate transactions.

`receiveWhatsAppInbound` now stores the message and applies the opt-out and the
consent confirmation in one transaction, only when it stored the message, and
tells the handler whether this was the first delivery.

## Evidence

- `formal/event-operations/WhatsAppRedelivery.tla`: three deliveries over two
  `STOP` messages and one enrolment message, up to two new subscriptions in
  between, and a process that may stop after any committed step. 19,314 distinct
  states, four invariants: `StoredOnce`, `AppliedOnce`, `RepliedOnce`,
  `StoredStopApplied`. Four controls each change one guard and each violate one
  invariant: every delivery applies (`AppliedOnce`), storage is not unique
  (`StoredOnce`), the effect follows the store in a later step
  (`StoredStopApplied`), every delivery replies (`RepliedOnce`).
- `tdf-hq/test/TDF/WhatsAppRedeliverySpec.hs` (`scripts/test-whatsapp-redelivery.sh`,
  PostgreSQL, fully migrated schema, no message sent): the first `STOP` withdraws
  consent; the same `STOP` delivered again after a new subscription leaves the
  subscription; a new `STOP` still withdraws it; a redelivered message has one row
  and one audit entry; of eight simultaneous deliveries exactly one is the first; a
  pending request is confirmed once and reported only by the first delivery; an
  earlier yes redelivered after a withdrawal and a new request confirms nothing.

## Limits

- The model is bounded and checks safety only. It represents the unique index, not
  the advisory lock: in the model a losing simultaneous delivery ends without
  effect, in the implementation it waits and finds the message stored.
- A `STOP` delivered for the first time after a later subscription (a delayed
  first delivery) still withdraws consent. The rule errs toward the opt-out.
- The automatic reply and the confirmation notice are sent at most once. If the
  process stops after the message is stored and before the reply is sent, the
  redelivery does not send it.
- A message without a provider id is given a generated id on each delivery and is
  treated as new each time.
- Not examined here: delivery-status updates in the same webhook, the scheduled
  WhatsApp reply worker, and Facebook and Instagram inbound messages.
- No test goes through HTTP with a signed body; the tests call the function the
  handler calls. The handler's use of the first-delivery flag for the automatic
  reply is covered by reading only.
