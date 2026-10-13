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
- An enrolment message (`inscribirme`) is registered by every delivery; the
  registration is keyed by the stored message, so it is made once, and a delivery
  that follows a failed one completes it. Its automatic reply is sent by the one
  delivery that claims it (an `auto_reply_claimed` audit entry of the message,
  written before sending).
- The consent confirmation notice is sent only by the delivery that confirmed.
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
consent confirmation in one transaction, only when it stored the message.
`claimWhatsAppAutoReply` gives the reply of a message to one caller. The course
registration still runs on every delivery, as before. The hourly limit admits a
delivery when the sender is under three registrations or when this message already
made its registration (`whatsAppCourseRegistrationAdmitted`): a redelivery after
the registration committed and before the reply was claimed creates nothing and
still completes the reply, even if that registration was the one that filled the
limit.

### Rows stored before this version (review of 2026-10-12)

The previous handler stored the message and applied its opt-out or
confirmation in later, separate transactions, and recorded no claim before
sending the enrolment reply. Those rows survive the rollout. Two compatibility
rules cover them: a stored message without an `effects_applied` audit entry
(every row stored by this version writes one in its storing transaction)
applies its effects on its next delivery, once, and then carries the entry; the
reply claim also refuses a message whose `replied_at` is set, which is how the
previous version recorded a sent reply. The one-time rule may re-apply a STOP
that a later subscription reversed, for such a row only; leaving a STOP
ineffective was judged worse. Not modelled: the model starts from rows stored
by this version.

## Evidence

- `formal/event-operations/WhatsAppRedelivery.tla`: three deliveries over two
  `STOP` messages and one enrolment message, up to two new subscriptions in
  between, and a process that may stop after any committed step. 21,848 distinct
  states, five invariants: `StoredOnce`, `AppliedOnce`, `LaterSubscriptionKept`
  (after a new subscription, consent stays until a `STOP` is newly stored),
  `RepliedOnce`, `StoredStopApplied`. Four controls each change one guard and are
  each caught by a named invariant: every delivery applies (`AppliedOnce`),
  storage is not unique (`StoredOnce`), the effect follows the store in a later
  step (`StoredStopApplied`), the reply is not claimed (`RepliedOnce`). A control
  can break further invariants deeper in its state space; the runner requires the
  named one.
- `tdf-hq/test/TDF/WhatsAppRedeliverySpec.hs` (`scripts/test-whatsapp-redelivery.sh`,
  PostgreSQL, fully migrated schema, no message sent): the first `STOP` withdraws
  consent; the same `STOP` delivered again after a new subscription leaves the
  subscription; a new `STOP` still withdraws it; a redelivered message has one row
  and one audit entry; of eight simultaneous deliveries exactly one is the first; a
  pending request is confirmed once and reported only by the first delivery; an
  earlier yes redelivered after a withdrawal and a new request confirms nothing;
  the reply of a message is given to one caller, and to exactly one of eight
  simultaneous callers. A redelivery in the tests repeats the original sent time.
  With the first-delivery guard removed, the later-subscription case and four
  others fail (run locally on 2026-10-11).

## Limits

- The model is bounded and checks safety only. It represents the unique index, not
  the advisory lock: in the model a losing simultaneous delivery ends without
  effect, in the implementation it waits and finds the message stored. Consent
  confirmation and the course registration are not modelled.
- A `STOP` delivered for the first time after a later subscription (a delayed
  first delivery) still withdraws consent. The rule errs toward the opt-out.
- The automatic reply and the confirmation notice are sent at most once. If the
  process stops after the reply is claimed and before it is sent, or after a
  confirmation commits and before its notice is sent, no later delivery sends it.
- An opt-out is applied only for a sender number that normalizes; the webhook
  drops other senders before this point.
- A message without a provider id is given an id derived from its sender, sent
  time and body, so its redeliveries are recognised; two different messages from
  one sender with the same second and body would be taken for one.
- Not examined here: delivery-status updates in the same webhook, the scheduled
  WhatsApp reply worker, and Facebook and Instagram inbound messages.
- No test goes through HTTP with a signed body; the tests call the function the
  handler calls. That the handler registers on every delivery and sends only
  after a successful claim is covered by reading only.
