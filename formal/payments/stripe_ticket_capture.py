#!/usr/bin/env python3
"""PAY-STRIPE-WEBHOOK-001 / PAY-CAPTURE-RECON-001: the legacy Stripe ticket webhook.

A signed payment_intent.succeeded event whose PaymentIntent id matches a stored
ticket order is decided by three pinned regions:

  * handleStripePaymentIntentSucceeded (SocialEventsHandlers.hs): parse the
    capture, parse and validate the ticket binding, then a conditional update
    pending -> paid that issues the tickets, or a review of a closed order;
  * validateStripeTicketPaymentEvidence (same module): the six comparisons of
    the binding against the stored order;
  * parseStripeTicketCapture and stripeTicketClosedOrderNeedsReview
    (Server/StripeTicketCapture.hs).

The event is abstracted to what those functions read: whether the core fields
(status, amount_received) decode, whether the binding fields decode, the
normalized status, the amount, and whether each bound value equals the stored
one. The order is its total, its status when the update runs, and whether the
row still exists when it is read back.

Proofs (UNSAT of the negation):
  - tickets are issued only for an event that carries the full binding
    (succeeded, same PaymentIntent, order, event, amount and currency) and
    only while the order is pending;
  - the answer is 400 exactly when the binding is not admitted, and 204
    otherwise;
  - while the order row exists, a succeeded capture of a positive amount never
    ends without a trace: the tickets are issued, or the order was already
    paid by the same PaymentIntent, or an open reconciliation exception is
    written;
  - an exception is written only for a succeeded positive capture, the two
    exception types exclude each other and exclude ticket issue, and the
    exception carries the order total as expected and the received amount as
    observed;
  - an admitted binding for an order with a positive total is a capture;
  - a redelivery for an order already paid changes nothing and records nothing.

Witnesses: issue, redelivery, each exception type, and a refusal without a
capture are reachable. A further witness shows the one silent case left: the
order row is deleted between the lookup and the read-back while the stale copy
says pending. No application code deletes ticket orders.

Necessity: each of the six comparisons, the pending fence of the update and
the two exception writes are removed in turn; each must make a proof fail.

Not covered: the signature check and event-id dedupe before dispatch, JSON
decoding itself, the separate transaction of the not-admitted exception (a
crash between that write and the 400 leaves the exception, which is the safe
side), ticket code generation, the confirmation e-mail, and events whose
PaymentIntent matches no ticket order (courses, tips, marketplace).

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
HANDLERS = 'tdf-hq/src/TDF/Server/SocialEventsHandlers.hs'
REGIONS = (
    (HANDLERS, 'handleStripePaymentIntentSucceeded :: UTCTime -> Aeson.Value -> AppM NoContent',
     'issueMissingTicketsForOrder ::', 'ab35251ddbdaaea35c10ec8f776b65f2c6245dbcd83d655160f9b0f44ada6975'),
    (HANDLERS, 'validateStripeTicketPaymentEvidence ::',
     'parseStripeWebhookMarketplaceOrderId :: Aeson.Value -> Maybe T.Text', 'b8a75b76787d2e0ff430d3e981da1c5a5d1679fd5e63d680f73eea68f624e2e7'),
    ('tdf-hq/src/TDF/Server/StripeTicketCapture.hs',
     'parseStripeTicketCapture :: Aeson.Value -> Maybe StripeTicketCapture',
     '{- | Leave an open reconciliation exception for a capture on a legacy ticket', 'b2ad84672fc374bea1190345d783138b055c13c10d8e7bddf41e2fbb80fb5e58'),
)

PENDING, PAID, CLOSED = 0, 1, 2  # order status: pending, paid, anything else
NONE, NOT_ADMITTED, CLOSED_ORDER = 0, 1, 2  # exception written
COMPARISONS = ('status', 'payment-intent', 'order', 'event', 'amount', 'currency')
GUARDS = COMPARISONS + ('pending-fence', 'not-admitted-record', 'closed-order-record')


def sources_are_current():
    ok = True
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) != 1 or text.count(end) != 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        digest = hashlib.sha256(text[text.index(start):text.index(end)].encode()).hexdigest()
        current = digest == pin
        ok = ok and current
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


class Delivery:
    def __init__(self, tag):
        b = lambda n: z3.Bool(f'{n}_{tag}')
        i = lambda n: z3.Int(f'{n}_{tag}')
        # the signed event
        self.core = b('core')            # data.object.status and amount_received decode
        self.binding = b('binding')      # id, currency and metadata.tdf_context decode
        self.succeeded = b('succeeded')  # normalized status == "succeeded"
        self.amount = i('amount')        # amount_received
        self.same_intent, self.same_order = b('same_intent'), b('same_order')
        self.same_event, self.same_currency = b('same_event'), b('same_currency')
        # the stored order
        self.total = i('total')          # amountCents
        self.status = i('status')        # when the conditional update runs
        self.row_present = b('row_present')
        self.stale_status = i('stale_status')  # status of the copy read at lookup

    def wellformed(self):
        domain = lambda s: z3.And(s >= PENDING, s <= CLOSED)
        return z3.And(domain(self.status), domain(self.stale_status))


class Outcome:
    def __init__(self, d, dropped=None):
        keep = lambda name, cond: z3.BoolVal(True) if dropped == name else cond
        # parseStripeTicketCapture
        self.capture = z3.And(d.core, d.succeeded, d.amount > 0)
        # parseStripeTicketPaymentEvidence needs the core fields and the binding fields
        parsed = z3.And(d.core, d.binding)
        # validateStripeTicketPaymentEvidence
        self.admitted = z3.And(
            parsed, keep('status', d.succeeded), keep('payment-intent', d.same_intent),
            keep('order', d.same_order), keep('event', d.same_event),
            keep('amount', d.amount == d.total), keep('currency', d.same_currency))
        self.refused = z3.Not(self.admitted)
        changed = z3.And(self.admitted, keep('pending-fence', d.status == PENDING))
        self.issued = changed
        # read-back when nothing changed: the row, or the stale copy if it is gone
        current = z3.If(d.row_present, d.status, d.stale_status)
        needs_review = z3.And(current != PENDING, current != PAID)
        not_admitted = z3.And(self.refused, self.capture, keep('not-admitted-record', z3.BoolVal(True)))
        if dropped == 'not-admitted-record':
            not_admitted = z3.BoolVal(False)
        closed_order = z3.And(self.admitted, z3.Not(changed), needs_review, self.capture)
        if dropped == 'closed-order-record':
            closed_order = z3.BoolVal(False)
        self.exception = z3.If(not_admitted, NOT_ADMITTED, z3.If(closed_order, CLOSED_ORDER, NONE))
        self.expected, self.observed = d.total, d.amount
        self.http = z3.If(self.refused, 400, 204)
        self.status_after = z3.If(changed, PAID, d.status)


def properties(dropped=None):
    d = Delivery('a')
    o = Outcome(d, dropped)
    full_binding = z3.And(d.core, d.binding, d.succeeded, d.same_intent, d.same_order, d.same_event,
                          d.amount == d.total, d.same_currency)
    claims = {
        'tickets-only-with-the-full-binding-on-a-pending-order': z3.Implies(
            o.issued, z3.And(full_binding, d.status == PENDING)),
        'answer-is-400-exactly-when-not-admitted': z3.And(
            (o.http == 400) == z3.Not(full_binding), (o.http == 204) == full_binding),
        'a-capture-on-an-existing-order-always-leaves-a-trace': z3.Implies(
            z3.And(o.capture, d.row_present),
            z3.Or(o.issued, z3.And(full_binding, d.status == PAID), o.exception != NONE)),
        'exceptions-only-for-captures-and-never-with-tickets': z3.Implies(
            o.exception != NONE,
            z3.And(d.core, d.succeeded, d.amount > 0, z3.Not(o.issued),
                   o.expected == d.total, o.observed == d.amount)),
        'not-admitted-exception-exactly-for-a-refused-capture': (o.exception == NOT_ADMITTED) == z3.And(
            z3.Not(full_binding), d.core, d.succeeded, d.amount > 0),
        'closed-order-exception-needs-the-binding-and-a-closed-order': z3.Implies(
            z3.And(o.exception == CLOSED_ORDER, d.row_present), z3.And(full_binding, d.status == CLOSED)),
        'an-admitted-binding-for-a-positive-total-is-a-capture': z3.Implies(
            z3.And(o.admitted, d.total > 0), o.capture),
        'paid-redelivery-changes-nothing-and-records-nothing': z3.Implies(
            z3.And(full_binding, d.status == PAID, d.row_present),
            z3.And(z3.Not(o.issued), o.exception == NONE, o.status_after == PAID, o.http == 204)),
        'only-a-pending-order-becomes-paid': z3.Implies(
            o.status_after != d.status, z3.And(d.status == PENDING, o.status_after == PAID, full_binding)),
    }
    return {name: z3.Implies(d.wellformed(), claim) for name, claim in claims.items()}


def proved(claim):
    solver = z3.Solver()
    solver.set('timeout', 60000)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def satisfiable(*facts):
    solver = z3.Solver()
    solver.set('timeout', 60000)
    solver.add(*facts)
    return solver.check() == z3.sat


def witnesses():
    d = Delivery('w')
    o = Outcome(d)
    return {
        'tickets-issued': (o.issued,),
        'paid-redelivery': (o.admitted, d.status == PAID, d.row_present, o.exception == NONE),
        'not-admitted-exception': (o.exception == NOT_ADMITTED, o.http == 400),
        'closed-order-exception': (o.exception == CLOSED_ORDER, o.http == 204, d.row_present),
        'refusal-without-a-capture-records-nothing': (o.refused, z3.Not(o.capture), o.exception == NONE),
        'limit-deleted-order-with-a-stale-pending-copy-is-silent': (
            o.capture, o.admitted, z3.Not(d.row_present), d.status == CLOSED, d.stale_status == PENDING,
            z3.Not(o.issued), o.exception == NONE),
    }, d


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    for name, claim in properties().items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    cases, d = witnesses()
    for name, facts in cases.items():
        result = satisfiable(d.wellformed(), *facts)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        broken = [name for name, claim in properties(guard).items() if not proved(claim)]
        result = bool(broken)
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity drop-{guard} "
              f"{'breaks ' + ','.join(broken) if result else 'FAILED (guard is not load-bearing)'}", flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
