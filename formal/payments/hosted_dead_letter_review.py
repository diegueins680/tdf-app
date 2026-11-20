#!/usr/bin/env python3
"""PAY-HOSTED-NOTIFICATIONS-001 / PAY-CAPTURE-RECON-001: review of hosted dead letters.

A PlaceToPay or PayPhone inbox event that ends as a dead letter while bound to
a real payment may stand for captured money that no checkout admitted. The
translation covers four pinned regions:

  * processProviderEventWith and queryAndApply (ProviderReconciliation.hs):
    which result each stage returns and whether it carries the bound payment
    in prrReview;
  * recordDeadLetterReview (same module): the fenced write;
  * processHostedPayload and markRetry (ProviderEventWorker.hs): what the
    worker writes for each result.

One attempt of one event is abstracted to the stage at which reconciliation
ends, whether this worker still holds the lease when it writes, and whether
the attempt is the last allowed one.

Proofs (UNSAT of the negation):
  - a bound event that this attempt turns into a dead letter always leaves an
    open exception: the dead-letter review, or the exception its own stage
    wrote (query mismatch, verified payment on a closed checkout);
  - the review is written only for a bound event whose inbox row is a dead
    letter at this attempt, never for an unbound one and never together with
    the stage's own exception;
  - a processed event and a retry that is not exhausted write no review;
  - a worker that lost its lease changes nothing and records nothing;
  - the inbox row ends as a dead letter exactly for a dead-letter result or
    an exhausted retry, under a held lease.

Witnesses: review after an exhausted retry, review after an unsupported
query, review after reconciliation throws on the last attempt, a mismatch
with its own exception only, and an unbound dead letter without exception.

Necessity: keeping the binding when reconciliation throws, keeping it for a
refused application and for a nonterminal provider state, the review of a
direct dead letter, the review at retry exhaustion, the fence, and clearing
the review where the stage wrote its own exception are removed in turn; each
must make a proof fail.

Not covered: that the two stages said to write their own exception do so
(recordQueryMismatch and reviewClosedCheckoutApproval are assumed), the
open-exception fingerprint that keeps replays to one row, lease expiry
timing, the PayPal branch of the worker, and an exception thrown before the
event is bound (it retries and dead-letters unbound, by design: forged
callbacks must not fill the reconciliation queue).

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
RECON = 'tdf-hq/src/TDF/Commerce/ProviderReconciliation.hs'
WORKER = 'tdf-hq/src/TDF/Commerce/ProviderEventWorker.hs'
REGIONS = (
    (RECON, 'processProviderEventWith\n', 'data QueryFailure = ', '276af4c661c01f4cfe0491c578b663e63332b934d7cc48b90c3febbae5d3a9bc'),
    (RECON, 'recordDeadLetterReview\n  :: ProviderEvent.ProviderEventReference', 'retry :: Text -> Maybe', '4670eec7367b0788b5baf10781a98c187354eceaf06a057ef746f168421a4344'),
    (WORKER, 'processHostedPayload\n', 'applyOutcome\n', '81960ddfcba3a796c05ea7024444fae914be0fd5ca8da17d79452d4a4d0cfbc2'),
    (WORKER, 'markRetry\n', 'validateStoredPaypalEvent\n', '2ad6c5d3c190880f77235206412c98d8bef6dc61010d8a39792c2bc03a21d7aa'),
)

# Stage at which processProviderEventWith ends (or the worker's outer catch).
STAGES = (
    'envelope-invalid', 'runtime-unavailable', 'callback-invalid', 'assessment-invalid',
    'binding-unavailable', 'threw-unbound',
    'threw-bound', 'budget-denied', 'query-mismatch', 'query-unsupported', 'query-unavailable',
    'apply-refused', 'processed', 'nonterminal', 'closed-checkout-held',
)
IX = {name: index for index, name in enumerate(STAGES)}
BOUND = STAGES[STAGES.index('threw-bound'):]
PROCESSED, RETRY, DEAD = 0, 1, 2          # ReconciliationDisposition
ROW_LEASED, ROW_PROCESSED, ROW_RETRY, ROW_DEAD = 0, 1, 2, 3
GUARDS = ('throw-keeps-binding', 'refused-keeps-binding', 'nonterminal-keeps-binding',
          'direct-dead-letter-review', 'exhausted-retry-review', 'fence', 'own-exception-clears-review')


def sources_are_current():
    ok = True
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) < 1 or text.count(end) < 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        begin = text.index(start)
        digest = hashlib.sha256(text[begin:text.index(end, begin)].encode()).hexdigest()
        current = digest == pin
        ok = ok and current
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


def one_of(stage, names):
    return z3.Or([stage == IX[name] for name in names])


class Attempt:
    def __init__(self, tag, dropped=None):
        self.stage = z3.Int(f'stage_{tag}')
        self.lease_held = z3.Bool(f'lease_held_{tag}')   # the fenced inbox write applies
        self.last_attempt = z3.Bool(f'last_attempt_{tag}')  # attemptCount >= maxProviderEventAttempts
        stage = self.stage
        self.wellformed = z3.And(stage >= 0, stage < len(STAGES))
        self.bound = one_of(stage, BOUND)

        # processProviderEventWith / queryAndApply, and the worker's outer tryAny
        self.disposition = z3.If(
            stage == IX['processed'], PROCESSED,
            z3.If(one_of(stage, ('envelope-invalid', 'callback-invalid', 'assessment-invalid',
                                 'query-mismatch', 'query-unsupported', 'closed-checkout-held')), DEAD, RETRY))
        carries = ['budget-denied', 'query-unsupported', 'query-unavailable']
        if dropped != 'throw-keeps-binding':
            carries.append('threw-bound')
        if dropped != 'refused-keeps-binding':
            carries.append('apply-refused')
        if dropped != 'nonterminal-keeps-binding':
            carries.append('nonterminal')
        if dropped == 'own-exception-clears-review':
            carries += ['query-mismatch', 'closed-checkout-held']
        self.review_carried = one_of(stage, carries)
        # recordQueryMismatch / reviewClosedCheckoutApproval (assumed to write)
        self.own_exception = one_of(stage, ('query-mismatch', 'closed-checkout-held'))

        # processHostedPayload / markRetry: fenced inbox write
        exhausted = z3.And(self.disposition == RETRY, self.last_attempt)
        wants_dead = z3.Or(self.disposition == DEAD, exhausted)
        self.row = z3.If(
            z3.Not(self.lease_held), ROW_LEASED,
            z3.If(self.disposition == PROCESSED, ROW_PROCESSED, z3.If(wants_dead, ROW_DEAD, ROW_RETRY)))
        # recordDeadLetterReview is attempted after a dead-letter result and after an exhausted retry
        attempted = z3.Or(
            z3.And(self.disposition == DEAD, z3.BoolVal(dropped != 'direct-dead-letter-review')),
            # markProviderEventRetry reports exhaustion only when its fenced update applied
            z3.And(exhausted, self.lease_held, z3.BoolVal(dropped != 'exhausted-retry-review')))
        fence = z3.BoolVal(True) if dropped == 'fence' else self.row == ROW_DEAD
        self.review_written = z3.And(attempted, self.review_carried, fence)


def properties(dropped=None):
    a = Attempt('a', dropped)
    claims = {
        'a-bound-dead-letter-always-leaves-an-exception': z3.Implies(
            z3.And(a.bound, a.row == ROW_DEAD), z3.Or(a.review_written, a.own_exception)),
        'review-only-for-a-bound-dead-letter-at-this-attempt': z3.Implies(
            a.review_written, z3.And(a.bound, a.row == ROW_DEAD, a.lease_held)),
        'review-never-doubles-the-stage-exception': z3.Not(z3.And(a.review_written, a.own_exception)),
        'processed-and-unexhausted-retry-write-no-review': z3.Implies(
            z3.Or(a.row == ROW_PROCESSED, a.row == ROW_RETRY), z3.Not(a.review_written)),
        'a-lost-lease-changes-nothing-and-records-nothing': z3.Implies(
            z3.Not(a.lease_held), z3.And(a.row == ROW_LEASED, z3.Not(a.review_written))),
        'dead-letter-exactly-for-a-dead-result-or-an-exhausted-retry': (a.row == ROW_DEAD) == z3.And(
            a.lease_held, z3.Or(a.disposition == DEAD, z3.And(a.disposition == RETRY, a.last_attempt))),
        'unbound-events-never-get-a-review': z3.Implies(z3.Not(a.bound), z3.Not(a.review_written)),
    }
    return {name: z3.Implies(a.wellformed, claim) for name, claim in claims.items()}


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
    a = Attempt('w')
    return a, {
        'review-after-an-exhausted-retry': (a.stage == IX['query-unavailable'], a.review_written),
        'review-after-an-unsupported-query': (a.stage == IX['query-unsupported'], a.review_written),
        'review-after-reconciliation-throws-on-the-last-attempt': (a.stage == IX['threw-bound'], a.review_written),
        'mismatch-keeps-only-its-own-exception': (
            a.stage == IX['query-mismatch'], a.row == ROW_DEAD, z3.Not(a.review_written), a.own_exception),
        'unbound-dead-letter-without-exception': (
            a.stage == IX['envelope-invalid'], a.row == ROW_DEAD, z3.Not(a.review_written), z3.Not(a.own_exception)),
    }


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    for name, claim in properties().items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    a, cases = witnesses()
    for name, facts in cases.items():
        result = satisfiable(a.wellformed, *facts)
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
