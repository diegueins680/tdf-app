#!/usr/bin/env python3
"""PAY-DOMO-CANCEL-001 (AUTHORITY-093): unbounded lemmas behind
TDF.Commerce.DomoCancellation.domoCancellationRefund.

retained(d, bps) = (d * bps + 5000) div 10000 over mathematical integers;
refund = (d - retained) + balance for a customer cancellation, d + balance for a
TDF cancellation. Lemmas are proved by UNSAT of their negation; witnesses show
the hypotheses are satisfiable; controls show the bounds are necessary.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'


def retained(d, bps):
    return (d * bps + 5000) / 10000


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    d, b, bps, bps2 = z3.Ints('deposit balance retainedBps retainedBps2')
    refund = d - retained(d, bps) + b
    results = [
        # The customer refund never exceeds the collected money and is never negative.
        check('refund-within-collected', [d >= 0, b >= 0, bps >= 0, bps <= 10000],
              z3.And(refund >= 0, refund <= d + b), 'proof'),
        # Full retention refunds exactly the balance; zero retention refunds everything.
        check('full-retention-refunds-balance', [d >= 0, b >= 0], d - retained(d, 10000) + b == b, 'proof'),
        check('zero-retention-refunds-all', [d >= 0, b >= 0], d - retained(d, 0) + b == d + b, 'proof'),
        # A lower retention rate never lowers the refund (monotone in the policy ladder).
        check('refund-monotone-in-retention', [d >= 0, bps >= 0, bps2 >= 0, bps2 <= bps, bps <= 10000],
              d - retained(d, bps2) >= d - retained(d, bps), 'proof'),
        # Retained plus refunded deposit is exactly the deposit: nothing is lost or created.
        check('deposit-conserved', [d >= 0, bps >= 0, bps <= 10000],
              retained(d, bps) + (d - retained(d, bps)) == d, 'proof'),
        # Witnesses and controls.
        check('partial-window-reachable', [d >= 1, b >= 0, bps == 5000], z3.And(refund > b, refund < d + b), 'witness'),
        check('control-rate-above-10000', [d >= 1, b >= 0, bps == 10001], refund < b, 'witness'),
        check('control-negative-deposit', [d <= -1, b >= 0, bps >= 0, bps <= 10000], refund > d + b, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
