#!/usr/bin/env python3
"""PAY-DOMO-QUOTE-001: unbounded arithmetic lemmas behind calculateDomoQuote
(tdf-hq/src/TDF/Commerce/DomoQuotes.hs).

roundBasisPoints a b = (a * b + 5000) `div` 10000 over mathematical integers
(Haskell Integer). Each safety lemma is proved by showing its negation UNSAT.
Witnesses show the hypotheses are satisfiable, and mutation controls show the
lemmas depend on the stated bounds.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'
M = 2**63 - 1  # maxBound :: Int64


def rbp(amount, bps):
    return (amount * bps + 5000) / 10000  # z3 Int division; divisor positive


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
    t, s, b, x, q, u = z3.Ints('total subtotal depositBps taxBps quantity unit')
    deposit = rbp(t, b)
    tax = rbp(s, x)
    results = [
        # Deposit never exceeds the total and is never negative.
        check('deposit-within-total', [t >= 0, b >= 1, b <= 10000],
              z3.And(deposit >= 0, deposit <= t), 'proof'),
        # Hence the balance is never negative.
        check('balance-nonnegative', [t >= 0, b >= 1, b <= 10000], t - deposit >= 0, 'proof'),
        # Tax is between zero and the subtotal.
        check('tax-within-subtotal', [s >= 0, x >= 0, x <= 10000],
              z3.And(tax >= 0, tax <= s), 'proof'),
        # The overflow guard on line amounts also bounds every unit amount.
        check('unit-bounded-by-line', [q >= 1, u >= 0, q * u <= M], u <= M, 'proof'),
        # A positive total always yields a positive deposit only when the total
        # is large enough; the code rejects zero deposits explicitly.
        check('zero-deposit-reachable', [t >= 1, b >= 1, b <= 10000], deposit == 0, 'witness'),
        # Non-vacuity of the main hypotheses.
        check('hypotheses-satisfiable', [t >= 1, b >= 1, b <= 10000, s >= 1, x >= 0, x <= 10000],
              z3.And(deposit >= 1, tax >= 0), 'witness'),
        # Mutation controls: the bounds are necessary.
        check('control-deposit-over-10000bps', [t >= 0, b >= 1, b <= 10001], deposit > t, 'witness'),
        check('control-negative-total', [t >= -1, b >= 1, b <= 10000], t - deposit < 0, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
