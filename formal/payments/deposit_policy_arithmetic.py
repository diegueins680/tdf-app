#!/usr/bin/env python3
"""PAY-DEPOSIT-POLICY-001: unbounded lemmas behind
TDF.Commerce.DepositPolicy.applyDepositPolicy, used by Domo quotes and course
checkouts.

  raised = max 1 (max minimumDeposit computedDeposit)
  FullPaymentNow  when total <= 0, raised >= total,
                  or raised * 10000 > total * fullPaymentAboveBps
  DepositOf raised otherwise

The lemmas assume a policy that validateDepositPolicy accepts (minimum >= 0,
threshold in 0..10000) and Int64 inputs, and prove by UNSAT of the negation that
an admitted deposit leaves a positive balance, respects the approved share, and
is never below the minimum or the computed deposit. Witnesses show both outcomes
are reachable with the default policy; controls show each guard is needed.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'
INT64_MIN = -(2 ** 63)
INT64_MAX = 2 ** 63 - 1


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def zmax(a, b):
    return z3.If(a >= b, a, b)


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    minimum, bps, total, computed = z3.Ints('minimumDeposit fullPaymentAboveBps total computedDeposit')
    raised = zmax(1, zmax(minimum, computed))

    def outcome(use_total_guard=True, use_share_guard=True):
        full = total <= 0
        if use_total_guard:
            full = z3.Or(full, raised >= total)
        if use_share_guard:
            full = z3.Or(full, raised * 10000 > total * bps)
        return full

    full = outcome()
    deposit = z3.Not(full)
    domain = [minimum >= 0, minimum <= INT64_MAX, bps >= 0, bps <= 10000,
              total >= INT64_MIN, total <= INT64_MAX, computed >= INT64_MIN, computed <= INT64_MAX]
    admitted = domain + [deposit]
    results = [
        # An admitted deposit leaves a positive balance and fits Int64.
        check('deposit-leaves-positive-balance', admitted,
              z3.And(raised >= 1, raised <= total - 1, total - raised >= 1, raised <= INT64_MAX), 'proof'),
        # It never exceeds the approved share of the total.
        check('deposit-within-approved-share', admitted, raised * 10000 <= total * bps, 'proof'),
        # It is never below the minimum or the computed deposit.
        check('deposit-not-below-minimum-or-computed', admitted,
              z3.And(raised >= minimum, raised >= computed), 'proof'),
        # Nonpositive totals always charge the total (nothing is called a deposit).
        check('nonpositive-total-is-full-payment', domain + [total <= 0], full, 'proof'),
        # Raising the minimum never turns a full payment into a deposit.
        check('higher-minimum-keeps-full-payment',
              domain + [full, z3.Int('higherMinimum') >= minimum],
              z3.substitute(full, (minimum, z3.Int('higherMinimum'))), 'proof'),
        # Witnesses with the default policy (500 minor units, 9000 bps).
        check('default-policy-admits-deposit',
              domain + [minimum == 500, bps == 9000, total == 100000, computed == 30000, deposit], raised == 30000, 'witness'),
        check('default-policy-raises-to-minimum',
              domain + [minimum == 500, bps == 9000, total == 2000, computed == 100, deposit], raised == 500, 'witness'),
        check('default-policy-full-payment-for-small-total',
              domain + [minimum == 500, bps == 9000, total == 520, computed == 52], full, 'witness'),
        # Controls: without the total guard a 100% threshold admits a zero-balance
        # "deposit"; without the share guard a deposit can exceed the approved share.
        check('control-no-total-guard',
              domain + [z3.Not(outcome(use_total_guard=False)), bps == 10000], raised >= total, 'witness'),
        check('control-no-share-guard',
              domain + [z3.Not(outcome(use_share_guard=False))], raised * 10000 > total * bps, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
