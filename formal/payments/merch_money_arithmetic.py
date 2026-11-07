#!/usr/bin/env python3
"""PAY-CHECKOUT-003: unbounded lemmas behind TDF.Commerce.Merch.calculateMerchMoney
and checkedMerchSubtotals.

calculateMerchMoney computes over Haskell Integer:
  base       = subtotal - discount
  commission = base * bps `div` 10000
  total      = base + tax + shipping
  sellerNet  = total - commission - processorFee
and returns a value only when every guard passes. The lemmas assume exactly
those guards on Int64 inputs and prove, by UNSAT of the negation, that every
stored field fits Int64 and that the money is conserved. Haskell `div` floors;
SMT-LIB integer division agrees with it for the positive divisor 10000.
Witnesses show the hypotheses are satisfiable; controls show each guard is
needed.

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


def int64(*values):
    return [z3.And(v >= INT64_MIN, v <= INT64_MAX) for v in values]


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    subtotal, discount, tax, shipping, fee, bps = z3.Ints('subtotal discount tax shipping processorFee commissionBps')
    base = subtotal - discount
    commission = base * bps / 10000
    total = base + tax + shipping
    seller_net = total - commission - fee

    inputs = int64(subtotal, discount, tax, shipping, fee)
    guards = inputs + [
        subtotal > 0,
        discount >= 0, discount <= subtotal,
        tax >= 0, shipping >= 0, fee >= 0,
        bps >= 0, bps <= 10000,
        seller_net >= 0,
        total > 0, total <= INT64_MAX,
        seller_net <= INT64_MAX,
    ]
    # The same guards without one of them, for the controls.
    def without(*dropped):
        return [g for g in guards if not any(z3.eq(g, d) for d in dropped)]

    q, price, rest, s = z3.Ints('quantity price rest sum')
    results = [
        # Commission is the floor of base * bps / 10000 and lies within 0..base.
        check('commission-is-floor', guards,
              z3.And(commission * 10000 <= base * bps, base * bps < (commission + 1) * 10000), 'proof'),
        check('commission-within-base', guards, z3.And(commission >= 0, commission <= base), 'proof'),
        # Rounding never favours the platform: commission <= exact share.
        check('commission-not-above-exact-share', guards, commission * 10000 <= base * bps, 'proof'),
        # Every stored field fits Int64.
        check('stored-fields-fit-int64', guards,
              z3.And(commission >= 0, commission <= INT64_MAX,
                     seller_net >= 0, seller_net <= INT64_MAX,
                     total >= 1, total <= INT64_MAX), 'proof'),
        # Conservation: the payable total splits exactly into commission, processor fee and seller net.
        check('total-conserved', guards, commission + fee + seller_net == total, 'proof'),
        # Free-line rule of checkedMerchSubtotals: a nonnegative line within a nonnegative
        # remainder never exceeds the accepted sum, so every line amount fits Int64.
        check('line-within-accepted-sum',
              [q >= 1, q <= 100, price >= 0, price <= INT64_MAX, rest >= 0,
               s == q * price + rest, s > 0, s <= INT64_MAX],
              z3.And(q * price >= 0, q * price <= s), 'proof'),
        # Witnesses: an ordinary order and a full-commission order are accepted.
        check('ordinary-order-accepted', guards + [bps == 1500, discount > 0, fee > 0, seller_net > 0], z3.BoolVal(True), 'witness'),
        check('full-commission-accepted', guards + [bps == 10000], commission == base, 'witness'),
        # Controls: dropping a guard admits a value that breaks a lemma.
        check('control-rate-above-10000', without(bps <= 10000) + [bps == 10001], commission > base, 'witness'),
        check('control-discount-above-subtotal', without(discount <= subtotal), commission < 0, 'witness'),
        check('control-no-seller-net-guard', without(seller_net >= 0), seller_net < 0, 'witness'),
        check('control-no-total-bound', without(total <= INT64_MAX), total > INT64_MAX, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
