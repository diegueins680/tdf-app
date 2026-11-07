#!/usr/bin/env python3
"""PAY-CHECKOUT-001: unbounded lemmas behind TDF.Commerce.Money.

checkedCheckoutSubtotals computes each line product q * u and their sum over
Haskell Integer, and returns the products narrowed to Int64 only when
expected > 0, every quantity is in 1..maxBound Int32, every unit amount is
positive, no product exceeds maxBound Int64 and the sum equals expected.
checkedCartSubtotal / checkedCartTotal (legacy marketplace, host Int, assumed
64-bit) narrow an exact product or sum of nonnegative amounts only when it is at
most maxBound Int.

The lemmas assume exactly those guards and prove, by UNSAT of the negation, that
narrowing is the identity on every accepted value and that accepted amounts
compose to the parent. Lists are covered for every length from 1 to 6, and the
step lemma that extends any accepted list by one line holds for all lengths.
Witnesses show the hypotheses are satisfiable; controls show that narrowing
before computing (64-bit wrap-around) would accept a wrong amount.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'
INT32_MAX = 2 ** 31 - 1
INT64_MAX = 2 ** 63 - 1
INT64_MIN = -(2 ** 63)
WORD = 2 ** 64


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def wrap64(x):
    """Two's-complement Int64 value of x, as fromInteger would produce."""
    r = x % WORD
    return z3.If(r > INT64_MAX, r - WORD, r)


def canonical(n):
    quantities = z3.Ints(' '.join(f'q{i}' for i in range(n)))
    units = z3.Ints(' '.join(f'u{i}' for i in range(n)))
    expected = z3.Int('expected')
    products = [q * u for q, u in zip(quantities, units)]
    guards = [expected > 0, expected <= INT64_MAX]
    for q, u, p in zip(quantities, units, products):
        guards += [q >= 1, q <= INT32_MAX, u >= 1, u <= INT64_MAX, p <= INT64_MAX]
    guards.append(z3.Sum(products) == expected)
    return guards, products, expected


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    results = []
    for n in range(1, 7):
        guards, products, expected = canonical(n)
        # Every accepted line amount is positive, at most the parent, and survives
        # narrowing unchanged; the narrowed lines still sum to the parent.
        results.append(check(
            f'canonical-lines-exact-n{n}', guards,
            z3.And(*[z3.And(p >= 1, p <= expected, wrap64(p) == p) for p in products],
                   z3.Sum([wrap64(p) for p in products]) == expected),
            'proof'))
    # Step lemma for any length: a positive line added to a positive remainder
    # whose sum is an accepted parent stays within the parent.
    line, rest, parent = z3.Ints('line rest parent')
    results.append(check('canonical-step-any-length',
                         [line >= 1, rest >= 0, parent == line + rest, parent <= INT64_MAX],
                         z3.And(line <= parent, rest <= parent, wrap64(line) == line), 'proof'))
    # Legacy cart: an accepted exact product or sum narrows unchanged, and each
    # nonnegative amount lies within the accepted total.
    q, u, a, b, c = z3.Ints('cartQuantity cartUnit a b c')
    results.append(check('cart-subtotal-exact',
                         [q >= 1, q <= INT64_MAX, u >= 0, u <= INT64_MAX, q * u <= INT64_MAX],
                         z3.And(q * u >= 0, wrap64(q * u) == q * u), 'proof'))
    results.append(check('cart-total-exact',
                         [a >= 0, b >= 0, c >= 0, a <= INT64_MAX, b <= INT64_MAX, c <= INT64_MAX,
                          a + b + c <= INT64_MAX],
                         z3.And(wrap64(a + b + c) == a + b + c, a <= a + b + c), 'proof'))
    # Witness: a multi-line canonical checkout is accepted.
    guards, products, expected = canonical(3)
    results.append(check('three-line-checkout-accepted', guards + [products[0] != products[1]],
                         z3.BoolVal(True), 'witness'))
    # Controls: narrowing before computing (Int64 wrap-around) accepts a wrong amount.
    q0, u0, e0 = z3.Ints('wrapQuantity wrapUnit wrapExpected')
    results.append(check('control-wrapped-product-matches-parent',
                         [q0 >= 1, q0 <= INT32_MAX, u0 >= 1, u0 <= INT64_MAX,
                          e0 >= 1, e0 <= INT64_MAX, wrap64(q0 * u0) == e0],
                         q0 * u0 != e0, 'witness'))
    results.append(check('control-wrapped-cart-sum-nonnegative',
                         [a >= 0, b >= 0, c >= 0, a <= INT64_MAX, b <= INT64_MAX, c <= INT64_MAX,
                          wrap64(a + b + c) >= 0],
                         a + b + c != wrap64(a + b + c), 'witness'))
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
