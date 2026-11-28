#!/usr/bin/env python3
"""SOCIAL-DISCOVER-AFFINITY-001: lemmas behind the Discover order over the
integers (formal/system/social-discover-affinity.md).

  units(r, p) = min(r + 2 * p, 20)                    r, p >= 0
  K(page)     = page div 2                             1 <= page <= 50
  key         = led candidates first, by (units desc, id asc);
                then all others by the previous key
                (shared_interest desc, rotation asc, id asc)

`rotation` is any integer: the lemmas hold for every value of the daily md5
key, so they do not depend on it. Which candidates lead is decided by counting
and is proved on the model (formal/social/DiscoverAffinity.tla), not here.

Each lemma is proved by UNSAT of its negation. Controls are satisfiable
witnesses that a weakened rule breaks the lemma.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'
REPLY_WEIGHT, CAP, MAX_PAGE = 2, 20, 50


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def units(reactions, replies, cap=CAP):
    raw = reactions + REPLY_WEIGHT * replies
    return raw if cap is None else z3.If(raw <= cap, raw, cap)


class Candidate:
    """A candidate: whether it leads, its units and its previous key."""

    def __init__(self, tag):
        self.led = z3.Bool(f'led_{tag}')
        self.units = z3.Int(f'units_{tag}')
        self.shared = z3.Int(f'shared_{tag}')
        self.rotation = z3.Int(f'rotation_{tag}')
        self.ident = z3.Int(f'id_{tag}')


def previous_before(x, y, use_id=True):
    return z3.Or(
        x.shared > y.shared,
        z3.And(x.shared == y.shared, x.rotation < y.rotation),
        z3.And(x.shared == y.shared, x.rotation == y.rotation, x.ident < y.ident) if use_id else False)


def before(x, y, use_id=True):
    """x is placed before y by the key."""
    return z3.Or(
        z3.And(x.led, z3.Not(y.led)),
        z3.And(x.led, y.led, z3.Or(x.units > y.units, z3.And(x.units == y.units, x.ident < y.ident))),
        z3.And(z3.Not(x.led), z3.Not(y.led), previous_before(x, y, use_id)))


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    r, p, more = z3.Ints('r p more')
    page, k = z3.Ints('page k')
    x, y, z = Candidate('x'), Candidate('y'), Candidate('z')
    counts = [r >= 0, p >= 0]
    pages = [page >= 1, page <= MAX_PAGE, k == page / 2]
    results = [
        # P6: units stay in 0..20, are 0 exactly without interaction and never fall.
        check('units-range', counts, z3.And(units(r, p) >= 0, units(r, p) <= CAP), 'proof'),
        check('units-zero-iff-no-interaction', counts, (units(r, p) == 0) == z3.And(r == 0, p == 0), 'proof'),
        check('units-monotone-in-reactions', counts + [more >= r], units(more, p) >= units(r, p), 'proof'),
        check('units-monotone-in-replies', counts + [more >= p], units(r, more) >= units(r, p), 'proof'),
        check('units-cap-reached', counts + [r + REPLY_WEIGHT * p >= CAP], units(r, p) == CAP, 'proof'),
        check('units-reply-is-two-reactions', counts, units(r, p) == units(r + REPLY_WEIGHT * p, 0), 'proof'),
        # P4: at most half a page leads, so at least half a page keeps the previous
        # order, and a page of one is never changed.
        check('half-page-bounds', pages, z3.And(k >= 0, 2 * k <= page, page - k >= 1, k <= MAX_PAGE / 2), 'proof'),
        check('page-of-one-unchanged', pages + [page == 1], k == 0, 'proof'),
        # P5: the key is a strict total order on candidates with distinct ids.
        check('order-irreflexive', [], z3.Not(before(x, x)), 'proof'),
        check('order-asymmetric', [], z3.Not(z3.And(before(x, y), before(y, x))), 'proof'),
        check('order-transitive', [before(x, y), before(y, z)], before(x, z), 'proof'),
        check('order-total', [x.ident != y.ident], z3.Or(before(x, y), before(y, x)), 'proof'),
        # P1: where nobody leads the key is the previous key.
        check('no-leader-is-previous-order', [z3.Not(x.led), z3.Not(y.led)],
              before(x, y) == previous_before(x, y), 'proof'),
        # P4: a candidate that does not lead is overtaken only by a leader or by a
        # candidate that already preceded it.
        check('overtaken-only-by-leaders', [z3.Not(x.led), before(y, x)],
              z3.Or(y.led, previous_before(y, x)), 'proof'),
        check('previous-order-kept-among-others', [z3.Not(x.led), z3.Not(y.led), previous_before(x, y)],
              before(x, y), 'proof'),
        # Controls. Without the cap units are unbounded; with a reply weight of one
        # a reply is no longer two reactions; without the id two distinct
        # candidates can be unordered.
        check('control-no-cap', counts, units(r, p, cap=None) > CAP, 'witness'),
        check('control-reply-weight-one', counts, r + p != units(r, p), 'witness'),
        check('control-no-id-tie-break', [x.ident != y.ident],
              z3.Not(z3.Or(before(x, y, use_id=False), before(y, x, use_id=False))), 'witness'),
        check('control-whole-page', [page >= 1, page <= MAX_PAGE, k == page], page - k < 1, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
