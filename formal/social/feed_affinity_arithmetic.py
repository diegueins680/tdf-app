#!/usr/bin/env python3
"""COMMUNITY-FEED-AFFINITY-001: lemmas behind TDF.Fans.FeedAffinity over the
reals and integers (formal/system/feed-affinity-ranking.md).

  affinity(units) = 1 + min(units, 20) / 10          units >= 0
  score           = affinity * weight * (reactions + 1) / decay
  key             = (score desc, kind asc, created desc, id desc)

`decay` is any positive real: the lemmas hold for every age function, so they
do not depend on the exponent 0.8. They are statements about exact arithmetic.
The Haskell code computes with Double; that step is tested, not proved.

Each lemma is proved by UNSAT of its negation. Controls are satisfiable
witnesses that a weakened rule breaks the lemma.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import sys

import z3

PINNED = '4.13.3'
MIN_AFFINITY, MAX_AFFINITY, CAP = 1, 3, 20


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def affinity(units):
    capped = z3.If(units <= CAP, units, CAP)
    return MIN_AFFINITY + z3.ToReal(capped) * (MAX_AFFINITY - MIN_AFFINITY) / CAP


class Item:
    """An unpinned feed item: symbolic score inputs and tie-break fields."""

    def __init__(self, tag):
        self.affinity = z3.Real(f'affinity_{tag}')
        self.weight = z3.Real(f'weight_{tag}')
        self.reactions = z3.Int(f'reactions_{tag}')
        self.decay = z3.Real(f'decay_{tag}')
        self.kind = z3.Int(f'kind_{tag}')
        self.created = z3.Int(f'created_{tag}')
        self.ident = z3.Int(f'id_{tag}')
        self.score = z3.Real(f'score_{tag}')

    def base(self):
        return (z3.ToReal(self.reactions) + 1) / self.decay

    def domain(self, floor=MIN_AFFINITY, weights=(1, 1)):
        return [self.affinity >= floor, self.affinity <= MAX_AFFINITY,
                self.weight >= weights[0], self.weight <= weights[1],
                self.reactions >= 0, self.decay > 0,
                z3.Or(self.kind == 0, self.kind == 1)]

    def personalised(self):
        return self.affinity * self.weight * self.base()


def before(x, y, use_id=True):
    """x is placed before y by the key."""
    tie = z3.And(x.kind == y.kind, x.created == y.created, x.ident > y.ident) if use_id else False
    return z3.Or(
        x.score > y.score,
        z3.And(x.score == y.score, x.kind < y.kind),
        z3.And(x.score == y.score, x.kind == y.kind, x.created > y.created),
        z3.And(x.score == y.score, tie))


def distinct(x, y):
    return z3.Or(x.kind != y.kind, x.ident != y.ident)


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    units, more = z3.Ints('units more')
    x, y, z = Item('x'), Item('y'), Item('z')
    kinds = [z3.Or(i.kind == 0, i.kind == 1) for i in (x, y, z)]
    overtakes = y.personalised() >= x.personalised()
    results = [
        # Affinity: 1 without evidence, never outside [1, 3], never decreasing.
        check('affinity-range', [units >= 0],
              z3.And(affinity(units) >= MIN_AFFINITY, affinity(units) <= MAX_AFFINITY), 'proof'),
        check('affinity-no-evidence', [units == 0], affinity(units) == MIN_AFFINITY, 'proof'),
        check('affinity-cap', [units >= CAP], affinity(units) == MAX_AFFINITY, 'proof'),
        check('affinity-monotone', [units >= 0, more >= units], affinity(more) >= affinity(units), 'proof'),
        # P2: y can stand before x only if its base score is at least a third of x's.
        check('displacement-bound', x.domain() + y.domain() + [overtakes],
              MAX_AFFINITY * y.base() >= x.base(), 'proof'),
        check('displacement-bound-is-tight', x.domain() + y.domain() + [overtakes],
              MAX_AFFINITY * y.base() == x.base(), 'witness'),
        # No item is scored zero in personalised mode, so none is tied at the bottom.
        check('score-positive', x.domain(), x.personalised() > 0, 'proof'),
        # P4: the key is a strict total order on items with distinct (kind, id).
        check('order-irreflexive', kinds, z3.Not(before(x, x)), 'proof'),
        check('order-asymmetric', kinds, z3.Not(z3.And(before(x, y), before(y, x))), 'proof'),
        check('order-transitive', kinds + [before(x, y), before(y, z)], before(x, z), 'proof'),
        check('order-total', kinds + [distinct(x, y)], z3.Or(before(x, y), before(y, x)), 'proof'),
        # P3: with equal scores the key keeps the handler's input order (posts
        # before memories, each newest first), as the previous stable sort did.
        check('fallback-keeps-stable-order',
              kinds + [x.score == y.score,
                       z3.Or(x.kind < y.kind, z3.And(x.kind == y.kind, x.created > y.created))],
              before(x, y), 'proof'),
        check('fallback-score-first', kinds + [x.score > y.score], before(x, y), 'proof'),
        # P6 over exact arithmetic (the Double implementation is tested only).
        check('score-rises-with-reactions',
              x.domain() + y.domain() + [y.affinity == x.affinity, y.weight == x.weight,
                                         y.decay == x.decay, y.reactions > x.reactions],
              y.personalised() > x.personalised(), 'proof'),
        check('score-rises-with-affinity',
              x.domain() + y.domain() + [y.affinity > x.affinity, y.weight == x.weight,
                                         y.decay == x.decay, y.reactions == x.reactions],
              y.personalised() > x.personalised(), 'proof'),
        check('score-falls-with-decay',
              x.domain() + y.domain() + [y.affinity == x.affinity, y.weight == x.weight,
                                         y.decay >= x.decay, y.reactions == x.reactions],
              y.personalised() <= x.personalised(), 'proof'),
        # Controls. A zero floor lets an item be overtaken by one of any base
        # score; unequal type weights widen the bound beyond 3; without the id
        # two distinct items can be unordered.
        check('control-zero-floor', x.domain(floor=0) + y.domain(floor=0) + [overtakes],
              MAX_AFFINITY * y.base() < x.base(), 'witness'),
        check('control-unequal-type-weights',
              x.domain(weights=(1, 1.2)) + y.domain(weights=(1, 1.2)) + [overtakes],
              MAX_AFFINITY * y.base() < x.base(), 'witness'),
        check('control-no-id-tie-break', kinds + [distinct(x, y)],
              z3.Not(z3.Or(before(x, y, use_id=False), before(y, x, use_id=False))), 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
