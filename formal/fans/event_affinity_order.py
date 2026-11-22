#!/usr/bin/env python3
"""EVT-AFFINITY-ORDER-001: evidence and order of the event page permutation.

Hand translation of three pinned regions of
tdf-hq/src/TDF/Fans/EventAffinity.hs:

  * artistAffinities: which artists get an affinity, from which seeds, and
    its value (own signal: 1 + strength; otherwise min 1 of a mean);
  * eventAffinity and orderByAffinity: the sort key (affinity descending,
    position in the page ascending);
  * loadFanArtistAffinities: what the loader puts in the usable set.

One request is abstracted to an arbitrary artist `c` with an affinity, an
arbitrary seed `s` contributing to it, and uninterpreted facts about them.

Proofs (UNSAT of the negation):
  - an artist with an affinity is listed, is not the caller, and is followed
    by the caller or not excluded by the follow policy;
  - a seed that contributes to another artist's affinity is such an artist
    too and carries a positive weight of the caller;
  - with IEEE doubles: an own-signal affinity (1 plus a strength of at least
    1/8, the smallest the integer weights give) is a number in (1, 2], an
    inferred one a number in [0, 1], and an event affinity (the larger of 0
    and a number) is a number that is not negative;
  - the page key is a strict total order on distinct positions:
    irreflexive, transitive and total, so the permutation is unique.

Witnesses: an own-signal artist, an inferred artist, and two events with
equal affinity ordered by position.

Necessity: the usable check on the artist, the usable check on the seed, the
loader's listing, own-party and policy conditions, and the position
tie-break are removed in turn; each must make a proof fail. Allowing a NaN
affinity must also break the order.

Not covered: that `sortOn` implements the key and returns a permutation
(library; tested with QuickCheck), that the sets the loader builds equal the
database predicates they are named after (SQL; tested on PostgreSQL), and
the exact value of the mean.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SOURCE = 'tdf-hq/src/TDF/Fans/EventAffinity.hs'
REGIONS = (
    ('artistAffinities\n  :: Set.Set Int64', '-- | The affinity of an event',
     'PIN_AFFINITIES'),
    ('eventAffinity :: Map.Map Int64 Double', '-- | @(event, artist party)@',
     'PIN_ORDER'),
    ('loadFanArtistAffinities\n  :: MonadIO m', None,
     'PIN_LOADER'),
)
GUARDS = ('candidate-usable', 'seed-usable', 'loader-listed', 'loader-not-own',
          'loader-policy', 'position-tie-break')
DOUBLE = z3.Float64()


def sources_are_current():
    text = (ROOT / SOURCE).read_text()
    ok = True
    for start, end, pin in REGIONS:
        if text.count(start) != 1 or (end is not None and text.count(end) != 1):
            print(f'TDF_SMT_RESULT source {SOURCE} markers FAILED')
            ok = False
            continue
        begin = text.index(start)
        region = text[begin:] if end is None else text[begin:text.index(end, begin)]
        digest = hashlib.sha256(region.encode()).hexdigest()
        current = digest == pin
        ok = ok and current
        print(f"TDF_SMT_RESULT source {SOURCE} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


class Request:
    """One request, seen from an artist `c` with an affinity and a seed `s`."""

    def __init__(self, tag, dropped=None):
        kept = lambda guard: z3.BoolVal(dropped != guard)
        self.c, self.s, self.own = z3.Ints(f'c_{tag} s_{tag} own_{tag}')
        self.listed = z3.Function(f'listed_{tag}', z3.IntSort(), z3.BoolSort())
        self.followed = z3.Function(f'followed_{tag}', z3.IntSort(), z3.BoolSort())
        self.excluded = z3.Function(f'excluded_{tag}', z3.IntSort(), z3.BoolSort())
        self.weight = z3.Function(f'weight_{tag}', z3.IntSort(), z3.IntSort())
        similar = z3.Function(f'similar_{tag}', z3.IntSort(), z3.IntSort(), z3.BoolSort())

        # loadFanArtistAffinities: Set.delete own (Set.filter (followed || not excluded) listed)
        def usable(artist):
            return z3.And(
                z3.Or(z3.Not(kept('loader-listed')), self.listed(artist)),
                z3.Or(z3.Not(kept('loader-not-own')), artist != self.own),
                z3.Or(z3.Not(kept('loader-policy')),
                      self.followed(artist), z3.Not(self.excluded(artist))))
        self.usable = usable

        # strengths: weight > 0 and Set.member seed usable
        def strong(artist):
            return z3.And(self.weight(artist) > 0,
                          z3.Or(z3.Not(kept('seed-usable')), usable(artist)))
        # own = strengths; inferred: a pair with a strong seed, candidate usable
        # and not itself strong, with a positive contribution.
        self.own_signal = strong(self.c)
        self.inferred = z3.And(
            similar(self.s, self.c), strong(self.s),
            z3.Or(z3.Not(kept('candidate-usable')), usable(self.c)),
            z3.Not(strong(self.c)))
        self.has_affinity = z3.Or(self.own_signal, self.inferred)
        # Similarity pairs join distinct artists (artist_a < artist_b).
        self.wellformed = z3.ForAll([self.c], z3.Not(similar(self.c, self.c)))
        self.honest_usable = lambda artist: z3.And(
            self.listed(artist), artist != self.own,
            z3.Or(self.followed(artist), z3.Not(self.excluded(artist))))


def evidence_claims(dropped=None):
    r = Request('r', dropped)
    c, s = r.c, r.s
    claims = {
        'an-artist-with-affinity-is-listed': z3.Implies(r.has_affinity, r.listed(c)),
        'an-artist-with-affinity-is-not-the-caller': z3.Implies(r.has_affinity, c != r.own),
        'an-artist-with-affinity-is-followed-or-followable': z3.Implies(
            r.has_affinity, z3.Or(r.followed(c), z3.Not(r.excluded(c)))),
        'a-contributing-seed-is-usable': z3.Implies(r.inferred, r.honest_usable(s)),
        'a-contributing-seed-has-a-positive-weight': z3.Implies(r.inferred, r.weight(s) > 0),
        'a-contributing-seed-is-not-its-artist': z3.Implies(r.inferred, s != c),
    }
    return {name: z3.Implies(r.wellformed, claim) for name, claim in claims.items()}


def number(value):
    return z3.Not(z3.fpIsNaN(value))


def page_before(x, y, dropped=None):
    """(Down affinity, position), compared lexicographically."""
    (affinity_x, position_x), (affinity_y, position_y) = x, y
    last = position_x < position_y if dropped != 'position-tie-break' else z3.BoolVal(False)
    return z3.Or(z3.fpGT(affinity_x, affinity_y),
                 z3.And(z3.fpEQ(affinity_x, affinity_y), last))


def item(tag):
    return (z3.FP(f'affinity_{tag}', DOUBLE), z3.Int(f'position_{tag}'))


def order_claims(dropped=None, nan_allowed=False):
    x, y, z = item('x'), item('y'), item('z')
    numbers = z3.BoolVal(True) if nan_allowed else z3.And([number(i[0]) for i in (x, y, z)])
    lt = lambda a, b: page_before(a, b, dropped)
    claims = {
        'page-order-is-irreflexive': z3.Not(lt(x, x)),
        'page-order-is-transitive': z3.Implies(z3.And(lt(x, y), lt(y, z)), lt(x, z)),
        'page-order-is-total-on-distinct-positions': z3.Implies(
            x[1] != y[1], z3.Xor(lt(x, y), lt(y, x))),
    }
    return {name: z3.Implies(numbers, claim) for name, claim in claims.items()}


def arithmetic_claims():
    rne = z3.RNE()
    strength, mean, value = z3.FPs('strength mean value', DOUBLE)
    zero, eighth, one, two = (z3.FPVal(v, DOUBLE) for v in (0.0, 0.125, 1.0, 2.0))
    own = z3.fpAdd(rne, one, strength)
    inferred = z3.If(z3.fpLEQ(one, mean), one, mean)           # min 1 mean
    event = z3.If(z3.fpGEQ(zero, value), zero, value)          # max 0 value
    return {
        'an-own-signal-affinity-is-a-number-above-1-and-at-most-2': z3.Implies(
            z3.And(number(strength), z3.fpGEQ(strength, eighth), z3.fpLEQ(strength, one)),
            z3.And(number(own), z3.fpGT(own, one), z3.fpLEQ(own, two))),
        'an-inferred-affinity-is-a-number-between-0-and-1': z3.Implies(
            z3.And(number(mean), z3.fpGEQ(mean, zero)),
            z3.And(number(inferred), z3.fpGEQ(inferred, zero), z3.fpLEQ(inferred, one))),
        'an-event-affinity-is-a-number-that-is-not-negative': z3.Implies(
            number(value), z3.And(number(event), z3.fpGEQ(event, zero))),
    }


def proved(claim):
    solver = z3.Solver()
    solver.set('timeout', 600000)
    solver.add(z3.Not(claim))
    return solver.check() == z3.unsat


def satisfiable(*facts):
    solver = z3.Solver()
    solver.set('timeout', 60000)
    solver.add(*facts)
    return solver.check() == z3.sat


def witnesses():
    r = Request('w')
    x, y = item('wx'), item('wy')
    return {
        'an-own-signal-artist-exists': (r.wellformed, r.own_signal),
        'an-inferred-artist-exists': (r.wellformed, r.inferred),
        'equal-affinities-are-ordered-by-position': (
            number(x[0]), number(y[0]), z3.fpEQ(x[0], y[0]), x[1] != y[1], page_before(x, y)),
    }


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    claims = {**evidence_claims(), **order_claims(), **arithmetic_claims()}
    for name, claim in claims.items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    for name, facts in witnesses().items():
        result = satisfiable(*facts)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        dropped = {**evidence_claims(guard), **order_claims(guard)}
        broken = [name for name, claim in dropped.items() if not proved(claim)]
        result = bool(broken)
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity drop-{guard} "
              f"{'breaks ' + ','.join(broken) if result else 'FAILED (guard is not load-bearing)'}",
              flush=True)
    broken = [name for name, claim in order_claims(nan_allowed=True).items() if not proved(claim)]
    ok = ok and bool(broken)
    print(f"TDF_SMT_RESULT necessity allow-nan-affinity "
          f"{'breaks ' + ','.join(broken) if broken else 'FAILED (assumption is not load-bearing)'}",
          flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
