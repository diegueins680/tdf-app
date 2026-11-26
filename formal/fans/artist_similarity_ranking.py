#!/usr/bin/env python3
"""COMMUNITY-ARTIST-SIMILARITY-001: exclusions and order of the ranking.

Hand translation of four pinned regions:

  * rankBySimilarity and frameSeedEligible
    (tdf-hq/src/TDF/Fans/ArtistSimilarity.hs): which artists become
    `similar_fans` results, which seeds are named, the sort key, and which
    artists may be seeds at all;
  * loadSimilarityRecommendations and loadFanSimilarity (same module), which
    it calls and which the region includes: the seed weights are restricted
    to frameSeedEligible of the live frame;
  * rankArtistRecommendations (tdf-hq/src/TDF/Fans/ArtistRecommendations.hs),
    called with no evidence rows: the popularity fallback and its sort key;
  * loadRecommendationFrame (same module): what the loader puts in the
    eligible and excluded sets.

One request is abstracted to an arbitrary result artist `c`, an arbitrary
artist `s` named as its seed, and uninterpreted facts about them (listed,
followed, own party, followable, follower count, the caller's weight,
similarity). `take limit` only removes results, so a property of every
candidate holds for every result.

Proofs (UNSAT of the negation):
  - a result is listed, not followed by the caller, not the caller, and
    followable where the follow policy is installed;
  - a named seed is listed, carries a positive weight of the caller, is
    not the result it explains, and is followed by the caller or followable
    where the follow policy is installed (a block in either direction makes
    an artist neither, so a blocked artist is never evidence);
  - no artist is both a similar result and a fallback result;
  - the sort key of similar results (score descending, follower count
    descending, id ascending) is a strict total order on distinct ids:
    irreflexive, transitive and total, so the sorted list is unique;
  - the same for the fallback key (follower count descending, id ascending);
  - with IEEE doubles: a seed strength in [0, 1] times a similarity in
    [0, 1] is a number in [0, 1], and the sum of two non-negative numbers is
    a non-negative number, so no score is NaN.

Witnesses: a similar result, a fallback result, and two results whose scores
and follower counts tie and are ordered by id.

Necessity: the listing check on results, the exclusion check, the listing
check on seeds (in both places it is applied), the loader's seed restriction
to followed or followable artists, the loader's three exclusions,
the removal of similar results from the fallback and the id tie-break are
removed in turn; each must make a proof fail. Allowing a NaN score must also
break the order.

Not covered: that `sortOn` implements the key (library), that the sets the
loader builds equal the database predicates they are named after (SQL,
tested on PostgreSQL), the value of the score, and `take`.

Run with the pinned solver of formal/system/requirements-verification.txt
(z3-solver 4.13.3.0).
"""
import hashlib
import sys
from pathlib import Path

import z3

PINNED = '4.13.3'
ROOT = Path(__file__).resolve().parents[2]
SIMILARITY = 'tdf-hq/src/TDF/Fans/ArtistSimilarity.hs'
RECOMMENDATIONS = 'tdf-hq/src/TDF/Fans/ArtistRecommendations.hs'
REGIONS = (
    (SIMILARITY, 'rankBySimilarity\n  :: Int', '-- | Every follow as',
     '9668316445b1ed64663012e3e7f5bf5ba31c05b2d85c3619306b79a5dadfc37b'),
    (SIMILARITY, 'loadSimilarityRecommendations\n  :: MonadIO m', '-- | The stored rows a request',
     'f63922a0db3dbf3bb2d1c3af5d1ccaa1258f72895a530da8ac0fd2dcec87f0d9'),
    (RECOMMENDATIONS, 'rankArtistRecommendations\n  :: Int', '-- | Artists followed by the fan',
     '58e3cad7967b0766b80f274be1437526d70611425b29eb252ea10546209068e8'),
    (RECOMMENDATIONS, 'loadRecommendationFrame :: MonadIO m', '-- | Recommendations for @fan@',
     'eb3d4c7e42187bf651bcae1ce1b752bda3fd9503abbe4ac55d506466557b1311'),
)
GUARDS = ('result-listed', 'result-not-excluded', 'seed-listed', 'loader-restricts-seeds',
          'loader-excludes-followed', 'loader-excludes-own', 'loader-excludes-unfollowable',
          'fallback-skips-similar', 'id-tie-break')


def sources_are_current():
    ok = True
    for source, start, end, pin in REGIONS:
        text = (ROOT / source).read_text()
        if text.count(start) != 1 or text.count(end) != 1:
            print(f'TDF_SMT_RESULT source {source} markers FAILED')
            ok = False
            continue
        begin = text.index(start)
        digest = hashlib.sha256(text[begin:text.index(end, begin)].encode()).hexdigest()
        current = digest == pin
        ok = ok and current
        print(f"TDF_SMT_RESULT source {source} region-sha256={digest} {'ok' if current else 'FAILED'}")
    return ok


class Request:
    """One request, seen from a result `c` and a seed `s` named for it."""

    def __init__(self, tag, dropped=None):
        kept = lambda guard: z3.BoolVal(dropped != guard)
        self.c, self.s, self.own = z3.Ints(f'c_{tag} s_{tag} own_{tag}')
        listed = z3.Function(f'listed_{tag}', z3.IntSort(), z3.BoolSort())
        followed = z3.Function(f'followed_{tag}', z3.IntSort(), z3.BoolSort())
        followable = z3.Function(f'followable_{tag}', z3.IntSort(), z3.BoolSort())
        followers = z3.Function(f'followers_{tag}', z3.IntSort(), z3.IntSort())
        weight = z3.Function(f'weight_{tag}', z3.IntSort(), z3.IntSort())
        stored = z3.Function(f'stored_{tag}', z3.IntSort(), z3.IntSort(), z3.BoolSort())
        similarity = z3.Function(f'similarity_{tag}', z3.IntSort(), z3.IntSort(), z3.RealSort())
        self.policy = z3.Bool(f'policy_{tag}')
        self.listed, self.followed, self.followable = listed, followed, followable
        self.followers, self.weight = followers, weight
        c, s = self.c, self.s

        # similarityValue: zero for an artist with itself, never negative
        self.wellformed = z3.And(similarity(s, s) == 0, similarity(s, c) >= 0)

        # loadRecommendationFrame: rfListed, rfExcluded
        eligible = listed
        excluded = lambda a: z3.Or(
            z3.And(kept('loader-excludes-followed'), followed(a)),
            z3.And(kept('loader-excludes-own'), a == self.own),
            z3.And(kept('loader-excludes-unfollowable'), self.policy, listed(a),
                   z3.Not(followed(a)), a != self.own, z3.Not(followable(a))))

        # rankBySimilarity
        recommendable = lambda a: z3.And(
            z3.Or(z3.Not(kept('result-listed')), eligible(a)),
            z3.Or(z3.Not(kept('result-not-excluded')), z3.Not(excluded(a))))
        # frameSeedEligible: listed, and followed or not excluded
        # (the listing is checked here and again by rankBySimilarity; the
        # 'seed-listed' guard drops both)
        seed_eligible = lambda a: z3.And(
            z3.Or(z3.Not(kept('seed-listed')), listed(a)),
            z3.Or(followed(a), z3.Not(excluded(a))))
        # loadSimilarityRecommendations: weights restricted to frameSeedEligible
        strength_defined = z3.And(
            z3.Or(z3.Not(kept('loader-restricts-seeds')), seed_eligible(s)),
            z3.Or(z3.Not(kept('seed-positive-weight')), weight(s) > 0),
            z3.Or(z3.Not(kept('seed-listed')), eligible(s)))
        strength = z3.If(weight(s) > 8, z3.RealVal(1), z3.ToReal(weight(s)) / 8)
        # `s` contributes to `c`: a stored pair, a defined strength, a
        # recommendable candidate and a positive product.
        self.contributes = z3.And(
            z3.Or(stored(s, c), stored(c, s)), strength_defined, recommendable(c),
            strength * similarity(s, c) > 0)
        # `c` is a similar result with `s` among its named seeds
        self.similar = self.contributes
        # rankArtistRecommendations with no rows, excluded = excluded + similar ids
        self.fallback = z3.And(
            followers(c) > 0, eligible(c), z3.Not(excluded(c)),
            z3.Or(z3.Not(kept('fallback-skips-similar')), z3.Not(self.similar)))
        self.result = z3.Or(self.similar, self.fallback)


def exclusion_claims(dropped=None):
    r = Request('r', dropped)
    c, s = r.c, r.s
    claims = {
        'a-result-is-listed': z3.Implies(r.result, r.listed(c)),
        'a-result-is-not-followed': z3.Implies(r.result, z3.Not(r.followed(c))),
        'a-result-is-not-the-caller': z3.Implies(r.result, c != r.own),
        'a-result-is-followable-under-the-policy': z3.Implies(
            z3.And(r.result, r.policy), r.followable(c)),
        'a-named-seed-is-listed': z3.Implies(r.similar, r.listed(s)),
        'a-named-seed-has-a-positive-weight': z3.Implies(r.similar, r.weight(s) > 0),
        'a-named-seed-is-not-its-result': z3.Implies(r.similar, s != c),
        'a-named-seed-is-followed-or-followable-under-the-policy': z3.Implies(
            z3.And(r.similar, r.policy), z3.Or(r.followed(s), r.followable(s))),
        'no-artist-is-both-similar-and-fallback': z3.Not(z3.And(r.similar, r.fallback)),
    }
    return {name: z3.Implies(r.wellformed, claim) for name, claim in claims.items()}


DOUBLE = z3.Float64()


def number(value):
    return z3.Not(z3.fpIsNaN(value))


def similar_before(x, y, dropped=None):
    """rankKey: (Down score, Down followers, id), compared lexicographically."""
    (score_x, followers_x, id_x), (score_y, followers_y, id_y) = x, y
    last = id_x < id_y if dropped != 'id-tie-break' else z3.BoolVal(False)
    return z3.Or(
        z3.fpGT(score_x, score_y),
        z3.And(z3.fpEQ(score_x, score_y),
               z3.Or(followers_x > followers_y, z3.And(followers_x == followers_y, last))))


def fallback_before(x, y, dropped=None):
    """(Down count, id), compared lexicographically."""
    (_, followers_x, id_x), (_, followers_y, id_y) = x, y
    last = id_x < id_y if dropped != 'id-tie-break' else z3.BoolVal(False)
    return z3.Or(followers_x > followers_y, z3.And(followers_x == followers_y, last))


def item(tag):
    return (z3.FP(f'score_{tag}', DOUBLE), z3.Int(f'followers_{tag}'), z3.Int(f'id_{tag}'))


def order_claims(dropped=None, nan_allowed=False):
    x, y, z = item('x'), item('y'), item('z')
    numbers = z3.BoolVal(True) if nan_allowed else z3.And([number(i[0]) for i in (x, y, z)])
    claims = {}
    for name, before in (('similar', similar_before), ('fallback', fallback_before)):
        lt = lambda a, b: before(a, b, dropped)
        claims[f'{name}-order-is-irreflexive'] = z3.Not(lt(x, x))
        claims[f'{name}-order-is-transitive'] = z3.Implies(z3.And(lt(x, y), lt(y, z)), lt(x, z))
        claims[f'{name}-order-is-total-on-distinct-ids'] = z3.Implies(
            x[2] != y[2], z3.Xor(lt(x, y), lt(y, x)))
    return {name: z3.Implies(numbers, claim) for name, claim in claims.items()}


def arithmetic_claims():
    rne = z3.RNE()
    strength, similarity, a, b = z3.FPs('strength similarity a b', DOUBLE)
    zero, one = z3.FPVal(0.0, DOUBLE), z3.FPVal(1.0, DOUBLE)
    unit = lambda v: z3.And(number(v), z3.fpGEQ(v, zero), z3.fpLEQ(v, one))
    product = z3.fpMul(rne, strength, similarity)
    total = z3.fpAdd(rne, a, b)
    return {
        'a-contribution-is-a-number-between-0-and-1': z3.Implies(
            z3.And(unit(strength), unit(similarity)), unit(product)),
        'a-sum-of-non-negative-numbers-is-a-non-negative-number': z3.Implies(
            z3.And(number(a), number(b), z3.fpGEQ(a, zero), z3.fpGEQ(b, zero)),
            z3.And(number(total), z3.fpGEQ(total, zero))),
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
        'a-similar-result-exists': (r.wellformed, r.similar),
        'a-fallback-result-exists': (r.wellformed, r.fallback),
        'a-full-tie-is-ordered-by-id': (
            number(x[0]), number(y[0]), z3.fpEQ(x[0], y[0]), x[1] == y[1], x[2] != y[2],
            similar_before(x, y)),
    }


def main():
    ok = z3.get_version_string().startswith(PINNED)
    print(f"TDF_SMT_RESULT solver z3 {z3.get_version_string()} {'ok' if ok else 'FAILED'}", flush=True)
    ok = sources_are_current() and ok
    claims = {**exclusion_claims(), **order_claims(), **arithmetic_claims()}
    for name, claim in claims.items():
        result = proved(claim)
        ok = ok and result
        print(f"TDF_SMT_RESULT proof {name} {'unsat' if result else 'FAILED'}", flush=True)
    for name, facts in witnesses().items():
        result = satisfiable(*facts)
        ok = ok and result
        print(f"TDF_SMT_RESULT witness {name} {'sat' if result else 'FAILED'}", flush=True)
    for guard in GUARDS:
        dropped = {**exclusion_claims(guard), **order_claims(guard)}
        broken = [name for name, claim in dropped.items() if not proved(claim)]
        result = bool(broken)
        ok = ok and result
        print(f"TDF_SMT_RESULT necessity drop-{guard} "
              f"{'breaks ' + ','.join(broken) if result else 'FAILED (guard is not load-bearing)'}",
              flush=True)
    broken = [name for name, claim in order_claims(nan_allowed=True).items() if not proved(claim)]
    ok = ok and bool(broken)
    print(f"TDF_SMT_RESULT necessity allow-nan-score "
          f"{'breaks ' + ','.join(broken) if broken else 'FAILED (assumption is not load-bearing)'}",
          flush=True)
    print(f"TDF_SMT_RESULT summary {'ok' if ok else 'FAILED'}", flush=True)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
