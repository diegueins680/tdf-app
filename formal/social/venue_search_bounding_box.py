#!/usr/bin/env python3
"""EVT-VENUE-SEARCH-001: the bounding box of TDF.Social.VenueSearch.venueBoundingBox
never excludes a venue the haversine test accepts (real arithmetic).

Model. A query point at latitude p1 and a venue at latitude p2, longitudes differing by
L, are written with algebraic stand-ins for the trigonometric values:

  s1 = sin p1, c1 = cos p1, s2 = sin p2, c2 = cos p2, k = cos L, D = cos(radius / 6371)

with s^2 + c^2 = 1, c1 >= 0 and c2 >= 0 (latitudes in -90..90) and -1 <= k <= 1. The
central angle t between the points satisfies cos t = s1*s2 + c1*c2*k, and the haversine
term of haversineDistanceKm is a = (1 - cos t) / 2, so `distance <= radius` is
`cos t >= D` (the distance is 6371 * t and cosine decreases on 0..pi).

Proved by UNSAT of the negation, for all reals:

  haversine-central-angle  hav(p2 - p1) + c1*c2*hav(L) = (1 - cos t) / 2
  latitude-bound           cos t <= cos(p2 - p1); hence within the radius implies
                           |p2 - p1| <= radius / 6371
  longitude-bound          when the circle holds no pole (c1 > sin(radius / 6371), i.e.
                           c1^2 > 1 - D^2), within the radius implies k > 0 and
                           k^2 >= 1 - (1 - D^2) / c1^2, i.e. |L| <= asin(sin d / cos p1)

Because k = cos L has period 360 degrees, the longitude bound is on the wrapped difference;
the box tests the venue longitude and its +-360 images, which is what covers the 180 degree
meridian. Controls show the pole guard, the full half-width and the latitude range are each
needed; witnesses show the hypotheses are satisfiable and the longitude bound is attained.

Not covered here: IEEE rounding in the Haskell and SQL arithmetic (absorbed by the
widening margin and checked by tests), the identities sin^2(x/2) = (1 - cos x)/2 and the
monotonicity of cosine and arcsine (standard, used in the reading above), and the compiled
SQL. Run with the pinned solver of formal/system/requirements-verification.txt.
"""
import sys

import z3

PINNED = '4.13.3'


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.set('timeout', 120000)
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
    s1, c1, s2, c2, k, D = z3.Reals('s1 c1 s2 c2 k D')
    unit = [s1 * s1 + c1 * c1 == 1, s2 * s2 + c2 * c2 == 1, k >= -1, k <= 1]
    query_in_range = [c1 >= 0]
    venue_in_range = [c2 >= 0]
    radius = [D > 0, D < 1]
    cos_t = s1 * s2 + c1 * c2 * k
    cos_dlat = s1 * s2 + c1 * c2
    within = cos_t >= D
    no_pole = c1 * c1 > 1 - D * D
    lng_claim = z3.And(k > 0, c1 * c1 * k * k >= c1 * c1 - (1 - D * D))
    base = unit + query_in_range + venue_in_range

    results = [
        check('haversine-central-angle', unit,
              (1 - cos_dlat) / 2 + c1 * c2 * ((1 - k) / 2) == (1 - cos_t) / 2, 'proof'),
        check('latitude-bound', base, cos_t <= cos_dlat, 'proof'),
        check('latitude-bound-within-radius', base + radius + [within], cos_dlat >= D, 'proof'),
        check('longitude-bound', base + radius + [no_pole, within], lng_claim, 'proof'),
        # Witnesses: the hypotheses are satisfiable and the bound is attained.
        check('witness-within-radius', base + radius + [no_pole], z3.And(within, k < 1), 'witness'),
        check('witness-longitude-bound-tight', base + radius + [no_pole, within],
              c1 * c1 * k * k == c1 * c1 - (1 - D * D), 'witness'),
        # Controls: each weakening admits a venue inside the radius and outside the box.
        check('control-no-pole-guard', base + radius + [within],
              z3.Not(lng_claim), 'witness'),
        check('control-half-width-too-small', base + radius + [no_pole, within],
              z3.Not(z3.And(k > 0, c1 * c1 * k * k >= c1 * c1 - (1 - D * D) / 2)), 'witness'),
        check('control-venue-latitude-out-of-range', unit + query_in_range + radius + [within],
              cos_dlat < D, 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
