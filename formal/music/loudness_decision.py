#!/usr/bin/env python3
"""MEDIA-MUSIC-LOUDNESS-001, invariant L4: unbounded lemmas about
TDF.MusicRelease.Loudness.loudnessFlags.

  too_loud        integrated > target + tolerance
  too_quiet       integrated < target - tolerance
  true_peak_over  truePeak   > ceiling
  warn            when any flag is raised, otherwise pass

All values are integers in tenths. The lemmas hold for every integer, not just
the range the database accepts, and prove by UNSAT of the negation that each
flag is monotone in its value, that pass/warn is monotone in the true peak and
in the distance from the target on either side of it, and that a non-negative
tolerance never raises both loudness flags. A witness records what is NOT true:
with the two-sided band pass/warn is not monotone across the whole loudness
axis. Controls show the strict comparisons and the tolerance sign are needed.

The encoding below is a hand translation of the pinned source region; the pin
fails when that region changes. Run with the pinned solver of
formal/system/requirements-verification.txt (z3-solver 4.13.3.0).
"""
import hashlib
import pathlib
import sys

import z3

PINNED = '4.13.3'
ROOT = pathlib.Path(__file__).resolve().parents[2]
SOURCE = 'tdf-hq/src/TDF/MusicRelease/Loudness.hs'
REGIONS = (
    (SOURCE, 'defaultLoudnessLimits :: LoudnessLimits\n', 'data LoudnessFlag = ', 'fb6ce8f9df91d022b65b8d4b22ccc81a8a91108bb91a0c7e1ad9991ef12abf36'),
    (SOURCE, 'loudnessFlags :: LoudnessLimits -> Tenths -> Tenths -> [LoudnessFlag]\n',
     'data MeasurementState = ', '822f2c4a34753316c3e7d48deb565f7f49acd84e641cf5eed551dc6d48eef250'),
)


def check(name, hypotheses, claim, expect):
    solver = z3.Solver()
    solver.add(*hypotheses)
    solver.add(z3.Not(claim) if expect == 'proof' else claim)
    result = solver.check()
    wanted = z3.unsat if expect == 'proof' else z3.sat
    status = 'ok' if result == wanted else 'FAILED'
    print(f'TDF_SMT_RESULT {expect} {name} {result} {status}')
    return result == wanted


def sources_current():
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


def main():
    if z3.get_version_string() != PINNED:
        print(f'Unexpected z3 version {z3.get_version_string()}; pinned {PINNED}', file=sys.stderr)
        return 2
    target, tolerance, ceiling = z3.Ints('target tolerance ceiling')
    low, high, peak, other_peak = z3.Ints('low high peak otherPeak')

    def too_loud(integrated, strict=True):
        return integrated > target + tolerance if strict else integrated >= target + tolerance

    def too_quiet(integrated):
        return integrated < target - tolerance

    def peak_over(true_peak):
        return true_peak > ceiling

    def warn(integrated, true_peak):
        return z3.Or(too_loud(integrated), too_quiet(integrated), peak_over(true_peak))

    ordered = [low <= high]
    band = [tolerance >= 0]
    defaults = [target == -140, tolerance == 20, ceiling == -10]
    results = [
        sources_current(),
        # Each flag is monotone in the one value it depends on.
        check('too-loud-monotone-increasing', ordered, z3.Implies(too_loud(low), too_loud(high)), 'proof'),
        check('too-quiet-monotone-decreasing', ordered, z3.Implies(too_quiet(high), too_quiet(low)), 'proof'),
        check('true-peak-over-monotone-increasing', ordered, z3.Implies(peak_over(low), peak_over(high)), 'proof'),
        # Pass/warn is monotone in the true peak for any fixed loudness.
        check('warn-monotone-in-true-peak', ordered,
              z3.Implies(warn(peak, low), warn(peak, high)), 'proof'),
        # ... and in the distance from the target on either side of it.
        check('warn-monotone-above-target', ordered + band + [low >= target],
              z3.Implies(warn(low, peak), warn(high, peak)), 'proof'),
        check('warn-monotone-below-target', ordered + band + [high <= target],
              z3.Implies(warn(high, peak), warn(low, peak)), 'proof'),
        # Inside the band the peak alone decides.
        check('peak-flag-decides-alone-inside-band', band + [low >= target - tolerance, low <= target + tolerance],
              warn(low, peak) == peak_over(peak), 'proof'),
        # A non-negative tolerance never raises both loudness flags, and the
        # target itself always passes on loudness.
        check('never-loud-and-quiet', band, z3.Not(z3.And(too_loud(low), too_quiet(low))), 'proof'),
        check('target-never-flagged-for-loudness', band,
              z3.Not(z3.Or(too_loud(target), too_quiet(target))), 'proof'),
        # The approved constants: the band is -16.0 .. -12.0 inclusive, the
        # ceiling -1.0 inclusive.
        check('default-band-is-inclusive', defaults,
              z3.And(z3.Not(warn(-160, -10)), z3.Not(warn(-120, -10)), warn(-161, -10), warn(-119, -10), warn(-140, -9)),
              'proof'),
        check('default-pass-set-is-exactly-the-band', defaults,
              z3.Not(warn(low, peak)) == z3.And(low >= -160, low <= -120, peak <= -10), 'proof'),
        # What is NOT true: across the whole loudness axis pass/warn is not
        # monotone, because the band is two-sided.
        check('two-sided-band-is-not-globally-monotone', defaults + ordered + [peak == -10],
              z3.And(warn(low, peak), z3.Not(warn(high, peak))), 'witness'),
        check('default-pass-and-each-flag-reachable', defaults,
              z3.And(z3.Not(warn(-142, -13)), too_loud(-80), too_quiet(-200), peak_over(3)), 'witness'),
        # Controls: a negative tolerance can raise both loudness flags; a
        # non-strict comparison would flag the edge of the approved band.
        check('control-negative-tolerance-raises-both', [], z3.And(too_loud(low), too_quiet(low)), 'witness'),
        check('control-non-strict-flags-band-edge', defaults,
              z3.And(too_loud(-120, strict=False), z3.Not(too_loud(-120))), 'witness'),
    ]
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
