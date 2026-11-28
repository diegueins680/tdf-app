#!/usr/bin/env python3
"""SCHOOL-SLOT-SUGGESTION-001: Z3 obligations for lesson slot suggestions.

1. overlap (always): the Haskell predicate `overlaps`, read from
   tdf-hq/src/TDF/Trials/SlotSuggestions.hs, is equivalent to two non-empty
   half-open ranges sharing an instant, which is what the exclusion constraint
   of tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql tests with
   `tstzrange(starts_at, ends_at, '[)') WITH &&` under CHECK (ends_at > starts_at).
   Universal over all integer instants. Trusted: that reading of `&&` (PostgreSQL
   range documentation; exercised on PostgreSQL by
   scripts/test-lesson-slot-suggestions-sql.py) and the small translator below.

2. optimality (with --oracle): for EVERY instance of the bounded family
   described in `instances()`, the first suggestion of the compiled core
   (tdf-slot-suggestions-oracle) is feasible and no feasible slot has a
   lexicographically smaller cost, or nothing is suggested and nothing is
   feasible. Feasibility and cost are restated here in SMT from
   formal/system/lesson-slot-suggestions.md, independently of the Haskell.
   This is a proof per enumerated instance, not a proof for larger calendars.

Each part has a negative control that must be refuted by the solver.

Usage: verify-lesson-slot-suggestions.py [--oracle PATH] [--stride N]
  --stride N checks every Nth instance only and reports that the family was
  NOT exhausted; the default 1 checks all of them.
"""
import argparse
import itertools
import json
import re
import subprocess
import sys
from pathlib import Path

import z3

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Trials/SlotSuggestions.hs'
MIGRATION = ROOT / 'tdf-hq/sql/2026-11-21_school_schedule_occupancy.sql'


def fail(message):
    raise SystemExit('FAILED: ' + message)


# --------------------------------------------------------------------------
# 1. overlap
# --------------------------------------------------------------------------

def haskell_overlap_expression():
    found = re.findall(r'(?m)^overlaps :: Interval -> Interval -> Bool\noverlaps a b = ([^\n]+)\n', SOURCE.read_text())
    if len(found) != 1:
        fail('unsupported definition of overlaps; update the translator with the source')
    return found[0]


def translate(expression, bounds):
    """Conjunction of comparisons between ivStart/ivEnd of a and b."""
    def term(text):
        match = re.fullmatch(r'(ivStart|ivEnd) ([ab])', text.strip())
        if not match:
            fail(f'unsupported term {text!r}')
        return bounds[match.group(2)][0 if match.group(1) == 'ivStart' else 1]
    clauses = []
    for clause in expression.split('&&'):
        match = re.fullmatch(r'\s*(.+?) (<=|<|>=|>) (.+?)\s*', clause)
        if not match:
            fail(f'unsupported clause {clause!r}')
        left, right = term(match.group(1)), term(match.group(3))
        clauses.append({'<': left < right, '<=': left <= right, '>': left > right, '>=': left >= right}[match.group(2)])
    return z3.And(clauses)


def require_constraint_shape():
    sql = ' '.join(MIGRATION.read_text().split())
    for fragment in ("EXCLUDE USING gist ( subject_kind WITH =, subject_id WITH =, tstzrange(starts_at, ends_at, '[)') WITH && )",
                     'CHECK (ends_at > starts_at)',
                     'starts_at TIMESTAMPTZ NOT NULL', 'ends_at TIMESTAMPTZ NOT NULL'):
        if sql.count(fragment) != 1:
            fail(f'migration no longer contains exactly one {fragment!r}; review the proof')


def prove(label, claim):
    solver = z3.Solver()
    solver.add(z3.Not(claim))
    result = solver.check()
    if result != z3.unsat:
        fail(f'{label}: {result} {solver.model() if result == z3.sat else ""}')
    print(f'PROVED {label}')


def refute(label, claim):
    solver = z3.Solver()
    solver.add(z3.Not(claim))
    if solver.check() != z3.sat:
        fail(f'negative control {label} was not refuted')
    print(f'REFUTED (negative control) {label}')


def verify_overlap():
    require_constraint_shape()
    expression = haskell_overlap_expression()
    a_start, a_end, b_start, b_end, t = z3.Ints('a_start a_end b_start b_end t')
    bounds = {'a': (a_start, a_end), 'b': (b_start, b_end)}
    non_empty = z3.And(a_start < a_end, b_start < b_end)  # CHECK (ends_at > starts_at)
    shares_instant = z3.Exists([t], z3.And(a_start <= t, t < a_end, b_start <= t, t < b_end))
    haskell = translate(expression, bounds)
    prove('overlap-equivalence: overlaps a b <=> the half-open ranges share an instant',
          z3.Implies(non_empty, haskell == shares_instant))
    swapped = translate(expression, {'a': bounds['b'], 'b': bounds['a']})
    prove('overlap-symmetry', haskell == swapped)
    prove('overlap-touching: an interval ending where another starts does not overlap it',
          z3.Implies(z3.And(non_empty, a_end == b_start), z3.Not(haskell)))
    # Soundness step: a candidate that overlaps none of the rows cannot make the
    # constraint fire against any single row (the constraint is pairwise).
    prove('overlap-soundness: not (overlaps a b) => the ranges share no instant',
          z3.Implies(z3.And(non_empty, z3.Not(haskell)), z3.Not(shares_instant)))
    closed = translate(expression.replace('<', '<='), bounds)
    refute('closed-interval comparison is not equivalent', z3.Implies(non_empty, closed == shares_instant))
    return expression


# --------------------------------------------------------------------------
# 2. optimality on a bounded family
# --------------------------------------------------------------------------
# One studio day. Minute 0 is 13:00Z on 2030-12-02, 08:00 in the studio, the
# opening time. The request is made at 20:00Z the day before with days = 1, so
# the horizon ends at minute 420; the 12-hour notice ended before opening.
EPOCH_MINUTE_0 = 1922446800  # seconds: 2030-12-02T13:00:00Z
NOW = EPOCH_MINUTE_0 - 17 * 3600
HORIZON_END = 420
OPENING_DAY = 840
GRID = 30
ROOMS = (1, 2)

TEACHER_MENU = ((60, 120), (165, 195), (270, 360))   # one off the grid
STUDENT_MENU = ((90, 150), (300, 330))
ROOM_MENU = ((0, 60), (150, 240), (330, 420))
WINDOW_MENU = (None, (0, 420), (90, 300))
PREFERRED_MENU = (None, (180, 270))
PACKAGE_MENU = (100000, 240)
PRIORITY_MENU = ((1, 1), (2, 1))
DURATION_MENU = (30, 60)


def at_most(menu, size):
    return [list(c) for n in range(size + 1) for c in itertools.combinations(menu, n)]


def instances():
    """Every combination: <=2 teacher commitments, <=1 student commitment, <=1
    commitment per room, one availability option per room, <=1 preferred
    window, two package expiries, two priority orders, two durations."""
    for teacher, student, busy1, busy2, window1, window2, preferred, package, priority, duration in itertools.product(
            at_most(TEACHER_MENU, 2), at_most(STUDENT_MENU, 1), at_most(ROOM_MENU, 1), at_most(ROOM_MENU, 1),
            WINDOW_MENU, WINDOW_MENU, PREFERRED_MENU, PACKAGE_MENU, PRIORITY_MENU, DURATION_MENU):
        yield {'teacher': teacher, 'student': student, 'busy': {1: busy1, 2: busy2},
               'window': {1: window1, 2: window2}, 'preferred': preferred, 'package': package,
               'priority': dict(zip(ROOMS, priority)), 'duration': duration}


def micros(minute):
    return (EPOCH_MINUTE_0 + minute * 60) * 1000000


def pair(interval):
    return [micros(interval[0]), micros(interval[1])]


def oracle_input(inst):
    return {
        'now': NOW * 1000000, 'from': None, 'days': 1, 'durationMinutes': inst['duration'], 'limit': 20,
        'preferred': [pair(inst['preferred'])] if inst['preferred'] else [],
        'snapshot': {
            'teacherTeachesSubject': True,
            'rooms': [{'id': room, 'name': f'Sala {room}', 'priority': inst['priority'][room]} for room in ROOMS],
            'teacherBusy': [pair(iv) for iv in inst['teacher']],
            'studentBusy': [pair(iv) for iv in inst['student']],
            'resourceBusy': [[room] + pair(iv) for room in ROOMS for iv in inst['busy'][room]],
            'availability': [[room] + pair(inst['window'][room]) for room in ROOMS if inst['window'][room]],
            'packageValidUntil': micros(inst['package']),
        },
    }


def smt_feasible(inst, k, room):
    """The contract's hard constraints for grid cell k in a room (minutes)."""
    start, end = GRID * k, GRID * k + inst['duration']
    free = lambda busy: z3.And([z3.Or(end <= lo, hi <= start) for lo, hi in busy])
    per_room = []
    for candidate in ROOMS:
        window = inst['window'][candidate]
        inside = z3.And(window[0] <= start, end <= window[1]) if window else z3.BoolVal(False)
        per_room.append(z3.And(room == candidate, free(inst['busy'][candidate]), inside))
    return z3.And(k >= 0, end <= HORIZON_END, end <= OPENING_DAY, free(inst['teacher']), free(inst['student']),
                  z3.Or(per_room), end <= inst['package'])


def smt_cost(inst, k, room):
    start, end = GRID * k, GRID * k + inst['duration']
    if inst['preferred']:
        lo, hi = inst['preferred']
        outside = z3.If(z3.And(lo <= start, end <= hi), 0, 1)
    else:
        outside = z3.IntVal(0)
    gap = z3.IntVal(OPENING_DAY)
    if inst['teacher']:
        distances = [z3.If(hi <= start, start - hi, z3.If(lo >= end, lo - end, 0)) for lo, hi in inst['teacher']]
        gap = distances[0]
        for distance in distances[1:]:
            gap = z3.If(distance < gap, distance, gap)
    priority = z3.If(room == 1, inst['priority'][1], inst['priority'][2])
    return [outside, gap, priority, start, room]


def lex_less(left, right):
    result = z3.BoolVal(False)
    for a, b in reversed(list(zip(left, right))):
        result = z3.Or(a < b, z3.And(a == b, result))
    return result


def cell_of(start_micros):
    minute, remainder = divmod(start_micros // 1000000 - EPOCH_MINUTE_0, 60)
    cell, off_grid = divmod(minute, GRID)
    if remainder or off_grid:
        fail('oracle suggested a start off the grid')
    return cell


def verify_optimality(oracle, stride):
    family = [inst for index, inst in enumerate(instances()) if index % stride == 0]
    feed = ''.join(json.dumps(oracle_input(inst)) + '\n' for inst in family)
    out = subprocess.run([oracle], input=feed, text=True, capture_output=True, check=True).stdout.splitlines()
    if len(out) != len(family):
        fail(f'oracle answered {len(out)} of {len(family)} instances')
    solver = z3.Solver()
    k, room = z3.Ints('k room')
    with_top = empty = controls = 0
    for index, (inst, line) in enumerate(zip(family, out)):
        answer = json.loads(line)
        if 'error' in answer:
            fail('oracle refused an instance: ' + answer['error'])
        feasible = smt_feasible(inst, k, room)
        if not answer['suggestions']:
            solver.push()
            solver.add(feasible)
            if solver.check() != z3.unsat:
                fail(f'completeness: nothing suggested but {solver.model()} is feasible in {inst}')
            solver.pop()
            empty += 1
            continue
        top = answer['suggestions'][0]
        top_k, top_room = cell_of(top['start']), top['roomId']
        top_cost = smt_cost(inst, z3.IntVal(top_k), z3.IntVal(top_room))
        solver.push()
        solver.add(z3.Not(smt_feasible(inst, z3.IntVal(top_k), z3.IntVal(top_room))))
        if solver.check() != z3.unsat:
            fail(f'soundness: the first suggestion {top} is not feasible in {inst}')
        solver.pop()
        reported = answer['feasible'][0]['cost'][:3]
        solver.push()
        solver.add(z3.Or([cost != value for cost, value in zip(top_cost[:3], reported)]))
        if solver.check() != z3.unsat:
            fail(f'cost: the core reports {reported} for {top} in {inst}')
        solver.pop()
        solver.push()
        solver.add(feasible, lex_less(smt_cost(inst, k, room), top_cost))
        if solver.check() != z3.unsat:
            fail(f'optimality: {solver.model()} beats the first suggestion {top} in {inst}')
        solver.pop()
        with_top += 1
        # Negative control: the runner-up claimed as the optimum must be refuted.
        if index % 101 == 0 and len(answer['feasible']) > 1:
            second = answer['feasible'][1]
            second_cost = smt_cost(inst, z3.IntVal(cell_of(second['start'])), z3.IntVal(second['roomId']))
            solver.push()
            solver.add(feasible, lex_less(smt_cost(inst, k, room), second_cost))
            if solver.check() != z3.sat:
                fail('negative control: the runner-up was accepted as optimal')
            solver.pop()
            controls += 1
    if not with_top or not empty or not controls:
        fail(f'degenerate family: {with_top} with a suggestion, {empty} without, {controls} controls')
    scope = 'every instance' if stride == 1 else f'every {stride}th instance (family NOT exhausted)'
    print(f'PROVED optimality-bounded: {with_top} instances with an optimal first suggestion and {empty} '
          f'with no feasible slot; {scope} of {len(family) * stride if stride == 1 else "the"} family')
    print(f'REFUTED (negative control) runner-up claimed optimal: {controls} instances')
    return len(family), with_top, empty


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--oracle')
    parser.add_argument('--stride', type=int, default=1)
    args = parser.parse_args()
    if args.stride < 1:
        fail('stride must be positive')
    expression = verify_overlap()
    print(f'overlap source expression: {expression}')
    if args.oracle:
        verify_optimality(str(Path(args.oracle).resolve(strict=True)), args.stride)
    else:
        print('NOT CHECKED: optimality (pass --oracle with the tdf-slot-suggestions-oracle binary)')
    print(f'z3 {z3.get_version_string()}')


if __name__ == '__main__':
    main()
