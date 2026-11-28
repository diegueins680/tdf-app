#!/usr/bin/env python3
"""Z3 obligations for COMMUNITY-FRIEND-SUGGESTION-001 (friend suggestions by PageRank).

Checks, over exact reals and for every graph, viewer and exclusion set with at most
--max-nodes accounts, the one-step facts that the hand proof in
formal/system/friend-suggestions-pagerank.md lifts to every iteration by induction:

  distribution  a step maps a distribution on the viewer's graph to one (dead ends included)
  exclusion     an account outside the graph holds no score, and follow rows that touch
                such an account do not change the step
  contraction   a step shrinks the L1 distance between two distributions by the damping
  bound         2 * damping ^ iterations is below the stated error bound

The step is a hand translation of walkStep in tdf-hq/src/TDF/Social/SuggestionRank.hs.
The constants are read from that file and the translated lines must be present in it;
the translation itself is trusted, and Double rounding is outside these obligations.
Each obligation has a negative control: a broken variant that Z3 must refute.
"""
import argparse
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
SOURCE = 'tdf-hq/src/TDF/Social/SuggestionRank.hs'
# Above this size the symbolic contraction query is too slow; see
# contraction_by_enumeration.
SYMBOLIC_CONTRACTION_NODES = 3
TRANSLATED_LINES = (
    'let share = damping * mass / fromIntegral (length followed)',
    ', sent + damping * mass',
    'restart = total - moved',
    'Map.insertWith (+) (walkViewer graph) restart forwarded',
    '| Set.member viewer blocked || not (Map.member viewer base) = Nothing',
    ', Set.member follower eligible',
    ', Set.member followed eligible',
    ', follower /= followed',
)


def constants(source):
    def one(pattern):
        found = re.findall(pattern, source, re.M)
        if len(found) != 1:
            raise ValueError(f'Unsupported SuggestionRank definition: {pattern}')
        return found[0]
    for line in TRANSLATED_LINES:
        if source.count(line) != 1:
            raise ValueError(f'walkStep no longer matches the translated step: {line}')
    return (Fraction(one(r'^walkDamping = ([0-9.]+)$')),
            int(one(r'^walkIterations = ([0-9]+)$')),
            Fraction(one(r'^walkErrorBound = ([0-9.e-]+)$')))


def world(z3, tag, size):
    """A symbolic viewer, node set and follow rows over `size` accounts."""
    viewer = [z3.Bool(f'viewer_{tag}_{i}') for i in range(size)]
    node = [z3.Bool(f'node_{tag}_{i}') for i in range(size)]
    rows = [[z3.Bool(f'row_{tag}_{i}_{j}') for j in range(size)] for i in range(size)]
    well_formed = [z3.PbEq([(v, 1) for v in viewer], 1)]
    well_formed += [z3.Implies(viewer[i], node[i]) for i in range(size)]
    return viewer, node, rows, well_formed


def step(z3, damping, viewer, node, rows, scores, *, dead_ends_return=True, filter_target=True):
    """walkStep: scores after one step, as a list of real terms."""
    size = len(scores)
    edge = [[z3.And(rows[i][j], node[i], node[j] if filter_target else True) if i != j
             else z3.BoolVal(False) for j in range(size)] for i in range(size)]
    out = [z3.Sum([z3.If(edge[i][j], 1, 0) for j in range(size)]) for i in range(size)]
    share = []
    for i in range(size):
        term = z3.RealVal(0)
        for degree in range(1, size):
            term = z3.If(out[i] == degree, damping * scores[i] / degree, term)
        share.append(term)
    moved = z3.Sum([z3.If(out[i] > 0, damping * scores[i], 0) for i in range(size)])
    total = z3.Sum(scores)
    restart = total - moved if dead_ends_return else (1 - damping) * total
    return [z3.Sum([z3.If(edge[i][j], share[i], 0) for i in range(size)])
            + z3.If(viewer[j], restart, 0) for j in range(size)]


def distribution(z3, node, scores):
    return ([s >= 0 for s in scores] + [z3.Sum(scores) == 1]
            + [z3.Implies(z3.Not(node[i]), scores[i] == 0) for i in range(len(scores))])


def norm1(z3, left, right):
    return z3.Sum([z3.If(a - b >= 0, a - b, b - a) for a, b in zip(left, right)])


def refuted(z3, assumptions, claim):
    """True when no world satisfies the assumptions and violates the claim."""
    solver = z3.Solver()
    solver.set('timeout', 600_000)
    solver.add(*assumptions)
    solver.add(z3.Not(claim))
    result = solver.check()
    if result == z3.unknown:
        raise RuntimeError(f'Z3 returned unknown: {solver.reason_unknown()}')
    return result == z3.unsat


def contraction_by_enumeration(z3, size, damping, factor):
    """The contraction for every follow relation on `size` accounts, one query each.

    The viewer is account 0 and every account is a node. Relabelling accounts gives
    any other viewer, and by exclusion-step an account outside the graph changes
    neither step, so these cases cover the symbolic statement at this size.
    """
    d = z3.RealVal(str(damping))
    viewer, node, rows, _ = world(z3, 'e', size)
    p = [z3.Real(f'ep_{i}') for i in range(size)]
    q = [z3.Real(f'eq_{i}') for i in range(size)]
    solver = z3.Solver()
    solver.set('timeout', 600_000)
    solver.add(*[viewer[i] == (i == 0) for i in range(size)], *node)
    solver.add(*distribution(z3, node, p), *distribution(z3, node, q))
    solver.add(z3.Not(norm1(z3, step(z3, d, viewer, node, rows, p),
                            step(z3, d, viewer, node, rows, q))
                      <= z3.RealVal(str(factor)) * norm1(z3, p, q)))
    pairs = [(i, j) for i in range(size) for j in range(size) if i != j]
    for code in range(2 ** len(pairs)):
        solver.push()
        solver.add(*[rows[i][j] == bool(code >> bit & 1) for bit, (i, j) in enumerate(pairs)])
        result = solver.check()
        solver.pop()
        if result == z3.unknown:
            raise RuntimeError(f'Z3 returned unknown: {solver.reason_unknown()}')
        if result == z3.sat:
            return False, code + 1
    return True, 2 ** len(pairs)


def obligations(z3, size, damping, **broken):
    d = z3.RealVal(str(damping))
    viewer, node, rows, well_formed = world(z3, 'a', size)
    p = [z3.Real(f'p_{i}') for i in range(size)]
    q = [z3.Real(f'q_{i}') for i in range(size)]
    start = [z3.If(viewer[i], z3.RealVal(1), z3.RealVal(0)) for i in range(size)]
    stepped = step(z3, d, viewer, node, rows, p,
                   dead_ends_return=not broken.get('lose_dead_ends'),
                   filter_target=not broken.get('open_target'))
    yield 'distribution-start', well_formed, z3.And(*distribution(z3, node, start))
    yield ('distribution-step', well_formed + distribution(z3, node, p),
           z3.And(*distribution(z3, node, stepped)))
    # Second set of follow rows that agrees with the first between nodes only.
    other = [[z3.Bool(f'row_b_{i}_{j}') for j in range(size)] for i in range(size)]
    same_inside = [z3.Implies(z3.And(node[i], node[j]), rows[i][j] == other[i][j])
                   for i in range(size) for j in range(size)]
    stepped_other = step(z3, d, viewer, node, other, p,
                         filter_target=not broken.get('open_target'))
    yield ('exclusion-step', well_formed + distribution(z3, node, p) + same_inside,
           z3.And(*[a == b for a, b in zip(stepped, stepped_other)]))
    factor = z3.RealVal(str(broken.get('factor', damping)))
    stepped_q = step(z3, d, viewer, node, rows, q)
    stepped_p = step(z3, d, viewer, node, rows, p)
    yield ('contraction-step',
           well_formed + distribution(z3, node, p) + distribution(z3, node, q),
           norm1(z3, stepped_p, stepped_q) <= factor * norm1(z3, p, q))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--max-nodes', type=int, default=4)
    parser.add_argument('--output', help='new directory for result.json')
    arguments = parser.parse_args()
    import z3
    source = (ROOT / SOURCE).read_bytes()
    damping, iterations, bound = constants(source.decode())
    result = {'schemaVersion': 1, 'requirement': 'COMMUNITY-FRIEND-SUGGESTION-001',
              'source': SOURCE, 'sourceSha256': hashlib.sha256(source).hexdigest(),
              'z3': z3.get_version_string(), 'maxNodes': arguments.max_nodes,
              'damping': str(damping), 'iterations': iterations, 'errorBound': str(bound),
              'scope': 'Exact reals; every viewer, node set and follow relation up to maxNodes '
                       'accounts; one-step obligations only. Above three accounts the '
                       'contraction is checked per follow relation with the viewer fixed '
                       'and every account a node. The induction over iterations, '
                       'the extension to larger graphs and the correspondence with the '
                       'Haskell and SQL text are not machine-checked. Double rounding is '
                       'outside the model.',
              'obligations': [], 'controls': []}
    failures = []
    for size in range(1, arguments.max_nodes + 1):
        for name, assumptions, claim in obligations(z3, size, damping):
            record = {'name': name, 'nodes': size, 'method': 'symbolic'}
            if name == 'contraction-step' and size > SYMBOLIC_CONTRACTION_NODES:
                holds, cases = contraction_by_enumeration(z3, size, damping, damping)
                record.update(method='enumerated follow relations, viewer 0, all nodes',
                              cases=cases)
            else:
                holds = refuted(z3, assumptions, claim)
            record['status'] = 'proved' if holds else 'refuted'
            result['obligations'].append(record)
            print(json.dumps(record), flush=True)
            if not holds:
                failures.append(f'{name} fails with {size} accounts')
    exact = 2 * damping ** iterations
    result['obligations'].append({'name': 'bound', 'value': float(exact),
                                  'status': 'proved' if exact < bound else 'refuted'})
    if not exact < bound or not 0 < damping < 1:
        failures.append('2 * damping ^ iterations is not below the stated bound')
    controls = (('distribution-step', {'lose_dead_ends': True}, 'dead ends lose their mass'),
                ('exclusion-step', {'open_target': True}, 'follows into excluded accounts count'),
                ('contraction-step', {'factor': damping - Fraction(1, 100)},
                 'a contraction factor below the damping'),
                ('bound', {}, 'ten iterations instead of the contract count'))
    size = min(SYMBOLIC_CONTRACTION_NODES, arguments.max_nodes)
    for target, broken, description in controls:
        if target == 'bound':
            caught = not 2 * damping ** 10 < bound
        else:
            caught = any(name == target and not refuted(z3, assumptions, claim)
                         for name, assumptions, claim in obligations(z3, size, damping, **broken))
        result['controls'].append({'obligation': target, 'mutation': description,
                                   'status': 'refuted as expected' if caught else 'not detected'})
        if not caught:
            failures.append(f'negative control not detected: {description}')
    result['status'] = 'failed' if failures else 'passed'
    result['failures'] = failures
    if arguments.output:
        directory = Path(arguments.output)
        directory.mkdir(parents=True)  # never reuse a previous receipt
        (directory / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
    for item in result['obligations'][-1:] + result['controls']:
        print(json.dumps(item))
    print(result['status'])
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
