#!/usr/bin/env python3
"""Exact-candidate assurance report. No test/model-to-implementation promotion."""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
STATUSES = ('PROVEN', 'REFUTED', 'UNPROVEN', 'NOT_APPLICABLE')


def sha(data):
    return hashlib.sha256(data).hexdigest()


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT, text=True).strip()


def fingerprint(requirements):
    paths = {'formal/system/requirements.json', 'formal/system/authority-decisions.json',
             'formal/system/assurance-report.md', 'formal/system/payment-arithmetic.md',
             'docs/payments/ecuador-payment-platform-audit-2026-09-09.md',
             'docs/adr/0102-versioned-money-and-holds.md',
             'formal/system/requirements-verification.txt',
             'scripts/verification-report.py', 'scripts/verify-payment-arithmetic.py',
             'scripts/test-payment-arithmetic-verifier.py', 'scripts/test-verification-report.py',
             '.github/workflows/event-operations-formal.yml'}
    for row in requirements:
        for field in ('implementation', 'tests', 'formalModels'):
            paths.update(row[field])
    files = {p: sha((ROOT/p).read_bytes()) for p in sorted(paths)}
    return {'files': files, 'digest': sha(json.dumps(files, sort_keys=True).encode()),
            'revision': git('rev-parse', 'HEAD'), 'mobileGitlink': git('ls-tree', 'HEAD', 'tdf-mobile'),
            'worktreeDirty': bool(git('status', '--porcelain'))}


def count_status(rows):
    counts = Counter(row['status'] for row in rows)
    if set(counts) - set(STATUSES):
        raise ValueError('Unknown proof status')
    return {status: counts[status] for status in STATUSES}


def build_report(requirements, provenance, arithmetic_ok, reason, refuted=()):
    rows, obligations = [], []
    for req in requirements:
        # Legacy PARTIAL/PASS labels never establish a mathematical proof.
        # All existing critical rows are conservatively P0 until individually triaged.
        risk = 'P0' if req.get('critical', False) else 'P1'
        row = {'requirementId': req['id'], 'requirement': req['statement'], 'authority': req['source'],
               'specification': {'file': 'formal/system/requirements.json', 'id': req['id']},
               'implementation': req['implementation'], 'interfaceBoundary': req['domain'],
               'risk': risk, 'riskBasis': 'Conservative mapping of existing critical flag; not a new business rule',
               'implementationCommit': provenance['revision'], 'sourceDigest': provenance['digest'],
               'status': 'UNPROVEN', 'legacyConformance': req['conformance'],
               'models': req['formalModels'], 'complementaryTests': req['tests'],
               'ambiguities': req['ambiguity'], 'obligations': []}
        for category, claim in (
            ('implementation', 'Every behavior at the mapped implementation boundary satisfies this requirement.'),
            ('refinement', 'Implementation initialization, steps and observations refine the normative specification.')):
            identity = req['id'] + ':' + category
            row['obligations'].append(identity)
            obligations.append({'id': identity, 'requirementId': req['id'], 'category': category,
                                'claim': claim, 'risk': risk, 'status': 'UNPROVEN',
                                'mapping': 'No complete machine-checked correspondence established by this runner',
                                'method': 'Not discharged', 'artifact': None,
                                'limitations': 'Existing models and tests are separate evidence; all unmapped behavior remains outside coverage.'})
        rows.append(row)
    for name, requirement in (('capture', 'SYS-MONEY-001'), ('refund', 'SYS-MONEY-002')):
        if requirement not in {row['requirementId'] for row in rows}:
            raise ValueError('Arithmetic requirement missing from register')
        for kind, claim in (
            ('safety', 'Admitted result equals the exact sum, strictly increases current and is <= limit.'),
            ('admission', 'Admission iff amount > 0 and the exact sum <= limit.')):
            obligations.append({
                'id': f'{requirement}:{kind}', 'requirementId': requirement,
                'category': 'source-derived-SMT', 'risk': 'P0',
                'status': ('REFUTED' if f'{name}-{kind}' in refuted else
                           'PROVEN' if arithmetic_ok else 'UNPROVEN'), 'claim': claim,
                'mapping': f'Restricted source recognizer lowers actual {name} guards/update to signed 64-bit bitvectors; 65-bit sign extension gives exact sums.',
                'method': 'Z3 4.13.3 QF_BV UNSAT of negated claim, SAT witnesses; SMT-LIB replay',
                'artifact': f'arithmetic/{name}-{kind}.smt2' if arithmetic_ok or f'{name}-{kind}' in refuted else None,
                'assumptions': ['current and limit are nonnegative signed Int64 values',
                                'restricted Haskell-to-SMT translator is correct', 'Z3 is sound'],
                'limitations': 'No verified compiler/translator, caller precondition, dispatch, persistence, active payment handler, concurrency or whole-program refinement proof.',
                'execution': reason})
            next(row for row in rows if row['requirementId'] == requirement)['obligations'].append(f'{requirement}:{kind}')
    critical = [row['requirementId'] for row in rows if row['risk'] == 'P0' and row['status'] != 'PROVEN']
    return {
        'schemaVersion': 1, 'provenance': provenance,
        'scope': 'All registered requirements classified; only capture/refund arithmetic discharged. This is not an exhaustive inventory of all behaviors or derived obligations.',
        'requirements': rows, 'obligations': obligations,
        'metrics': {'requirements': count_status(rows),
                    'requirementsWithDeclaredAmbiguityEntries': sum(bool(row['ambiguities']) for row in rows),
                    'obligationsByCategory': {category: count_status([o for o in obligations if o['category'] == category])
                                              for category in sorted({o['category'] for o in obligations})}},
        'release': {'decision': 'BLOCKED', 'executionAllowed': False,
                    'criticalUnprovenRequirements': critical, 'acceptedRisks': [],
                    'reason': 'Mandatory implementation/refinement evidence is missing. No authenticated owner risk acceptance or release-artifact correspondence is established.',
                    'enforcementBoundary': 'The --require-release command refuses release. Existing Hetzner planner remains executionAllowed=false; no guarded production executor exists here.'}}


def render(report):
    lines = ['# Exact-candidate verification report', '',
             f"Revision: `{report['provenance']['revision']}`. Source digest: `{report['provenance']['digest']}`.",
             f"Dirty checkout: `{report['provenance']['worktreeDirty']}`. Decision: **{report['release']['decision']}**.",
             '', report['scope'], '', '## Separate assurance metrics', '',
             '| Category | PROVEN | REFUTED | UNPROVEN | NOT_APPLICABLE |', '|---|---:|---:|---:|---:|']
    for category, counts in {'requirements': report['metrics']['requirements'], **report['metrics']['obligationsByCategory']}.items():
        lines.append('| ' + category + ' | ' + ' | '.join(str(counts[s]) for s in STATUSES) + ' |')
    lines += ['', '## Requirement traceability', '',
              'Exact source files, interfaces, authority, assumptions, artifacts and obligation details are in `report.json` and `sources.json`.', '',
              '| Requirement | Risk | Status | Open implementation / refinement |', '|---|---|---|---|']
    for row in report['requirements']:
        lines.append(f"| {row['requirementId']} | {row['risk']} | {row['status']} | Both UNPROVEN |")
    lines += ['', '## Release decision', '', report['release']['reason'], '',
              'No critical UNPROVEN obligation is accepted. The JSON enumerates every critical blocker.', '',
              'SMT files are replayable solver inputs, not independently checked proof certificates. The solver and source translation remain trusted.', '',
              'See `formal/system/assurance-report.md` for architecture, TCB, counterexamples, CI, limitations and continuation steps.', '']
    return '\n'.join(lines)


def run(out, require_release):
    out.mkdir()  # Exclusive output; a previous run can never supply missing evidence.
    requirements = json.loads((ROOT/'formal/system/requirements.json').read_text())['requirements']
    before = fingerprint(requirements)
    (out/'sources.json').write_text(json.dumps(before, indent=2) + '\n')
    started = datetime.now(timezone.utc).isoformat()
    ok, reason = False, 'Verification not completed'
    try:
        commands = ([sys.executable, 'scripts/verify-payment-arithmetic.py', '--output', str(out/'arithmetic')],
                    [sys.executable, 'scripts/test-payment-arithmetic-verifier.py'])
        for i, command in enumerate(commands):
            with (out/f'execution-{i}.log').open('x') as log:
                result = subprocess.run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT, timeout=180)
            if result.returncode != 0:
                raise ValueError(f'Command {i} failed; see execution-{i}.log')
        # Reparse serialized formulas in a fresh solver. This catches serialization
        # drift but does not remove the same solver or translator from the TCB.
        import z3
        arithmetic = json.loads((out/'arithmetic/result.json').read_text())
        expected_ids = {f'{name}-{kind}' for name in ('capture', 'refund')
                        for kind in ('safety', 'admission', 'witness-0', 'witness-1', 'legacy-counterexample')}
        artifacts = arithmetic['artifacts']
        if len(artifacts) != 10 or {a['id'] for a in artifacts} != expected_ids:
            raise ValueError('Incomplete SMT input inventory')
        replay = []
        for artifact in artifacts:
            content = (out/'arithmetic'/artifact['file']).read_bytes()
            if sha(content) != artifact['sha256']:
                raise ValueError('SMT input digest mismatch')
            solver = z3.Solver()
            solver.set(timeout=30_000)
            solver.from_string(content.decode())
            result = str(solver.check())
            if result != artifact['expected']:
                raise ValueError('SMT replay failed or unknown')
            replay.append({'id': artifact['id'], 'result': result})
        (out/'replay.json').write_text(json.dumps(replay, indent=2) + '\n')
        if fingerprint(requirements) != before:
            raise ValueError('Sources changed during verification')
        ok, reason = True, 'Fresh solver execution, controls and ten serialized SMT problems replayed successfully'
    except Exception as error:  # Preserve failed/inconclusive execution; never promote it.
        reason = str(error)
    refuted = []
    arithmetic_path = out/'arithmetic/result.json'
    if not ok and arithmetic_path.is_file() and fingerprint(requirements) == before:
        failed = json.loads(arithmetic_path.read_text())
        refuted = [a['id'] for a in failed.get('artifacts', [])
                   if a.get('expected') == 'unsat' and a.get('actual') == 'sat']
    report = build_report(requirements, before, ok, reason, refuted)
    report.update({'startedAt': started, 'finishedAt': datetime.now(timezone.utc).isoformat(),
                   'execution': {'passed': ok, 'reason': reason, 'python': sys.version},
                   'evidenceHashes': {p.relative_to(out).as_posix(): sha(p.read_bytes())
                                      for p in sorted(out.rglob('*')) if p.is_file()}})
    (out/'report.json').write_text(json.dumps(report, indent=2) + '\n')
    (out/'report.md').write_text(render(report))
    print(json.dumps({'output': str(out), 'executionPassed': ok, 'release': report['release']['decision'],
                      'metrics': report['metrics']}))
    return 1 if not ok else (2 if require_release and report['release']['decision'] != 'READY' else 0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--require-release', action='store_true', help='Exit 2 unless release is READY')
    options = parser.parse_args()
    sys.exit(run(options.output.resolve(), options.require_release))
