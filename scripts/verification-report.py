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
ARITHMETIC_EXPECTATIONS = {
    f'{name}-{kind}': expected
    for name in ('capture', 'refund')
    for kind, expected in (('safety', 'unsat'), ('admission', 'unsat'),
                           ('witness-0', 'sat'), ('witness-1', 'sat'),
                           ('legacy-counterexample', 'sat'))
}

# These inventories are defined by the consumer, independently of producer labels.
AUTHORIZATION_CLAIMS = ('strict-admin', 'module-access', 'ingestion-admission')
AUTHORIZATION_LEMMAS = ('cardinality-nonnegative', 'cardinality-zero', 'cardinality-sum')
RECOVERY_CLAIMS = ('window-equivalence', 'schema-correspondence',
                   'issuance-admission', 'issuance-exact')
SUITES = {
    'authorization': {
        'contract': 'formal/system/auth-predicates.json',
        'script': 'scripts/verify-auth-predicates.py',
        'claims': AUTHORIZATION_CLAIMS, 'lemmas': AUTHORIZATION_LEMMAS,
        'expectations': {
            **{identity: 'unsat' for identity in AUTHORIZATION_CLAIMS + AUTHORIZATION_LEMMAS},
            **{f'{identity}-witness-{value}': 'sat' for identity in AUTHORIZATION_CLAIMS
               for value in ('true', 'false')},
            **{'control-' + identity: 'sat' for identity in
               ('drop-coherence', 'admit-operational-role', 'allow-zero-identity',
                'drop-module-grant', 'module-implies-admin')},
        },
    },
    'recovery': {
        'contract': 'formal/system/recovery-expiry-arithmetic.json',
        'script': 'scripts/verify-recovery-expiry.py',
        'claims': RECOVERY_CLAIMS, 'lemmas': (),
        'expectations': {
            **{identity: 'unsat' for identity in RECOVERY_CLAIMS},
            **{f'witness-{issued}-{offset}': 'sat' for issued in (0, 2**63 - 1 - 900)
               for offset in (0, 899)},
            'witness-overflow-rejection': 'sat',
            **{'control-' + identity: 'sat' for identity in
               ('inclusive-deadline', 'future-issuance', 'validator-lifetime',
                'database-lifetime', 'issuance-lifetime')},
        },
    },
}


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
    paths.update({'formal/system/auth-predicates.json', 'formal/system/auth-predicates.md',
                  'scripts/verify-auth-predicates.py', 'scripts/test-auth-predicates.py'})
    paths.update(json.loads((ROOT/'formal/system/auth-predicates.json').read_text())['reviewedSources'])
    paths.update({'formal/system/recovery-expiry-arithmetic.json', 'formal/system/recovery-expiry-arithmetic.md',
                  'scripts/verify-recovery-expiry.py', 'scripts/test-recovery-expiry-verifier.py',
                  'scripts/recovery-time-boundaries.py', 'scripts/test-recovery-time-postgres.sh'})
    paths.update(json.loads((ROOT/'formal/system/recovery-expiry-arithmetic.json').read_text())['reviewedSources'])
    paths.update({'formal/system/certificate-tools.json', 'formal/system/proof-certificates.md'})
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


def build_report(requirements, provenance, arithmetic_ok, reason, refuted=(), authorization=None, recovery=None):
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
    for name, receipt, contract_path in (
        ('authorization', authorization, 'formal/system/auth-predicates.json'),
        ('recovery', recovery, 'formal/system/recovery-expiry-arithmetic.json')):
        contract = json.loads((ROOT/contract_path).read_text())
        for claim in contract['obligations']:
            identity = claim['requirementId'] + ':' + claim['id']
            actual = next((o for o in (receipt or {}).get('obligations', [])
                           if o['id'] == claim['id']), {})
            status = actual.get('status', 'UNPROVEN')
            if status == 'PROVEN' and not ((receipt or {}).get('executionPassed') is True
                                           and (receipt or {}).get('replayPassed') is True):
                status = 'UNPROVEN'
            obligations.append({**claim, 'id': identity, 'status': status,
                                'artifact': name + '/' + actual['artifact'] if actual.get('artifact') else None})
            next(row for row in rows if row['requirementId'] == claim['requirementId'])['obligations'].append(identity)
    critical = [row['requirementId'] for row in rows if row['risk'] == 'P0' and row['status'] != 'PROVEN']
    return {
        'schemaVersion': 1, 'provenance': provenance,
        'scope': 'All registered requirements classified; only capture/refund arithmetic, three authorization predicates and four recovery-time contracts can be discharged by this runner. This is not an exhaustive inventory of all behaviors or derived obligations.',
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


def replay_smt(directory, artifacts, expectations, complete=True):
    import z3
    identities = [a['id'] for a in artifacts]
    if (len(identities) != len(set(identities)) or not set(identities) <= set(expectations)
            or (complete and set(identities) != set(expectations))):
        raise ValueError('Incomplete SMT input inventory')
    replay = []
    for artifact in artifacts:
        expected = expectations[artifact['id']]
        if artifact['expected'] != expected:
            raise ValueError('SMT obligation expectation mismatch: ' + artifact['id'])
        if artifact['file'] != artifact['id'] + '.smt2':
            raise ValueError('SMT obligation file mismatch')
        path = directory/artifact['file']
        if path.is_symlink():
            raise ValueError('SMT artifact must be retained directly, not a symlink')
        content = path.read_bytes()
        if sha(content) != artifact['sha256']:
            raise ValueError('SMT input digest mismatch')
        solver = z3.Solver()
        solver.set(timeout=30_000)
        solver.from_string(content.decode())
        result = solver.check()
        replay.append({'id': artifact['id'], 'expected': expected, 'result': str(result),
                       'model': str(solver.model()) if result == z3.sat else None,
                       'reasonUnknown': solver.reason_unknown() if result == z3.unknown else None})
    return replay


def replay_arithmetic(directory, artifacts):
    replay = replay_smt(directory, artifacts, ARITHMETIC_EXPECTATIONS)
    if any(row['result'] != row['expected'] for row in replay):
        raise ValueError('SMT replay failed or unknown')
    return replay


def admit_receipt(name, directory, receipt, provenance, exit_code):
    """Replay every obligation/control before admitting any producer proof label."""
    suite = SUITES[name]
    source = receipt['provenance']
    if (source['revision'] != provenance['revision']
            or source['worktreeDirty'] is not provenance['worktreeDirty']):
        raise ValueError('Receipt revision or dirty-state mismatch')
    contract = json.loads((ROOT/suite['contract']).read_text())
    files = source['files']
    if source.get('digest') != sha(json.dumps(files, sort_keys=True).encode()):
        raise ValueError('Receipt source digest mismatch')
    required = set(contract['reviewedSources']) | {
        suite['contract'], suite['script'], 'formal/system/requirements.json'}
    if (not required <= set(files)
            or any(provenance['files'].get(path) != digest
                   for path, digest in contract['reviewedSources'].items())
            or any(path not in provenance['files'] or provenance['files'][path] != digest
                   for path, digest in files.items())):
        raise ValueError('Receipt source fingerprint mismatch')
    if set(suite['claims']) != {row['id'] for row in contract['obligations']}:
        raise ValueError('Contract obligation inventory mismatch')

    def rows_for(field, identities):
        rows = receipt.get(field, [])
        if len(rows) != len(identities) or {row['id'] for row in rows} != set(identities):
            raise ValueError('Receipt obligation inventory mismatch: ' + field)
        for row in rows:
            if row['status'] not in STATUSES or row.get('artifact') != row['id'] + '.smt2':
                raise ValueError('Receipt obligation status or artifact mismatch')
        return rows

    claims = rows_for('obligations', suite['claims'])
    lemmas = rows_for('supportingLemmas', suite['lemmas'])
    replay = replay_smt(directory, receipt['artifacts'], suite['expectations'])
    (directory.parent/(name + '-replay.json')).write_text(json.dumps(replay, indent=2) + '\n')
    results = {row['id']: row['result'] for row in replay}
    replay_passed = all(row['result'] == row['expected'] for row in replay)
    passed = (exit_code == 0 and receipt.get('executionPassed') is True
              and receipt.get('error') is None and replay_passed
              and all(row['status'] == 'PROVEN' for row in claims + lemmas))
    # Report status is derived from fresh replay. Producer text cannot create
    # PROVEN or REFUTED evidence without a corresponding checked formula.
    def classified(rows):
        return [{**row, 'status': 'REFUTED' if results[row['id']] == 'sat'
                 else 'PROVEN' if passed else 'UNPROVEN'} for row in rows]
    return {**receipt, 'executionPassed': bool(passed), 'replayPassed': replay_passed,
            'obligations': classified(claims), 'supportingLemmas': classified(lemmas),
            'error': receipt.get('error') or (None if passed else 'Producer or consumer replay failed')}


def run(out, require_release):
    out = out.resolve()
    if out.is_relative_to(ROOT.resolve()):
        raise ValueError('Output directory must be outside the source checkout')
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
        arithmetic = json.loads((out/'arithmetic/result.json').read_text())
        if arithmetic.get('executionPassed') is not True or arithmetic.get('error') is not None:
            raise ValueError('Arithmetic producer did not report successful execution')
        replay = replay_arithmetic(out/'arithmetic', arithmetic['artifacts'])
        (out/'replay.json').write_text(json.dumps(replay, indent=2) + '\n')
        if fingerprint(requirements) != before:
            raise ValueError('Sources changed during verification')
        ok, reason = True, 'Fresh solver execution, controls and ten serialized SMT problems replayed successfully'
    except Exception as error:  # Preserve failed/inconclusive execution; never promote it.
        reason = str(error)
    refuted = []
    arithmetic_path = out/'arithmetic/result.json'
    if not ok and arithmetic_path.is_file() and fingerprint(requirements) == before:
        try:
            failed = json.loads(arithmetic_path.read_text())
            replay = replay_smt(out/'arithmetic', failed.get('artifacts', []),
                                ARITHMETIC_EXPECTATIONS, complete=False)
            (out/'arithmetic-counterexample-replay.json').write_text(json.dumps(replay, indent=2) + '\n')
            refuted = [row['id'] for row in replay
                       if row['expected'] == 'unsat' and row['result'] == 'sat']
        except Exception as error:
            reason += '; arithmetic counterexample replay incomplete: ' + str(error)
    receipts, runs = {}, []
    for name, script in (('authorization', 'verify-auth-predicates.py'),
                         ('recovery', 'verify-recovery-expiry.py')):
        passed = False
        try:
            command = [sys.executable, 'scripts/' + script, '--output', str(out/name)]
            with (out/(name + '-execution.log')).open('x') as log:
                result = subprocess.run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT, timeout=180)
            receipt = json.loads((out/name/'result.json').read_text())
            receipt = admit_receipt(name, out/name, receipt, before, result.returncode)
            passed = receipt['executionPassed']
            receipts[name] = receipt
            if not passed:
                reason += '; ' + name + ' failed: ' + str(receipt.get('error'))
        except Exception as error:
            reason += '; ' + name + ' incomplete: ' + str(error)
        runs.append(passed)
    if fingerprint(requirements) != before:
        ok, receipts, refuted = False, {}, []
        reason = 'Sources changed during verification; all claims stale'
    report = build_report(requirements, before, ok, reason, refuted, **receipts)
    ok = ok and all(runs)
    report.update({'startedAt': started, 'finishedAt': datetime.now(timezone.utc).isoformat(),
                   'execution': {'passed': ok, 'reason': reason, 'python': sys.version},
                   'supportingAlgebraLemmas': receipts.get('authorization', {}).get('supportingLemmas', []),
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
