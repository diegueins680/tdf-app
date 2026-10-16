#!/usr/bin/env python3
"""Cross-check exact SMT inputs and independently check selected CPC refutations."""
import argparse
from collections import Counter
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import shutil
import subprocess

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('assurance', ROOT/'scripts/verification-report.py')
assurance = importlib.util.module_from_spec(spec)
spec.loader.exec_module(assurance)
CERTIFICATES = {
    'arithmetic': tuple(name for name, expected in assurance.ARITHMETIC_EXPECTATIONS.items() if expected == 'unsat'),
    'authorization': assurance.AUTHORIZATION_CLAIMS + assurance.AUTHORIZATION_LEMMAS,
    'recovery': assurance.RECOVERY_CLAIMS,
}
EXPECTATIONS = {'arithmetic': assurance.ARITHMETIC_EXPECTATIONS,
                **{k: v['expectations'] for k, v in assurance.SUITES.items()}}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def sexprs(text):
    """Restricted unquoted s-expressions; all unsupported lexical forms refuse."""
    text = re.sub(r';[^\n]*', '', text)
    if any(c in text for c in ('"', '|', '\\')):
        raise ValueError('Quoted/escaped symbols unsupported by certificate admission')
    tokens = re.findall(r'\(|\)|[^\s()]+', text)
    stack, result = [], []
    for token in tokens:
        if token == '(':
            stack.append([])
            if len(stack) > 512:
                raise ValueError('Expression depth limit')
        elif token == ')':
            if not stack:
                raise ValueError('Unbalanced expression')
            value = stack.pop()
            (stack[-1] if stack else result).append(value)
        elif stack:
            stack[-1].append(token)
        else:
            raise ValueError('Unexpected top-level atom')
    if stack:
        raise ValueError('Incomplete expression')
    return result


def emit(value):
    return '('+' '.join(emit(x) for x in value)+')' if isinstance(value, list) else value


def bind_certificate(raw, original):
    """Remove duplicate declarations, never assertions; Ethos binds assumptions."""
    lines = raw.splitlines()
    if lines[:2] == ['unsupported', 'unsat']:
        # CVC5 does not implement Z3's :timeout option. Both CLI and process
        # timeouts are independently set below; only this known option is ignored.
        if not original.startswith('(set-option :timeout 30000)\n'):
            raise ValueError('Unexpected unsupported solver command')
        lines = lines[1:]
    if not lines or lines[0] != 'unsat' or 'WARNING' in raw:
        raise ValueError('No complete, warning-free UNSAT proof')
    wrapper = sexprs('\n'.join(lines[1:]))
    if len(wrapper) != 1:
        raise ValueError('Expected one CPC proof wrapper')
    commands = wrapper[0]
    inputs = sexprs(original)
    declarations = {}
    assertions = 0
    for command in inputs:
        if command[:1] == ['declare-fun'] and len(command) == 4 and command[2] == []:
            declarations[command[1]] = command[3]
        elif command[:1] == ['assert'] and len(command) == 2:
            assertions += 1
        elif command[:1] not in (['set-option'], ['set-logic'], ['check-sat']):
            raise ValueError('Unsupported reference command')
    if assertions != 1:
        raise ValueError('Expected one exact reference assertion')
    allowed = {'define', 'assume', 'assume-push', 'step', 'step-pop', 'declare-const'}
    filtered, declared, definitions, scopes, assumptions = [], set(), set(), 0, 0
    for command in commands:
        if not command or command[0] not in allowed:
            raise ValueError('Unsupported CPC command')
        kind = command[0]
        if kind == 'declare-const':
            if len(command) != 3 or command[1] in declared or declarations.get(command[1]) != command[2]:
                raise ValueError('CPC declaration differs from the reference')
            declared.add(command[1])
            # Reference declarations already introduce these exact variables.
            # Redeclaring creates fresh variables and loses assumption binding.
            continue
        if kind == 'define':
            if len(command) != 4 or not re.fullmatch(r'@t\d+', command[1]) or command[2] != [] or command[1] in definitions:
                raise ValueError('Unsupported or shadowing definition')
            definitions.add(command[1])
        elif kind == 'assume':
            assumptions += 1
            if len(command) != 3 or scopes:
                raise ValueError('Unsupported root assumption')
        elif kind == 'assume-push':
            scopes += 1
        elif kind == 'step-pop':
            scopes -= 1
            if scopes < 0:
                raise ValueError('Invalid assumption scope')
        filtered.append(command)
    if assumptions != 1 or scopes or not filtered or filtered[-1][:1] != ['step'] or filtered[-1][2:3] != ['false']:
        raise ValueError('Proof must close all scopes and end in an explicit false step')
    return '\n'.join(emit(c) for c in filtered)+'\n'


def execute(command, output, timeout=45):
    result = subprocess.run(command, capture_output=True, text=True, timeout=timeout)
    output.with_suffix(output.suffix+'.stdout').write_text(result.stdout)
    output.with_suffix(output.suffix+'.stderr').write_text(result.stderr)
    return result


def run(report_dir, tools, out):
    out.mkdir(mode=0o700)
    contracts = {}
    receipt = {'schemaVersion': 1, 'startedAt': datetime.now(timezone.utc).isoformat(),
               'executionPassed': False, 'crossSolver': [], 'certificates': [], 'error': None,
               'limitations': 'Formula refutations only. Source translation, Ethos/CPC rules, compilers, GMP and host remain trusted. No implementation/refinement status or release permission is promoted.'}
    try:
        metadata = assurance.capture_metadata()
        requirements = json.loads(metadata['formal/system/requirements.json'])['requirements']
        before = assurance.fingerprint(requirements, metadata)
        contracts = {suite: assurance.metadata_json(config['contract'], before, metadata)['obligations']
                     for suite, config in assurance.SUITES.items()}
        receipt['provenance'] = before
        report_bytes = (report_dir/'report.json').read_bytes()
        report = json.loads(report_bytes)
        if report['provenance'] != before or report['execution']['passed'] is not True:
            raise ValueError('Assurance evidence is stale or execution failed')
        receipt['reportSha256'] = sha(report_bytes)
        installation = json.loads((tools/'installation.json').read_text())
        configuration = ROOT/'formal/system/certificate-tools.json'
        if installation['configurationSha256'] != sha(configuration.read_bytes()):
            raise ValueError('Tool installation uses a different pinned configuration')
        for name, expected in installation['binaries'].items():
            if name not in ('cvc5', 'ethos') or (tools/name).is_symlink() or sha((tools/name).read_bytes()) != expected:
                raise ValueError('Tool binary changed')
        if set(installation['binaries']) != {'cvc5', 'ethos'}:
            raise ValueError('Missing proof tool')
        signatures = tools/'cvc5-source/proofs/eo/cpc'
        actual = {str(p.relative_to(signatures)): sha(p.read_bytes())
                  for p in sorted(signatures.rglob('*')) if p.is_file() and not p.is_symlink()}
        if actual != installation['signatures']:
            raise ValueError('CPC signatures changed')
        # Retained checker inputs use relative references and include their pinned
        # signatures, so downloading/moving the artifact preserves reproduction.
        retained_signatures = out/'signatures'
        shutil.copytree(signatures, retained_signatures)
        receipt['toolchain'] = installation
        passed = True
        checked_formulas = {}  # Only byte-identical inputs in this fresh run can share a refutation.
        for suite, expectations in EXPECTATIONS.items():
            directory = out/suite
            directory.mkdir()
            for identity, expected in expectations.items():
                key = suite+'/'+identity+'.smt2'
                original = report_dir/key
                if original.is_symlink() or original.parent.is_symlink():
                    raise ValueError('SMT input must be retained directly: '+key)
                original_bytes = original.read_bytes()
                if sha(original_bytes) != report['evidenceHashes'].get(key):
                    raise ValueError('Missing/changed SMT input: '+key)
                target = directory/(identity+'.smt2')
                target.write_bytes(original_bytes)
                result = execute([str(tools/'cvc5'), '--lang=smt2', '--tlimit=30000', str(target)], target)
                lines = result.stdout.splitlines()
                matched = result.returncode == 0 and not result.stderr and (lines == [expected] or
                    lines == ['unsupported', expected] and target.read_text().startswith('(set-option :timeout 30000)\n'))
                receipt['crossSolver'].append({'id': suite+':'+identity, 'expected': expected,
                    'actual': lines, 'matched': matched, 'sha256': sha(target.read_bytes())})
                passed = passed and matched
                if identity not in CERTIFICATES.get(suite, ()):
                    continue
                problem = target.read_bytes()
                entry = {'id': suite+':'+identity, 'status': 'UNPROVEN',
                         'input': str(target.relative_to(out)), 'inputSha256': sha(problem)}
                receipt['certificates'].append(entry)
                previous = checked_formulas.get(problem)
                if previous is not None:
                    # The original proof references identical bytes. Its checked
                    # refutation applies to this formula too; preserve each
                    # obligation's own ID/input and disclose the shared proof.
                    entry.update({k: v for k, v in previous.items() if k not in ('id', 'input')})
                    entry['reusedFrom'] = previous['id']
                    continue
                # Even generation errors must leave the attempted obligation UNPROVEN.
                result = execute([str(tools/'cvc5'), '--lang=smt2', '--tlimit=120000',
                    '--produce-proofs', '--dump-proofs', '--proof-format-mode=cpc',
                    '--proof-granularity=dsl-rewrite', '--no-proof-allow-trust', str(target)], target.with_suffix('.generation'), timeout=135)
                if result.returncode != 0 or result.stderr:
                    raise ValueError('Certificate generation failed: '+entry['id'])
                proof = directory/(identity+'.cpc')
                proof.write_text(f'(include "../signatures/Cpc.eo")\n(reference "{target.name}")\n' +
                                 bind_certificate(result.stdout, target.read_text()))
                proof_hash = sha(proof.read_bytes())
                checked = execute([str(tools/'ethos'), str(proof)], proof, timeout=600)
                if sha(proof.read_bytes()) != proof_hash:
                    raise ValueError('Certificate changed during checking')
                complete = checked.returncode == 0 and checked.stdout.strip() == 'correct' and not checked.stderr
                entry.update(status='PROVEN' if complete else 'UNPROVEN',
                             proof=str(proof.relative_to(out)), proofSha256=proof_hash,
                             checkerOutput=checked.stdout, checkerExitCode=checked.returncode,
                             scope='Independent CPC refutation of the exact referenced SMT assertion; conditional source correspondence remains unchanged')
                passed = passed and complete
                if complete:
                    checked_formulas[problem] = dict(entry)
        if assurance.fingerprint(requirements) != before:
            raise ValueError('Sources changed during verification')
        if sha((report_dir/'report.json').read_bytes()) != receipt['reportSha256']:
            raise ValueError('Assurance report changed during verification')
        for entry in receipt['crossSolver']:
            suite, identity = entry['id'].split(':')
            if sha((out/suite/(identity+'.smt2')).read_bytes()) != entry['sha256']:
                raise ValueError('Retained SMT input changed during verification')
        for name, expected in installation['binaries'].items():
            if sha((tools/name).read_bytes()) != expected:
                raise ValueError('Proof tool changed during verification')
        for entry in receipt['certificates']:
            if entry.get('proof') and sha((out/entry['proof']).read_bytes()) != entry['proofSha256']:
                raise ValueError('Retained certificate changed during verification')
        for directory in (signatures, retained_signatures):
            hashes = {str(p.relative_to(directory)): sha(p.read_bytes())
                      for p in sorted(directory.rglob('*')) if p.is_file() and not p.is_symlink()}
            if hashes != installation['signatures']:
                raise ValueError('CPC signatures changed during verification')
        receipt['executionPassed'] = passed
    except Exception as error:
        receipt['error'] = str(error)
    receipt['finishedAt'] = datetime.now(timezone.utc).isoformat()
    recorded = {entry['id'] for entry in receipt['certificates']}
    for suite, identities in CERTIFICATES.items():
        for identity in identities:
            if suite+':'+identity not in recorded:
                receipt['certificates'].append({'id': suite+':'+identity, 'status': 'UNPROVEN',
                    'reason': 'Required certificate was not reached; inspect execution error'})
    receipt['pendingCertificateObligations'] = [
        {'id': suite+':'+name, 'status': 'UNPROVEN',
         'reason': 'Independent certificate validation not supported by this configured job; conditional SMT result remains separate'}
        for suite, expectations in EXPECTATIONS.items() for name, expected in expectations.items()
        if expected == 'unsat' and name not in CERTIFICATES.get(suite, ())]
    for entry in receipt['certificates'] + receipt['pendingCertificateObligations']:
        suite, identity = entry['id'].split(':')
        if suite == 'arithmetic':
            req = 'SYS-MONEY-001' if identity.startswith('capture-') else 'SYS-MONEY-002'
            entry['sourceObligationIds'] = [req+':'+identity.split('-', 1)[1]]
            entry['requirementIds'] = [req]
        else:
            rows = [c for c in contracts.get(suite, []) if c['id'] == identity]
            entry['sourceObligationIds'] = [c['requirementId']+':'+c['id'] for c in rows]
            entry['supportingLemma'] = not bool(rows) if suite in contracts else None
            entry['requirementIds'] = sorted({c['requirementId'] for c in rows or contracts.get(suite, [])})
        entry['category'] = 'independently-checked-CPC'
        entry['risk'] = 'P0'
    counts = Counter(entry['status'] for entry in receipt['certificates'] + receipt['pendingCertificateObligations'])
    receipt['metrics'] = {'crossSolverInputsExpected': sum(len(e) for e in EXPECTATIONS.values()),
                          'crossSolverInputsMatched': sum(e['matched'] for e in receipt['crossSolver']),
                          'independentlyCheckedDistinctInputs': len({e['proofSha256'] for e in receipt['certificates']
                                                                     if e['status'] == 'PROVEN'}),
                          'certificateObligations': {s: counts[s] for s in assurance.STATUSES}}
    (out/'result.json').write_text(json.dumps(receipt, indent=2)+'\n')
    return receipt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('report', 'tools', 'output'):
        parser.add_argument('--'+name, required=True, type=Path)
    args = parser.parse_args()
    paths = [getattr(args, name).resolve() for name in ('report', 'tools', 'output')]
    if any(p.is_relative_to(ROOT) for p in paths) or any('"' in str(p) or '\\' in str(p) for p in paths):
        raise ValueError('Use external paths without quotes or backslashes')
    receipt = run(*paths)
    print(json.dumps({'executionPassed': receipt['executionPassed'], 'error': receipt['error'],
                      'certificates': len(receipt['certificates'])}))
    raise SystemExit(0 if receipt['executionPassed'] else 1)


if __name__ == '__main__':
    main()
