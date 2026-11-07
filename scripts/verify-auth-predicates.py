#!/usr/bin/env python3
"""Conditional source-derived authorization proofs; not endpoint/session proofs."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
CONTRACT = 'formal/system/auth-predicates.json'
AUTH = 'tdf-hq/src/TDF/Auth.hs'
MODELS = 'tdf-hq/src/TDF/Models.hs'
INGESTION = 'tdf-hq/src/TDF/Server/RecordsIngestion.hs'
CLAIMS = ('strict-admin', 'module-access', 'ingestion-admission')
LEMMA_CLAIMS = {
    'cardinality-nonnegative': 'For count >= 0, count - indicator(count > 0) >= 0.',
    'cardinality-zero': 'For count >= 0, count - indicator(count > 0) = 0 iff count <= 1.',
    'cardinality-sum': 'A sum over all RoleEnum constructors of nonnegative excesses is zero iff every excess is zero.',
}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def function(source, name, signature, arguments):
    # Consume the entire contiguous definition, not just the first matching guard.
    blocks = re.findall(rf'(?m)^{name} :: ([^\n]+)\n([^\n]+(?:\n[^\n]+)*)', source)
    if len(blocks) != 1:
        raise ValueError(f'Expected one complete {name} definition')
    actual_signature, definition = blocks[0]
    normalized = ' '.join(definition.split())
    prefix = f'{name} {arguments} = '
    if actual_signature != signature or not normalized.startswith(prefix):
        raise ValueError(f'Unsupported {name} signature or arguments')
    return normalized[len(prefix):]


def boolean(expression, atoms):
    import z3
    # The accepted grammar has no general application, parentheses, binders or
    # arbitrary Python evaluation. Haskell && binds more tightly than ||.
    if '||' in expression:
        return z3.Or(*(boolean(term.strip(), atoms) for term in expression.split('||')))
    if '&&' in expression:
        return z3.And(*(boolean(term.strip(), atoms) for term in expression.split('&&')))
    if expression in atoms:
        return atoms[expression]
    raise ValueError(f'Unsupported Boolean expression: {expression}')


def lower(auth, models, ingestion):
    import z3
    enums = re.findall(r'(?ms)^data RoleEnum\n(.*?)\n  deriving \(Show, Read, Eq, Ord, Enum, Bounded, Generic\)', models)
    if len(enums) != 1 or not re.fullmatch(r'\s*= \w+(?:\s*\| \w+)*', enums[0]):
        raise ValueError('Unsupported RoleEnum')
    roles = re.findall(r'[=|] (\w+)', enums[0])
    if len(set(roles)) != len(roles):
        raise ValueError('Duplicate role constructor')
    counts = {role: z3.Int(f'count_{role}') for role in roles}
    party = z3.Int('party')
    grant, admin_grant = z3.Bools('requested_module_grant admin_module_grant')
    # Integer arithmetic is exact only under the explicit finite/total list and
    # machine-Int length bound; this is not a proof for infinite or bottom lists.
    domain = z3.And(party >= -(2**63), party < 2**63,
                    *(c >= 0 for c in counts.values()), z3.Sum(list(counts.values())) < 2**63)
    unique_body = function(auth, 'rolesAreUnique', '[RoleEnum] -> Bool', 'roles')
    if unique_body != 'length roles == Set.size (Set.fromList roles)':
        raise ValueError('Unsupported uniqueness implementation')
    # Cardinality reduction is justified by the three checked algebra lemmas below.
    unique = z3.And(*(c <= 1 for c in counts.values()))
    valid_body = function(auth, 'hasValidAuthPartyId', 'AuthedUser -> Bool', 'AuthedUser{..}')
    valid_match = re.fullmatch(r'fromSqlKey auPartyId (>|>=) (0|1)', valid_body)
    if not valid_match:
        raise ValueError('Unsupported identity predicate')
    bound = int(valid_match[2])
    valid = party > bound if valid_match[1] == '>' else party >= bound
    coherent = boolean(function(auth, 'hasCoherentRoleGrants', 'AuthedUser -> Bool',
                                'user@AuthedUser{..}'),
                       {'hasValidAuthPartyId user': valid, 'rolesAreUnique auRoles': unique})
    scope_body = function(auth, 'isStrictAdminRoleScope', 'RoleEnum -> Bool', 'role')
    scope_match = re.fullmatch(r'role `elem` \[(\w+(?:, \w+)*)\]', scope_body)
    if not scope_match:
        raise ValueError('Unsupported strict role scope')
    scope = scope_match[1].split(', ')
    if not set(scope) <= set(roles):
        raise ValueError('Unknown role in strict scope')
    atoms = {f'{role} `elem` auRoles': count > 0 for role, count in counts.items()}
    atoms.update({'all isStrictAdminRoleScope auRoles':
                  z3.And(*(c == 0 for role, c in counts.items() if role not in scope)),
                  'hasCoherentRoleGrants user': coherent})
    strict = boolean(function(auth, 'hasStrictAdminAccess', 'AuthedUser -> Bool',
                              'user@AuthedUser{..}'), atoms)
    module_body = function(auth, 'hasModuleAccess', 'ModuleAccess -> AuthedUser -> Bool',
                           'moduleTag user@AuthedUser{..}')
    def module(value):
        return boolean(module_body, {'hasCoherentRoleGrants user': coherent,
                                    'moduleTag `Set.member` auModules': value})
    guards = re.findall(r'(?m)^    requireAdmin = unless \(([^\n]+)\) \(throwError err403\)$', ingestion)
    if len(guards) != 1:
        raise ValueError('Unsupported ingestion admission guard')
    admission = boolean(guards[0], {'hasStrictAdminAccess user': strict,
                                   'hasModuleAccess ModuleAdmin user': module(admin_grant)})
    return {'domain': domain, 'counts': counts, 'party': party, 'grant': grant,
            'adminGrant': admin_grant, 'strict-admin': strict,
            'module-access': module(grant), 'ingestion-admission': admission}


def properties(translated):
    import z3
    t = translated
    # Normative policy is independent of the parsed source allowlist/operators:
    # AUTH-ARTIST-INVITE-001 authorization and AUTH-REGISTRY-001 authorization.
    unique = z3.And(*(c <= 1 for c in t['counts'].values()))
    coherent = z3.And(t['party'] > 0, unique)
    strict = z3.And(coherent, t['counts']['Admin'] == 1,
                    *(c == 0 for role, c in t['counts'].items()
                      if role not in {'Admin', 'Fan', 'Customer'}))
    return {'strict-admin': strict, 'module-access': z3.And(coherent, t['grant']),
            'ingestion-admission': z3.And(strict, t['adminGrant'])}


def cardinality_lemmas(size):
    import z3
    count = z3.Int('count')
    excess = count - z3.If(count > 0, 1, 0)
    residuals = [z3.Int(f'excess_{i}') for i in range(size)]
    return {
        'cardinality-nonnegative': z3.And(count >= 0, excess < 0),
        'cardinality-zero': z3.And(count >= 0, (excess == 0) != (count <= 1)),
        'cardinality-sum': z3.And(*(r >= 0 for r in residuals),
                                (z3.Sum(residuals) == 0) != z3.And(*(r == 0 for r in residuals))),
    }


def mutations(auth, models, ingestion):
    cases = [
        ('drop-coherence', auth.replace('&& hasCoherentRoleGrants user', '|| hasCoherentRoleGrants user'), ingestion, 'strict-admin'),
        ('admit-operational-role', auth.replace('[Admin, Fan, Customer]', '[Admin, Fan, Customer, Webmaster]'), ingestion, 'strict-admin'),
        ('allow-zero-identity', auth.replace('fromSqlKey auPartyId > 0', 'fromSqlKey auPartyId >= 0'), ingestion, 'strict-admin'),
        ('drop-module-grant', auth.replace('hasCoherentRoleGrants user && moduleTag', 'hasCoherentRoleGrants user || moduleTag'), ingestion, 'module-access'),
        ('module-implies-admin', auth, ingestion.replace('hasStrictAdminAccess user && hasModuleAccess', 'hasStrictAdminAccess user || hasModuleAccess'), 'ingestion-admission'),
    ]
    for name, changed_auth, changed_ingestion, claim in cases:
        if changed_auth == auth and changed_ingestion == ingestion:
            raise ValueError(f'Mutation {name} did not change source')
        t = lower(changed_auth, models, changed_ingestion)
        import z3
        yield name, z3.And(t['domain'], t[claim] != properties(t)[claim])


def run(output):
    output.mkdir()  # Never reuse or overwrite evidence.
    receipt = {'schemaVersion': 1, 'executionPassed': False, 'obligations': [], 'artifacts': [],
               'startedAt': datetime.now(timezone.utc).isoformat(), 'error': None}
    try:
        contract_bytes = (ROOT/CONTRACT).read_bytes()
        spec = json.loads(contract_bytes)
        paths = set(spec['reviewedSources']) | {CONTRACT, 'scripts/verify-auth-predicates.py',
                'scripts/test-auth-predicates.py', 'formal/system/auth-predicates.md',
                'formal/system/requirements.json', '.github/workflows/event-operations-formal.yml'}
        snapshot = {p: (contract_bytes if p == CONTRACT else (ROOT/p).read_bytes())
                    for p in sorted(paths)}
        before = {p: digest(data) for p, data in snapshot.items()}
        receipt['provenance'] = {'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                                 'worktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT, text=True).strip()),
                                 'files': before, 'digest': digest(json.dumps(before, sort_keys=True).encode())}
        for path, reviewed in spec['reviewedSources'].items():
            if before[path] != reviewed:
                raise ValueError(f'Unreviewed source snapshot: {path}; review imports, bindings and translation before updating the pin')
        import z3
        if z3.get_version_string() != '4.13.3':
            raise ValueError('Expected z3-solver 4.13.3.0')
        receipt['toolchain'] = {'python': sys.version, 'z3': z3.get_full_version()}
        auth, models, ingestion = [snapshot[p].decode('utf-8') for p in (AUTH, MODELS, INGESTION)]
        translated = lower(auth, models, ingestion)
        policies = properties(translated)
        def check(identity, formula, expected):
            solver = z3.Solver()
            solver.set(timeout=30_000)
            solver.add(formula)
            data = '(set-option :timeout 30000)\n(set-logic QF_LIA)\n' + solver.sexpr() + '\n(check-sat)\n'
            path = output/f'{identity}.smt2'
            path.write_text(data)
            actual = solver.check()
            replay = z3.Solver()
            replay.set(timeout=30_000)
            replay.from_string(data)
            repeated = str(replay.check())
            artifact = {'id': identity, 'file': path.name, 'sha256': digest(data.encode()),
                        'expected': expected, 'actual': str(actual), 'replay': repeated,
                        'model': str(solver.model()) if actual == z3.sat else None}
            receipt['artifacts'].append(artifact)
            return str(actual) == expected and repeated == expected
        passed = True
        for name, formula in cardinality_lemmas(len(translated['counts'])).items():
            passed = check(name, formula, 'unsat') and passed
        for name in CLAIMS:
            passed = check(name, z3.And(translated['domain'], translated[name] != policies[name]), 'unsat') and passed
            # Require both permitted and refused states, not just consistency.
            for desired in (True, False):
                passed = check(f'{name}-witness-{str(desired).lower()}',
                               z3.And(translated['domain'], translated[name] == desired,
                                      policies[name] == desired), 'sat') and passed
        for name, formula in mutations(auth, models, ingestion):
            passed = check(f'control-{name}', formula, 'sat') and passed
        if {p: digest((ROOT/p).read_bytes()) for p in before} != before:
            raise ValueError('Sources changed during verification')
        receipt['executionPassed'] = passed
        for claim in spec['obligations']:
            artifact = next(a for a in receipt['artifacts'] if a['id'] == claim['id'])
            status = 'PROVEN' if passed else ('REFUTED' if artifact['actual'] == 'sat' else 'UNPROVEN')
            receipt['obligations'].append({**claim, 'status': status, 'artifact': artifact['file']})
        if not passed:
            receipt['error'] = 'Proof, witness, mutation control or replay failed; inspect retained formulas'
    except Exception as error:
        receipt['error'] = str(error)
        # No stale or partially collected result may be promoted.
        receipt['obligations'] = [{'id': name, 'status': 'UNPROVEN'} for name in CLAIMS]
    receipt['supportingLemmas'] = [
        {'id': name, 'claim': claim, 'status': 'PROVEN' if receipt['executionPassed'] else 'UNPROVEN',
         'artifact': name + '.smt2' if any(a['id'] == name for a in receipt['artifacts']) else None,
         'category': 'supporting-algebra', 'assumptions': ['Z3 soundness'],
         'limitation': 'Algebra only; composition with the source translator remains trusted.'}
        for name, claim in LEMMA_CLAIMS.items()]
    receipt['finishedAt'] = datetime.now(timezone.utc).isoformat()
    (output/'result.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps({'executionPassed': receipt['executionPassed'], 'error': receipt['error'],
                      'obligations': {o['id']: o['status'] for o in receipt['obligations']}, 'output': str(output)}))
    return 0 if receipt['executionPassed'] else 1


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    options = parser.parse_args()
    sys.exit(run(options.output.resolve()))
