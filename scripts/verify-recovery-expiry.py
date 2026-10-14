#!/usr/bin/env python3
"""Source-derived Int64 recovery-time contracts; full ID-SESSION-003 stays UNPROVEN."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
CONTRACT = 'formal/system/recovery-expiry-arithmetic.json'
SOURCE = 'tdf-hq/src/TDF/ServerAuth.hs'
MIGRATION = 'tdf-hq/sql/2026-10-04_auth_recovery_expiry.sql'
CLAIMS = ('window-equivalence', 'schema-correspondence', 'issuance-admission', 'issuance-exact')
MAX = 2**63 - 1


def sha(data):
    return hashlib.sha256(data).hexdigest()


def expressions(source, migration):
    functions = re.findall(
        r'(?m)^recoveryWindowValid :: Int64 -> Int64 -> Int64 -> Bool\n'
        r'recoveryWindowValid issued expires current =\n((?:  [^\n]+\n)+)', source)
    if len(functions) != 1:
        raise ValueError('Unsupported recoveryWindowValid definition')
    blocks = re.findall(r'(?s)\ncreatePasswordResetToken :: (.*?)\ncreateReusableSessionToken ::', source)
    if len(blocks) != 1:
        raise ValueError('Unsupported recovery issuance definition')
    issuance = re.findall(
        r'\[toPersistValue tokenId,toPersistValue credentialId,PersistInt64 issued,'
        r'PersistInt64 \(([^\n]+)\)\]', blocks[0])
    constraints = re.findall(r'(?s)CONSTRAINT auth_recovery_time_window CHECK \(\s*(.*?)\s*\)\s*\);', migration)
    for name in ('issued_at_epoch', 'expires_at_epoch'):
        if len(re.findall(rf'(?m)^  {name} BIGINT NOT NULL,$', migration)) != 1:
            raise ValueError('Time columns must be non-null signed bigint; NULL semantics unsupported')
    if len(issuance) != 1 or len(constraints) != 1:
        raise ValueError('Expected one issuance expression and one time CHECK')
    return ' '.join(functions[0].split()), issuance[0], ' '.join(constraints[0].split())


def boolean(expression, atom, sql=False):
    import z3
    or_token, and_token = (' OR ', ' AND ') if sql else (' || ', ' && ')
    if or_token in expression:
        if sql:
            raise ValueError('SQL OR/error evaluation semantics unsupported')
        return z3.Or(*(boolean(x.strip(), atom, sql) for x in expression.split(or_token)))
    if and_token in expression:
        return z3.And(*(boolean(x.strip(), atom, sql) for x in expression.split(and_token)))
    return atom(expression)


def lower(source, migration):
    import z3
    window, issuance, constraint = expressions(source, migration)
    issued, expires, current = z3.BitVecs('issued expires current', 64)
    names = {'issued': issued, 'expires': expires, 'current': current,
             'issued_at_epoch': issued, 'expires_at_epoch': expires}
    sql_defined = []
    def term(value, sql):
        if value in names:
            return names[value]
        if re.fullmatch(r'\d+', value) and int(value) <= MAX:
            return int(value)
        pattern = (r'(expires_at_epoch|issued_at_epoch) - (expires_at_epoch|issued_at_epoch)'
                   if sql else r'toInteger (issued|expires|current) - toInteger (issued|expires|current)')
        match = re.fullmatch(pattern, value)
        if match:
            difference = z3.SignExt(1, names[match[1]]) - z3.SignExt(1, names[match[2]])
            if sql:
                # PostgreSQL bigint arithmetic raises instead of wrapping. False
                # and error both refuse this time constraint; no evaluation order assumed.
                sql_defined.append(z3.And(difference >= -(2**63), difference <= MAX))
            return difference
        raise ValueError('Unsupported arithmetic term: ' + value)
    def atom(expression, sql=False):
        match = re.fullmatch(r'(.+?) (>=|<=|==|=|>|<) (.+)', expression)
        if not match:
            raise ValueError('Unsupported comparison: ' + expression)
        a, b = term(match[1], sql), term(match[3], sql)
        if type(a) is int and type(b) is int:
            raise ValueError('Constant-only comparison unsupported')
        if type(a) is int:
            a = z3.BitVecVal(a, b.size())
        if type(b) is int:
            b = z3.BitVecVal(b, a.size())
        if a.size() != b.size():
            raise ValueError('Mixed arithmetic widths unsupported')
        return {'>=': lambda: a >= b, '<=': lambda: a <= b, '>': lambda: a > b,
                '<': lambda: a < b, '==': lambda: a == b, '=': lambda: a == b}[match[2]]()
    accepted = boolean(window, atom)
    sql_expression = boolean(constraint, lambda x: atom(x, True), True)
    database = z3.And(sql_expression, *sql_defined)
    increment = re.fullmatch(r'issued\s*\+\s*(\d+)', issuance)
    if not increment or int(increment[1]) > MAX:
        raise ValueError('Unsupported issuance expression')
    generated = issued + int(increment[1])  # Actual Int64 addition wraps.
    return {'issued': issued, 'expires': expires, 'current': current,
            'accepted': accepted, 'database': database, 'generated': generated}


def obligations(t):
    import z3
    i, e, n = [t[k] for k in ('issued', 'expires', 'current')]
    wide_i, wide_e, wide_n = [z3.SignExt(1, x) for x in (i, e, n)]
    # Policy is the normative 900-second half-open interval; independent of source constants.
    policy = z3.And(i >= 0, wide_e == wide_i + 900, wide_n >= wide_i, wide_n < wide_i + 900)
    at_issuance = z3.substitute(t['accepted'], (n, i))
    generated_ok = z3.substitute(t['database'], (e, t['generated']))
    return {
        'window-equivalence': t['accepted'] != policy,
        'schema-correspondence': t['database'] != at_issuance,
        'issuance-admission': generated_ok != z3.And(i >= 0, i <= MAX - 900),
        'issuance-exact': z3.And(generated_ok, z3.SignExt(1, t['generated']) != wide_i + 900),
    }


def problems(source, migration):
    import z3
    t = lower(source, migration)
    for name, formula in obligations(t).items():
        yield name, formula, 'unsat'
    # Explicit boundary witnesses include the largest representable valid window.
    for i in (0, MAX - 900):
        for offset in (0, 899):
            yield f'witness-{i}-{offset}', z3.And(t['issued'] == i, t['expires'] == i + 900,
                                                t['current'] == i + offset, t['accepted'], t['database']), 'sat'
    yield 'witness-overflow-rejection', z3.And(t['issued'] == MAX - 899,
        z3.Not(z3.substitute(t['database'], (t['expires'], t['generated'])))), 'sat'
    mutations = [
        ('inclusive-deadline', source.replace('current < expires', 'current <= expires'), migration, 'window-equivalence'),
        ('future-issuance', source.replace('issued <= current', 'issued <= expires'), migration, 'window-equivalence'),
        ('validator-lifetime', source.replace('toInteger issued == 900', 'toInteger issued == 901'), migration, 'window-equivalence'),
        ('database-lifetime', source, migration.replace('issued_at_epoch = 900', 'issued_at_epoch = 901'), 'schema-correspondence'),
        ('issuance-lifetime', source.replace('PersistInt64 (issued+900)', 'PersistInt64 (issued+901)'), migration, 'issuance-admission'),
    ]
    for name, changed, changed_sql, claim in mutations:
        if (changed, changed_sql) == (source, migration):
            raise ValueError('Source mutation did not apply: ' + name)
        yield 'control-' + name, obligations(lower(changed, changed_sql))[claim], 'sat'


def run(out):
    out.mkdir()
    record = {'schemaVersion': 1, 'executionPassed': False, 'artifacts': [], 'obligations': [],
              'startedAt': datetime.now(timezone.utc).isoformat(), 'error': None}
    try:
        contract = json.loads((ROOT/CONTRACT).read_text())
        paths = set(contract['reviewedSources']) | {CONTRACT, 'scripts/verify-recovery-expiry.py',
                 'scripts/test-recovery-expiry-verifier.py', 'formal/system/recovery-expiry-arithmetic.md',
                 'formal/system/requirements-verification.txt', 'formal/system/requirements.json',
                 '.github/workflows/event-operations-formal.yml'}
        before = {p: sha((ROOT/p).read_bytes()) for p in sorted(paths)}
        record['provenance'] = {'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                               'worktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT, text=True).strip()),
                               'files': before, 'digest': sha(json.dumps(before, sort_keys=True).encode())}
        for p, value in contract['reviewedSources'].items():
            if before[p] != value:
                raise ValueError('Unreviewed source snapshot: ' + p)
        import z3
        if z3.get_version_string() != '4.13.3':
            raise ValueError('Expected z3-solver 4.13.3.0')
        record['toolchain'] = {'python': sys.version, 'z3': z3.get_full_version()}
        passed = True
        for name, formula, expected in problems((ROOT/SOURCE).read_text(), (ROOT/MIGRATION).read_text()):
            solver = z3.Solver()
            solver.set(timeout=30_000)
            solver.add(formula)
            text = '(set-option :timeout 30000)\n(set-logic QF_BV)\n' + solver.sexpr() + '\n(check-sat)\n'
            (out/(name+'.smt2')).write_text(text)
            actual = solver.check()
            replay = z3.Solver()
            replay.set(timeout=30_000)
            replay.from_string(text)
            repeated = str(replay.check())
            record['artifacts'].append({'id': name, 'file': name+'.smt2', 'sha256': sha(text.encode()),
                'expected': expected, 'actual': str(actual), 'replay': repeated,
                'model': str(solver.model()) if actual == z3.sat else None})
            passed = passed and str(actual) == expected and repeated == expected
        if {p: sha((ROOT/p).read_bytes()) for p in before} != before:
            raise ValueError('Source changed during verification')
        record['executionPassed'] = passed
        for claim in contract['obligations']:
            artifact = next(a for a in record['artifacts'] if a['id'] == claim['id'])
            record['obligations'].append({**claim, 'status': 'PROVEN' if passed else
                ('REFUTED' if artifact['actual'] == 'sat' else 'UNPROVEN'), 'artifact': artifact['file']})
        if not passed:
            record['error'] = 'Proof/witness/control/replay failed; inspect retained inputs and valuations'
    except Exception as error:
        record['error'] = str(error)
        record['obligations'] = [{'id': name, 'status': 'UNPROVEN'} for name in CLAIMS]
    record['finishedAt'] = datetime.now(timezone.utc).isoformat()
    (out/'result.json').write_text(json.dumps(record, indent=2)+'\n')
    print(json.dumps({'output': str(out), 'executionPassed': record['executionPassed'], 'error': record['error'],
                      'obligations': {o['id']: o['status'] for o in record['obligations']}}))
    return 0 if record['executionPassed'] else 1


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    options = parser.parse_args()
    sys.exit(run(options.output.resolve()))
