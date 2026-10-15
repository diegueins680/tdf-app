#!/usr/bin/env python3
"""Source-bound numeric-branch obligation; not a JSON/library refinement proof."""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import yaml
import z3

ROOT = Path(__file__).resolve().parent.parent
HASKELL = ROOT / 'tdf-hq/src/TDF/MusicRelease/Money.hs'
SQL = ROOT / 'tdf-hq/sql/2026-10-15_music_exact_minor_units.sql'
OPENAPI = ROOT / 'tdf-hq/docs/openapi/api.yaml'


def decimal_pattern(positive=False):
    maximum = str(2**63 - 1)
    choices = [] if positive else ['0']
    choices.append('[1-9][0-9]{0,17}')
    for index, digit in enumerate(maximum):
        low, high = (1 if index == 0 else 0), int(digit) - 1
        if high < low:
            continue
        char = str(low) if low == high else f'[{low}-{high}]'
        suffix = f'[0-9]{{{18-index}}}' if index < 18 else ''
        choices.append(maximum[:index] + char + suffix)
    choices.append(maximum)
    # JSON Schema uses regex search semantics. '$' alone can accept a final
    # newline in JavaScript, so require actual end of input explicitly.
    return '^(' + '|'.join(choices) + r')(?![\s\S])'


def check_contract(text, limit):
    schemas = yaml.safe_load(text)['components']['schemas']
    for name, positive in [('MusicMinorUnitsDecimal', False), ('MusicPositiveMinorUnitsDecimal', True)]:
        scalar = {key: value for key, value in schemas[name].items() if key != 'description'}
        if scalar != {'type': 'string', 'maxLength': 19, 'pattern': decimal_pattern(positive)}:
            raise ValueError('Money decimal contract drift')
    scalar = {key: value for key, value in schemas['MusicMinorUnits'].items() if key != 'description'}
    if scalar != {'oneOf': [
        {'type': 'integer', 'format': 'int64', 'minimum': 0, 'maximum': limit},
        {'$ref': '#/components/schemas/MusicMinorUnitsDecimal'},
    ]}:
        raise ValueError('Money numeric contract drift')


def normalize(text):
    return ' '.join(text.split())


def source_bound(haskell, sql):
    match = re.search(r'^maxNumericMinor = ([0-9]+)$', haskell, re.M)
    if not match:
        raise ValueError('Unsupported money bound declaration')
    limit = int(match[1])
    body = haskell.split('musicMinorUnitsJSON :: Int64 -> Value\n', 1)[1]
    body = body.split('\nparseMusicMinorUnits ::', 1)[0]
    expected = '''musicMinorUnitsJSON amount
      | amount >= negate maxNumericMinor && amount <= maxNumericMinor =
          Number (fromIntegral amount)
      | otherwise = String (T.pack (show amount))'''
    if normalize(body) != normalize(expected):
        raise ValueError('Unsupported Haskell money encoding shape')
    sql = '\n'.join(line.split('--', 1)[0] for line in sql.splitlines())
    expected_sql = f'''CREATE OR REPLACE FUNCTION public.music_minor_units_json(amount BIGINT)
      RETURNS JSONB LANGUAGE SQL IMMUTABLE STRICT PARALLEL SAFE AS $$
      SELECT CASE WHEN amount BETWEEN {-limit} AND {limit}
        THEN to_jsonb(amount) ELSE to_jsonb(amount::TEXT) END $$;'''
    if normalize(sql).lower() != normalize(expected_sql).lower():
        raise ValueError('Unsupported or divergent SQL money encoding shape')
    if not 0 < limit <= 2**63 - 1:
        raise ValueError('Unsupported signed Int64 money bound')
    return limit


def check_numeric(limit, *, lower_guard=True, precision=64):
    amount = z3.BitVec('amount_minor', 64)
    target = z3.Float64() if precision == 64 else z3.Float32()
    numeric = z3.fpSignedToFP(z3.RNE(), amount, target)
    decoded = z3.fpToSBV(z3.RTZ(), numeric, z3.BitVecSort(64))
    solver = z3.Solver()
    solver.set(timeout=30000)
    solver.add(amount <= limit, decoded != amount)
    if lower_guard:
        solver.add(amount >= -limit)
    result = solver.check()
    if result == z3.unknown:
        raise AssertionError('Money obligation did not finish decisively')
    return {'result': str(result), 'counterexample':
            solver.model().eval(amount).as_signed_long() if result == z3.sat else None}


def verify(haskell, sql):
    limit = source_bound(haskell, sql)
    intended = check_numeric(limit)
    if intended['result'] != 'unsat':
        raise AssertionError('ExactNumericWire violated')
    controls = [
        ('widen numeric range', check_numeric(2**53 + 1)),
        ('omit lower guard', check_numeric(limit, lower_guard=False)),
        ('use binary32', check_numeric(limit, precision=32)),
    ]
    if not all(value['result'] == 'sat' for _, value in controls):
        raise AssertionError('A money negative control did not refute ExactNumericWire')
    return {'numericBound': limit, 'ExactNumericWire': intended,
            'negativeControls': [{'mutation': name, **value} for name, value in controls]}


if __name__ == '__main__':
    if sys.argv[1:] == ['--write-openapi-money-patterns']:
        text = OPENAPI.read_text()
        for name, positive in [('MusicMinorUnitsDecimal', False), ('MusicPositiveMinorUnitsDecimal', True)]:
            expression = rf'(    {name}:\n(?:(?:      [^\n]*)\n)*?      pattern: )[^\n]+'
            text, count = re.subn(expression, lambda match: match[1] + "'" + decimal_pattern(positive) + "'", text)
            if count != 1:
                raise ValueError('Cannot locate canonical money schema')
        OPENAPI.write_text(text)
        raise SystemExit(0)
    if sys.argv[1:]:
        raise SystemExit('Only --write-openapi-money-patterns is supported')
    haskell, sql = HASKELL.read_bytes(), SQL.read_bytes()
    result = verify(haskell.decode(), sql.decode())
    check_contract(OPENAPI.read_text(), result['numericBound'])
    print(json.dumps({
        'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
        'solver': z3.get_version_string(), 'status': 'numeric-wire-obligation-passed',
        'sources': {str(p.relative_to(ROOT)): hashlib.sha256(data).hexdigest()
                    for p, data in [(HASKELL, haskell), (SQL, sql), (OPENAPI, OPENAPI.read_bytes())]},
        'results': result,
        'limitations': 'Full signed64 numeric-branch domain under trusted source recognition '
        'and IEEE754 binary64 round-to-nearest parsing assumptions. Decimal text, Aeson, '
        'PostgreSQL, browser Intl, price admission and program refinement are tested separately.'
    }, indent=2))
