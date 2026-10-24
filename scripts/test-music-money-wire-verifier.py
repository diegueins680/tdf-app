#!/usr/bin/env python3
import importlib.util
from pathlib import Path
import unittest
import re
import copy
import json
import subprocess
import yaml

spec = importlib.util.spec_from_file_location('money_wire', Path(__file__).with_name('verify-music-money-wire.py'))
proof = importlib.util.module_from_spec(spec)
spec.loader.exec_module(proof)


class MoneyWireVerification(unittest.TestCase):
    def test_actual_source_and_named_controls(self):
        result = proof.verify(proof.HASKELL.read_text(), proof.SQL.read_text())
        self.assertEqual(result['ExactNumericWire']['result'], 'unsat')
        self.assertEqual(len(result['negativeControls']), 3)

    def test_matching_unsafe_source_changes_do_not_keep_the_old_proof(self):
        haskell = proof.HASKELL.read_text().replace('9007199254740991', '9007199254740993')
        sql = proof.SQL.read_text().replace('9007199254740991', '9007199254740993')
        with self.assertRaisesRegex(AssertionError, 'ExactNumericWire violated'):
            proof.verify(haskell, sql)

    def test_divergent_or_unknown_translation_is_rejected(self):
        with self.assertRaises(ValueError):
            proof.source_bound(proof.HASKELL.read_text(), proof.SQL.read_text().replace('amount::TEXT', 'amount'))
        with self.assertRaises(ValueError):
            proof.source_bound(proof.HASKELL.read_text().replace('Number (fromIntegral amount)', 'Number 0'), proof.SQL.read_text())

    def test_contract_mutations_cannot_remove_or_weaken_the_text_branch(self):
        original = yaml.safe_load(proof.OPENAPI.read_text())
        mutations = [
            ('MusicMinorUnits', lambda value: value['oneOf'].pop()),
            ('MusicMinorUnits', lambda value: value['oneOf'].__setitem__(1, {'type': 'string'})),
            ('MusicMinorUnits', lambda value: value['oneOf'].append({'type': 'number'})),
            ('MusicMinorUnitsDecimal', lambda value: value.__setitem__('type', 'integer')),
            ('MusicPositiveMinorUnitsDecimal', lambda value: value.__setitem__('maxLength', 20)),
        ]
        for name, mutate in mutations:
            with self.subTest(schema=name, mutation=mutate):
                changed = copy.deepcopy(original)
                mutate(changed['components']['schemas'][name])
                with self.assertRaisesRegex(ValueError, 'Money .* contract drift'):
                    proof.check_contract(yaml.safe_dump(changed), 9007199254740991)

    def test_decimal_patterns_in_actual_javascript_regex_engine(self):
        cases = ['0', '1', '9007199254740993', '9223372036854775807',
                 '9223372036854775808', '01', '-1', '+1', '1e2', '1.0', ' 1', '1\n']
        program = "const x=JSON.parse(process.argv[1]);process.stdout.write(JSON.stringify(x.patterns.map(p=>x.cases.map(v=>new RegExp(p).test(v)))));"
        result = subprocess.check_output(['node', '-e', program, json.dumps({
            'patterns': [proof.decimal_pattern(p) for p in [False, True]], 'cases': cases,
        })], text=True)
        expected = [[bool(re.search(proof.decimal_pattern(p), value)) for value in cases]
                    for p in [False, True]]
        self.assertEqual(json.loads(result), expected)

    def test_contract_decimal_boundaries(self):
        proof.check_contract(proof.OPENAPI.read_text(), 9007199254740991)
        for value in [0, 1, 9007199254740993, 2**63 - 1, 2**63, 10**19]:
            for positive in [False, True]:
                self.assertEqual(bool(re.search(proof.decimal_pattern(positive), str(value))),
                                 (0 < value if positive else 0 <= value) and value <= 2**63 - 1)
        for invalid in ['01', '-1', '+1', '1e2', '1.0', ' 1', '1\n']:
            self.assertIsNone(re.search(proof.decimal_pattern(), invalid))


if __name__ == '__main__':
    unittest.main()
