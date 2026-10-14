#!/usr/bin/env python3
"""Verifier regression controls; tests are not universal implementation proofs."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import z3

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('recovery_proof', ROOT/'scripts/verify-recovery-expiry.py')
proof = importlib.util.module_from_spec(spec)
spec.loader.exec_module(proof)


class RecoveryProofTests(unittest.TestCase):
    def setUp(self):
        self.source = (ROOT/proof.SOURCE).read_text()
        self.sql = (ROOT/proof.MIGRATION).read_text()

    def test_all_contracts_witnesses_and_mutations(self):
        results = []
        for name, formula, expected in proof.problems(self.source, self.sql):
            solver = z3.Solver()
            solver.set(timeout=30000)
            solver.add(formula)
            self.assertEqual(str(solver.check()), expected, name)
            results.append(name)
        self.assertEqual(len(results), 14)

    def test_nonnull_and_unknown_syntax_are_required(self):
        for changed, sql in [
            (self.source, self.sql.replace('issued_at_epoch BIGINT NOT NULL', 'issued_at_epoch BIGINT')),
            (self.source.replace('issued >= 0', 'opaqueGuard issued'), self.sql),
            (self.source.replace('toInteger expires - toInteger issued', 'expires - issued'), self.sql),
            (self.source.replace('PersistInt64 (issued+900)', 'PersistInt64 (opaqueExpiry issued)'), self.sql),
            (self.source, self.sql.replace('AND expires_at_epoch >', 'OR expires_at_epoch >')),
        ]:
            with self.assertRaises(ValueError):
                proof.lower(changed, sql)

    def test_boundary_values_match_mathematical_policy(self):
        t = proof.lower(self.source, self.sql)
        cases = [(0, 900, 0), (0, 900, 899), (0, 900, 900), (1, 901, 0),
                 (-1, 899, 1), (proof.MAX-900, proof.MAX, proof.MAX-1),
                 (proof.MAX-899, -(2**63), proof.MAX), (0, proof.MAX, 1)]
        for i, e, n in cases:
            expected = i >= 0 and e == i + 900 and i <= n < e
            concrete = z3.simplify(z3.substitute(t['accepted'],
                (t['issued'], z3.BitVecVal(i,64)), (t['expires'], z3.BitVecVal(e,64)),
                (t['current'], z3.BitVecVal(n,64))))
            self.assertEqual(z3.is_true(concrete), expected, (i,e,n))

    def test_all_serialized_inputs_replay_and_output_is_exclusive(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp)/'evidence'
            command = [sys.executable, str(ROOT/'scripts/verify-recovery-expiry.py'), '--output', str(out)]
            result = subprocess.run(command, capture_output=True, text=True, timeout=180)
            self.assertEqual(result.returncode, 0, result.stdout+result.stderr)
            receipt = json.loads((out/'result.json').read_text())
            self.assertEqual([o['status'] for o in receipt['obligations']], ['PROVEN']*4)
            self.assertEqual(len(receipt['artifacts']), 14)
            for artifact in receipt['artifacts']:
                data = (out/artifact['file']).read_bytes()
                self.assertEqual(proof.sha(data), artifact['sha256'])
                solver = z3.Solver()
                solver.set(timeout=30000)
                solver.from_string(data.decode())
                self.assertEqual(str(solver.check()), artifact['expected'])
            before = (out/'result.json').read_bytes()
            self.assertNotEqual(subprocess.run(command, capture_output=True, timeout=180).returncode, 0)
            self.assertEqual((out/'result.json').read_bytes(), before)

    def test_source_mutation_counterexample_is_retained(self):
        original = proof.lower
        def mutate(source, sql):
            return original(source.replace('current < expires', 'current <= expires'), sql)
        with tempfile.TemporaryDirectory() as tmp, patch.object(proof, 'lower', mutate):
            out = Path(tmp)/'counterexample'
            self.assertEqual(proof.run(out), 1)
            receipt = json.loads((out/'result.json').read_text())
            row = next(o for o in receipt['obligations'] if o['id']=='window-equivalence')
            self.assertEqual(row['status'], 'REFUTED')
            artifact = next(a for a in receipt['artifacts'] if a['id']=='window-equivalence')
            self.assertEqual(artifact['actual'], 'sat')
            self.assertTrue(artifact['model'])
            self.assertTrue((out/artifact['file']).is_file())

    def test_unknown_unreviewed_and_drift_never_become_proven(self):
        with tempfile.TemporaryDirectory() as tmp:
            with patch.object(z3.Solver, 'check', return_value=z3.unknown):
                self.assertEqual(proof.run(Path(tmp)/'unknown'), 1)
            with patch.object(proof, 'sha', return_value='unreviewed'):
                self.assertEqual(proof.run(Path(tmp)/'unreviewed'), 1)
            original = Path.read_bytes
            reads = 0
            def drift(path):
                nonlocal reads
                data = original(path)
                if path == ROOT/proof.SOURCE:
                    reads += 1
                    if reads > 1:
                        return data+b'\n-- changed'
                return data
            with patch.object(Path, 'read_bytes', drift):
                self.assertEqual(proof.run(Path(tmp)/'drift'), 1)
            for name in ['unknown', 'unreviewed', 'drift']:
                receipt = json.loads((Path(tmp)/name/'result.json').read_text())
                self.assertFalse(receipt['executionPassed'])
                self.assertTrue(all(o['status']=='UNPROVEN' for o in receipt['obligations']))


if __name__ == '__main__':
    unittest.main()
