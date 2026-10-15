#!/usr/bin/env python3
"""Tests of proof infrastructure, not a substitute for authorization proofs."""
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
spec = importlib.util.spec_from_file_location('auth_proof', ROOT/'scripts/verify-auth-predicates.py')
proof = importlib.util.module_from_spec(spec)
spec.loader.exec_module(proof)


class AuthProofTests(unittest.TestCase):
    def setUp(self):
        self.auth, self.models, self.ingestion = [(ROOT/p).read_text() for p in (proof.AUTH, proof.MODELS, proof.INGESTION)]

    def test_current_formulas_and_source_mutants(self):
        t = proof.lower(self.auth, self.models, self.ingestion)
        for name, policy in proof.properties(t).items():
            solver = z3.Solver()
            solver.set(timeout=30000)
            solver.add(t['domain'], t[name] != policy)
            self.assertEqual(solver.check(), z3.unsat, name)
        for name, formula in proof.mutations(self.auth, self.models, self.ingestion):
            solver = z3.Solver()
            solver.set(timeout=30000)
            solver.add(formula)
            self.assertEqual(solver.check(), z3.sat, name)

    def test_duplicates_and_non_admin_module_holders_remain_denied(self):
        # These constraints exercise mapping boundaries, separate from universal SMT claims.
        t = proof.lower(self.auth, self.models, self.ingestion)
        for role, count in [('Admin', 2), ('Webmaster', 1), ('StudioManager', 1)]:
            solver = z3.Solver()
            solver.set(timeout=30000)
            solver.add(t['domain'], t['party'] == 1, t['adminGrant'],
                       *(c == (count if r == role else 0) for r, c in t['counts'].items()),
                       t['ingestion-admission'])
            self.assertEqual(solver.check(), z3.unsat)

    def test_unrecognized_code_is_not_silently_ignored(self):
        for changed in [self.auth.replace('Admin `elem` auRoles', 'opaquePolicy user'),
                        self.auth.replace('hasStrictAdminAccess ::', 'hasStrictAdminAccess :: AuthedUser -> Bool\nhasStrictAdminAccess ::'),
                        self.auth.replace('length roles == Set.size (Set.fromList roles)', 'True'),
                        self.auth.replace('&& all isStrictAdminRoleScope auRoles', '&& all isStrictAdminRoleScope auRoles where bypass = True')]:
            with self.assertRaises(ValueError):
                proof.lower(changed, self.models, self.ingestion)

    def test_boolean_precedence_and_fail_closed_grammar(self):
        a, b, c = z3.Bools('a b c')
        formula = proof.boolean('a || b && c', {'a': a, 'b': b, 'c': c})
        solver = z3.Solver()
        solver.set(timeout=30000)
        solver.add(formula != z3.Or(a, z3.And(b, c)))
        self.assertEqual(solver.check(), z3.unsat)
        for expression in ['a &&', '(a)', 'not a', 'True', 'a where x = b']:
            with self.assertRaises(ValueError):
                proof.boolean(expression, {'a': a})

    def test_receipts_replay_and_refuse_reuse(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp)/'evidence'
            command = [sys.executable, str(ROOT/'scripts/verify-auth-predicates.py'), '--output', str(output)]
            result = subprocess.run(command, capture_output=True, text=True, timeout=120)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            data = json.loads((output/'result.json').read_text())
            self.assertEqual(len(data['artifacts']), 17)
            self.assertEqual([o['status'] for o in data['obligations']], ['PROVEN']*3)
            for artifact in data['artifacts']:
                content = (output/artifact['file']).read_bytes()
                self.assertEqual(proof.digest(content), artifact['sha256'])
                solver = z3.Solver()
                solver.set(timeout=30000)
                solver.from_string(content.decode())
                self.assertEqual(str(solver.check()), artifact['expected'])
            receipt = (output/'result.json').read_bytes()
            self.assertNotEqual(subprocess.run(command, capture_output=True, timeout=120).returncode, 0)
            self.assertEqual((output/'result.json').read_bytes(), receipt)

    def test_unreviewed_snapshot_unknown_and_stale_source_fail_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            # In-memory patches test actual failure paths without editing live source.
            with patch.object(proof, 'digest', return_value='unreviewed'):
                self.assertEqual(proof.run(Path(tmp)/'unreviewed'), 1)
            original = z3.Solver.check
            with patch.object(z3.Solver, 'check', return_value=z3.unknown):
                self.assertEqual(proof.run(Path(tmp)/'unknown'), 1)
            self.assertIs(z3.Solver.check, original)
            real_read = Path.read_bytes
            reads = 0
            def drift(path):
                nonlocal reads
                data = real_read(path)
                if path == ROOT/proof.AUTH:
                    reads += 1
                    if reads > 1:
                        return data + b'\n-- changed'
                return data
            with patch.object(Path, 'read_bytes', drift):
                self.assertEqual(proof.run(Path(tmp)/'stale'), 1)
            for name in ['unreviewed', 'unknown', 'stale']:
                receipt = json.loads((Path(tmp)/name/'result.json').read_text())
                self.assertFalse(receipt['executionPassed'])
                self.assertTrue(all(o['status'] == 'UNPROVEN' for o in receipt['obligations']))

    def test_refuted_predicate_preserves_counterexample(self):
        original = proof.lower
        def mutate(auth, models, ingestion):
            return original(auth.replace('fromSqlKey auPartyId > 0',
                                         'fromSqlKey auPartyId >= 0'), models, ingestion)
        with tempfile.TemporaryDirectory() as tmp, patch.object(proof, 'lower', mutate):
            output = Path(tmp)/'counterexample'
            self.assertEqual(proof.run(output), 1)
            receipt = json.loads((output/'result.json').read_text())
            self.assertTrue(all(o['status'] == 'REFUTED' for o in receipt['obligations']))
            for name in proof.CLAIMS:
                artifact = next(a for a in receipt['artifacts'] if a['id'] == name)
                self.assertEqual(artifact['actual'], 'sat')
                self.assertTrue(artifact['model'])
                self.assertTrue((output/artifact['file']).is_file())


if __name__ == '__main__':
    unittest.main()
