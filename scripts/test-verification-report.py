#!/usr/bin/env python3
"""Regression controls for assurance-category separation and replay artifacts."""
import importlib.util
import json
from pathlib import Path
import subprocess
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('report', ROOT/'scripts/verification-report.py')
report = importlib.util.module_from_spec(spec)
spec.loader.exec_module(report)


class AssuranceTests(unittest.TestCase):
    def setUp(self):
        self.requirements = json.loads((ROOT/'formal/system/requirements.json').read_text())['requirements']
        self.provenance = {'revision': 'a'*40, 'digest': 'b'*64, 'worktreeDirty': False}

    def test_in_checkout_output_is_rejected_before_creating_evidence(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)/'source'
            root.mkdir()
            alias = Path(tmp)/'outside-link'
            alias.symlink_to(root, target_is_directory=True)
            with patch.object(report, 'ROOT', root):
                for out in (root, root/'new-output', root/'nested'/'output', alias/'output'):
                    with self.subTest(out=out), self.assertRaisesRegex(ValueError, 'outside'):
                        report.run(out, False)
            self.assertEqual(list(root.iterdir()), [])

    def test_replay_rejects_self_consistent_relabelled_proof_formulas(self):
        # A producer regression emits a satisfiable safety/admission formula and
        # labels it SAT. Replaying against that self-declared expectation passed
        # before the independent per-obligation expectation map was introduced.
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            artifacts = []
            for identity, expected in report.ARITHMETIC_EXPECTATIONS.items():
                content = ('(assert false)' if expected == 'unsat' else '(assert true)').encode()
                name = identity + '.smt2'
                (directory/name).write_bytes(content)
                artifacts.append({'id': identity, 'file': name, 'expected': expected,
                                  'actual': expected, 'sha256': report.sha(content)})
            self.assertEqual(len(report.replay_arithmetic(directory, artifacts)), 10)
            for artifact in artifacts:
                if artifact['expected'] != 'unsat':
                    continue
                with self.subTest(identity=artifact['id']):
                    file = directory/artifact['file']
                    original = file.read_bytes()
                    content = b'(assert true)'
                    file.write_bytes(content)
                    forged = [dict(a) for a in artifacts]
                    entry = next(a for a in forged if a['id'] == artifact['id'])
                    entry.update(expected='sat', actual='sat', sha256=report.sha(content))
                    with self.assertRaisesRegex(ValueError, 'expectation mismatch'):
                        report.replay_arithmetic(directory, forged)
                    file.write_bytes(original)

    def test_replay_rejects_swapped_expectations_and_duplicate_obligations(self):
        artifacts = [{'id': identity, 'expected': expected}
                     for identity, expected in report.ARITHMETIC_EXPECTATIONS.items()]
        artifacts[0]['expected'], artifacts[2]['expected'] = (
            artifacts[2]['expected'], artifacts[0]['expected'])
        with self.assertRaisesRegex(ValueError, 'expectation mismatch'):
            report.replay_arithmetic(Path('/unused'), artifacts)
        artifacts[-1] = artifacts[0]
        with self.assertRaisesRegex(ValueError, 'inventory'):
            report.replay_arithmetic(Path('/unused'), artifacts)

    def test_narrow_proofs_cannot_discharge_critical_requirements(self):
        result = report.build_report(self.requirements, self.provenance, True, 'solver passed')
        self.assertTrue(all(r['status'] == 'UNPROVEN' for r in result['requirements']))
        self.assertTrue(all(o['status'] == 'UNPROVEN' for o in result['obligations']
                            if o['category'] in ('implementation', 'refinement')))
        self.assertEqual(result['metrics']['obligationsByCategory']['source-derived-SMT']['PROVEN'], 4)
        self.assertFalse(result['release']['executionAllowed'])
        self.assertEqual(result['release']['acceptedRisks'], [])
        self.assertEqual(set(result['release']['criticalUnprovenRequirements']),
                         {r['id'] for r in self.requirements if r.get('critical')})

    def test_legacy_pass_cannot_become_proof(self):
        for row in self.requirements:
            row['conformance']['classification'] = 'PASS'
        result = report.build_report(self.requirements, self.provenance, True, 'tests passed')
        self.assertEqual(result['metrics']['requirements']['PROVEN'], 0)
        self.assertEqual(result['release']['decision'], 'BLOCKED')

    def test_failed_unknown_timeout_or_stale_attempt_has_no_proven_claim(self):
        for reason in ('solver failed', 'unknown', 'timeout', 'source changed'):
            result = report.build_report(self.requirements, self.provenance, False, reason)
            self.assertTrue(all(o['status'] == 'UNPROVEN' for o in result['obligations']))

    def test_ci_keeps_report_attempt_after_an_earlier_failure(self):
        import yaml
        workflow = yaml.safe_load((ROOT/'.github/workflows/event-operations-formal.yml').read_text())
        steps = workflow['jobs']['specification-contracts']['steps']
        generation = next(s for s in steps if s.get('name') == 'Generate formal assurance and release decision')
        self.assertEqual(generation['if'], '${{ !cancelled() }}')
        self.assertTrue(generation['run'].startswith('python scripts/verification-report.py --output '))
        self.assertNotIn('test-verification-report.py', generation['run'])
        archive = next(s for s in steps if s.get('name') == 'Preserve formal assurance report')
        self.assertEqual(archive['if'], 'always()')
        testing = next(s for s in steps if s.get('name') == 'Test assurance reporting and counterexample retention')
        self.assertIn('python scripts/test-recovery-expiry-verifier.py', testing['run'])
        database = workflow['jobs']['postgres-foundation']['steps']
        self.assertTrue(any(s.get('run') == 'sh scripts/test-recovery-time-postgres.sh' for s in database))

    def test_unknown_status_is_rejected(self):
        with self.assertRaises(ValueError):
            report.count_status([{'status': 'PASS'}])

    def test_authentication_predicate_proofs_do_not_promote_endpoints_or_refinement(self):
        contract = json.loads((ROOT/'formal/system/auth-predicates.json').read_text())
        auth = {'executionPassed': True, 'obligations': [
            {**o, 'status': 'PROVEN', 'artifact': o['id'] + '.smt2'} for o in contract['obligations']]}
        result = report.build_report(self.requirements, self.provenance, True, 'passed', authorization=auth)
        self.assertEqual(result['metrics']['obligationsByCategory']['source-derived-authorization-SMT']['PROVEN'], 3)
        self.assertEqual(result['metrics']['requirements']['PROVEN'], 0)
        self.assertEqual(result['release']['decision'], 'BLOCKED')
        auth['executionPassed'] = False
        auth['obligations'][0]['status'] = 'REFUTED'
        result = report.build_report(self.requirements, self.provenance, True, 'auth failed', authorization=auth)
        counts = result['metrics']['obligationsByCategory']['source-derived-authorization-SMT']
        self.assertEqual((counts['PROVEN'], counts['REFUTED'], counts['UNPROVEN']), (0, 1, 2))

    def test_recovery_proofs_do_not_promote_recovery_transaction(self):
        contract = json.loads((ROOT/'formal/system/recovery-expiry-arithmetic.json').read_text())
        receipt = {'executionPassed': True, 'obligations': [
            {**o, 'status': 'PROVEN', 'artifact': o['id'] + '.smt2'} for o in contract['obligations']]}
        result = report.build_report(self.requirements, self.provenance, True, 'passed', recovery=receipt)
        self.assertEqual(result['metrics']['obligationsByCategory']['source-derived-recovery-SMT']['PROVEN'], 4)
        self.assertEqual(result['metrics']['requirements']['PROVEN'], 0)
        self.assertTrue(all(o['status'] == 'UNPROVEN' for o in result['obligations']
                            if o['category'] in ('implementation', 'refinement')))
        self.assertEqual(result['release']['decision'], 'BLOCKED')
        receipt['executionPassed'] = False
        receipt['obligations'][0]['status'] = 'REFUTED'
        result = report.build_report(self.requirements, self.provenance, True, 'failed', recovery=receipt)
        counts = result['metrics']['obligationsByCategory']['source-derived-recovery-SMT']
        self.assertEqual((counts['PROVEN'], counts['REFUTED'], counts['UNPROVEN']), (0, 1, 3))

    def test_real_counterexample_is_retained_and_separate_from_unknown(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root/'scripts').mkdir()
            script = root/'scripts/verify-payment-arithmetic.py'
            shutil.copyfile(ROOT/'scripts/verify-payment-arithmetic.py', script)
            source = root/'tdf-hq/src/TDF/Commerce/StateMachine.hs'
            source.parent.mkdir(parents=True)
            source.write_text((ROOT/'tdf-hq/src/TDF/Commerce/StateMachine.hs').read_text().replace(
                'amount > maximumCapture - paymentCapturedMinor lifecycle', 'newCaptured > maximumCapture'))
            target = root/'failed-proof'
            result = subprocess.run([sys.executable, str(script), '--output', str(target)],
                                    capture_output=True, timeout=180)
            self.assertEqual(result.returncode, 1)
            receipt = json.loads((target/'result.json').read_text())
            self.assertFalse(receipt['executionPassed'])
            claim = receipt['artifacts'][0]
            self.assertEqual((claim['id'], claim['expected'], claim['actual']), ('capture-safety', 'unsat', 'sat'))
            self.assertTrue(claim['model'])
            self.assertTrue((target/claim['file']).is_file())
            matrix = report.build_report(self.requirements, self.provenance, False, 'counterexample', ['capture-safety'])
            self.assertEqual(next(o['status'] for o in matrix['obligations'] if o['id'] == 'SYS-MONEY-001:safety'), 'REFUTED')
            self.assertEqual(matrix['metrics']['obligationsByCategory']['source-derived-SMT']['PROVEN'], 0)
            self.assertFalse(matrix['release']['executionAllowed'])

    def test_export_replays_all_claims_and_refuses_reuse(self):
        import z3
        with tempfile.TemporaryDirectory() as tmp:
            target = Path(tmp)/'arithmetic'
            command = [sys.executable, str(ROOT/'scripts/verify-payment-arithmetic.py'), '--output', str(target)]
            run = subprocess.run(command, capture_output=True, timeout=180)
            self.assertEqual(run.returncode, 0, run.stderr.decode())
            receipt = json.loads((target/'result.json').read_text())
            self.assertEqual(len(receipt['artifacts']), 10)
            self.assertEqual(len(report.replay_arithmetic(target, receipt['artifacts'])), 10)
            for artifact in receipt['artifacts']:
                data = (target/artifact['file']).read_bytes()
                self.assertEqual(report.sha(data), artifact['sha256'])
                solver = z3.Solver()
                solver.set(timeout=30_000)
                solver.from_string(data.decode())
                self.assertEqual(str(solver.check()), artifact['expected'])
            before = (target/'result.json').read_bytes()
            self.assertNotEqual(subprocess.run(command, capture_output=True, timeout=180).returncode, 0)
            self.assertEqual((target/'result.json').read_bytes(), before)


if __name__ == '__main__':
    unittest.main()
