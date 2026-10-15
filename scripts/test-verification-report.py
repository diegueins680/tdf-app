#!/usr/bin/env python3
"""Regression controls for assurance-category separation and replay artifacts."""
from contextlib import nullcontext
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

    def synthetic_receipt(self, name, directory):
        suite = report.SUITES[name]
        directory.mkdir()
        contract = json.loads((ROOT/suite['contract']).read_text())
        files = {p: report.sha((ROOT/p).read_bytes()) for p in set(contract['reviewedSources']) | {
            suite['contract'], suite['script'], 'formal/system/requirements.json'}}
        provenance = {**self.provenance, 'files': files}
        source = {**provenance, 'files': dict(files),
                  'digest': report.sha(json.dumps(files, sort_keys=True).encode())}
        artifacts = []
        for identity, expected in suite['expectations'].items():
            content = b'(assert false)' if expected == 'unsat' else b'(assert true)'
            (directory/(identity + '.smt2')).write_bytes(content)
            artifacts.append({'id': identity, 'file': identity + '.smt2',
                              'expected': expected, 'actual': expected,
                              'sha256': report.sha(content)})
        rows = lambda ids: [{'id': i, 'status': 'PROVEN', 'artifact': i + '.smt2'} for i in ids]
        return {'executionPassed': True, 'provenance': source, 'artifacts': artifacts,
                'obligations': rows(suite['claims']), 'supportingLemmas': rows(suite['lemmas'])}, provenance

    def test_authorization_and_recovery_require_consumer_admission(self):
        for name in report.SUITES:
            with tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)/name
                receipt, provenance = self.synthetic_receipt(name, directory)
                # A producer's PROVEN text alone does not satisfy the replay gate.
                result = report.build_report(self.requirements, provenance, False, 'producer only',
                                             **{name: receipt})
                self.assertEqual(sum(o['status'] == 'PROVEN' for o in result['obligations']), 0)
                admitted = report.admit_receipt(name, directory, receipt, provenance, 0)
                self.assertTrue(admitted['executionPassed'])
                self.assertTrue(admitted['replayPassed'])
                replay = json.loads((Path(tmp)/(name + '-replay.json')).read_text())
                self.assertEqual(len(replay), len(report.SUITES[name]['expectations']))
                self.assertTrue(all(r['expected'] == r['result'] for r in replay))

    def test_relabelled_authorization_and_recovery_claims_are_rejected(self):
        for name, suite in report.SUITES.items():
            for identity, expected in suite['expectations'].items():
                if expected != 'unsat':
                    continue
                with self.subTest(suite=name, obligation=identity), tempfile.TemporaryDirectory() as tmp:
                    directory = Path(tmp)/name
                    receipt, provenance = self.synthetic_receipt(name, directory)
                    content = b'(assert true)'
                    (directory/(identity + '.smt2')).write_bytes(content)
                    row = next(a for a in receipt['artifacts'] if a['id'] == identity)
                    row.update(expected='sat', actual='sat', sha256=report.sha(content))
                    with self.assertRaisesRegex(ValueError, 'expectation mismatch'):
                        report.admit_receipt(name, directory, receipt, provenance, 0)

    def test_failed_witness_unknown_and_failed_producer_never_promote_claims(self):
        import z3
        for name, suite in report.SUITES.items():
            for mode in ('witness', 'unknown', 'failed-exit', 'string-boolean', 'false-refutation', 'producer-error'):
                with self.subTest(suite=name, mode=mode), tempfile.TemporaryDirectory() as tmp:
                    directory = Path(tmp)/name
                    receipt, provenance = self.synthetic_receipt(name, directory)
                    if mode == 'witness':
                        identity = next(i for i, expected in suite['expectations'].items() if expected == 'sat')
                        content = b'(assert false)'
                        (directory/(identity + '.smt2')).write_bytes(content)
                        next(a for a in receipt['artifacts'] if a['id'] == identity)['sha256'] = report.sha(content)
                    if mode == 'string-boolean':
                        receipt['executionPassed'] = 'true'
                    if mode == 'producer-error':
                        receipt['error'] = 'Producer reported a failure despite its success flag'
                    if mode == 'false-refutation':
                        receipt['obligations'][0]['status'] = 'REFUTED'
                    with patch.object(z3.Solver, 'check', return_value=z3.unknown) if mode == 'unknown' else nullcontext():
                        admitted = report.admit_receipt(name, directory, receipt, provenance,
                                                        1 if mode == 'failed-exit' else 0)
                    self.assertFalse(admitted['executionPassed'])
                    self.assertTrue(all(r['status'] == 'UNPROVEN' for r in
                                        admitted['obligations'] + admitted['supportingLemmas']))

    def test_receipt_sources_and_artifact_inventory_fail_closed(self):
        import copy
        for name in report.SUITES:
            for mode in ('revision', 'dirty', 'missing-source', 'source-hash', 'extra-source',
                         'digest', 'hash', 'missing', 'duplicate', 'path', 'symlink'):
                with self.subTest(suite=name, mode=mode), tempfile.TemporaryDirectory() as tmp:
                    directory = Path(tmp)/name
                    receipt, provenance = self.synthetic_receipt(name, directory)
                    if mode == 'revision': receipt['provenance']['revision'] = 'wrong'
                    if mode == 'dirty': receipt['provenance']['worktreeDirty'] = True
                    if mode == 'missing-source': receipt['provenance']['files'] = {}
                    if mode == 'source-hash':
                        first = next(iter(receipt['provenance']['files']))
                        receipt['provenance']['files'][first] = 'wrong'
                    if mode == 'extra-source': receipt['provenance']['files']['unknown-source'] = None
                    if mode in ('missing-source', 'source-hash', 'extra-source'):
                        receipt['provenance']['digest'] = report.sha(
                            json.dumps(receipt['provenance']['files'], sort_keys=True).encode())
                    if mode == 'digest': receipt['provenance']['digest'] = 'wrong'
                    if mode == 'hash': receipt['artifacts'][0]['sha256'] = 'wrong'
                    if mode == 'missing': receipt['artifacts'].pop()
                    if mode == 'duplicate': receipt['artifacts'][-1] = copy.deepcopy(receipt['artifacts'][0])
                    if mode == 'path': receipt['artifacts'][0]['file'] = '../outside.smt2'
                    if mode == 'symlink':
                        target = directory/receipt['artifacts'][0]['file']
                        outside = Path(tmp)/'outside.smt2'
                        outside.write_bytes(target.read_bytes())
                        target.unlink()
                        target.symlink_to(outside)
                    with self.assertRaises(ValueError):
                        report.admit_receipt(name, directory, receipt, provenance, 0)

    def test_producer_false_pass_is_refuted_by_report_replay(self):
        # Preserve a SAT negated contract even when producer exit/status/hash text
        # are self-consistent and claim success. All other suites stay separate.
        original = report.subprocess.run
        def producer_fault(command, **kwargs):
            result = original(command, **kwargs)
            if 'scripts/verify-auth-predicates.py' in command:
                directory = Path(command[command.index('--output') + 1])
                receipt = json.loads((directory/'result.json').read_text())
                content = '(assert true)\n(check-sat)\n'
                (directory/'strict-admin.smt2').write_text(content)
                artifact = next(a for a in receipt['artifacts'] if a['id'] == 'strict-admin')
                artifact['sha256'] = report.sha(content.encode())
                (directory/'result.json').write_text(json.dumps(receipt))
            return result
        with tempfile.TemporaryDirectory() as tmp, patch.object(report.subprocess, 'run', producer_fault):
            output = Path(tmp)/'report'
            self.assertEqual(report.run(output, False), 1)
            result = json.loads((output/'report.json').read_text())
            categories = result['metrics']['obligationsByCategory']
            self.assertEqual(categories['source-derived-authorization-SMT'],
                             {'PROVEN': 0, 'REFUTED': 1, 'UNPROVEN': 2, 'NOT_APPLICABLE': 0})
            self.assertEqual(categories['source-derived-SMT']['PROVEN'], 4)
            self.assertEqual(categories['source-derived-recovery-SMT']['PROVEN'], 4)
            self.assertTrue(all(row['status'] == 'UNPROVEN' for row in result['supportingAlgebraLemmas']))
            self.assertFalse(result['execution']['passed'])
            self.assertFalse(result['release']['executionAllowed'])
            replay = json.loads((output/'authorization-replay.json').read_text())
            self.assertEqual(next(r['result'] for r in replay if r['id'] == 'strict-admin'), 'sat')

    def test_producer_false_refutation_requires_fresh_sat_counterexample(self):
        original = report.subprocess.run
        def producer_fault(command, **kwargs):
            result = original(command, **kwargs)
            if 'scripts/verify-payment-arithmetic.py' in command:
                directory = Path(command[command.index('--output') + 1])
                receipt = json.loads((directory/'result.json').read_text())
                receipt['executionPassed'] = False
                next(a for a in receipt['artifacts'] if a['id'] == 'capture-safety')['actual'] = 'sat'
                (directory/'result.json').write_text(json.dumps(receipt))
                return subprocess.CompletedProcess(command, 1)
            return result
        with tempfile.TemporaryDirectory() as tmp, patch.object(report.subprocess, 'run', producer_fault):
            output = Path(tmp)/'report'
            self.assertEqual(report.run(output, False), 1)
            result = json.loads((output/'report.json').read_text())
            self.assertEqual(result['metrics']['obligationsByCategory']['source-derived-SMT'],
                             {'PROVEN': 0, 'REFUTED': 0, 'UNPROVEN': 4, 'NOT_APPLICABLE': 0})
            replay = json.loads((output/'arithmetic-counterexample-replay.json').read_text())
            self.assertEqual(next(r['result'] for r in replay if r['id'] == 'capture-safety'), 'unsat')

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

    def test_unreadable_sources_preserve_an_unproven_report_without_running_solvers(self):
        errors = [FileNotFoundError('mapped Mobile source missing'),
                  PermissionError('mapped source unreadable'),
                  subprocess.CalledProcessError(128, ['git', 'rev-parse', 'HEAD'])]
        for error in errors:
            with self.subTest(error=error), tempfile.TemporaryDirectory() as tmp:
                out = Path(tmp)/'report'
                with patch.object(report, 'fingerprint', side_effect=error), \
                     patch.object(report.subprocess, 'run') as execute:
                    self.assertEqual(report.run(out, True), 1)
                execute.assert_not_called()
                saved = json.loads((out/'report.json').read_text())
                self.assertFalse(saved['execution']['passed'])
                self.assertIsNone(saved['provenance']['revision'])
                self.assertIsNone(saved['provenance']['digest'])
                self.assertEqual(saved['release']['decision'], 'BLOCKED')
                self.assertTrue(all(o['status'] == 'UNPROVEN' for o in saved['obligations']))
                self.assertTrue((out/'report.md').is_file())

    def test_source_loss_after_execution_invalidates_claims_and_keeps_report(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp)/'report'
            with patch.object(report, 'fingerprint', side_effect=[
                    self.provenance, FileNotFoundError('source disappeared')]), \
                 patch.object(report.subprocess, 'run', return_value=subprocess.CompletedProcess([], 1)):
                self.assertEqual(report.run(out, True), 1)
            saved = json.loads((out/'report.json').read_text())
            self.assertFalse(saved['execution']['passed'])
            self.assertIn('unreadable', saved['execution']['reason'])
            self.assertTrue(all(o['status'] == 'UNPROVEN' for o in saved['obligations']))

    def test_missing_requirement_register_does_not_report_zero_obligations(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)/'source'
            root.mkdir()
            out = Path(tmp)/'report'
            with patch.object(report, 'ROOT', root), \
                 patch.object(report.subprocess, 'run') as execute:
                self.assertEqual(report.run(out, True), 1)
            execute.assert_not_called()
            saved = json.loads((out/'report.json').read_text())
            self.assertFalse(saved['metrics']['requirementInventoryAvailable'])
            self.assertTrue(all(n is None for n in saved['metrics']['requirements'].values()))
            self.assertIsNone(saved['release']['criticalUnprovenRequirements'])
            self.assertFalse(saved['release']['executionAllowed'])
            self.assertTrue((out/'report.md').is_file())

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
        auth = {'executionPassed': True, 'replayPassed': True, 'obligations': [
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
        receipt = {'executionPassed': True, 'replayPassed': True, 'obligations': [
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
            replay = report.replay_smt(target, receipt['artifacts'], report.ARITHMETIC_EXPECTATIONS,
                                       complete=False)
            refuted = [row['id'] for row in replay if row['expected'] == 'unsat' and row['result'] == 'sat']
            self.assertIn('capture-safety', refuted)
            matrix = report.build_report(self.requirements, self.provenance, False, 'counterexample', refuted)
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
