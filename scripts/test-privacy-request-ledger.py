#!/usr/bin/env python3
import concurrent.futures
from contextlib import closing
import shutil
from datetime import datetime, timedelta, timezone
import importlib.util
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import unittest

SOURCE = Path(__file__).with_name('privacy-request-ledger.py')
spec = importlib.util.spec_from_file_location('privacy_ledger', SOURCE)
ledger = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ledger)


class LedgerTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix='tdf-privacy-synthetic-')
        self.root = Path(self.directory.name).resolve()
        os.chmod(self.root, 0o700)
        self.db = self.root / 'requests.sqlite'
        ledger.initialize(self.db)
        self.now = datetime(2026, 10, 5, tzinfo=timezone.utc)
        self.nonce = 0
        self.receipt = self.evidence('received_request')
        self.opening = {'action': 'open', 'receivedAt': ledger.stamp(self.now), 'channel': 'mobile', 'key': 'open-synthetic'}
        self.case = ledger.mutate(self.db, self.opening, self.receipt, self.now)

    def tearDown(self):
        self.directory.cleanup()

    def evidence(self, kind, case=None, surfaces=None):
        self.nonce += 1
        file = self.root / f'evidence-{self.nonce}.json'
        payload = {'kind': kind, 'caseId': case, 'artifacts': ['a' * 64]}
        if surfaces is not None:
            payload['surfaces'] = surfaces
        file.write_text(json.dumps(payload))
        os.chmod(file, 0o600)
        return file

    def surfaces(self, retained=False):
        result = {name: {'disposition': 'erase', 'proofHash': 'b' * 64} for name in ledger.POLICY['surfaces']}
        if retained:
            result['commerce_financial_audit'] = {'disposition': 'retain', 'proofHash': 'c' * 64,
                'reviewAt': ledger.stamp(self.now + timedelta(days=90))}
        return result

    def step(self, action, surfaces=None, key=None, version=None):
        version = self.case['version'] if version is None else version
        proof = self.evidence(ledger.POLICY['transitions'][action]['evidence'], self.case['caseId'], surfaces)
        request = {'action': action, 'caseId': self.case['caseId'], 'expectedVersion': version, 'key': key or f'key-{action}-{self.nonce}'}
        result = ledger.mutate(self.db, request, proof, self.now)
        self.case = result
        return request, proof, result

    def planned(self, retained=False):
        self.step('verify_identity')
        self.step('plan', self.surfaces(retained))

    def test_complete_requires_verified_scope_effects_and_notice(self):
        with self.assertRaises(ValueError): self.step('close')
        self.planned(retained=True)
        with self.assertRaises(ValueError): self.step('verify_effects', self.surfaces(True))
        self.step('start')
        with self.assertRaises(ValueError): self.step('close')
        self.step('verify_effects', self.surfaces(True))
        self.step('close')
        result = ledger.report(self.db, self.now)
        self.assertEqual(result['cases'][0]['state'], 'closed')
        self.assertEqual(result['cases'][0]['retainedSurfaces'], ['commerce_financial_audit'])
        self.assertFalse(result['attentionRequired'])
        # Closure does not hide the future duty to review retained evidence.
        result = ledger.report(self.db, self.now + timedelta(days=91))
        self.assertTrue(result['cases'][0]['retentionReviewOverdue'])
        self.assertTrue(result['attentionRequired'])

    def test_retention_review_can_discharge_or_extend_without_hiding_history(self):
        self.planned(True)
        self.step('start')
        self.step('verify_effects', self.surfaces(True))
        self.step('close')
        due = ledger.report(self.db, self.now)['cases'][0]['dueAt']
        self.now += timedelta(days=91)
        self.assertTrue(ledger.report(self.db, self.now)['attentionRequired'])
        retained = self.surfaces(True)
        self.step('review_retention', retained)
        self.assertFalse(ledger.report(self.db, self.now)['attentionRequired'])
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['dueAt'], due)
        self.assertFalse(ledger.report(self.db, self.now)['cases'][0]['completedLate'])
        changed = self.surfaces(); changed['profiles_contacts']['disposition'] = 'not_applicable'
        with self.assertRaises(ValueError): self.step('review_retention', changed)
        self.step('review_retention', self.surfaces())
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['retainedSurfaces'], [])

    def test_late_completion_remains_visible_after_closure(self):
        self.planned()
        self.step('start')
        self.now += timedelta(days=31)
        self.step('verify_effects', self.surfaces())
        self.step('close')
        self.assertTrue(ledger.report(self.db, self.now)['cases'][0]['completedLate'])

    def test_identity_wait_and_failure_do_not_restart_deadline(self):
        self.step('request_identity')
        self.now += timedelta(days=31)
        result = ledger.report(self.db, self.now)
        self.assertTrue(result['cases'][0]['overdue'])
        due = result['cases'][0]['dueAt']
        self.planned()
        self.step('start')
        self.step('fail')
        self.step('replan', self.surfaces())
        self.step('start')
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['dueAt'], due)
        self.assertTrue(ledger.report(self.db, self.now)['cases'][0]['overdue'])

    def test_replay_binding_and_stale_revision(self):
        repeat = ledger.mutate(self.db, self.opening, self.receipt, self.now)
        self.assertEqual(repeat['caseId'], self.case['caseId'])
        self.assertTrue(repeat['replayed'])
        with self.assertRaises(ValueError):
            ledger.mutate(self.db, dict(self.opening, channel='account'), self.receipt, self.now)
        request, proof, _ = self.step('verify_identity')
        self.assertTrue(ledger.mutate(self.db, request, proof, self.now)['replayed'])
        with self.assertRaises(ValueError): self.step('plan', self.surfaces(), version=0)
        changed_proof = self.evidence('identity_verification', self.case['caseId'])
        changed_proof.write_text(changed_proof.read_text().replace('a' * 64, 'c' * 64))
        with self.assertRaises(ValueError): ledger.mutate(self.db, request, changed_proof, self.now)

    def test_eight_concurrent_writers_have_one_winner(self):
        proof = self.evidence('identity_verification', self.case['caseId'])
        def attempt(number):
            try:
                return ledger.mutate(self.db, {'action': 'verify_identity', 'caseId': self.case['caseId'],
                    'expectedVersion': 0, 'key': f'parallel-{number}'}, proof, self.now)
            except ValueError:
                return None
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            results = list(pool.map(attempt, range(8)))
        self.assertEqual(sum(result is not None for result in results), 1)
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['version'], 1)

    def test_concurrent_identical_retry_is_one_event(self):
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            results = list(pool.map(lambda _: ledger.mutate(self.db, self.opening, self.receipt, self.now), range(8)))
        self.assertEqual({result['caseId'] for result in results}, {self.case['caseId']})
        self.assertTrue(all(result['replayed'] for result in results))
        with closing(sqlite3.connect(self.db)) as db:
            self.assertEqual(db.execute('SELECT count(*) FROM event').fetchone()[0], 1)

    def test_partial_plan_and_effect_scope_change_reject_without_write(self):
        self.step('verify_identity')
        incomplete = self.surfaces(); incomplete.pop('backups')
        with self.assertRaises(ValueError): self.step('plan', incomplete)
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['version'], 1)
        self.step('plan', self.surfaces())
        self.step('start')
        with self.assertRaises(ValueError): self.step('verify_effects', self.surfaces(True))
        self.assertEqual(ledger.report(self.db, self.now)['cases'][0]['state'], 'executing')

    def test_evidence_case_kind_and_retention_deadline_reject(self):
        proof = self.evidence('identity_verification', '00000000-0000-4000-8000-000000000000')
        with self.assertRaises(ValueError): ledger.mutate(self.db, {'action': 'verify_identity', 'caseId': self.case['caseId'], 'expectedVersion': 0, 'key': 'wrong-case'}, proof, self.now)
        self.step('verify_identity')
        bad = self.surfaces(True); bad['commerce_financial_audit']['reviewAt'] = ledger.stamp(self.now)
        with self.assertRaises(ValueError): self.step('plan', bad)
        with self.assertRaises(ValueError): ledger.mutate(self.db, self.opening, proof, self.now)

    def test_private_storage_symlink_and_reinitialization_controls(self):
        with self.assertRaises(FileExistsError): ledger.initialize(self.db)
        os.chmod(self.db, 0o644)
        with self.assertRaises(ValueError): ledger.report(self.db, self.now)
        os.chmod(self.db, 0o600)
        link = self.root / 'link.sqlite'; link.symlink_to(self.db)
        with self.assertRaises(ValueError): ledger.report(link, self.now)
        os.chmod(self.root, 0o755)
        with self.assertRaises(ValueError): ledger.report(self.db, self.now)
        os.chmod(self.root, 0o700)

    def test_tamper_and_policy_drift_are_detected(self):
        with closing(sqlite3.connect(self.db)) as db:
            with self.assertRaises(sqlite3.IntegrityError): db.execute("UPDATE event SET payload='{}'")
            db.execute('DROP TRIGGER no_event_update')
            db.execute("UPDATE event SET payload='{}'")
            db.commit()
        with self.assertRaises(ValueError): ledger.report(self.db, self.now)

    def test_policy_changes_require_reviewed_migration(self):
        with closing(sqlite3.connect(self.db)) as db:
            db.execute("UPDATE metadata SET policy_hash=?", ('0' * 64,))
            db.commit()
        with self.assertRaises(ValueError): ledger.report(self.db, self.now)

    def test_isolated_restore_preserves_state_chain_and_retry_binding(self):
        self.planned()
        destination = self.root / 'restored.sqlite'
        # A real SQLite backup snapshot, not a concurrent raw file copy.
        with closing(sqlite3.connect(self.db)) as original, closing(sqlite3.connect(destination)) as restored:
            original.backup(restored)
        os.chmod(destination, 0o600)
        self.assertEqual(ledger.report(destination, self.now), ledger.report(self.db, self.now))
        self.assertTrue(ledger.mutate(destination, self.opening, self.receipt, self.now)['replayed'])
        with closing(sqlite3.connect(destination)) as db:
            self.assertEqual(db.execute('SELECT count(*) FROM event').fetchone()[0], 3)

    def test_operational_report_exit_code_flags_overdue_without_customer_details(self):
        old = self.evidence('received_request')
        opened = ledger.mutate(self.db, dict(self.opening, receivedAt='2020-01-01T00:00:00Z', key='old-request'), old, self.now)
        process = subprocess.run([sys.executable, str(SOURCE), '--ledger', str(self.db), 'report'], text=True, capture_output=True)
        self.assertEqual(process.returncode, 2)
        parsed = json.loads(process.stdout)
        self.assertTrue(parsed['attentionRequired'])
        self.assertIn(opened['caseId'], process.stdout)
        self.assertNotIn('evidence-', process.stdout)
        self.assertNotIn(str(self.root), process.stdout)


if __name__ == '__main__':
    unittest.main()
