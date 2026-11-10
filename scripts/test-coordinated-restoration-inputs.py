#!/usr/bin/env python3
"""Real journals/files with a crypto seam; Linux adds actual age and tree replay."""
import copy
import importlib.util
import inspect
import os
from pathlib import Path
from types import SimpleNamespace
import textwrap
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('inputs_capture_fixture', ROOT/'scripts/test-coordinated-capture.py')
t = importlib.util.module_from_spec(spec)
spec.loader.exec_module(t)
m = t.c.load('restoration_inputs_fixture', 'coordinated-restoration-inputs.py')


class StopBeforeIsolateCompletion(Exception):
    pass


class InputsTests(unittest.TestCase):
    def setUp(self):
        self.fixture = t.CaptureTests()
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        self.capture, _, self.observed = self.fixture.fixture()
        self.root = self.fixture.root
        self.journal = self.capture.fence.journal
        self.original_plain = b'synthetic plaintext, not an actual encrypted archive'
        self.cipher = b'synthetic ciphertext, crypto is mocked in portable controls'
        self.capture.output.write_bytes(self.original_plain)
        self.capture.output.chmod(0o600)
        plain = m.bundle.archive_digest(self.capture.output)
        self.encrypted = SimpleNamespace(capture=self.capture,
            output=self.capture.clone.directory/'coordinated-encrypted.age',
            receipt_path=self.capture.clone.directory/'coordinated-encryption-receipt.json')
        self.retrieved = SimpleNamespace(encrypted=self.encrypted,
            output=self.capture.clone.directory/'coordinated-retrieved.age',
            receipt_path=self.capture.clone.directory/'coordinated-retrieval-receipt.json')
        self.encrypted.output.write_bytes(self.cipher)
        self.retrieved.output.write_bytes(self.cipher)
        for path in (self.encrypted.output, self.retrieved.output): path.chmod(0o600)
        self.cipher_digest = m.bundle.archive_digest(self.retrieved.output)
        self.capture_record = self.record('capture', self.capture.receipt_path,
            {'bundle': {'binding': t.BINDING, 'archive': plain},
             'encryptionVerified': False, 'offHostVerified': False, 'databaseRecoveryVerified': False})
        self.encryption_record = self.record('encrypt', self.encrypted.receipt_path,
            {'captureReceiptSha256': m.digest(self.capture_record), 'envelope': {
                'plaintext': plain, 'ciphertext': self.cipher_digest,
                'recipientSha256': t.PLAN['recipientHash'], 'toolSha256': 'b'*64}})
        self.record('retrieve-off-host', self.retrieved.receipt_path,
            {'encryptionReceiptSha256': m.digest(self.encryption_record), 'transfer': {
                'status': 'ciphertext-round-trip-verified',
                **m.retrieval.transfer.binding(t.BINDING['releaseNonce'], self.cipher_digest),
                'decrypted': False, 'restored': False}})
        self.operation = m.Inputs(self.retrieved, self.root/'synthetic-age', self.root/'synthetic-key')
        self.decrypt = self.fixture.stack.enter_context(patch.object(m.envelope, 'decrypt', side_effect=self.fake_decrypt))

    def record(self, stage, path, value):
        saved = {}
        def effect(context):
            saved.update({'schemaVersion': 1, 'context': context, **copy.deepcopy(value)})
            m.bundle.write_index(path, saved)
            return {**context, 'evidenceHash': m.digest(saved)}
        self.journal.perform(stage, 'a'*64, effect)
        return saved

    def fake_decrypt(self, binary, source, destination, identity, expected):
        self.assertEqual(source, self.retrieved.output)
        self.assertNotEqual(source, self.encrypted.output)
        self.assertEqual(m.bundle.archive_digest(source), expected['ciphertext'])
        with destination.open('xb') as out: out.write(self.original_plain)
        destination.chmod(0o600)
        return {'schemaVersion': 1, 'status': 'decrypted-content-verified',
                'plaintext': m.bundle.archive_digest(destination), 'ciphertext': expected['ciphertext'],
                'toolSha256': 'b'*64, 'databaseRestored': False, 'filesRestored': False, 'offHost': False}

    def inside_intent(self, action=None, *, targets=None):
        targets = self.operation.targets() if targets is None else targets
        def effect(context):
            self.context = context
            if action is not None: return action(context)
            self.result = self.operation.prepare(context)
            raise StopBeforeIsolateCompletion()
        return self.journal.perform('restore-isolate', targets, effect)

    def assert_pending(self):
        status = self.journal.status()
        self.assertEqual(status['pendingStage'], 'restore-isolate')
        self.assertEqual(status['completedStages'][-1], 'retrieve-off-host')
        self.assertFalse(status['newWritesPossible'])
        self.assertFalse(status['sequenceComplete'])

    def test_verified_inputs_survive_loss_of_original_copies_without_completing_stage(self):
        self.capture.output.unlink()
        self.encrypted.output.unlink()
        with self.assertRaises(StopBeforeIsolateCompletion): self.inside_intent()
        self.assertEqual(self.operation.output.read_bytes(), self.original_plain)
        self.assertEqual(self.result['captureBundle'], self.capture_record['bundle'])
        self.assertEqual(self.result['binding'], t.BINDING)
        for key in ('keyCustodyVerified', 'independentStorageVerifiedByHelper',
                    'databaseRecoveryVerified', 'applicationRecoveryVerified', 'isolateStageComplete'):
            self.assertIs(self.result[key], False)
        self.assertEqual(self.operation.receipt_path.stat().st_mode & 0o777, 0o600)
        self.assert_pending()
        with self.assertRaises(ValueError): self.operation.prepare(self.context)

    def test_plaintext_result_is_not_a_journal_observation(self):
        with self.assertRaises(ValueError): self.inside_intent(self.operation.prepare)
        self.assertTrue(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_missing_intent_and_none_context_reject_before_decryption(self):
        for context in ({}, None):
            operation = m.Inputs(self.retrieved, None, None)
            with self.assertRaises(ValueError): operation.prepare(context)
        self.decrypt.assert_not_called()
        self.assertFalse(self.operation.output.exists())

    def test_wrong_intent_targets_reject_before_decryption(self):
        with self.assertRaises(ValueError): self.inside_intent(targets='c'*64)
        self.decrypt.assert_not_called()
        self.assert_pending()

    def test_mismatched_operation_context_rejects(self):
        def changed(context):
            return self.operation.prepare({**context, 'operationId': '0'*64})
        with self.assertRaises(ValueError): self.inside_intent(changed)
        self.decrypt.assert_not_called()
        self.assert_pending()

    def test_corrupt_retrieved_bytes_reject_before_intent(self):
        self.retrieved.output.write_bytes(b'changed')
        with self.assertRaises(ValueError): self.operation.targets()
        self.decrypt.assert_not_called()
        self.assertIsNone(self.journal.status()['pendingStage'])

    def test_backup_mount_alias_rejects_before_decryption(self):
        with patch.object(m.physical, 'verify_mounts', side_effect=ValueError('synthetic backup mount alias')):
            with self.assertRaisesRegex(ValueError, 'backup mount alias'): self.operation.targets()
        self.decrypt.assert_not_called()
        self.assertFalse(self.operation.output.exists())

    def test_original_ciphertext_path_cannot_replace_retrieved_artifact(self):
        self.retrieved.output = self.encrypted.output
        with self.assertRaises(ValueError): self.operation.targets()
        self.decrypt.assert_not_called()

    def test_receipt_tampering_rejects_before_intent(self):
        self.encrypted.receipt_path.write_bytes(b'{}\n')
        with self.assertRaises(ValueError): self.operation.targets()
        self.decrypt.assert_not_called()

    def test_broken_retrieval_chain_rejects_before_intent(self):
        # A real journal observation can bind a semantically wrong receipt.
        # Rebuild only this synthetic fixture's final stage, without rewriting
        # any existing record: construct the wrong chain from the start below.
        self.rewrite_retrieval_fixture(encryptionReceiptSha256='0'*64)
        with self.assertRaises(ValueError): self.operation.targets()
        self.decrypt.assert_not_called()

    def rewrite_retrieval_fixture(self, **changes):
        # Fresh journal, same plan and real receipt publication. The original
        # complete journal stays intact; no retry or history rewrite is used.
        journal = self.fixture.stack.enter_context(t.journal.open_journal(
            str(self.fixture.directory('alternate-journal'))))
        journal.initialize(t.PLAN, t.BINDING['releaseNonce'])
        prior = self.journal.records()
        for stage in t.journal.STAGES[:5]:
            index = 1+2*t.journal.STAGES.index(stage)
            evidence = prior[index+1]['event']['evidenceHash']
            journal.perform(stage, 'a'*64, lambda context, evidence=evidence: {**context, 'evidenceHash': evidence})
        # Earlier receipt contexts have the same nonce/plan/targets/operation IDs.
        self.journal = self.capture.fence.journal = journal
        self.retrieved.receipt_path = self.capture.clone.directory/'alternate-retrieval-receipt.json'
        transfer = {'status': 'ciphertext-round-trip-verified',
                    **m.retrieval.transfer.binding(t.BINDING['releaseNonce'], self.cipher_digest),
                    'decrypted': False, 'restored': False}
        self.record('retrieve-off-host', self.retrieved.receipt_path,
            {'encryptionReceiptSha256': m.digest(self.encryption_record), 'transfer': transfer, **changes})

    def test_boolean_lookalike_transfer_receipt_is_rejected(self):
        transfer = {'status': 'ciphertext-round-trip-verified',
                    **m.retrieval.transfer.binding(t.BINDING['releaseNonce'], self.cipher_digest),
                    'decrypted': 0, 'restored': False}
        self.rewrite_retrieval_fixture(transfer=transfer)
        with self.assertRaises(ValueError): self.operation.targets()

    def test_late_writer_rejects_and_retains_private_partial_plaintext(self):
        original = self.fake_decrypt
        def changed(*args):
            result = original(*args)
            self.observed['sources']['dockerWritersStopped'] = False
            return result
        self.decrypt.side_effect = changed
        with self.assertRaises(ValueError): self.inside_intent()
        self.assertTrue(self.operation.output.exists())
        self.assertFalse(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_substituted_plaintext_is_rejected_even_with_reported_digest(self):
        original = self.fake_decrypt
        def changed(*args):
            result = original(*args)
            self.operation.output.write_bytes(b'different plaintext')
            return result
        self.decrypt.side_effect = changed
        with self.assertRaises(ValueError): self.inside_intent()
        self.assertFalse(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_receipt_sync_failure_preserves_outputs_and_pending_intent(self):
        write = m.bundle.write_index
        def fail(path, value):
            write(path, value)
            raise OSError('synthetic replay-input receipt sync failure')
        with patch.object(m.bundle, 'write_index', side_effect=fail):
            with self.assertRaisesRegex(OSError, 'receipt sync'): self.inside_intent()
        self.assertTrue(self.operation.output.exists())
        self.assertTrue(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_change_after_intent_is_not_admitted(self):
        targets = self.operation.targets()
        def changed(context):
            self.retrieved.output.write_bytes(b'late changed ciphertext')
            return self.operation.prepare(context)
        with self.assertRaises(ValueError): self.inside_intent(changed, targets=targets)
        self.decrypt.assert_not_called()
        self.assert_pending()

    def test_receipt_change_during_decrypt_is_rejected(self):
        original = self.fake_decrypt
        def changed(*args):
            result = original(*args)
            self.retrieved.receipt_path.write_bytes(b'{}\n')
            return result
        self.decrypt.side_effect = changed
        with self.assertRaises(ValueError): self.inside_intent()
        self.assertFalse(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_existing_plaintext_is_not_adopted_or_overwritten(self):
        self.operation.output.write_bytes(b'retained uncertain output')
        self.operation.output.chmod(0o600)
        with self.assertRaises(FileExistsError): self.inside_intent()
        self.assertEqual(self.operation.output.read_bytes(), b'retained uncertain output')
        self.assertFalse(self.operation.receipt_path.exists())
        self.assert_pending()

    def test_unbound_output_and_inherited_owner_are_rejected(self):
        self.operation.output = self.root/'wrong-output'
        with self.assertRaises(ValueError): self.operation.targets()
        self.operation.output = self.capture.clone.directory/'coordinated-decrypted.tar'
        self.operation.owner = -1
        with self.assertRaises(ValueError): self.operation.targets()
        self.decrypt.assert_not_called()

    def test_chain_guard_mutation_is_detected(self):
        source = textwrap.dedent(inspect.getsource(m.Inputs._records))
        guard = "require(retrieved['encryptionReceiptSha256'] == envelope_hash)"
        self.assertEqual(source.count(guard), 1)
        namespace = dict(m.__dict__)
        exec(compile(source.replace(guard, 'require(True)'), '<retrieval-chain-mutation>', 'exec'), namespace)
        result = unittest.TestResult()
        with patch.object(m.Inputs, '_records', namespace['_records']):
            InputsTests('test_broken_retrieval_chain_rejects_before_intent').run(result)
        self.assertEqual(result.testsRun, 1)
        self.assertEqual(result.errors, [])
        self.assertEqual(len(result.failures), 1)
        self.assertIn('ValueError not raised', result.failures[0][1])


@unittest.skipUnless(os.geteuid() == 0 and m.envelope.platform.system() == 'Linux',
                     'Actual capture/age/UID1000 composition requires owned Linux root')
class ActualAgeTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location('inputs_actual_retrieval', ROOT/'scripts/test-coordinated-retrieval.py')
        self.real = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.real)
        self.fixture = self.real.RetrievalTests()
        self.fixture.setUp()
        self.addCleanup(self.fixture.doCleanups)
        self.fixture.round_trip()
        self.capture = self.fixture.capture
        self.operation = m.Inputs(self.fixture.operation, self.real.t.TOOLS/'age', self.fixture.key)
        self.journal = self.capture.fence.journal

    def prepare(self):
        def effect(context):
            self.result = self.operation.prepare(context)
            raise StopBeforeIsolateCompletion()
        return self.journal.perform('restore-isolate', self.operation.targets(), effect)

    def test_real_retrieved_age_replays_six_trees_after_original_copies_are_lost(self):
        expected = self.capture.output.read_bytes()
        self.capture.output.unlink()
        self.fixture.encrypted.output.unlink()
        with self.assertRaises(StopBeforeIsolateCompletion): self.prepare()
        self.assertEqual(self.operation.output.read_bytes(), expected)
        replay = self.fixture.fixture.root/'replayed-from-retrieval'
        result = m.bundle.restore(str(self.operation.output), self.result['captureBundle'],
                                  self.result['binding'], str(replay))
        self.assertEqual(set(result['components']), set(m.bundle.ROLES))
        self.assertEqual((replay/'database/sentinel').read_bytes(), b'synthetic stopped database')
        self.assertEqual((replay/'legacy-uploads').stat().st_uid, 1000)
        for name in ('isolateStageComplete', 'databaseRecoveryVerified', 'applicationRecoveryVerified',
                     'keyCustodyVerified', 'independentStorageVerifiedByHelper'):
            self.assertIs(self.result[name], False)
        self.assertEqual(self.journal.status()['pendingStage'], 'restore-isolate')
        self.assertFalse(result['databaseRecoveryVerified'])

    def test_real_wrong_identity_never_publishes_verified_replay_inputs(self):
        import subprocess
        wrong = self.fixture.fixture.root/'wrong-synthetic-identity'
        subprocess.run([str(self.real.t.TOOLS/'age-keygen'), '-o', str(wrong)], check=True,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.operation.identity = wrong
        with self.assertRaises(ValueError): self.prepare()
        self.assertFalse(self.operation.receipt_path.exists())
        self.assertEqual(self.journal.status()['pendingStage'], 'restore-isolate')

    def test_real_plaintext_alone_cannot_complete_isolate_stage(self):
        with self.assertRaises(ValueError):
            self.journal.perform('restore-isolate', self.operation.targets(), self.operation.prepare)
        self.assertTrue(self.operation.receipt_path.exists())
        self.assertEqual(self.journal.status()['pendingStage'], 'restore-isolate')
        self.assertFalse(self.journal.status()['newWritesPossible'])


if __name__ == '__main__':
    if os.environ.get('TDF_REQUIRE_ACTUAL_RECOVERY_INPUTS') == '1':
        if os.geteuid() != 0 or m.envelope.platform.system() != 'Linux':
            raise SystemExit('Actual recovery-input controls require Linux root; skips cannot qualify this lane')
    unittest.main()
