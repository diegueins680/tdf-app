#!/usr/bin/env python3
"""Real recorded capture and age composition on an owned Linux root fixture."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('encryption_capture_fixture',ROOT/'scripts/test-coordinated-capture.py')
t=importlib.util.module_from_spec(spec);spec.loader.exec_module(t)
e=t.c.load('coordinated_encryption_fixture','coordinated-encryption.py')
TOOLS=Path(os.environ.get('TDF_RECOVERY_TOOLS','/missing-pinned-recovery-tools'))


# Avoid accidentally counting inherited capture cases as additional coverage.
class EncryptionTests(unittest.TestCase):
    def setUp(self):
        self.fixture=t.CaptureTests();self.fixture.setUp();self.addCleanup(self.fixture.doCleanups)
        self.root=self.fixture.root
    def test_unobserved_or_changed_receipt_is_not_evidence(self):
        capture,_,_=self.fixture.fixture()
        fake=capture.clone.directory/'receipt.json'
        t.c.bundle.write_index(fake,{'schemaVersion':1,'context':{}})
        with self.assertRaises(ValueError):e.recorded_receipt(capture.fence.journal,'capture',fake)
        context={}
        def effect(value):
            context.update(value)
            return {**value,'evidenceHash':hashlib.sha256(fake.read_bytes()).hexdigest()}
        capture.fence.journal.perform('capture','1'*64,effect)
        with self.assertRaises(ValueError):e.recorded_receipt(capture.fence.journal,'capture',fake)
        fake.write_bytes(b'changed')
        with self.assertRaises(ValueError):e.recorded_receipt(capture.fence.journal,'capture',fake)
    def prepare(self):
        key=self.root/'synthetic-identity'
        subprocess.run([str(TOOLS/'age-keygen'),'-o',str(key)],check=True,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
        recipient=subprocess.check_output([str(TOOLS/'age-keygen'),'-y',str(key)],text=True).strip()
        with patch.dict(t.PLAN,{'recipientHash':hashlib.sha256(recipient.encode()).hexdigest()}):
            capture,_,_=self.fixture.fixture()
        capture.capture()
        return capture,e.Encryption(capture,TOOLS/'age',recipient),key
    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_recorded_capture_encrypts_and_decrypts_to_identical_bundle(self):
        capture,operation,key=self.prepare();receipt=operation.encrypt()
        self.assertEqual(e.recorded_receipt(capture.fence.journal,'encrypt',operation.receipt_path),receipt)
        self.assertEqual(capture.fence.journal.status()['completedStages'][-1],'encrypt')
        output=self.root/'decrypted.tar'
        recovered=e.envelope.decrypt(TOOLS/'age',operation.output,output,key,
                                    {name:receipt['envelope'][name] for name in ('plaintext','ciphertext')})
        self.assertEqual(recovered['plaintext'],t.c.bundle.archive_digest(capture.output))
        self.assertEqual(output.read_bytes(),capture.output.read_bytes())
        for name in ('offHostVerified','keyCustodyVerified','databaseRecoveryVerified'):self.assertFalse(receipt[name])
        with self.assertRaises(ValueError):operation.encrypt()
    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_wrong_recipient_rejects_before_encrypt_intent(self):
        capture,operation,_=self.prepare();operation.recipient='age1'+'a'*58
        with self.assertRaises(ValueError):operation.encrypt()
        self.assertIsNone(capture.fence.journal.status()['pendingStage']);self.assertFalse(operation.output.exists())
    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_changed_plaintext_rejects_before_encrypt_intent(self):
        capture,operation,_=self.prepare()
        with capture.output.open('r+b') as out:out.seek(512);out.write(b'changed')
        with self.assertRaises(ValueError):operation.encrypt()
        self.assertIsNone(capture.fence.journal.status()['pendingStage']);self.assertFalse(operation.output.exists())
    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_changed_capture_receipt_rejects_before_encrypt_intent(self):
        capture,operation,_=self.prepare();receipt=json.loads(capture.receipt_path.read_bytes())
        receipt['context']['releaseNonce']='9'*32;capture.receipt_path.write_bytes(t.c.bundle.canonical(receipt))
        with self.assertRaises(ValueError):operation.encrypt()
        self.assertIsNone(capture.fence.journal.status()['pendingStage']);self.assertFalse(operation.output.exists())
    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_private_receipt_failure_retains_ciphertext_and_pending_intent(self):
        capture,operation,_=self.prepare();write=e.bundle.write_index
        def fail(path,value):
            write(path,value);raise OSError('synthetic receipt fsync failure')
        with patch.object(e.bundle,'write_index',side_effect=fail):
            with self.assertRaisesRegex(OSError,'receipt fsync'):operation.encrypt()
        self.assertTrue(operation.output.exists());self.assertTrue(operation.receipt_path.exists())
        self.assertEqual(capture.fence.journal.status()['pendingStage'],'encrypt')
        with self.assertRaises(ValueError):e.recorded_receipt(capture.fence.journal,'encrypt',operation.receipt_path)

    @unittest.skipUnless(os.geteuid()==0 and e.envelope.platform.system()=='Linux','Actual age and UID1000 require owned Linux root')
    def test_late_process_rejection_retains_ciphertext_without_observation(self):
        capture,operation,_=self.prepare();encrypt=e.envelope.encrypt
        def late_worker(*args,**kwargs):
            result=encrypt(*args,**kwargs)
            t.c.processes.observe.side_effect=ValueError('synthetic late writer')
            return result
        with patch.object(e.envelope,'encrypt',side_effect=late_worker):
            with self.assertRaisesRegex(ValueError,'late writer'):operation.encrypt()
        self.assertTrue(operation.output.exists());self.assertFalse(operation.receipt_path.exists())
        self.assertEqual(capture.fence.journal.status()['pendingStage'],'encrypt')
        self.assertEqual(capture.fence.journal.status()['completedStages'][-1],'capture')


if __name__=='__main__':unittest.main()
