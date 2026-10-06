#!/usr/bin/env python3
"""Actual journal/capture/age/socket round trip; synthetic local peers only."""
import importlib.util
import os
from pathlib import Path
import socket
import threading
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('retrieval_encryption_fixture',ROOT/'scripts/test-coordinated-encryption.py')
t=importlib.util.module_from_spec(spec);spec.loader.exec_module(t)
r=t.e.load('retrieval_fixture','coordinated-retrieval.py')


@unittest.skipUnless(os.geteuid()==0 and t.e.envelope.platform.system()=='Linux','Actual age/capture requires owned Linux root')
class RetrievalTests(unittest.TestCase):
    def setUp(self):
        self.fixture=t.EncryptionTests();self.fixture.setUp();self.addCleanup(self.fixture.doCleanups)
        self.capture,self.encrypted,self.key=self.fixture.prepare();self.encrypted.encrypt()
        self.left,self.right=socket.socketpair()
        self.addCleanup(self.left.close);self.addCleanup(self.right.close)
        self.peer_path=self.fixture.root/'synthetic-peer.age'

    def round_trip(self,*,corrupt=False):
        expected=self.encrypted.receipt['envelope']['ciphertext'];failures=[]
        def peer():
            try:
                with r.transfer.Channel(self.right.fileno(),self.right.fileno(),3) as channel:
                    if corrupt:
                        r.transfer.receive(channel,self.peer_path,self.capture.clone.nonce,expected,'offer')
                        channel.send_header({**r.transfer.binding(self.capture.clone.nonce,expected),'kind':'retrieval'})
                        data=bytearray(self.peer_path.read_bytes());data[-1]^=1
                        for offset in range(0,len(data),r.transfer.BLOCK):
                            channel.write(bytes(data[offset:offset+r.transfer.BLOCK]))
                    else:r.transfer.retain_and_return(channel,self.peer_path,self.capture.clone.nonce,expected)
            except BaseException as error:failures.append(error)
        thread=threading.Thread(target=peer);thread.start()
        try:
            with r.transfer.Channel(self.left.fileno(),self.left.fileno(),3) as channel:
                self.operation=r.Retrieval(self.encrypted,channel)
                return self.operation.retrieve()
        finally:
            thread.join(4);self.assertFalse(thread.is_alive());self.assertFalse(failures)

    def test_recorded_ciphertext_round_trip_and_retrieved_decryption(self):
        receipt=self.round_trip()
        self.assertEqual(r.encryption.recorded_receipt(self.capture.fence.journal,'retrieve-off-host',self.operation.receipt_path),receipt)
        self.assertEqual(self.operation.output.read_bytes(),self.peer_path.read_bytes())
        output=self.fixture.root/'retrieved-plain.tar'
        envelope=self.encrypted.receipt['envelope']
        t.e.envelope.decrypt(t.TOOLS/'age',self.operation.output,output,self.key,
                             {name:envelope[name] for name in ('plaintext','ciphertext')})
        self.assertEqual(output.read_bytes(),self.capture.output.read_bytes())
        self.assertFalse(receipt['endpointAuthenticationVerifiedByHelper'])
        self.assertFalse(receipt['independentStorageVerifiedByHelper'])
        self.assertFalse(receipt['keyCustodyVerified']);self.assertFalse(receipt['databaseRecoveryVerified'])
        with self.assertRaises(ValueError):self.operation.retrieve()

    def test_changed_ciphertext_rejects_before_retrieval_intent(self):
        with self.encrypted.output.open('r+b') as out:out.seek(64);out.write(b'changed')
        with self.assertRaises(ValueError):r.Retrieval(self.encrypted,None).retrieve()
        self.assertIsNone(self.capture.fence.journal.status()['pendingStage'])

    def test_changed_encryption_receipt_rejects_before_retrieval_intent(self):
        self.encrypted.receipt_path.write_bytes(b'{}\n')
        with self.assertRaises(ValueError):r.Retrieval(self.encrypted,None).retrieve()
        self.assertIsNone(self.capture.fence.journal.status()['pendingStage'])

    def test_corrupted_return_does_not_complete_journal(self):
        with self.assertRaises(ValueError):self.round_trip(corrupt=True)
        self.assertEqual(self.capture.fence.journal.status()['pendingStage'],'retrieve-off-host')
        self.assertTrue(self.operation.output.exists());self.assertFalse(self.operation.receipt_path.exists())

    def test_late_writer_preserves_retrieved_copy_without_completion(self):
        original=r.transfer.round_trip
        def late(*args,**kwargs):
            result=original(*args,**kwargs)
            t.t.c.processes.observe.side_effect=ValueError('synthetic late retrieval writer')
            return result
        with patch.object(r.transfer,'round_trip',side_effect=late):
            with self.assertRaisesRegex(ValueError,'late retrieval writer'):self.round_trip()
        self.assertEqual(self.capture.fence.journal.status()['pendingStage'],'retrieve-off-host')
        self.assertTrue(self.operation.output.exists());self.assertFalse(self.operation.receipt_path.exists())

    def test_receipt_sync_failure_retains_both_copies_and_pending_intent(self):
        write=r.bundle.write_index
        def fail(path,value):write(path,value);raise OSError('synthetic retrieval receipt sync')
        with patch.object(r.bundle,'write_index',side_effect=fail):
            with self.assertRaisesRegex(OSError,'receipt sync'):self.round_trip()
        self.assertEqual(self.capture.fence.journal.status()['pendingStage'],'retrieve-off-host')
        self.assertTrue(self.operation.output.exists());self.assertTrue(self.peer_path.exists())
        with self.assertRaises(ValueError):r.encryption.recorded_receipt(self.capture.fence.journal,'retrieve-off-host',self.operation.receipt_path)


if __name__=='__main__':unittest.main()
