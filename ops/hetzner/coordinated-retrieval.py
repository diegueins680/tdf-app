#!/usr/bin/env python3
"""Journal exact encrypted-bundle round trip over a caller-authenticated channel.

Endpoint authentication, independent storage and key custody are caller duties.
No network address, private identity, decrypt or production lifecycle operation.
"""
import hashlib
import os
from pathlib import Path
import importlib.util


def load(name,filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


encryption=load('retrieval_encryption','coordinated-encryption.py')
transfer=load('retrieval_transfer','recovery-transfer.py')
bundle=encryption.bundle
require=encryption.require


class Retrieval:
    def __init__(self,encrypted,channel):
        self.encrypted,self.channel=encrypted,channel
        self.owner=os.getpid();self.used=False
        self.output=encrypted.capture.clone.directory/'coordinated-retrieved.age'
        self.receipt_path=encrypted.capture.clone.directory/'coordinated-retrieval-receipt.json'
        self.receipt=None

    def retrieve(self):
        require(os.getpid()==self.owner and not self.used);self.used=True
        encrypted=self.encrypted;capture=encrypted.capture;journal=capture.fence.journal
        observed=capture.guard(stage='retrieve-off-host')
        captured=encryption.recorded_receipt(journal,'capture',capture.receipt_path)
        envelope=encryption.recorded_receipt(journal,'encrypt',encrypted.receipt_path)
        captured_hash=hashlib.sha256(bundle.canonical(captured)).hexdigest()
        envelope_hash=hashlib.sha256(bundle.canonical(envelope)).hexdigest()
        require(captured['bundle']['binding']==capture.binding
                and envelope['captureReceiptSha256']==captured_hash
                and envelope['envelope']['plaintext']==captured['bundle']['archive']
                and envelope['envelope']['recipientSha256']==journal.records()[0]['event']['plan']['recipientHash'])
        expected=envelope['envelope']['ciphertext']
        require(bundle.archive_digest(encrypted.output)==expected)
        targets=hashlib.sha256(bundle.canonical({'encryptionReceiptSha256':envelope_hash,
                                               'ciphertext':expected})).hexdigest()
        def effect(context):
            require(context['releaseNonce']==capture.clone.nonce and context['stage']=='retrieve-off-host')
            require(capture.guard(stage='retrieve-off-host',pending=True)==observed)
            require(encryption.recorded_receipt(journal,'capture',capture.receipt_path)==captured
                    and encryption.recorded_receipt(journal,'encrypt',encrypted.receipt_path)==envelope)
            result=transfer.round_trip(self.channel,encrypted.output,self.output,capture.clone.nonce,expected)
            require(bundle.archive_digest(self.output)==expected
                    and bundle.archive_digest(encrypted.output)==expected)
            require(capture.guard(stage='retrieve-off-host',pending=True)==observed)
            self.receipt={'schemaVersion':1,'context':dict(context),'transfer':result,
                          'encryptionReceiptSha256':envelope_hash,
                          'endpointAuthenticationVerifiedByHelper':False,'independentStorageVerifiedByHelper':False,
                          'keyCustodyVerified':False,'databaseRecoveryVerified':False}
            bundle.write_index(self.receipt_path,self.receipt)
            require(capture.guard(stage='retrieve-off-host',pending=True)==observed)
            return {**context,'evidenceHash':hashlib.sha256(bundle.canonical(self.receipt)).hexdigest()}
        journal.perform('retrieve-off-host',targets,effect)
        return self.receipt
