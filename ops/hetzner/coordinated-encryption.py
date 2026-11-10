#!/usr/bin/env python3
"""Journal pinned encryption of the exact recorded capture; no custody/transfer."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import stat


def load(name,filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


bundle=load('journal_encryption_bundle','coordinated-recovery-bundle.py')
envelope=load('journal_encryption_age','recovery-envelope.py')
journals=load('journal_encryption_journal','release-journal.py')
files=bundle.files


def require(value):
    if not value:raise ValueError('Coordinated encryption rejected')


def recorded_receipt(journal,stage,path):
    """Read private canonical evidence already committed by this same journal.

    Hash correspondence is not authentication against privileged journal writers.
    Never infer successful completion merely from an existing receipt path.
    """
    rows=journal.records();require(stage in journals.STAGES)
    index=1+2*journals.STAGES.index(stage);require(len(rows)>index+1)
    intent=rows[index];observed=rows[index+1]
    require(intent['event']['kind']=='intent' and observed['event']['kind']=='observed'
            and intent['event']['stage']==stage==observed['event']['stage'])
    context={'releaseNonce':intent['releaseNonce'],'planHash':intent['planHash'],
             'stage':stage,'targetsHash':intent['event']['targetsHash'],
             'operationId':intent['event']['operationId']}
    with files.directory(str(path.parent),private=True) as parent:
        fd=os.open(path.name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
        try:
            before=os.fstat(fd)
            require(stat.S_ISREG(before.st_mode) and before.st_uid==os.geteuid() and before.st_nlink==1
                    and stat.S_IMODE(before.st_mode)==0o600 and 0<before.st_size<=bundle.MAX_INDEX)
            with os.fdopen(os.dup(fd),'rb') as handle:raw=handle.read(bundle.MAX_INDEX+1)
            require(len(raw)==before.st_size and files.identity(os.fstat(fd))==files.identity(before)
                    and files.identity(os.stat(path.name,dir_fd=parent,follow_symlinks=False))==files.identity(before))
        finally:os.close(fd)
    require(hashlib.sha256(raw).hexdigest()==observed['event']['evidenceHash'])
    value=json.loads(raw)
    require(isinstance(value,dict) and bundle.canonical(value)==raw
            and type(value.get('schemaVersion')) is int and value['schemaVersion']==1
            and value.get('context')==context)
    return value


class Encryption:
    def __init__(self,capture,binary,recipient):
        require(isinstance(recipient,str))
        self.capture,self.binary,self.recipient=capture,binary,recipient
        self.owner=os.getpid();self.used=False
        self.output=capture.clone.directory/'coordinated-encrypted.age'
        self.receipt_path=capture.clone.directory/'coordinated-encryption-receipt.json'
        self.receipt=None

    def encrypt(self):
        require(os.getpid()==self.owner and not self.used);self.used=True
        capture=self.capture;observed=capture.guard(stage='encrypt')
        journal=capture.fence.journal
        recipient_hash=hashlib.sha256(self.recipient.encode()).hexdigest()
        require(journal.records()[0]['event']['plan']['recipientHash']==recipient_hash)
        captured=recorded_receipt(journal,'capture',capture.receipt_path)
        require(captured['bundle']['binding']==capture.binding
                and all(captured[key] is False for key in ('encryptionVerified','offHostVerified','databaseRecoveryVerified')))
        original=bundle.archive_digest(capture.output);require(original==captured['bundle']['archive'])
        capture_hash=hashlib.sha256(bundle.canonical(captured)).hexdigest()
        targets=hashlib.sha256(bundle.canonical({'plaintext':original,'recipientSha256':recipient_hash,
                                               'captureReceiptSha256':capture_hash})).hexdigest()
        def effect(context):
            require(context['releaseNonce']==capture.clone.nonce and context['stage']=='encrypt')
            require(capture.guard(stage='encrypt',pending=True)==observed)
            require(recorded_receipt(journal,'capture',capture.receipt_path)==captured)
            encrypted=envelope.encrypt(self.binary,capture.output,self.output,self.recipient)
            require(encrypted['plaintext']==original and encrypted['recipientSha256']==recipient_hash
                    and bundle.archive_digest(capture.output)==original)
            require(capture.guard(stage='encrypt',pending=True)==observed)
            self.receipt={'schemaVersion':1,'context':dict(context),'envelope':encrypted,
                          'captureReceiptSha256':capture_hash,'offHostVerified':False,
                          'keyCustodyVerified':False,'databaseRecoveryVerified':False}
            bundle.write_index(self.receipt_path,self.receipt)
            require(capture.guard(stage='encrypt',pending=True)==observed)
            return {**context,'evidenceHash':hashlib.sha256(bundle.canonical(self.receipt)).hexdigest()}
        journal.perform('encrypt',targets,effect)
        return self.receipt
