#!/usr/bin/env python3
"""Authenticate replay inputs inside an already pending isolate-restore intent.

No intent publication/completion, key provisioning, network or Docker operation.
The caller must still restore, start and verify the disposable DB/application.
"""
import copy
import hashlib
import os

import importlib.util
from pathlib import Path


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


retrieval = load('restoration_inputs_retrieval', 'coordinated-retrieval.py')
physical = load('restoration_inputs_topology', 'physical-postgres-recovery.py')
encryption = retrieval.encryption
envelope = encryption.envelope
bundle = encryption.bundle
require = encryption.require


def digest(value):
    return hashlib.sha256(bundle.canonical(value)).hexdigest()


class Inputs:
    def __init__(self, retrieved, binary, identity):
        self.retrieved = retrieved
        self.binary, self.identity = binary, identity
        self.capture = retrieved.encrypted.capture
        self.owner, self.used = os.getpid(), False
        self.output = self.capture.clone.directory/'coordinated-decrypted.tar'
        self.receipt_path = self.capture.clone.directory/'coordinated-restoration-inputs.json'
        self.receipt = None

    def _records(self):
        capture = self.capture
        journal = capture.fence.journal
        captured = encryption.recorded_receipt(journal, 'capture', capture.receipt_path)
        encrypted = encryption.recorded_receipt(journal, 'encrypt', self.retrieved.encrypted.receipt_path)
        retrieved = encryption.recorded_receipt(journal, 'retrieve-off-host', self.retrieved.receipt_path)
        capture_hash, envelope_hash = digest(captured), digest(encrypted)
        require(captured['bundle']['binding'] == capture.binding)
        require(encrypted['captureReceiptSha256'] == capture_hash)
        require(retrieved['encryptionReceiptSha256'] == envelope_hash)
        expected = {key: encrypted['envelope'][key] for key in ('plaintext', 'ciphertext')}
        require(expected['plaintext'] == captured['bundle']['archive'])
        require(encrypted['envelope']['recipientSha256'] == journal.records()[0]['event']['plan']['recipientHash'])
        transfer = {
            'status': 'ciphertext-round-trip-verified',
            **retrieval.transfer.binding(capture.clone.nonce, expected['ciphertext']),
            'decrypted': False, 'restored': False,
        }
        require(bundle.canonical(retrieved['transfer']) == bundle.canonical(transfer))
        require(self.retrieved.output == capture.clone.directory/'coordinated-retrieved.age')
        require(bundle.archive_digest(self.retrieved.output) == expected['ciphertext'])
        return captured, encrypted, retrieved, expected

    def _admit(self, context=None):
        require(os.getpid() == self.owner)
        directory = self.capture.clone.directory
        require(self.output == directory/'coordinated-decrypted.tar'
                and self.receipt_path == directory/'coordinated-restoration-inputs.json')
        observed = self.capture.guard(stage='restore-isolate', pending=context is not None)
        physical.verify_mounts()
        records = self._records()
        targets = {
            'captureReceiptSha256': digest(records[0]),
            'encryptionReceiptSha256': digest(records[1]),
            'retrievalReceiptSha256': digest(records[2]),
            **records[3],
        }
        if context is not None:
            row = self.capture.fence.journal.records()[-1]
            event = row['event']
            require(event['kind'] == 'intent' and event['stage'] == 'restore-isolate')
            actual = {
                'releaseNonce': row['releaseNonce'], 'planHash': row['planHash'],
                'stage': event['stage'], 'targetsHash': event['targetsHash'],
                'operationId': event['operationId'],
            }
            require(bundle.canonical(context) == bundle.canonical(actual))
            require(context['targetsHash'] == digest(targets))
        return observed, records, targets

    def targets(self):
        """Read-only target binding for the caller's restore-isolate intent."""
        require(not self.used)
        return digest(self._admit()[2])

    def prepare(self, context):
        """Return private verified inputs, never a completed isolate observation.

        Only use this output with its returned captureBundle/binding after success.
        Any failure retains partial files and the caller's pending intent.
        """
        require(os.getpid() == self.owner and not self.used and isinstance(context, dict))
        self.used = True
        observed, records, targets = self._admit(context)
        captured, encrypted, _, expected = records
        result = envelope.decrypt(self.binary, self.retrieved.output, self.output,
                                  self.identity, expected)
        require(result['status'] == 'decrypted-content-verified')
        require(result['plaintext'] == expected['plaintext'] and result['ciphertext'] == expected['ciphertext'])
        require(result['toolSha256'] == encrypted['envelope']['toolSha256'])
        require(bundle.archive_digest(self.output) == expected['plaintext'])
        require(self._admit(context) == (observed, records, targets))
        self.receipt = {
            'schemaVersion': 1, 'status': 'plaintext-verified-for-replay',
            'context': copy.deepcopy(context), **targets,
            'binding': copy.deepcopy(captured['bundle']['binding']),
            'captureBundle': copy.deepcopy(captured['bundle']),
            'decryption': copy.deepcopy(result),
            'keyCustodyVerified': False, 'independentStorageVerifiedByHelper': False,
            'databaseRecoveryVerified': False, 'applicationRecoveryVerified': False,
            'isolateStageComplete': False,
        }
        bundle.write_index(self.receipt_path, self.receipt)
        require(bundle.archive_digest(self.output) == expected['plaintext'])
        require(self._admit(context) == (observed, records, targets))
        # The nested context deliberately does not satisfy journal.perform's
        # observation protocol: plaintext alone cannot complete restore-isolate.
        return copy.deepcopy(self.receipt)
