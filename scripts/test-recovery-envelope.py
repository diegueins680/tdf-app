#!/usr/bin/env python3
"""Real age boundary checks with temporary synthetic identities and data only."""
import copy
import hashlib
import fcntl
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('envelope', ROOT/'ops/hetzner/recovery-envelope.py')
envelope = importlib.util.module_from_spec(spec); spec.loader.exec_module(envelope)
TOOLS = Path(os.environ.get('TDF_RECOVERY_TOOLS', '/missing-pinned-recovery-tools')).resolve()
AGE, KEYGEN = TOOLS/'age', TOOLS/'age-keygen'


@unittest.skipUnless(envelope.platform.system() == 'Linux', 'Linux sealed executable checks require Linux; CI must run them')
class EnvelopeTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(); self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.key = self.root/'identity'
        subprocess.run([str(KEYGEN), '-o', str(self.key)], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.recipient = subprocess.check_output([str(KEYGEN), '-y', str(self.key)], text=True).strip()
        self.plain = self.root/'bundle.tar'; self.plain.write_bytes(bytes(range(256))*1024); self.plain.chmod(0o600)
        self.cipher = self.root/'bundle.age'
        self.output = self.root/'recovered.tar'

    def encrypt(self): return envelope.encrypt(AGE, self.plain, self.cipher, self.recipient)

    def decrypt(self, receipt, key=None):
        return envelope.decrypt(AGE, self.cipher, self.output, key or self.key,
                                {k: receipt[k] for k in ('plaintext','ciphertext')})

    def test_real_encryption_and_recovery_without_broader_claims(self):
        receipt = self.encrypt(); recovered = self.decrypt(receipt)
        self.assertEqual(self.plain.read_bytes(), self.output.read_bytes())
        self.assertNotIn(self.plain.read_bytes(), self.cipher.read_bytes())
        self.assertEqual(recovered['status'], 'decrypted-content-verified')
        self.assertFalse(recovered['offHost']); self.assertFalse(recovered['databaseRestored']); self.assertFalse(recovered['filesRestored'])
        self.assertEqual(self.output.stat().st_mode & 0o777, 0o600)
        self.assertNotIn('AGE-SECRET-KEY', str(receipt)+str(recovered))

    def test_corrupt_ciphertext_rejected_even_when_supplied_cipher_hash_is_changed(self):
        receipt = self.encrypt(); data = bytearray(self.cipher.read_bytes()); data[-20] ^= 1; self.cipher.write_bytes(data)
        receipt['ciphertext']['sha256'] = hashlib.sha256(data).hexdigest()
        with self.assertRaises(ValueError): self.decrypt(receipt)
        # Streaming decryption may leave private partial bytes, never a success receipt.
        if self.output.exists(): self.assertEqual(self.output.stat().st_mode & 0o777, 0o600)

    def test_truncated_ciphertext_rejected_even_with_matching_transferred_hash(self):
        receipt = self.encrypt(); data=self.cipher.read_bytes()[:-32]; self.cipher.write_bytes(data)
        receipt['ciphertext']={'sha256':hashlib.sha256(data).hexdigest(),'bytes':len(data)}
        with self.assertRaises(ValueError): self.decrypt(receipt)

    def test_wrong_identity_rejects(self):
        receipt = self.encrypt(); key=self.root/'wrong'
        subprocess.run([str(KEYGEN), '-o', str(key)], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        with self.assertRaises(ValueError): self.decrypt(receipt, key)

    def test_valid_substituted_encryption_cannot_match_trusted_capture_digest(self):
        receipt=self.encrypt(); receipt['plaintext']['sha256']=hashlib.sha256(b'other approved bundle').hexdigest()
        with self.assertRaises(ValueError): self.decrypt(receipt)

    def test_mismatched_transfer_digest_rejects_before_output_creation(self):
        receipt=self.encrypt(); receipt['ciphertext']['sha256']='f'*64
        with self.assertRaises(ValueError): self.decrypt(receipt)
        self.assertFalse(self.output.exists())

    def test_public_identity_or_symlink_or_duplicate_output_rejected(self):
        receipt=self.encrypt(); self.key.chmod(0o644)
        with self.assertRaises(ValueError): self.decrypt(receipt)
        self.key.chmod(0o600); alias=self.root/'alias'; alias.symlink_to(self.key)
        with self.assertRaises(OSError): self.decrypt(receipt,alias)
        self.decrypt(receipt); preserved=self.output.read_bytes()
        with self.assertRaises(FileExistsError): self.decrypt(receipt)
        self.assertEqual(self.output.read_bytes(), preserved)
        with self.assertRaises(FileExistsError): self.encrypt()

    def test_unsigned_tool_substitution_is_not_executed(self):
        fake=self.root/'age'; fake.write_text('#!/bin/sh\ntouch '+str(self.root/'ran')+'\n'); fake.chmod(0o700)
        with self.assertRaises(ValueError): envelope.encrypt(fake,self.plain,self.cipher,self.recipient)
        self.assertFalse((self.root/'ran').exists())

    def test_verified_execution_survives_path_swap_without_plaintext_false_success(self):
        binary = self.root/'age'
        genuine = AGE.read_bytes(); binary.write_bytes(genuine); binary.chmod(0o700)
        original = envelope.process
        def swapped(*args, **kwargs):
            binary.write_bytes(b'#!/bin/sh\ncat\n')
            try: return original(*args, **kwargs)
            finally: binary.write_bytes(genuine)
        with patch.object(envelope, 'process', side_effect=swapped):
            receipt = envelope.encrypt(binary, self.plain, self.cipher, self.recipient)
        self.assertNotEqual(self.plain.read_bytes(), self.cipher.read_bytes())
        self.decrypt(receipt)
        self.assertEqual(self.plain.read_bytes(), self.output.read_bytes())

    def test_verified_executable_bytes_cannot_be_modified_or_truncated(self):
        with envelope.verified_executable(AGE) as (fd, _):
            with self.assertRaises(OSError): os.write(fd, b'changed')
            with self.assertRaises(OSError): os.ftruncate(fd, 0)
            with self.assertRaises(OSError): os.ftruncate(fd, os.fstat(fd).st_size+1)
            with self.assertRaises(OSError): fcntl.fcntl(fd, fcntl.F_ADD_SEALS, 0)


    def test_plugin_recipient_identity_and_key_output_are_unavailable(self):
        with self.assertRaises(ValueError): envelope.encrypt(AGE,self.plain,self.cipher,'age1plugin-synthetic')
        receipt=self.encrypt(); self.key.write_text('AGE-PLUGIN-SYNTHETIC-1KEY\n')
        with self.assertRaises(ValueError): self.decrypt(receipt)
        self.assertFalse(self.output.exists())


if __name__ == '__main__': unittest.main()
