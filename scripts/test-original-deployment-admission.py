#!/usr/bin/env python3
"""Durable original-admission controls; Docker/SQL observations synthetic here."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('original_admission_test',ROOT/'ops/hetzner/original-deployment-admission.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
j=m.abort.j
PLAN={key:('sha256:'+'a'*64 if key.endswith('Image') else 'a'*(40 if key.endswith('Revision') else 64)) for key in j.PLAN_KEYS}
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
VALUE={'runtimeConfigurationSha256':PLAN['runtimeHash'],'syntheticFullAdmission':'private-value-not-for-output'}


class AdmissionTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve();self.control=self.root/'journal';self.control.mkdir(mode=0o700)
        self.evidence=self.root/'evidence';self.evidence.mkdir(mode=0o700)
        with j.open_journal(self.control) as q:q.initialize(PLAN,'b'*32)

    def test_saved_before_shutdown_can_reopen_and_bind_abort(self):
        with j.open_journal(self.control) as q,patch.object(m,'observe',return_value=VALUE),patch.object(m.abort,'boot_identity',return_value=HOST):
            receipt=m.prepare(q,self.evidence,{},{});self.assertTrue(receipt['preparedBeforeShutdown'])
            self.assertNotIn('private-value-not-for-output',str(receipt))
            saved=m.read_prepared(self.evidence,'b'*32,q.records()[0]['planHash'])
            q.perform('maintenance','c'*64,lambda c:{**c,'evidenceHash':'d'*64})
        with m.abort.open_abort(self.control) as q:
            q.latch(saved,receipt['sha256']);self.assertFalse(q.status()['releaseContinuationAllowed'])

    def test_ledger_configuration_or_host_drift_never_emits_prepared_receipt(self):
        changed={**VALUE,'changed':'ledger-or-config'}
        with j.open_journal(self.control) as q,patch.object(m,'observe',side_effect=[VALUE,changed]),patch.object(m.abort,'boot_identity',return_value=HOST):
            with self.assertRaises(ValueError):m.prepare(q,self.evidence,{},{})
        self.assertTrue((self.evidence/m.NAME).exists());self.assertFalse((self.evidence/m.RECEIPT).exists())
        with self.assertRaises(FileNotFoundError):m.read_prepared(self.evidence,'b'*32,j.sha(j.canonical(PLAN)))

    def test_cannot_prepare_after_any_shutdown_intent(self):
        with j.open_journal(self.control) as q:
            def failed(c):raise RuntimeError('lost')
            with self.assertRaises(RuntimeError):q.perform('maintenance','c'*64,failed)
            with patch.object(m,'observe') as observe:
                with self.assertRaises(ValueError):m.prepare(q,self.evidence,{},{})
                observe.assert_not_called()

    def test_evidence_cannot_pollute_original_journal_directory(self):
        with j.open_journal(self.control) as q:
            with self.assertRaises(ValueError):m.prepare(q,self.control,{},{})
            self.assertEqual(len(q.records()),1)

    def test_runtime_plan_mismatch_and_changed_saved_bytes_deny(self):
        with j.open_journal(self.control) as q,patch.object(m.abort,'boot_identity',return_value=HOST):
            with patch.object(m,'observe',return_value={**VALUE,'runtimeConfigurationSha256':'f'*64}):
                with self.assertRaises(ValueError):m.prepare(q,self.evidence,{},{})
            with patch.object(m,'observe',return_value=VALUE):m.prepare(q,self.evidence,{},{})
        p=self.evidence/m.NAME;p.write_bytes(p.read_bytes()+b' ')
        with self.assertRaises(ValueError):m.read_prepared(self.evidence,'b'*32,j.sha(j.canonical(PLAN)))

    def test_configuration_hashes_all_root_files_but_not_mutable_asset_contents(self):
        for name in ('compose.yaml','Caddyfile','postgres_password','.env'):(self.root/name).write_text('synthetic-private')
        (self.root/'assets').mkdir();(self.root/'assets/photo').write_text('first')
        first=m.configuration_files(self.root);self.assertIn('.env',first)
        self.assertNotIn('synthetic-private',str(first));self.assertNotIn('assets',first)
        (self.root/'assets/photo').write_text('second');self.assertEqual(first,m.configuration_files(self.root))
        (self.root/'.env').write_text('changed');self.assertNotEqual(first,m.configuration_files(self.root))
        (self.root/'alias').symlink_to(self.root/'.env')
        with self.assertRaises(ValueError):m.configuration_files(self.root)

    def test_bind_directory_replacement_changes_identity_without_hashing_contents(self):
        assets=self.root/'assets';assets.mkdir();(assets/'photo').write_text('first')
        first=m.directory_identity(assets)
        (assets/'photo').write_text('second');self.assertEqual(first,m.directory_identity(assets))
        assets.rename(self.root/'old-assets');assets.mkdir()
        self.assertNotEqual(first,m.directory_identity(assets))

    def test_host_boot_change_prevents_prepared_receipt(self):
        changed={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}
        with j.open_journal(self.control) as q,patch.object(m,'observe',return_value=VALUE),patch.object(m.abort,'boot_identity',side_effect=[HOST,changed]):
            with self.assertRaises(ValueError):m.prepare(q,self.evidence,{},{})
        self.assertFalse((self.evidence/m.RECEIPT).exists())

    def test_publication_failure_does_not_create_readiness_receipt(self):
        with j.open_journal(self.control) as q,patch.object(m,'observe',return_value=VALUE),patch.object(m.abort,'boot_identity',return_value=HOST):
            with patch.object(m.abort.os,'fsync',side_effect=OSError('synthetic')):
                with self.assertRaises(OSError):m.prepare(q,self.evidence,{},{})
        self.assertFalse((self.evidence/m.RECEIPT).exists())


if __name__=='__main__':unittest.main(verbosity=2)
