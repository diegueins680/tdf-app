#!/usr/bin/env python3
"""Fixed epoch journal controls; boot observations and effects are synthetic."""
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('service_journal_test',ROOT/'ops/hetzner/abort-service-journal.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
a=s.a;j=a.j
PLAN={k:('sha256:'+'a'*64 if k.endswith('Image') else 'a'*(40 if k.endswith('Revision') else 64)) for k in j.PLAN_KEYS}
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
NEW={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}
NEXT={**HOST,'bootId':'33333333-3333-3333-3333-333333333333'}


def observed(c):return {**c,'evidenceHash':'e'*64}


class ServiceJournalTests(unittest.TestCase):
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.addCleanup(self.tmp.cleanup)
        self.root=Path(self.tmp.name).resolve()
        with j.open_journal(self.root) as q:q.initialize(PLAN,'b'*32)
        admission={'schemaVersion':1,'releaseNonce':'b'*32,'planHash':j.sha(j.canonical(PLAN)),
                   'host':HOST,'originalDeployment':{'synthetic':True}}
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=HOST):
            q.latch(admission,a.sha(a.canonical(admission)));q.request_reboot(lambda:None)

    def test_fresh_epoch_orders_all_stages_without_authorizing_normal_release(self):
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            for stage in s.STAGES:
                def effect(c):
                    self.assertEqual(recovery.current()['pendingStage'],stage)
                    self.assertTrue(recovery.current()['newWritesPossible'])
                    return observed(c)
                status=recovery.perform(stage,'d'*64,effect)
            self.assertTrue(status['originalDeploymentRecoverySequenceComplete'])
            self.assertFalse(status['releaseContinuationAllowed'])
            with self.assertRaises(ValueError):recovery.perform('recover-db','d'*64,observed)
            with self.assertRaises(ValueError):recovery.request_next_reboot(lambda:self.fail('terminal reboot'))
        with self.assertRaises(ValueError):
            with j.open_journal(self.root):pass

    def test_same_or_foreign_boot_cannot_open_epoch(self):
        with a.open_abort(self.root) as q:
            recovery=s.ServiceJournal(q)
            for host in (HOST,{**NEW,'machineId':'2'*32}):
                with patch.object(a,'boot_identity',return_value=host),self.assertRaises(ValueError):recovery.begin_epoch()

    def test_uncertain_effect_blocks_same_boot_until_recorded_new_reboot(self):
        effects=[]
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            def lost(c):effects.append(c);raise RuntimeError('lost response')
            with self.assertRaises(RuntimeError):recovery.perform('remove-disposables','d'*64,lost)
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q)
            with self.assertRaises(ValueError):recovery.perform('remove-disposables','d'*64,lost)
            with self.assertRaises(ValueError):recovery.begin_epoch()
            recovery.request_next_reboot(lambda:effects.append('reboot'))
            with self.assertRaises(ValueError):recovery.request_next_reboot(lambda:self.fail('duplicate reboot'))
            with self.assertRaises(ValueError):recovery.begin_epoch()
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEXT):
            recovery=s.ServiceJournal(q);state=recovery.begin_epoch();self.assertEqual(state['epoch'],2)
            self.assertTrue(state['newWritesPossible']);recovery.perform('remove-disposables','d'*64,observed)
        self.assertEqual(len(effects),2)

    def test_unrecorded_boot_change_denies_service_effects(self):
        with a.open_abort(self.root) as q:
            recovery=s.ServiceJournal(q)
            with patch.object(a,'boot_identity',return_value=NEW):recovery.begin_epoch()
            with patch.object(a,'boot_identity',return_value=NEXT):
                with self.assertRaises(ValueError):recovery.perform('remove-disposables','d'*64,observed)
                with self.assertRaises(ValueError):recovery.begin_epoch()

    def test_wrong_stage_and_mismatched_observation_are_not_completion(self):
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            with self.assertRaises(ValueError):recovery.perform('recover-db','d'*64,observed)
            with self.assertRaises(ValueError):recovery.perform('remove-disposables','d'*64,lambda c:{**observed(c),'bootId':HOST['bootId']})
            self.assertEqual(recovery.current()['pendingStage'],'remove-disposables')

    def test_late_host_change_prevents_observation(self):
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW) as host:
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            def effect(c):host.return_value=NEXT;return observed(c)
            with self.assertRaises(ValueError):recovery.perform('remove-disposables','d'*64,effect)
            self.assertEqual(recovery.records()[1]['pendingStage'],'remove-disposables')

    def test_partial_record_or_changed_original_blocks_recovery(self):
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            p=self.root/'abort/recovery/001.json.pending';p.touch(mode=0o600)
            with self.assertRaises(ValueError):recovery.perform('remove-disposables','d'*64,observed)
            p.unlink()  # Synthetic test corruption only, never a recovery operation.
            original=self.root/'000.json';original.write_bytes(original.read_bytes()+b' ')
            with self.assertRaises(ValueError):recovery.records()

    def test_intent_sync_failure_does_not_call_effect(self):
        with a.open_abort(self.root) as q,patch.object(a,'boot_identity',return_value=NEW):
            recovery=s.ServiceJournal(q);recovery.begin_epoch()
            real=a.publish
            def fail(parent,name,value):
                if value['event']['kind']=='intent':raise OSError('synthetic storage fault')
                return real(parent,name,value)
            with patch.object(a,'publish',side_effect=fail),self.assertRaises(OSError):
                recovery.perform('remove-disposables','d'*64,lambda c:self.fail('uncommitted effect'))
            with self.assertRaises(ValueError):recovery.records()


if __name__=='__main__':unittest.main(verbosity=2)
