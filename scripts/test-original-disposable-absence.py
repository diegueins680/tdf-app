#!/usr/bin/env python3
"""Real abort journal and restore lock; synthetic boot and container inventory."""
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
def load(name,path):
    s=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m
r=load('absence_recovery_test',ROOT/'ops/hetzner/original-disposable-absence.py')
base=load('absence_recovery_fixture',ROOT/'scripts/test-original-database-recovery.py')
base.r=r.d
base.SAVED={**base.SAVED,'expected':{k:{'containerId':v*64} for k,v in (('db','d'),('api','a'),('edge','e'))}}
IDS='\n'.join(v['containerId'] for v in base.SAVED['expected'].values())+'\n'


class AbsenceTests(unittest.TestCase):
    def setUp(self):
        base.DatabaseRecoveryTests.setUp(self)
        # Preserve the fixture's first epoch, then start a recorded second epoch.
        self.journal.request_next_reboot(lambda:None)
        self.enterContext(patch.object(base.a,'boot_identity',return_value={**base.NEW,'bootId':'33333333-3333-3333-3333-333333333333'}))
        self.journal.begin_epoch()
        self.adapter=r.OriginalDisposableAbsence(self.journal,self.root,self.reservation)
        self.enterContext(patch.object(r.d.o.sources.inspector,'capture',return_value=IDS))

    def rejected(self):
        with self.assertRaises(ValueError):self.adapter.recover()
        state=self.journal.current()
        self.assertEqual(state['pendingStage'],'remove-disposables')
        self.assertEqual(state['completedStages'],[])
        self.assertEqual(self.commands,[])

    def test_only_three_originals_without_marker_complete_without_removal(self):
        with patch.object(r.d.o.sources.inspector,'capture',return_value=IDS) as calls:
            state=self.adapter.recover()
        self.assertEqual(state['completedStages'],['remove-disposables'])
        self.assertFalse(state['releaseContinuationAllowed'])
        self.assertEqual(self.commands,[])
        self.assertEqual(calls.call_count,2)
        for call in calls.call_args_list:self.assertEqual(call.args[0],r.d.o.sources.inspector.DOCKER+['ps','--all','--quiet','--no-trunc'])
        with self.assertRaises(ValueError):self.adapter.recover()

    def test_extra_stopped_or_running_container_is_preserved(self):
        with patch.object(r.d.o.sources.inspector,'capture',return_value=IDS+'f'*64+'\n'):self.rejected()

    def test_missing_or_replaced_original_is_not_adopted(self):
        with patch.object(r.d.o.sources.inspector,'capture',return_value=IDS.replace('a'*64,'f'*64)):self.rejected()

    def test_duplicate_inventory_is_rejected(self):
        with patch.object(r.d.o.sources.inspector,'capture',return_value=('d'*64+'\n')*3):self.rejected()

    def test_pending_marker_is_preserved_and_no_inspection_runs(self):
        marker=self.root/r.d.restore.PENDING_NAME;marker.write_bytes(b'uncertain creation')
        with patch.object(r.d.o.sources.inspector,'capture') as calls:self.rejected()
        calls.assert_not_called();self.assertEqual(marker.read_bytes(),b'uncertain creation')

    def test_dangling_marker_symlink_is_not_treated_as_absence(self):
        marker=self.root/r.d.restore.PENDING_NAME;marker.symlink_to(self.root/'missing')
        self.rejected();self.assertTrue(marker.is_symlink())

    def test_marker_created_during_inspection_prevents_completion(self):
        def changed(command):
            (self.root/r.d.restore.PENDING_NAME).touch(mode=0o600);return IDS
        with patch.object(r.d.o.sources.inspector,'capture',side_effect=changed):self.rejected()

    def test_changed_closing_inventory_prevents_completion(self):
        with patch.object(r.d.o.sources.inspector,'capture',side_effect=[IDS,IDS+'f'*64+'\n']):self.rejected()

    def test_lost_inspection_response_stays_pending_without_replay(self):
        with patch.object(r.d.o.sources.inspector,'capture',side_effect=TimeoutError('lost read')):
            with self.assertRaises(TimeoutError):self.adapter.recover()
        with patch.object(r.d.o.sources.inspector,'capture') as calls:
            with self.assertRaises(ValueError):self.adapter.recover()
        calls.assert_not_called();self.assertEqual(self.journal.current()['pendingStage'],'remove-disposables')


if __name__=='__main__':unittest.main(verbosity=2)
