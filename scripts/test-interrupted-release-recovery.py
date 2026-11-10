#!/usr/bin/env python3
"""Actual filesystem/lock/process controls; host observations are synthetic.

These controls do not reboot a machine or qualify service/database recovery.
"""
import copy
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
SOURCE=ROOT/'ops/hetzner/interrupted-release-recovery.py'
spec=importlib.util.spec_from_file_location('abort_test',SOURCE)
r=importlib.util.module_from_spec(spec);spec.loader.exec_module(r)
j=r.j
PLAN={key:('sha256:'+'a'*64 if key.endswith('Image') else 'a'*(40 if key.endswith('Revision') else 64)) for key in j.PLAN_KEYS}
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
NEW_HOST={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}


class AbortTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve()
        with j.open_journal(self.root) as q:q.initialize(PLAN,'b'*32)
        self.admission={'schemaVersion':1,'releaseNonce':'b'*32,'planHash':j.sha(j.canonical(PLAN)),
            'host':HOST,'originalDeployment':{'syntheticAdmissionOnly':True}}

    def latch(self,q):return q.latch(self.admission,r.sha(r.canonical(self.admission)))

    def stopped(self,count=1,pending=False):
        with j.open_journal(self.root) as q:
            for phase in j.STAGES[:count]:q.perform(phase,'c'*64,lambda c:{**c,'evidenceHash':'d'*64})
            if pending:
                def failed(c):raise RuntimeError('lost response')
                with self.assertRaises(RuntimeError):q.perform(j.STAGES[count],'c'*64,failed)

    def test_intent_precedes_effect_and_only_fresh_boot_passes(self):
        self.stopped(3)
        with r.open_abort(self.root) as q:
            self.assertFalse(self.latch(q)['newWritesPossible'])
            with patch.object(r,'boot_identity',return_value=HOST):
                def effect():self.assertTrue(q.status()['newWritesPossible'])
                q.request_reboot(effect)
                with self.assertRaises(ValueError):q.observe_new_boot()
                with self.assertRaises(ValueError):q.request_reboot(lambda:self.fail('replayed reboot'))
            with patch.object(r,'boot_identity',return_value=NEW_HOST):
                result=q.observe_new_boot()
                self.assertTrue(result['oldBootTasksExcluded'])
                self.assertFalse(result['originalDeploymentRecovered'])
                self.assertFalse(result['continuousMaintenanceVerified'])
                self.assertFalse(result['databaseCleanShutdownVerified'])
        with self.assertRaises(ValueError):
            with j.open_journal(self.root):pass

    def test_foreign_machine_and_original_admission_replacement_denied(self):
        with r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):q.latch(self.admission,'e'*64)
            self.latch(q)
            with patch.object(r,'boot_identity',return_value={**HOST,'machineId':'2'*32}):
                with self.assertRaises(ValueError):q.request_reboot(lambda:self.fail('foreign host'))
            with patch.object(r,'boot_identity',return_value=HOST):q.request_reboot(lambda:None)
            with patch.object(r,'boot_identity',return_value={**NEW_HOST,'machineId':'2'*32}):
                with self.assertRaises(ValueError):q.observe_new_boot()

    def test_lost_stop_response_can_latch_but_never_completes_original(self):
        self.stopped(1,pending=True)
        before={p.name:p.read_bytes() for p in self.root.glob('*.json')}
        with r.open_abort(self.root) as q:self.latch(q)
        self.assertEqual(before,{p.name:p.read_bytes() for p in self.root.glob('*.json')})

    def test_lone_pending_and_hardlinked_publication_retained(self):
        self.stopped(0,pending=True)
        original=self.root/'001.json';pending=self.root/'001.json.pending'
        original.rename(pending)
        with r.open_abort(self.root) as q:
            single=r.frozen_prefix(q.parent);self.assertEqual(single['files'][-1]['links'],1)
            os.link(pending,original)
            linked=r.frozen_prefix(q.parent);self.assertEqual(linked['files'][-1]['links'],2)
            self.latch(q)
        self.assertEqual(original.stat().st_ino,pending.stat().st_ino)
        self.assertEqual(original.stat().st_nlink,2)

    def test_same_bytes_different_inodes_are_not_publication_hardlinks(self):
        self.stopped(0,pending=True)
        pending=self.root/'001.json.pending';pending.write_bytes((self.root/'001.json').read_bytes());pending.chmod(0o600)
        with r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):self.latch(q)

    def test_truncated_pending_is_preserved_but_denied(self):
        pending=self.root/'001.json.pending';pending.write_bytes(b'{');pending.chmod(0o600)
        with r.open_abort(self.root) as q:
            with self.assertRaises((ValueError,json.JSONDecodeError)):self.latch(q)
        self.assertEqual(pending.read_bytes(),b'{')

    def test_any_write_stage_filename_denies_abort_before_parsing(self):
        for suffix in ('.json','.json.pending'):
            p=self.root/('015'+suffix);p.touch(mode=0o600)
            with r.open_abort(self.root) as q:
                with self.assertRaises(ValueError):self.latch(q)
            p.unlink()
        self.stopped(7,pending=True)
        with r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):self.latch(q)

    def test_release_stage_order_drift_denies_abort(self):
        with patch.object(j,'STAGES',('start-database',)+j.STAGES[1:]),r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):self.latch(q)

    def test_modified_original_after_latch_rejects_every_effect(self):
        with r.open_abort(self.root) as q:
            self.latch(q)
            p=self.root/'000.json';p.write_bytes(p.read_bytes()+b' ')
            with patch.object(r,'boot_identity',return_value=HOST):
                with self.assertRaises(ValueError):q.request_reboot(lambda:self.fail('changed source'))

    def test_incomplete_or_malformed_abort_permanently_blocks_release(self):
        (self.root/'abort').mkdir(mode=0o700)
        with self.assertRaises(ValueError):
            with j.open_journal(self.root):pass
        with r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):q.status()
        p=self.root/'abort/latch.json';p.write_bytes(b'{}');p.chmod(0o600)
        with self.assertRaises(ValueError):
            with j.open_journal(self.root):pass
        with r.open_abort(self.root) as q:
            with self.assertRaises(ValueError):q.status()

    def test_release_and_abort_share_lock_and_reject_closed_handle(self):
        with j.open_journal(self.root):
            with self.assertRaises(BlockingIOError):
                with r.open_abort(self.root):pass
        with r.open_abort(self.root) as q:
            with self.assertRaises(BlockingIOError):
                with j.open_journal(self.root):pass
        with self.assertRaises(ValueError):q.guard()

    def test_reboot_publication_fsync_failures_never_call_effect(self):
        for failure in range(1,4):
            with self.subTest(fsync=failure),tempfile.TemporaryDirectory(dir=self.root) as child:
                with j.open_journal(child) as initial:initial.initialize(PLAN,'b'*32)
                with r.open_abort(child) as q:
                    self.latch(q)
                    real=os.fsync;seen=[];effects=[]
                    def fail(fd):
                        seen.append(fd)
                        if len(seen)==failure:raise OSError('synthetic sync failure')
                        return real(fd)
                    with patch.object(r,'boot_identity',return_value=HOST),patch.object(r.os,'fsync',side_effect=fail):
                        with self.assertRaises(OSError):q.request_reboot(lambda:effects.append(True))
                    self.assertEqual(len(seen),failure);self.assertFalse(effects)
                with r.open_abort(child) as reopened:
                    if failure<3:
                        with self.assertRaises(ValueError):reopened.status()
                    else:
                        # Intent publication was synced before pending unlink.
                        # Fresh admission may observe it, never replay the effect.
                        # The unsynced unlink could reappear after another crash;
                        # such a pending alias conservatively denies admission.
                        self.assertTrue(reopened.status()['newWritesPossible'])
                        with self.assertRaises(ValueError):reopened.request_reboot(lambda:effects.append(True))
                with self.assertRaises(ValueError):
                    with j.open_journal(child):pass

    def test_process_death_after_reboot_intent_does_not_authorize_retry(self):
        with r.open_abort(self.root) as q:self.latch(q)
        code='''import importlib.util,os,sys
from unittest.mock import patch
s=importlib.util.spec_from_file_location('r',sys.argv[1]);r=importlib.util.module_from_spec(s);s.loader.exec_module(r)
with r.open_abort(sys.argv[2]) as q,patch.object(r,'boot_identity',return_value=__import__('json').loads(sys.argv[3])):
 q.request_reboot(lambda:os._exit(23))
'''
        result=subprocess.run([sys.executable,'-c',code,str(SOURCE),str(self.root),json.dumps(HOST)],capture_output=True,timeout=15)
        self.assertEqual(result.returncode,23)
        with r.open_abort(self.root) as q:
            self.assertTrue(q.status()['newWritesPossible'])
            with patch.object(r,'boot_identity',return_value=HOST):
                with self.assertRaises(ValueError):q.request_reboot(lambda:self.fail('replayed'))
                with self.assertRaises(ValueError):q.observe_new_boot()


if __name__=='__main__':
    suite=unittest.defaultTestLoader.loadTestsFromTestCase(AbortTests)
    result=unittest.TextTestRunner(verbosity=2).run(suite)
    if not result.wasSuccessful():sys.exit(1)
    # Controlled source mutations must make the named positive test fail.
    original=r.Abort.observe_new_boot
    text=SOURCE.read_text()
    for before,after,test in [
        (" and host['bootId']!=intent['host']['bootId']",'', 'test_intent_precedes_effect_and_only_fresh_boot_passes'),
        ("host['machineId']==intent['host']['machineId'] and ",'', 'test_foreign_machine_and_original_admission_replacement_denied')]:
        assert text.count(before)==1
        scope={'__file__':str(SOURCE),'__name__':'abort_mutant'}
        exec(compile(text.replace(before,after),str(SOURCE),'exec'),scope)
        # Keep sampler patching attached to the tested module's globals.
        import types
        r.Abort.observe_new_boot=types.FunctionType(scope['Abort'].observe_new_boot.__code__,r.__dict__)
        control=unittest.TextTestRunner(stream=io.StringIO()).run(AbortTests(test))
        r.Abort.observe_new_boot=original
        assert not control.wasSuccessful() and control.failures and not control.errors, test
    print('Two boot/host-boundary source mutations rejected by named controls.')
