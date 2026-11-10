#!/usr/bin/env python3
"""Real durable abort epochs and records; synthetic Docker and host observations."""
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,ROOT/path)
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

c=load('disposable_cleanup','ops/hetzner/original-disposable-cleanup.py')
b=load('cleanup_spec_fixture','scripts/test-disposable-creation-spec.py')
s=load('cleanup_service_journal','ops/hetzner/abort-service-journal.py');a=s.a;j=a.j
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
NEW={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}


class CleanupTests(unittest.TestCase):
    def setUp(self):
        tmp=tempfile.TemporaryDirectory();self.addCleanup(tmp.cleanup)
        self.root=Path(tmp.name).resolve();self.root.chmod(0o700)
        self.prepared=self.root/'prepared';self.prepared.mkdir(mode=0o700)
        self.control=self.root/'control';self.control.mkdir(mode=0o700)
        self.fixture=b.CreationSpecTests();self.fixture.setUp();self.addCleanup(self.fixture.doCleanups)
        clone=self.fixture.physical;app=self.fixture.application
        plan={key:('sha256:'+'2'*64 if key.endswith('Image') else '2'*(40 if key.endswith('Revision') else 64)) for key in j.PLAN_KEYS}
        plan.update(candidateImage=app.image.split('@')[1],sourceRevision=app.revision)
        with j.open_journal(str(self.control)) as journal:journal.initialize(plan,clone.nonce)
        self.admission={'schemaVersion':1,'releaseNonce':clone.nonce,'planHash':j.sha(j.canonical(plan)),
            'host':HOST,'originalDeployment':{'expected':{role:{'containerId':cid,'image':clone.image,'imageId':clone.image_id} for role,cid in b.ORIGINALS.items()}}}
        with j.files.directory(str(self.prepared),private=True) as fd:
            c.r.o.abort.publish(fd,c.r.o.NAME,self.admission)
            c.r.o.abort.publish(fd,c.r.o.RECEIPT,{'schemaVersion':1,'sha256':j.sha(j.canonical(self.admission)),
                'releaseNonce':clone.nonce,'planHash':self.admission['planHash'],'preparedBeforeShutdown':True})
        original=c.r.o.directory_identity
        self.enterContext(patch.object(c.r.o,'directory_identity',side_effect=lambda path:
            b.identity(path) if str(path).startswith('/opt/tdf/backups/') else original(path)))
        writer=c.r.Writer(self.prepared,self.admission,lambda:None)
        writer.publish('physical-database',clone);writer.publish('application-canary',app)
        marker={'schemaVersion':2,'binding':c.r.binding(self.admission),'recoveryImage':clone.image,
                'candidateImage':plan['candidateImage'],'descriptorDirectory':writer.identity}
        self.marker=self.root/c.d.restore.PENDING_NAME
        with j.files.directory(str(self.root),private=True) as fd:c.r.o.abort.publish(fd,self.marker.name,marker)
        with a.open_abort(self.control) as abort,patch.object(a,'boot_identity',return_value=HOST):
            abort.latch(self.admission,a.sha(a.canonical(self.admission)));abort.request_reboot(lambda:None)
        self.abort=self.enterContext(a.open_abort(self.control))
        self.enterContext(patch.object(a,'boot_identity',return_value=NEW))
        self.journal=s.ServiceJournal(self.abort);self.journal.begin_epoch()
        fd=self.enterContext(c.d.restore.rehearsal_lock(self.root));self.reservation=c.d.Reservation(self.root,fd)
        self.adapter=c.OriginalDisposableCleanup(self.journal,self.prepared,self.reservation)
        self.enterContext(patch.object(c.d,'observe',return_value={}))
        self.rows=[{'Id':cid,'Name':'/original-'+role} for role,cid in b.ORIGINALS.items()]
        for row in self.fixture.containers.values():
            row=copy.deepcopy(row);row['State']={'Running':False,'Paused':False};self.rows.append(row)
        self.commands=[]
        self.enterContext(patch.object(c.d.o.sources.inspector,'capture',side_effect=self.capture))
        self.enterContext(patch.object(c.d.o.fence,'execute',side_effect=self.remove))

    def capture(self,command):
        verb=command[len(c.d.o.sources.inspector.DOCKER)]
        if verb=='ps':return '\n'.join(row['Id'] for row in self.rows)
        self.assertEqual(verb,'inspect');self.assertEqual(set(command[len(c.d.o.sources.inspector.DOCKER)+1:]),{row['Id'] for row in self.rows})
        return json.dumps(self.rows)

    def remove(self,command):
        self.assertEqual(command[:-1],c.d.o.sources.inspector.DOCKER+['rm','--force'])
        target=command[-1];self.assertNotIn(target,b.ORIGINALS.values())
        self.commands.append(target);self.rows=[row for row in self.rows if row['Id']!=target]
        return target+'\n'

    def rejected(self):
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertTrue(self.marker.exists());self.assertFalse(self.commands)
        self.assertEqual(self.journal.current()['pendingStage'],'remove-disposables')

    def test_canary_then_database_removed_originals_and_all_records_preserved(self):
        before={p.name:p.read_bytes() for p in (self.prepared/c.r.DIRECTORY).iterdir()}
        state=self.adapter.recover()
        self.assertEqual(self.commands,[b.c.APP,b.c.DB]);self.assertFalse(self.marker.exists())
        self.assertEqual({row['Id'] for row in self.rows},set(b.ORIGINALS.values()))
        self.assertEqual(state['completedStages'],['remove-disposables'])
        self.assertEqual(before,{p.name:p.read_bytes() for p in (self.prepared/c.r.DIRECTORY).iterdir()})
        with self.assertRaises(ValueError):self.adapter.recover()

    def test_already_absent_database_does_not_start_canary_dependency(self):
        self.rows=[row for row in self.rows if row['Id']!=b.c.DB]
        self.adapter.recover();self.assertEqual(self.commands,[b.c.APP])

    def test_both_absent_still_require_complete_inventory_before_marker_release(self):
        self.rows=self.rows[:3];self.adapter.recover();self.assertFalse(self.commands);self.assertFalse(self.marker.exists())

    def test_unknown_extra_stopped_container_blocks_all_removal(self):
        self.rows[-1]={'Id':'f'*64,'Name':'/unowned'};self.rejected()

    def test_wrong_image_is_not_adopted(self):
        self.rows[-1]['Image']='sha256:'+'0'*64;self.rejected()

    def test_missing_original_container_blocks_all_removal(self):
        self.rows=self.rows[1:];self.rejected()

    def test_running_disposable_requires_separate_investigation(self):
        self.rows[-1]['State']['Running']=True;self.rejected()

    def test_legacy_marker_or_partial_descriptor_cannot_authorize_removal(self):
        self.marker.write_bytes(b'{"nonce":"legacy"}');self.rejected()

    def test_partial_hardlink_publication_is_preserved(self):
        (self.root/(self.marker.name+'.pending')).write_bytes(b'partial');self.rejected()

    def test_lost_remove_reply_blocks_same_boot_replay_even_when_container_gone(self):
        def lost(command):self.remove(command);raise TimeoutError('synthetic lost reply')
        with patch.object(c.d.o.fence,'execute',side_effect=lost),self.assertRaises(TimeoutError):self.adapter.recover()
        self.assertEqual(self.commands,[b.c.APP]);self.assertTrue(self.marker.exists())
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[b.c.APP])
        self.journal.request_next_reboot(lambda:None)
        with patch.object(a,'boot_identity',return_value={**NEW,'bootId':'33333333-3333-3333-3333-333333333333'}):
            self.journal.begin_epoch();self.adapter.recover()
        self.assertEqual(self.commands,[b.c.APP,b.c.DB]);self.assertFalse(self.marker.exists())

    def test_partial_descriptor_is_retained_without_removal(self):
        path=self.prepared/c.r.DIRECTORY/'application-canary.json';path.write_bytes(b'{')
        self.rejected();self.assertEqual(path.read_bytes(),b'{')

    def test_changed_original_runtime_rejects_before_any_removal(self):
        with patch.object(c.d,'observe',side_effect=ValueError('changed original configuration')):self.rejected()

    def test_lost_inventory_response_never_means_absence(self):
        with patch.object(c.d.o.sources.inspector,'capture',side_effect=TimeoutError('lost inventory')):
            with self.assertRaises(TimeoutError):self.adapter.recover()
        self.assertFalse(self.commands);self.assertTrue(self.marker.exists())
        with self.assertRaises(ValueError):self.adapter.recover()

    def test_unrecorded_boot_change_never_dispatches(self):
        with patch.object(a,'boot_identity',return_value={**NEW,'bootId':'44444444-4444-4444-4444-444444444444'}),self.assertRaises(ValueError):self.adapter.recover()
        self.assertFalse(self.commands);self.assertTrue(self.marker.exists())


if __name__=='__main__':unittest.main()
