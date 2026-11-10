#!/usr/bin/env python3
"""Real journals, locks and immutable records; Docker inventory is synthetic."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,ROOT/path)
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

c=load('coordinated_creation','ops/hetzner/coordinated-disposable-creation.py')
b=load('creation_fixture','scripts/test-disposable-creation-spec.py')
j=c.o.abort.j


class CoordinatedTests(unittest.TestCase):
    def setUp(self):
        temp=tempfile.TemporaryDirectory();self.addCleanup(temp.cleanup)
        self.root=Path(temp.name).resolve();self.root.chmod(0o700)
        self.prepared=self.root/'prepared';self.prepared.mkdir(mode=0o700)
        self.backups=self.root/'backups';self.backups.mkdir(mode=0o700)
        control=self.root/'control';control.mkdir(mode=0o700)
        self.fixture=b.CreationSpecTests();self.fixture.setUp();self.addCleanup(self.fixture.doCleanups)
        self.clone=self.fixture.physical
        self.plan={key:('sha256:'+'2'*64 if key.endswith('Image') else '2'*(40 if key.endswith('Revision') else 64)) for key in j.PLAN_KEYS}
        self.plan.update(candidateImage=self.fixture.application.image.split('@')[1],sourceRevision=self.fixture.application.revision)
        self.journal=self.enterContext(j.open_journal(str(control)));self.journal.initialize(self.plan,self.clone.nonce)
        first=self.journal.records()[0]
        self.admission={'schemaVersion':1,'releaseNonce':first['releaseNonce'],'planHash':first['planHash'],
            'originalDeployment':{'expected':{role:{'containerId':cid,'image':self.clone.image,'imageId':self.clone.image_id} for role,cid in b.ORIGINALS.items()}}}
        with j.files.directory(str(self.prepared),private=True) as fd:
            c.o.abort.publish(fd,c.o.NAME,self.admission)
            c.o.abort.publish(fd,c.o.RECEIPT,{'schemaVersion':1,'sha256':c.sha(c.canonical(self.admission)),
                'releaseNonce':first['releaseNonce'],'planHash':first['planHash'],'preparedBeforeShutdown':True})
        original=c.o.directory_identity
        self.enterContext(patch.object(c.o,'directory_identity',side_effect=lambda path:
            b.identity(path) if str(path).startswith('/opt/tdf/backups/') else original(path)))
        self.enterContext(patch.object(c,'HOST_ROOT',self.backups))
        self.fd=self.enterContext(c.d.restore.rehearsal_lock(self.backups))
        self.creation=c.CoordinatedCreation(self.journal,self.prepared);self.addCleanup(self.creation.close)

    def reserve(self):
        self.creation.reserve(self.clone,self.fd);self.clone.reservation_pid=os.getpid()

    def at_restore(self,effect):
        for stage in j.STAGES[:j.STAGES.index('restore-isolate')]:
            self.journal.perform(stage,'a'*64,lambda ctx:{**ctx,'evidenceHash':'b'*64})
        return self.journal.perform('restore-isolate','a'*64,lambda ctx:(effect() or {**ctx,'evidenceHash':'b'*64}))

    def test_reservation_binds_plan_originals_and_private_marker_before_creation(self):
        self.reserve();marker=self.backups/c.d.restore.PENDING_NAME
        value=json.loads(marker.read_bytes())
        self.assertEqual(value['schemaVersion'],2)
        self.assertEqual(value['binding']['originalContainerIds'],b.ORIGINALS)
        self.assertEqual(marker.stat().st_mode&0o777,0o600)
        def effect():
            receipt=self.creation.publish('physical-database',self.clone)
            self.assertTrue(receipt['publishedBeforeCreation'])
        self.at_restore(effect)

    def test_wrong_stage_and_closed_or_replaced_lock_deny_publication(self):
        self.reserve()
        with self.assertRaises(ValueError):self.creation.publish('physical-database',self.clone)
        self.assertEqual(list(self.creation.writer.directory.iterdir()),[])
        lock=self.backups/'restore-rehearsal.lock';lock.rename(self.backups/'held-lock')
        lock.write_bytes(b'');lock.chmod(0o600)
        with self.assertRaises(ValueError):self.creation.marker_guard()
        self.creation.close()
        with self.assertRaises(ValueError):self.creation.marker_guard()

    def test_changed_original_image_denies_before_records_or_marker(self):
        self.clone.image='pgvector/pgvector@sha256:'+'f'*64
        with self.assertRaises(ValueError):self.reserve()
        self.assertFalse((self.backups/c.d.restore.PENDING_NAME).exists())
        self.assertFalse((self.prepared/c.r.DIRECTORY).exists())

    def test_legacy_marker_is_never_adopted_or_replaced(self):
        marker=self.backups/c.d.restore.PENDING_NAME;marker.write_bytes(b'{"nonce":"legacy"}');marker.chmod(0o600)
        with self.assertRaises(FileExistsError):self.reserve()
        self.assertEqual(marker.read_bytes(),b'{"nonce":"legacy"}')
        self.assertTrue(self.creation.closed)

    def test_canary_requires_actual_descriptor_bound_database_and_planned_image(self):
        self.reserve()
        def effect():
            self.creation.publish('physical-database',self.clone)
            app=self.fixture.application
            wrong=copy.deepcopy(self.fixture.containers['physical-database']);wrong['Image']='sha256:'+'f'*64
            with patch.object(c.d.restore,'execute',return_value=json.dumps([wrong])):
                with self.assertRaises(ValueError):self.creation.publish('application-canary',app)
            with patch.object(c.d.restore,'execute',return_value=json.dumps([self.fixture.containers['physical-database']])):
                self.creation.publish('application-canary',app)
        self.at_restore(effect)
        self.assertEqual(set(c.r.read_records(self.prepared,self.admission)),set(c.r.s.ROLES))

    def inventory(self,extra=None):
        rows=[{'Id':cid,'Name':'/original-'+role,'Config':{'Labels':{}}} for role,cid in b.ORIGINALS.items()]
        if extra:rows.append(extra)
        def execute(command,**_):
            if command[len(c.d.restore.DOCKER)]=='ps':return '\n'.join(row['Id'] for row in rows)
            return json.dumps(rows)
        return execute

    def test_marker_released_only_after_complete_absence_inventory(self):
        self.reserve()
        with patch.object(c.d.restore,'execute',side_effect=self.inventory()):self.creation.release(self.clone)
        self.assertFalse((self.backups/c.d.restore.PENDING_NAME).exists());self.assertTrue(self.creation.closed)
        self.assertTrue(self.creation.writer.directory.exists())

    def test_surviving_nonce_container_or_failed_inventory_retains_marker(self):
        self.reserve();marker=self.backups/c.d.restore.PENDING_NAME
        extra={'Id':'f'*64,'Name':'/renamed','Config':{'Labels':{'net.tdf.application-canary':self.clone.nonce}}}
        with patch.object(c.d.restore,'execute',side_effect=self.inventory(extra)),self.assertRaises(ValueError):self.creation.release(self.clone)
        with patch.object(c.d.restore,'execute',side_effect=ValueError('uncertain inventory')),self.assertRaises(ValueError):self.creation.release(self.clone)
        self.assertTrue(marker.exists())

    def test_uncertain_record_publication_cannot_release_marker(self):
        self.reserve();publish=c.o.abort.publish
        def uncertain(*args):publish(*args);raise OSError('synthetic fsync uncertainty')
        def effect():
            with patch.object(c.o.abort,'publish',side_effect=uncertain),self.assertRaises(OSError):self.creation.publish('physical-database',self.clone)
        self.at_restore(effect)
        with patch.object(c.d.restore,'execute') as execute,self.assertRaises(ValueError):self.creation.release(self.clone)
        execute.assert_not_called();self.assertTrue((self.backups/c.d.restore.PENDING_NAME).exists())


if __name__=='__main__':unittest.main()
