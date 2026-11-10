#!/usr/bin/env python3
"""Real journals/archives with synthetic host observations; no lifecycle effects."""
import copy
from contextlib import ExitStack
import hashlib
import importlib.util
import inspect
import json
import os
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import Mock,patch

spec=importlib.util.spec_from_file_location('coordinated_capture',Path(__file__).resolve().parent.parent/'ops/hetzner/coordinated-capture.py')
c=importlib.util.module_from_spec(spec);spec.loader.exec_module(c)
journal=c.load('capture_test_journal','release-journal.py')
BINDING={'sourceRevision':'1'*40,'mobileRevision':'2'*40,'runtimeSha256':'3'*64,
         'migrationManifestSha256':'4'*64,'releaseNonce':'5'*32,'databaseSystemIdentifier':'123456'}
PLAN={'sourceRevision':BINDING['sourceRevision'],'mobileRevision':BINDING['mobileRevision'],
      'runtimeHash':BINDING['runtimeSha256'],'manifestHash':BINDING['migrationManifestSha256'],
      'candidateImage':'sha256:'+'6'*64,'recoveryImage':'sha256:'+'7'*64,'recipientHash':'8'*64}


class CaptureTests(unittest.TestCase):
    def setUp(self):
        self.stack=ExitStack();self.addCleanup(self.stack.close)
        self.root=Path(self.stack.enter_context(tempfile.TemporaryDirectory())).resolve();self.root.chmod(0o700)
    def directory(self,name):
        path=self.root/name;path.mkdir(mode=0o700);return path
    def manifest(self,path):
        with c.files.directory(str(path)) as fd:return c.files.walk(fd)
    def fixture(self,*,legacy=True,present=False):
        j=self.stack.enter_context(journal.open_journal(str(self.directory('journal'))))
        j.initialize(PLAN,BINDING['releaseNonce'])
        for stage in journal.STAGES[:3]:j.perform(stage,'9'*64,lambda context:{**context,'evidenceHash':'a'*64})
        roots={name:str(self.directory(name)) for name in ('database','production','edge-data','edge-config')}
        (Path(roots['database'])/'sentinel').write_bytes(b'synthetic stopped database')
        units=self.directory('units');hashes={}
        for name in (c.schedulers.TDF_SERVICE,c.schedulers.TDF_TIMER):
            path=units/name;path.write_bytes(b'synthetic '+name.encode());path.chmod(0o644)
            hashes[name]=hashlib.sha256(path.read_bytes()).hexdigest()
        source=self.directory('container-root');(source/'app').mkdir(mode=0o700)
        if present:
            (source/'app/uploads').mkdir(mode=0o700)
            (source/'app/uploads/sentinel').write_bytes(b'private synthetic upload')
            os.chown(source/'app/uploads',1000,1000);os.chown(source/'app/uploads/sentinel',1000,1000)
        root_fd=os.open(source,os.O_RDONLY|os.O_DIRECTORY);self.stack.callback(os.close,root_fd)
        retained=SimpleNamespace(target='b'*64,root_fd=root_fd,guard=Mock(),inspect=Mock())
        def capture_uploads(destination):
            path=source/'app/uploads'
            return {'sourceContainer':'b'*64,'presence':'present' if path.exists() else 'absent',
                    'manifest':c.files.capture(str(path),destination) if path.exists() else None}
        retained.capture_uploads=capture_uploads
        observed={'sources':{'dockerWritersStopped':True,'legacyUploads':legacy,'roots':roots,
                             'runtimeConfigurationSha256':BINDING['runtimeSha256']},
                  'units':{'timerStopped':True,'backupServiceInactive':True}}
        fence=SimpleNamespace(journal=j,expected={'db':{'containerId':'a'*64},'api':{'containerId':'b'*64},
                              'edge':{'containerId':'c'*64}},unit_hashes=hashes,legacy_root=retained,observe=Mock(return_value=observed))
        clone=SimpleNamespace(directory=self.directory('clone'),reservation_pid=os.getpid(),target=None,
                creation_attempted=False,start_attempted=False,nonce=BINDING['releaseNonce'],
                source='a'*64,system_id=BINDING['databaseSystemIdentifier'])
        obj=c.Capture(fence,clone,BINDING,{},{});read_text=c.Path.read_text
        self.stack.enter_context(patch.object(c,'UNIT_DIRECTORY',units))
        self.stack.enter_context(patch.object(c.Path,'read_text',lambda path,*a,**kw:
            '1 0 8:1 / / rw - ext4 /dev/root rw\n' if str(path)=='/proc/self/mountinfo' else read_text(path,*a,**kw)))
        self.stack.enter_context(patch.object(c.schedulers,'observe'))
        self.stack.enter_context(patch.object(c.processes,'observe'))
        self.stack.enter_context(patch.object(c.capacity,'observe',return_value={'scope':'synthetic admitted capacity'}))
        return obj,source,observed
    def test_persistent_uploads_do_not_imply_legacy_contract_absence(self):
        obj,source,_=self.fixture(legacy=False)
        self.assertIsNone(obj.legacy_manifest(False))
        store=source/'app/contracts/store';store.mkdir(parents=True)
        (store/'contract.json').write_bytes(b'uncaptured synthetic contract')
        with self.assertRaises(ValueError):obj.legacy_manifest(False)
        self.assertEqual((store/'contract.json').read_bytes(),b'uncaptured synthetic contract')
        obj.fence.legacy_root=None
        with self.assertRaises(ValueError):obj.legacy_manifest(False)

    def test_mount_aliases_of_actual_sources_reject(self):
        roots={'database':'/var/lib/docker/volumes/example/_data','production':'/opt/tdf/production',
               'edge-data':'/var/lib/docker/volumes/edge/_data','edge-config':'/var/lib/docker/volumes/config/_data'}
        base='1 0 8:1 / / rw - ext4 /dev/root rw\n'
        c.source_mounts(base,roots)
        for path in ('/var/lib/docker','/opt/tdf/production','/opt/tdf/production/assets'):
            with self.assertRaises(ValueError):c.source_mounts(base+f'2 1 8:1 / {path} rw - ext4 /dev/root rw\n',roots)
    def test_outer_bound_covers_actual_pax_archives(self):
        roots={name:str(self.directory(name)) for name in c.bundle.ROLES}
        for path in roots.values():
            parent=Path(path)
            for i in range(3):parent=parent/('a'*180+str(i));parent.mkdir()
            (parent/('é'*70)).write_bytes(b'PAX long path test')
        bound,entries=c.upper_bound([self.manifest(path) for path in roots.values()])
        receipt=c.bundle.capture(roots,str(self.root/'components'),str(self.root/'archive.tar'),BINDING)
        self.assertLess(receipt['archive']['bytes'],bound);self.assertEqual(entries,30)
        with self.assertRaises(ValueError):c.upper_bound([])
        too_big=[self.manifest(path) for path in roots.values()]
        too_big[0]['entries'][-1]['bytes']=c.capacity.MAX_BUNDLE;too_big[0]['bytes']=c.capacity.MAX_BUNDLE
        with self.assertRaises(ValueError):c.upper_bound(too_big)
    def test_wrong_authority_rejects_before_any_capture_intent(self):
        obj,_,_=self.fixture()
        for attr,value in (('reservation_pid',-1),('target','d'*64),('nonce','e'*32),
                           ('source','e'*64),('system_id','999999')):
            before=getattr(obj.clone,attr);setattr(obj.clone,attr,value)
            with self.assertRaises(ValueError):obj.guard()
            setattr(obj.clone,attr,before)
        obj.binding['runtimeSha256']='0'*64
        with self.assertRaises(ValueError):obj.guard()
        self.assertEqual(list(obj.clone.directory.iterdir()),[])
        self.assertIsNone(obj.fence.journal.status()['pendingStage'])
    def test_matching_plan_cannot_mislabel_a_different_observed_runtime(self):
        obj,_,observed=self.fixture()
        observed['sources']['runtimeConfigurationSha256']='0'*64
        with self.assertRaises(ValueError):obj.capture()
        self.assertEqual(list(obj.clone.directory.iterdir()),[])
        self.assertIsNone(obj.fence.journal.status()['pendingStage'])
    def test_denied_scheduler_precedes_capture_effect(self):
        obj,_,_=self.fixture()
        with patch.object(c.schedulers,'observe',side_effect=ValueError('unknown scheduler')):
            with self.assertRaisesRegex(ValueError,'unknown scheduler'):obj.capture()
        self.assertEqual(list(obj.clone.directory.iterdir()),[])
        self.assertIsNone(obj.fence.journal.status()['pendingStage'])
    def test_capacity_failure_precedes_intent_and_staging(self):
        obj,_,_=self.fixture()
        with patch.object(c.capacity,'observe',side_effect=ValueError('capacity denied')):
            with self.assertRaisesRegex(ValueError,'capacity denied'):obj.capture()
        self.assertEqual(list(obj.clone.directory.iterdir()),[])
        self.assertIsNone(obj.fence.journal.status()['pendingStage'])
    def test_unit_set_rejects_before_staging(self):
        obj,_,_=self.fixture();obj.fence.unit_hashes.pop(c.schedulers.TDF_TIMER)
        with self.assertRaises(ValueError):obj.stage_units()
        self.assertFalse(obj.units.exists())
    @unittest.skipUnless(os.geteuid()==0 and hasattr(os,'setxattr'),'Actual root unit xattrs require Linux fixture')
    def test_unit_extended_metadata_is_rejected_without_normalization(self):
        obj,_,_=self.fixture()
        os.setxattr(c.UNIT_DIRECTORY/c.schedulers.TDF_SERVICE,'user.tdf-test',b'unsupported metadata')
        with self.assertRaises(ValueError):obj.capture()
        self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture')
        self.assertFalse(obj.receipt_path.exists());self.assertFalse(obj.output.exists())
    @unittest.skipUnless(os.geteuid()==0 and hasattr(os,'setxattr'),'Actual root unit xattrs require Linux fixture')
    def test_unit_metadata_guard_mutation_is_detected(self):
        import textwrap
        mutations={}
        for name in ('stage_units','verify_units'):
            source=textwrap.dedent(inspect.getsource(getattr(c.Capture,name)))
            self.assertEqual(source.count('files.no_extended_attributes(fd)'),1)
            # Both source admissions independently reject extended metadata.
            # Disable that entire boundary while keeping ordinary archive checks.
            exec(compile(source.replace('files.no_extended_attributes(fd)','pass  # controlled mutation'),
                         '<unit-metadata-guard-mutation>','exec'),c.__dict__)
            mutations[name]=getattr(c,name)
        result=unittest.TestResult()
        with patch.multiple(c.Capture,**mutations):
            CaptureTests('test_unit_extended_metadata_is_rejected_without_normalization').run(result)
        self.assertEqual(result.testsRun,1);self.assertEqual(result.errors,[])
        self.assertEqual(len(result.failures),1)
        self.assertIn('ValueError not raised',result.failures[0][1])
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_complete_capture_has_durable_receipt_and_exact_legacy_metadata(self):
        for legacy,present in ((True,False),(True,True),(False,False)):
            with self.subTest(legacy=legacy,present=present),tempfile.TemporaryDirectory(dir=self.root) as temp:
                previous=self.root;self.root=Path(temp)
                try:
                    obj,source,_=self.fixture(legacy=legacy,present=present)
                    result=obj.capture();raw=obj.receipt_path.read_bytes()
                    self.assertEqual(raw,c.bundle.canonical(result))
                    self.assertEqual(obj.receipt_path.stat().st_mode&0o777,0o600)
                    self.assertEqual(obj.fence.journal.records()[-1]['event']['evidenceHash'],hashlib.sha256(raw).hexdigest())
                    self.assertIsNone(obj.fence.journal.status()['pendingStage'])
                    restored=self.root/'restored'
                    c.bundle.restore(str(obj.output),result['bundle'],BINDING,str(restored))
                    if present:self.assertEqual(self.manifest(restored/'legacy-uploads'),self.manifest(source/'app/uploads'))
                    else:
                        info=(restored/'legacy-uploads').stat()
                        self.assertEqual((info.st_uid,info.st_gid,info.st_mode&0o777),(1000,1000,0o700))
                        self.assertEqual(list((restored/'legacy-uploads').iterdir()),[])
                    for name in obj.fence.unit_hashes:
                        info=(restored/'host-units'/name).stat()
                        self.assertEqual((info.st_uid,info.st_mode&0o777),(0,0o644))
                    for key in ('encryptionVerified','offHostVerified','databaseRecoveryVerified'):self.assertFalse(result[key])
                    with self.assertRaises(ValueError):obj.capture()
                finally:self.root=previous
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_receipt_sync_failure_preserves_pending_intent_and_archive(self):
        obj,_,_=self.fixture();write=c.bundle.write_index
        def fail(path,value):
            write(path,value)
            if path==obj.receipt_path:raise OSError('synthetic receipt directory fsync failure')
        with patch.object(c.bundle,'write_index',side_effect=fail):
            with self.assertRaisesRegex(OSError,'receipt directory fsync'):obj.capture()
        self.assertTrue(obj.output.exists());self.assertTrue(obj.receipt_path.exists())
        self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture')
        with self.assertRaises(ValueError):obj.capture()
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_mutated_source_never_completes(self):
        obj,_,observed=self.fixture();capture=c.bundle.capture
        def race(*args):
            result=capture(*args)
            (Path(observed['sources']['roots']['database'])/'sentinel').write_bytes(b'late changed bytes')
            return result
        with patch.object(c.bundle,'capture',side_effect=race),self.assertRaises(ValueError):obj.capture()
        self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture');self.assertFalse(obj.receipt_path.exists())
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_late_process_denial_keeps_durable_receipt_but_no_observation(self):
        obj,_,_=self.fixture()
        # Initial, before effect, after archive, after durable private receipt.
        with patch.object(c.processes,'observe',side_effect=[None,None,None,ValueError('unknown process')]):
            with self.assertRaisesRegex(ValueError,'unknown process'):obj.capture()
        self.assertTrue(obj.receipt_path.exists());self.assertTrue(obj.output.exists())
        self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture')
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_late_legacy_or_unit_change_cannot_complete_capture(self):
        for kind in ('legacy-appears','legacy-bytes','unit-metadata'):
            with self.subTest(kind=kind),tempfile.TemporaryDirectory(dir=self.root) as temp:
                previous=self.root;self.root=Path(temp)
                try:
                    obj,source,_=self.fixture(present=kind=='legacy-bytes');capture=c.bundle.capture
                    def race(*args):
                        result=capture(*args)
                        if kind=='legacy-appears':(source/'app/uploads').mkdir(mode=0o700)
                        elif kind=='legacy-bytes':(source/'app/uploads/sentinel').write_bytes(b'late change')
                        else:os.utime(c.UNIT_DIRECTORY/c.schedulers.TDF_TIMER,ns=(1000000000,1000000000))
                        return result
                    with patch.object(c.bundle,'capture',side_effect=race),self.assertRaises(ValueError):obj.capture()
                    self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture')
                    self.assertFalse(obj.receipt_path.exists())
                finally:self.root=previous
    @unittest.skipUnless(os.geteuid()==0,'Actual UID1000 staging requires owned Linux root fixture')
    def test_absence_becoming_present_cannot_be_recast_as_empty(self):
        obj,source,_=self.fixture();stage=obj.stage_units
        def race():
            stage();(source/'app/uploads').mkdir(mode=0o700)
        with patch.object(obj,'stage_units',side_effect=race),self.assertRaises(ValueError):obj.capture()
        self.assertEqual(obj.fence.journal.status()['pendingStage'],'capture');self.assertFalse(obj.output.exists())


if __name__=='__main__':unittest.main()
