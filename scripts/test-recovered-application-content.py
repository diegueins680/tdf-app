#!/usr/bin/env python3
"""Actual filesystem controls; root-only cases retain real UID1000 metadata."""
import copy
import importlib.util
import inspect
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('recovered_content',Path(__file__).resolve().parent.parent/'ops/hetzner/recovered-application-content.py')
c=importlib.util.module_from_spec(spec);spec.loader.exec_module(c)
BINDING={'sourceRevision':'1'*40,'mobileRevision':'2'*40,'runtimeSha256':'3'*64,
         'migrationManifestSha256':'4'*64,'releaseNonce':'5'*32,'databaseSystemIdentifier':'123456'}


class Clone:
    def __init__(self, directory):
        self.directory=directory;self.data=directory/'physical-data';self.config=directory/'physical-config'
        self.nonce=BINDING['releaseNonce'];self.system_id=BINDING['databaseSystemIdentifier']
        self.reservation_pid=os.getpid();self.target=None;self.creation_attempted=False
        self.start_attempted=False;self.prepared_manifest=None;self.calls=0
    def prepare(self, manifest):
        self.calls+=1;c.verify_tree(self.data,manifest);self.prepared_manifest=manifest
        return {'scope':'filesystem fixture, no PostgreSQL startup'}


class RecoveryContentTests(unittest.TestCase):
    def setUp(self):
        self.temporary=tempfile.TemporaryDirectory();self.addCleanup(self.temporary.cleanup)
        self.root=Path(self.temporary.name).resolve();self.root.chmod(0o700)
    def manifest(self,path):
        with c.files.directory(str(path)) as fd:return c.files.walk(fd)
    def test_subtree_preserves_exact_metadata_and_does_not_match_sibling_prefix(self):
        source=self.root/'source';source.mkdir()
        for name in ('assets','assets-other'):
            child=source/name;child.mkdir();(child/'sentinel').write_bytes(name.encode())
        original=self.manifest(source);before=copy.deepcopy(original)
        self.assertEqual(c.subtree(original,'assets'),self.manifest(source/'assets'))
        self.assertEqual(original,before)
        for invalid in ('','../assets','assets/','missing','assets/sentinel'):
            with self.assertRaises(ValueError):c.subtree(original,invalid)
    def test_move_rejects_changed_bytes_symlink_and_existing_target(self):
        source=self.root/'source';source.mkdir();(source/'sentinel').write_bytes(b'before')
        manifest=self.manifest(source);(source/'sentinel').write_bytes(b'changed')
        with self.assertRaises(ValueError):c.move_verified(source,self.root/'target',manifest)
        self.assertTrue(source.exists());self.assertFalse((self.root/'target').exists())
        manifest=self.manifest(source);target=self.root/'target';target.mkdir()
        with self.assertRaises(ValueError):c.move_verified(source,target,manifest)
        link=self.root/'link';link.symlink_to(source,target_is_directory=True)
        with self.assertRaises(OSError):c.move_verified(link,self.root/'other',manifest)
        c.move_verified(source,self.root/'owned',manifest)
        self.assertEqual(self.manifest(self.root/'owned'),manifest)
    def test_rejected_mount_topology_precedes_any_replay_or_configuration_write(self):
        work=self.root/'clone';work.mkdir(mode=0o700);clone=Clone(work)
        with patch.object(c.physical,'verify_mounts',side_effect=ValueError('synthetic bind alias')) as topology, \
                patch.object(c.bundle,'restore',side_effect=AssertionError('replay occurred before topology admission')) as replay:
            with self.assertRaisesRegex(ValueError,'synthetic bind alias'):
                c.prepare(clone,self.root/'never-read.tar',{},BINDING,legacy_uploads=True)
            topology.assert_called_once();replay.assert_not_called()
        self.assertEqual(list(work.iterdir()),[]);self.assertEqual(clone.calls,0)

    def test_guard_removal_control_fails_on_forbidden_replay(self):
        source=inspect.getsource(c.prepare)
        self.assertEqual(source.count('    physical.verify_mounts()'),1)
        namespace=dict(c.__dict__)
        exec(compile(source.replace('    physical.verify_mounts()', '    pass  # controlled missing topology guard'),
                     '<controlled-missing-topology-guard>', 'exec'),namespace)
        result=unittest.TestResult()
        with patch.object(c,'prepare',namespace['prepare']):
            RecoveryContentTests('test_rejected_mount_topology_precedes_any_replay_or_configuration_write').run(result)
        self.assertEqual(result.testsRun,1);self.assertEqual(result.errors,[])
        self.assertEqual(len(result.failures),1)
        self.assertIn('AssertionError: replay occurred before topology admission',result.failures[0][1])

    def fixture(self,legacy):
        sources={}
        for name in c.bundle.ROLES:
            path=self.root/name;path.mkdir(mode=0o700);sources[name]=str(path)
        production=self.root/'production'
        for path in (production/'assets', self.root/'legacy-uploads' if legacy else production/'uploads'):
            path.mkdir(mode=0o700,exist_ok=True);os.chown(path,1000,1000)
            sentinel=path/'sentinel';sentinel.write_bytes(path.name.encode());sentinel.chmod(0o600);os.chown(sentinel,1000,1000)
        (self.root/'database'/'synthetic-db-file').write_bytes(b'synthetic-cold-data')
        output=self.root/'retrieved-decrypted.tar'
        receipt=c.bundle.capture(sources,str(self.root/'captured'),str(output),BINDING)
        work=self.root/'clone';work.mkdir(mode=0o700)
        return Clone(work),output,receipt
    @unittest.skipUnless(os.geteuid()==0,'Real UID1000 metadata case requires owned Linux root fixture')
    def test_whole_bundle_replay_preserves_both_legacy_and_persistent_content(self):
        # Each variant uses an independent complete bundle and disposable root.
        for legacy in (True,False):
            with self.subTest(legacy=legacy),tempfile.TemporaryDirectory(dir=self.root) as temporary:
                previous=self.root;self.root=Path(temporary)
                try:
                    clone,archive,receipt=self.fixture(legacy)
                    result=c.prepare(clone,archive,receipt,BINDING,legacy_uploads=legacy)
                    self.assertEqual(clone.calls,1)
                    self.assertEqual((clone.data/'synthetic-db-file').read_bytes(),b'synthetic-cold-data')
                    for name,manifest in result['contentManifests'].items():
                        self.assertEqual(self.manifest(clone.directory/('canary-'+name)),manifest)
                    self.assertFalse(result['offHostCustodyVerifiedByHelper'])
                    self.assertFalse(result['databaseStarted'])
                    with self.assertRaises(ValueError):c.prepare(clone,archive,receipt,BINDING,legacy_uploads=legacy)
                finally:self.root=previous
    @unittest.skipUnless(os.geteuid()==0,'Real UID1000 metadata case requires owned Linux root fixture')
    def test_binding_reservation_and_content_tamper_reject_without_clone_configuration(self):
        clone,archive,receipt=self.fixture(True)
        wrong=dict(BINDING,releaseNonce='0'*32)
        with self.assertRaises(ValueError):c.prepare(clone,archive,receipt,wrong,legacy_uploads=True)
        self.assertFalse((clone.directory/'recovered-bundle').exists())
        clone.reservation_pid=-1
        with self.assertRaises(ValueError):c.prepare(clone,archive,receipt,BINDING,legacy_uploads=True)
        clone.reservation_pid=os.getpid()
        with archive.open('r+b') as output:output.seek(512);output.write(b'corrupt')
        with self.assertRaises(ValueError):c.prepare(clone,archive,receipt,BINDING,legacy_uploads=True)
        self.assertFalse(clone.data.exists());self.assertEqual(clone.calls,0)


if __name__=='__main__':unittest.main()
