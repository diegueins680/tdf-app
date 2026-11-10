#!/usr/bin/env python3
"""Real immutable publications; release/host admission is a synthetic fixture."""
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
    value=importlib.util.module_from_spec(spec);spec.loader.exec_module(value);return value

r=load('durable_records','ops/hetzner/durable-disposable-records.py')
b=load('durable_records_fixture','scripts/test-disposable-creation-spec.py')


class RecordTests(unittest.TestCase):
    def setUp(self):
        temporary=tempfile.TemporaryDirectory();self.addCleanup(temporary.cleanup)
        self.root=Path(temporary.name).resolve();self.root.chmod(0o700)
        self.fixture=b.CreationSpecTests();self.fixture.setUp();self.addCleanup(self.fixture.doCleanups)
        self.admission={'schemaVersion':1,'releaseNonce':b.c.NONCE,'planHash':'2'*64,
            'originalDeployment':{'expected':{role:{'containerId':cid} for role,cid in b.ORIGINALS.items()}}}
        with r.o.abort.j.files.directory(str(self.root),private=True) as fd:
            r.o.abort.publish(fd,r.o.NAME,self.admission)
            r.o.abort.publish(fd,r.o.RECEIPT,{'schemaVersion':1,'sha256':r.sha(r.canonical(self.admission)),
                'releaseNonce':self.admission['releaseNonce'],'planHash':self.admission['planHash'],
                'preparedBeforeShutdown':True})
        original=r.o.directory_identity
        self.enterContext(patch.object(r.o,'directory_identity',side_effect=lambda path:
            b.identity(path) if str(path).startswith('/opt/tdf/backups/') else original(path)))
        self.allowed=True;self.guards=0
        def guard():
            self.guards+=1;r.require(self.allowed)
        self.guard=guard
        self.writer=r.Writer(self.root,self.admission,guard)

    def physical(self):return self.writer.publish('physical-database',self.fixture.physical)

    def test_round_trip_private_canonical_records_and_dependency_binding(self):
        self.physical();self.writer.publish('application-canary',self.fixture.application)
        rows=r.read_records(self.root,self.admission)
        self.assertEqual(set(rows),set(r.s.ROLES))
        self.assertEqual(rows['application-canary']['physicalDescriptorSha256'],r.sha(r.canonical(rows['physical-database'])))
        for name in r.s.ROLES:
            path=self.writer.directory/(name+'.json')
            self.assertEqual(path.stat().st_mode & 0o777,0o600)
            self.assertEqual(path.read_bytes(),r.canonical(rows[name]))
        self.assertGreater(self.guards,5)

    def test_duplicate_publish_never_replaces_existing_record(self):
        self.physical();path=self.writer.directory/'physical-database.json';before=path.read_bytes()
        with self.assertRaises(ValueError):self.physical()
        self.assertEqual(path.read_bytes(),before)

    def test_canary_cannot_precede_physical_record(self):
        with self.assertRaises(ValueError):self.writer.publish('application-canary',self.fixture.application)
        self.assertEqual(list(self.writer.directory.iterdir()),[])

    def test_changed_guard_prevents_publication(self):
        self.allowed=False
        with self.assertRaises(ValueError):self.physical()
        self.assertEqual(list(self.writer.directory.iterdir()),[])

    def test_failed_publication_is_preserved_and_writer_cannot_retry(self):
        publish=r.o.abort.publish
        def failed(*args):publish(*args);raise OSError('synthetic uncertain synchronization')
        with patch.object(r.o.abort,'publish',side_effect=failed):
            with self.assertRaises(OSError):self.physical()
        self.assertTrue((self.writer.directory/'physical-database.json').exists())
        with self.assertRaises(ValueError):self.physical()
        self.assertTrue(self.writer.closed)

    def test_existing_record_directory_is_never_adopted_by_new_writer(self):
        with self.assertRaises(FileExistsError):r.Writer(self.root,self.admission,self.guard)

    def test_other_original_admission_or_plan_is_not_accepted(self):
        self.physical()
        for key in ['releaseNonce','planHash']:
            wrong=copy.deepcopy(self.admission);wrong[key]='f'*len(wrong[key])
            with self.assertRaises(ValueError):r.read_records(self.root,wrong)

    def test_truncated_unknown_or_noncanonical_records_fail_closed(self):
        self.physical();path=self.writer.directory/'physical-database.json';original=path.read_bytes()
        for broken in [b'{',b' '+original,original.replace(b'"schemaVersion":1',b'"schemaVersion":2',1)]:
            path.write_bytes(broken)
            with self.assertRaises((ValueError,json.JSONDecodeError)):r.read_records(self.root,self.admission)
        path.write_bytes(original)
        (self.writer.directory/'unknown.json').write_bytes(b'{}')
        with self.assertRaises(ValueError):r.read_records(self.root,self.admission)

    def test_replaced_record_directory_or_symlink_is_not_adopted(self):
        old=self.writer.directory.with_name('preserved-original')
        self.writer.directory.rename(old);self.writer.directory.mkdir(mode=0o700)
        with self.assertRaises(ValueError):self.physical()
        self.writer.directory.rmdir();self.writer.directory.symlink_to(old,target_is_directory=True)
        with self.assertRaises((ValueError,OSError)):r.read_records(self.root,self.admission)

    def test_other_rehearsal_nonce_cannot_be_published_under_release_binding(self):
        self.fixture.physical.nonce='e'*32
        self.fixture.physical.directory=Path('/opt/tdf/backups/rehearsal-'+'e'*32)
        self.fixture.physical.data=self.fixture.physical.directory/'physical-data'
        self.fixture.physical.config=self.fixture.physical.directory/'physical-config'
        with self.assertRaises(ValueError):self.physical()
        self.assertEqual(list(self.writer.directory.iterdir()),[])

    def test_canary_dependency_hash_must_match_the_physical_record(self):
        self.physical();self.writer.publish('application-canary',self.fixture.application)
        path=self.writer.directory/'application-canary.json'
        row=json.loads(path.read_bytes());row['physicalDescriptorSha256']='0'*64
        path.write_bytes(r.canonical(row))
        with self.assertRaises(ValueError):r.read_records(self.root,self.admission)


if __name__=='__main__':unittest.main(verbosity=2)
