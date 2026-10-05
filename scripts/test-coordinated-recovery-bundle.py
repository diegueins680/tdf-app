#!/usr/bin/env python3
"""Actual private multi-tree archives and adversarial replay; synthetic data only."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('bundle', Path(__file__).resolve().parent.parent/
                                            'ops/hetzner/coordinated-recovery-bundle.py')
b = importlib.util.module_from_spec(spec); spec.loader.exec_module(b)
BINDING = {'sourceRevision':'1'*40,'mobileRevision':'2'*40,'runtimeSha256':'3'*64,
           'migrationManifestSha256':'4'*64,'releaseNonce':'5'*32,'databaseSystemIdentifier':'123456'}


class BundleTests(unittest.TestCase):
    def setUp(self):
        self.temporary=tempfile.TemporaryDirectory();self.addCleanup(self.temporary.cleanup)
        self.root=Path(self.temporary.name).resolve();self.root.chmod(0o700)
        self.sources={}
        for i,name in enumerate(b.ROLES):
            p=self.root/name;p.mkdir(mode=0o700);(p/'sentinel').write_bytes(bytes(range(256))*(i+1))
            (p/'sentinel').chmod(0o600);(p/'empty').mkdir(mode=0o700)
            self.sources[name]=str(p)
        self.archive=self.root/'bundle.tar';self.workspace=self.root/'captured'

    def capture(self):
        return b.capture(self.sources,str(self.workspace),str(self.archive),BINDING)

    def test_all_components_restore_exact_metadata_and_binding(self):
        receipt=self.capture(); result=b.restore(str(self.archive),receipt,BINDING,str(self.root/'restored'))
        self.assertEqual(set(result['components']),set(b.ROLES))
        for name in b.ROLES:
            with b.files.directory(self.sources[name]) as source,b.files.directory(str(self.root/'restored'/name)) as target:
                self.assertEqual(b.files.walk(source),b.files.walk(target))
        for name in ('databaseRecoveryVerified','secretsUsable','offHost'):self.assertIs(result[name],False)
        with self.assertRaises(FileExistsError):b.restore(str(self.archive),receipt,BINDING,str(self.root/'restored'))
        with self.assertRaises(FileExistsError):self.capture()

    def test_missing_extra_or_overlapping_sources_reject_before_output(self):
        candidates=[]
        missing=dict(self.sources);missing.pop('host-units');candidates.append(missing)
        extra=dict(self.sources,unexpected=str(self.root));candidates.append(extra)
        alias=dict(self.sources);alias['production']=alias['database'];candidates.append(alias)
        nested=dict(self.sources);nested['production']=str(Path(nested['database'])/'empty');candidates.append(nested)
        double_slash=dict(nested);double_slash['production']='/'+double_slash['production'];candidates.append(double_slash)
        self.assertTrue(Path(double_slash['production']).samefile(Path(self.sources['database'])/'empty'))
        for sources in candidates:
            with self.subTest(sources=list(sources)),self.assertRaises(ValueError):
                b.capture(sources,str(self.workspace),str(self.archive),BINDING)
            self.assertFalse(self.workspace.exists());self.assertFalse(self.archive.exists())
        with self.assertRaises(ValueError):
            b.capture(self.sources,str(self.workspace),str(self.workspace/'recursive.tar'),BINDING)
        self.assertFalse(self.workspace.exists())
        with self.assertRaises(ValueError):
            b.capture(self.sources,'/'+str(Path(self.sources['database'])/'new-work'),str(self.archive),BINDING)
        self.assertFalse((Path(self.sources['database'])/'new-work').exists())

    def test_binding_and_raw_archive_change_reject_before_replay(self):
        receipt=self.capture()
        for name in BINDING:
            wrong=dict(BINDING);wrong[name]='6'*len(wrong[name])
            with self.subTest(name=name),self.assertRaises(ValueError):
                b.restore(str(self.archive),receipt,wrong,str(self.root/'never'))
            self.assertFalse((self.root/'never').exists())
        with self.archive.open('r+b') as stream:stream.seek(512);stream.write(b'broken')
        with self.assertRaises(ValueError):b.restore(str(self.archive),receipt,BINDING,str(self.root/'never'))
        self.assertFalse((self.root/'never').exists())

    def test_cross_component_change_prevents_successful_bundle(self):
        capture=b.files.capture_directory_fd
        count=0
        def racing(fd,destination):
            nonlocal count
            manifest=capture(fd,destination);count+=1
            if count==len(b.ROLES):
                (Path(self.sources['database'])/'sentinel').write_bytes(b'changed after earlier capture')
            return manifest
        with patch.object(b.files,'capture_directory_fd',side_effect=racing),self.assertRaises(ValueError):self.capture()
        self.assertFalse(self.archive.exists())

    def repack(self,receipt,mutate):
        index=self.workspace/'index.json'
        data=b.json.loads(index.read_bytes());mutate(data);index.write_bytes(b.canonical(data))
        alternate=self.root/'invalid.tar'
        receipt=copy.deepcopy(receipt)
        receipt['manifest']=b.files.capture(str(self.workspace),str(alternate))
        receipt['archive']=b.archive_digest(alternate)
        return alternate,receipt

    def test_inner_scope_binding_hash_and_manifest_controls(self):
        original=self.capture()
        original_index=(self.workspace/'index.json').read_bytes()
        for kind in ('missing','extra','binding','hash','manifest'):
            with self.subTest(kind=kind):
                # Deliberately rebind the outer receipt to exercise inner parsing;
                # normal replay must independently trust the original receipt.
                def mutate(index):
                    if kind=='missing':index['components'].pop('database')
                    elif kind=='extra':index['components']['other']=index['components']['database']
                    elif kind=='binding':index['binding']['releaseNonce']='6'*32
                    elif kind=='hash':index['components']['database']['archive']['sha256']='0'*64
                    else:index['components']['database']['manifest']['entries'][1]['path']='../escape'
                alternate,invalid=self.repack(original,mutate)
                target=str(self.root/('reject-'+kind))
                with self.assertRaises(ValueError):b.restore(str(alternate),invalid,BINDING,target)
                self.assertFalse((Path(target)/'database').exists())
                alternate.unlink();(self.workspace/'index.json').write_bytes(original_index)

    def test_named_source_replacement_does_not_silently_change_identity(self):
        walk=b.files.walk;count=0
        def replacing(fd,**kwargs):
            nonlocal count
            result=walk(fd,**kwargs);count+=1
            # Each capture does two walks. Swap the first pathname at the start
            # of the full final scan while its original descriptor stays held.
            if count==len(b.ROLES)*2+1:
                p=Path(self.sources['database']);p.rename(self.root/'old-database');p.mkdir(mode=0o700)
            return result
        with patch.object(b.files,'walk',side_effect=replacing),self.assertRaises(ValueError):self.capture()
        self.assertFalse(self.archive.exists())


if __name__=='__main__':unittest.main()
