#!/usr/bin/env python3
import copy
import hashlib
import importlib.util
import io
from pathlib import Path
import tarfile
import tempfile
import unittest

spec=importlib.util.spec_from_file_location('installer',Path(__file__).resolve().parent/'install-recovery-tools.py')
installer=importlib.util.module_from_spec(spec);spec.loader.exec_module(installer)


class InstallerTests(unittest.TestCase):
    def archive(self, *, symlink=False, duplicate=False):
        handle=io.BytesIO(); binaries={}
        with tarfile.open(fileobj=handle,mode='w:gz') as tar:
            for name in ('age','age-keygen'):
                content=b'synthetic non-executable '+name.encode();binaries[name]=hashlib.sha256(content).hexdigest()
                item=tarfile.TarInfo('age/'+name);item.size=len(content)
                if symlink: item.type=tarfile.SYMTYPE; item.linkname='../escape';item.size=0
                tar.addfile(item,io.BytesIO(content))
                if duplicate: tar.addfile(item,io.BytesIO(content))
        value=handle.getvalue()
        config={'version':'test','platforms':{'test':{'archiveSha256':hashlib.sha256(value).hexdigest(),'binaries':binaries}}}
        return value,config

    def test_verified_members_only_and_exclusive_private_installation(self):
        archive,config=self.archive()
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve(); target=root/'tools'
            result=installer.install(archive,str(target),config,'test')
            self.assertEqual(set(p.name for p in target.iterdir()), {'age','age-keygen'})
            self.assertEqual(result['version'],'test')
            for p in target.iterdir(): self.assertEqual(p.stat().st_mode & 0o777,0o700)
            with self.assertRaises(FileExistsError): installer.install(archive,str(target),config,'test')

    def test_bad_archive_binary_link_and_duplicate_fail_before_install(self):
        for kind in ('archive','binary','link','duplicate'):
            with self.subTest(kind=kind),tempfile.TemporaryDirectory() as temporary:
                archive,config=self.archive(symlink=kind=='link',duplicate=kind=='duplicate')
                if kind=='archive': archive+=b'drift'
                if kind=='binary': config['platforms']['test']['binaries']['age']='f'*64
                target=Path(temporary).resolve()/'tools'
                with self.assertRaises(ValueError): installer.install(archive,str(target),config,'test')
                self.assertFalse(target.exists())

    def test_public_or_linked_destination_parent_rejected(self):
        archive,config=self.archive()
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve();public=root/'public';public.mkdir(mode=0o755)
            with self.assertRaises(ValueError): installer.install(archive,str(public/'tools'),config,'test')
            alias=root/'alias';alias.symlink_to(public,target_is_directory=True)
            with self.assertRaises(OSError): installer.install(archive,str(alias/'tools'),config,'test')


if __name__=='__main__': unittest.main()
