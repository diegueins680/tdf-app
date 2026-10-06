#!/usr/bin/env python3
"""Actual directory substitution controls; never invokes Docker or systemd."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('fence_linux_fixture', Path(__file__).with_name('test-production-writer-fence-linux.py'))
f = importlib.util.module_from_spec(spec); spec.loader.exec_module(f)


class OwnershipTests(unittest.TestCase):
    def test_failed_exclusive_creation_never_moves_an_unowned_tree(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); source = root/'production'; source.mkdir(mode=0o700)
            (source/'sentinel').write_text('unowned')
            with self.assertRaises(FileExistsError): source.mkdir(mode=0o700)
            f.preserve_owned_directory(source, root/'retained', None)
            self.assertEqual((source/'sentinel').read_text(), 'unowned')
            self.assertFalse((root/'retained').exists())

    def test_replaced_directory_and_symlink_cannot_borrow_previous_ownership(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); source = root/'production'; source.mkdir(mode=0o700)
            info=source.lstat(); owned=(info.st_dev,info.st_ino)
            source.rename(root/'original'); source.mkdir(mode=0o700)
            with self.assertRaises(ValueError): f.preserve_owned_directory(source,root/'retained',owned)
            self.assertTrue(source.is_dir()); source.rename(root/'replacement')
            source.symlink_to(root/'original',target_is_directory=True)
            with self.assertRaises(ValueError): f.preserve_owned_directory(source,root/'retained',owned)
            self.assertTrue(source.is_symlink()); self.assertFalse((root/'retained').exists())

    def test_owned_files_retained_without_overwriting_prior_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory); source=root/'production'; source.mkdir(mode=0o700)
            (source/'sentinel').write_bytes(b'owned')
            info=source.lstat(); owned=(info.st_dev,info.st_ino)
            (root/'occupied').mkdir()
            with self.assertRaises(ValueError): f.preserve_owned_directory(source,root/'occupied',owned)
            f.preserve_owned_directory(source,root/'retained',owned)
            self.assertFalse(source.exists()); self.assertEqual((root/'retained/sentinel').read_bytes(),b'owned')


if __name__=='__main__':unittest.main()
