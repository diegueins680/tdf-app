#!/usr/bin/env python3
"""Actual directory substitution controls; never invokes Docker or systemd."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
import hashlib
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('fence_linux_fixture', Path(__file__).with_name('test-production-writer-fence-linux.py'))
f = importlib.util.module_from_spec(spec); spec.loader.exec_module(f)


class OwnershipTests(unittest.TestCase):
    def test_requested_recovery_stage_cannot_silently_skip_prerequisites(self):
        names=['TDF_TEST_ORIGINAL_'+name+'_RECOVERY' for name in ('DB','APPLICATION','EDGE','TIMER')]
        names.append('TDF_TEST_ORIGINAL_RECOVERY_COMPLETION')
        for count in range(len(names)+1):
            with patch.dict(f.os.environ,dict.fromkeys(names[:count],'1'),clear=True):f.validate_recovery_modes()
        for index in range(1,len(names)):
            with patch.dict(f.os.environ,{names[index]:'1'},clear=True),self.assertRaises(ValueError):f.validate_recovery_modes()

    def test_owned_tls_trust_removed_even_when_resource_cleanup_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            trust=Path(directory)/'synthetic.crt';trust.write_bytes(b'synthetic-only')
            st=trust.lstat();tls=object.__new__(f.SyntheticTLS)
            tls.trust=trust;tls.trust_identity=(st.st_dev,st.st_ino,hashlib.sha256(trust.read_bytes()).hexdigest())
            def fail():raise RuntimeError('owned Docker cleanup failed')
            with patch.object(f,'run',return_value='') as run:
                with self.assertRaisesRegex(RuntimeError,'owned Docker cleanup failed'):
                    f.cleanup_preserving_tls(fail,tls)
            self.assertFalse(trust.exists())
            run.assert_called_once_with(['update-ca-certificates'])

    def test_changed_trust_file_is_preserved_and_cleanup_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            trust=Path(directory)/'synthetic.crt';trust.write_bytes(b'synthetic-only')
            st=trust.lstat();tls=object.__new__(f.SyntheticTLS)
            tls.trust=trust;tls.trust_identity=(st.st_dev,st.st_ino,hashlib.sha256(trust.read_bytes()).hexdigest())
            trust.write_bytes(b'changed')
            with patch.object(f,'run') as run, self.assertRaises(ValueError):
                f.cleanup_preserving_tls(lambda:None,tls)
            self.assertEqual(trust.read_bytes(),b'changed');run.assert_not_called()

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
