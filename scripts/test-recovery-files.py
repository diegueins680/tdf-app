#!/usr/bin/env python3
"""Actual filesystem/archive adversarial controls, using only synthetic data."""
import copy
import hashlib
import importlib.util
import io
import os
from pathlib import Path
import tarfile
import tempfile
import subprocess
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('recovery_files', Path(__file__).resolve().parent.parent/'ops/hetzner/recovery-files.py')
files = importlib.util.module_from_spec(spec)
spec.loader.exec_module(files)


class RecoveryFilesTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.source = self.root/'source'; self.source.mkdir(mode=0o750)
        (self.source/'private').mkdir(mode=0o700)
        (self.source/'empty').mkdir(mode=0o750)
        (self.source/'private'/'attachment').write_bytes(bytes(range(256))*11)
        (self.source/'private'/'attachment').chmod(0o640)
        (self.source/('música-'+'x'*110)).write_bytes(b'synthetic public media')
        self.archive = self.root/'files.tar'

    def capture(self):
        return files.capture(str(self.source), str(self.archive))

    def restore(self, manifest, name='restored'):
        return files.restore(str(self.archive), manifest, str(self.root/name))

    def rewrite(self, change):
        # Independently rewrite a real tar, preserving all other members.
        with tarfile.open(self.archive) as original:
            members = [(copy.copy(m), original.extractfile(m).read() if m.isfile() else None) for m in original]
        change(members)
        with tarfile.open(self.archive, 'w', format=tarfile.PAX_FORMAT) as altered:
            for member, content in members:
                altered.addfile(member, io.BytesIO(content) if content is not None else None)

    def test_roundtrip_bytes_empty_directories_unicode_and_mode_and_time(self):
        manifest = self.capture()
        result = self.restore(manifest)
        self.assertEqual(result['status'], 'file-tree-restored')
        self.assertFalse(result['coordinatedDatabase']); self.assertFalse(result['offHost'])
        for original in self.source.rglob('*'):
            restored = self.root/'restored'/original.relative_to(self.source)
            self.assertEqual(restored.stat().st_mode, original.stat().st_mode)
            self.assertEqual(restored.stat().st_mtime_ns, original.stat().st_mtime_ns)
            if original.is_file(): self.assertEqual(restored.read_bytes(), original.read_bytes())
        self.assertEqual(self.archive.stat().st_mode & 0o777, 0o600)

    def test_source_symlink_and_hardlink_and_fifo_rejected(self):
        for kind in ('symlink', 'hardlink', 'fifo'):
            with self.subTest(kind=kind):
                path = self.source/'bad'
                if kind == 'symlink': path.symlink_to(self.source/'private'/'attachment')
                elif kind == 'hardlink': os.link(self.source/'private'/'attachment', path)
                else: os.mkfifo(path)
                try:
                    with self.assertRaises((ValueError, OSError)): self.capture()
                finally:
                    path.unlink()
                    self.archive.unlink(missing_ok=True)

    def test_retained_directory_handle_does_not_follow_replaced_source_path(self):
        with files.directory(str(self.source)) as source_fd:
            retained = self.root/'retained-source'
            self.source.rename(retained)
            self.source.mkdir(); (self.source/'replacement').write_text('wrong tree')
            manifest = files.capture_directory_fd(source_fd, str(self.archive))
        self.restore(manifest)
        self.assertFalse((self.root/'restored'/'replacement').exists())
        self.assertEqual((self.root/'restored'/'private'/'attachment').read_bytes(), bytes(range(256))*11)
        fd = os.open(self.archive, os.O_RDONLY)
        try:
            with self.assertRaises(ValueError): files.capture_directory_fd(fd, str(self.root/'invalid.tar'))
        finally: os.close(fd)
        self.assertFalse((self.root/'invalid.tar').exists())

    def test_symlink_ancestor_cannot_redirect_capture_or_restore(self):
        alias = self.root/'alias'; alias.symlink_to(self.source, target_is_directory=True)
        with self.assertRaises(OSError): files.capture(str(alias/'private'), str(self.archive))
        manifest = self.capture()
        with self.assertRaises(OSError): files.restore(str(self.archive), manifest, str(alias/'elsewhere'/'result'))
        self.assertFalse((self.source/'elsewhere').exists())

    def test_existing_archive_or_restore_directory_never_overwritten(self):
        manifest = self.capture()
        before = self.archive.read_bytes()
        with self.assertRaises(FileExistsError): self.capture()
        self.assertEqual(self.archive.read_bytes(), before)
        destination = self.root/'restored'; destination.mkdir(); (destination/'sentinel').write_text('preserve')
        with self.assertRaises(FileExistsError): self.restore(manifest)
        self.assertEqual((destination/'sentinel').read_text(), 'preserve')

    def test_public_parent_or_archive_rejected(self):
        manifest = self.capture()
        self.archive.chmod(0o644)
        with self.assertRaises(ValueError): self.restore(manifest)
        self.archive.chmod(0o600)
        self.root.chmod(0o755)
        with self.assertRaises(ValueError): self.restore(manifest)
        self.root.chmod(0o700)

    def test_size_entry_and_special_permission_limits(self):
        for constant, limit in [('MAX_BYTES', 1), ('MAX_ENTRIES', 2)]:
            with self.subTest(constant=constant), patch.object(files, constant, limit):
                with self.assertRaises(ValueError): self.capture()
                self.archive.unlink(missing_ok=True)
        (self.source/'private').chmod(0o1700)
        with self.assertRaises(ValueError): self.capture()

    def test_source_mutation_during_archive_rejects_success(self):
        original = files.add
        changed = False
        def mutate(archive, row, handle):
            nonlocal changed
            original(archive, row, handle)
            if handle and not changed:
                (self.source/row['path']).write_bytes(b'changed during capture')
                changed = True
        with patch.object(files, 'add', side_effect=mutate):
            with self.assertRaises(ValueError): self.capture()

    def test_capture_rejects_added_file_after_first_walk(self):
        original = files.walk
        def mutate(fd, **kwargs):
            result = original(fd, **kwargs)
            if kwargs.get('archive'): (self.source/'late').write_bytes(b'late')
            return result
        with patch.object(files, 'walk', side_effect=mutate):
            with self.assertRaises(ValueError): self.capture()

    def test_archive_escape_absolute_link_device_duplicate_and_unlisted_members_rejected(self):
        manifest = self.capture(); original = self.archive.read_bytes()
        def extra(name, kind=tarfile.REGTYPE, link=''):
            item = tarfile.TarInfo(name); item.type = kind; item.linkname = link
            return (item, b'' if kind == tarfile.REGTYPE else None)
        cases = [extra('../escape'), extra('/absolute'), extra('private/link', tarfile.SYMTYPE, '../../escape'),
                 extra('hardlink', tarfile.LNKTYPE, 'private/attachment'), extra('device', tarfile.CHRTYPE),
                 extra('unlisted'), extra('private', tarfile.DIRTYPE)]
        for index, member in enumerate(cases):
            with self.subTest(index=index):
                self.archive.write_bytes(original)
                self.rewrite(lambda members: members.append(member))
                with self.assertRaises((ValueError, tarfile.TarError, OSError)): self.restore(manifest, 'failed-'+str(index))
        self.assertFalse((self.root/'escape').exists())

    def test_archive_truncation_missing_member_wrong_bytes_or_metadata_rejected(self):
        manifest = self.capture(); original = self.archive.read_bytes()
        def wrong_bytes(members):
            index = next(i for i, (m, _) in enumerate(members) if m.isfile())
            item, content = members[index]; members[index] = (item, b'z'*len(content))
        def wrong_mode(members): members[0][0].mode ^= 0o100
        for index, mutation in enumerate([lambda members: members.pop(), wrong_bytes, wrong_mode]):
            self.archive.write_bytes(original); self.rewrite(mutation)
            with self.assertRaises(ValueError): self.restore(manifest, 'bad-'+str(index))
        self.archive.write_bytes(original[:1024])
        with self.assertRaises((ValueError, tarfile.TarError)): self.restore(manifest, 'truncated')

    def test_manifest_duplicate_parent_escape_hash_and_boolean_size_rejected_before_target_creation(self):
        manifest = self.capture()
        for mutate in [lambda m: m['entries'].append(m['entries'][1]),
                       lambda m: m['entries'][1].update(path='../bad'),
                       lambda m: m['entries'][1].update(path='absent/child'),
                       lambda m: m.update(bytes=True),
                       lambda m: m['entries'][0].update(mode=0o4755)]:
            bad = copy.deepcopy(manifest); mutate(bad)
            with self.assertRaises(ValueError): self.restore(bad)
            self.assertFalse((self.root/'restored').exists())
        bad = copy.deepcopy(manifest)
        next(row for row in bad['entries'] if row['kind']=='file')['sha256'] = hashlib.sha256(b'incorrect').hexdigest()
        with self.assertRaises(ValueError): self.restore(bad)

    def test_growing_source_cannot_force_unbounded_hash_read(self):
        class Endless:
            def __init__(self): self.reads = 0
            def read(self, size):
                self.reads += 1
                return b'x' * size
        source = Endless()
        with self.assertRaises(ValueError): files.digest(source, 10)
        self.assertEqual(source.reads, 1)


    def test_capture_cannot_return_manifest_with_unrestorable_timestamp(self):
        os.utime(self.source/'private'/'attachment', ns=(-1_000_000_000, -1_000_000_000))
        with self.assertRaises(ValueError): self.capture()

    def test_valid_pax_numeric_metadata_is_admitted(self):
        manifest = self.capture()
        def numeric_headers(members):
            for item, _ in members:
                item.pax_headers.update(uid=str(item.uid), gid=str(item.gid), mtime=str(item.mtime))
        self.rewrite(numeric_headers)
        self.assertEqual(self.restore(manifest)['status'], 'file-tree-restored')


    def test_extended_attributes_are_not_silently_lost(self):
        attribute = 'user.tdf_test' if os.uname().sysname == 'Linux' else 'tdf_test'
        path = self.source/'private'/'attachment'
        if hasattr(os, 'setxattr'): os.setxattr(path, attribute, b'synthetic')
        else: subprocess.run(['xattr', '-w', attribute, 'synthetic', str(path)], check=True)
        with self.assertRaises(ValueError): self.capture()


if __name__ == '__main__': unittest.main()
