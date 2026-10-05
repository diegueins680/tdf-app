#!/usr/bin/env python3
"""Exercise private backup completion and rejection without touching production."""
import copy
import importlib.util
import json
import os
import signal
import shutil
import subprocess
import sys
import time
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module
backup = load('backup', ROOT/'ops/hetzner/backup-postgres.py')
fixtures = load('fixtures', ROOT/'scripts/test-hetzner-inspection.py')
SOURCE = backup.runtime.summarize_container('db', fixtures.container('db'))


class BackupTests(unittest.TestCase):
    def only_run_directory(self, root):
        # The durable pending marker also starts with logical-. Directory
        # enumeration order differs across filesystems; never select that file.
        runs = [path for path in root.glob('logical-*') if path.is_dir()]
        self.assertEqual(len(runs), 1)
        pending = root/backup.PENDING
        if pending.exists():
            self.assertEqual(json.loads(pending.read_text())['run'], runs[0].name)
        return runs[0]

    def fixture(self, failure=None):
        directory = tempfile.TemporaryDirectory(); self.addCleanup(directory.cleanup)
        root = Path(directory.name)
        calls = []
        def run(command, **kwargs):
            calls.append((command, kwargs))
            program = next(p for p in ['pg_dumpall', 'pg_dump', 'pg_restore'] if p in command)
            if failure == program:
                raise RuntimeError('synthetic-secret must not be recorded')
            if program != 'pg_restore':
                kwargs['stdout'].write(b'synthetic-private-archive')
            return SimpleNamespace(returncode=0)
        observations = [SOURCE, {**SOURCE, 'containerId': 'c'*64} if failure=='changed-source' else SOURCE]
        with patch.object(backup, 'observe', side_effect=observations), \
             patch.object(backup.os, 'statvfs', return_value=SimpleNamespace(f_bavail=10, f_frsize=backup.MAX_ARCHIVE)), \
             patch.object(backup.subprocess, 'run', side_effect=run):
            if failure:
                with self.assertRaises((RuntimeError, ValueError)): backup.backup(root)
            else:
                backup.backup(root)
        run_dir = self.only_run_directory(root)
        self.assertTrue((root/'backup.lock').is_file())
        self.assertEqual(run_dir.stat().st_mode & 0o777, 0o700)
        for path in run_dir.iterdir(): self.assertEqual(path.stat().st_mode & 0o777, 0o600)
        return root, run_dir, calls

    def test_success_records_hashes_without_claiming_restore_or_off_host_recovery(self):
        _, directory, calls = self.fixture()
        receipt = json.loads((directory/'complete.json').read_text())
        self.assertEqual(receipt['status'], 'logical-archive-created')
        for key in ['restored', 'offHost', 'coordinatedAssets']: self.assertIs(receipt[key], False)
        self.assertEqual(set(receipt['files']), {'database.pgdump', 'globals.sql'})
        for name, info in receipt['files'].items():
            self.assertEqual(info['sha256'], backup.digest(directory/name))
            self.assertEqual(info['bytes'], (directory/name).stat().st_size)
        self.assertFalse((directory/'failure.json').exists())
        self.assertFalse((directory.parent/backup.PENDING).exists())
        self.assertEqual(len(calls), 3)
        for command, options in calls:
            self.assertEqual(options['env'], backup.runtime.COMMAND_ENV)
            self.assertIn('unix:///var/run/docker.sock', command)
            self.assertEqual(command[command.index('env',len(backup.runtime.DOCKER))+1], '-i')

    def test_dump_roles_listing_or_identity_failure_never_report_completion(self):
        for stage in ['pg_dump', 'pg_dumpall', 'pg_restore', 'changed-source']:
            with self.subTest(stage=stage):
                _, directory, _ = self.fixture(stage)
                self.assertFalse((directory/'complete.json').exists())
                failure = (directory/'failure.json').read_text()
                self.assertNotIn('synthetic-secret', failure)
                self.assertFalse(json.loads(failure)['complete'])
                self.assertTrue((directory.parent/backup.PENDING).exists())
                with patch.object(backup, 'observe') as observe, self.assertRaises(ValueError):
                    backup.backup(directory.parent)
                observe.assert_not_called()

    def test_killed_parent_cannot_admit_retry_while_child_still_runs(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            pidfile = root/'child.pid'
            child = "import os,time,pathlib; pathlib.Path(%r).write_text(str(os.getpid())); time.sleep(30)" % str(pidfile)
            parent = ('import importlib.util,pathlib\n'
                + 's=importlib.util.spec_from_file_location("backup",%r); m=importlib.util.module_from_spec(s); s.loader.exec_module(m)\n' % str(ROOT/'ops/hetzner/backup-postgres.py')
                + 'm.observe=lambda: %r\n' % SOURCE
                + 'm.database_command=lambda *args: %r\n' % [sys.executable, '-c', child]
                + 'm.backup(pathlib.Path(%r))\n' % str(root))
            process = subprocess.Popen([sys.executable, '-c', parent], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            child_pid = None
            try:
                deadline = time.monotonic()+8
                while not pidfile.exists() and time.monotonic()<deadline and process.poll() is None:
                    time.sleep(0.02)
                self.assertTrue(pidfile.exists(), 'owned child never started')
                child_pid = int(pidfile.read_text())
                process.kill(); process.wait(timeout=5)
                os.kill(child_pid, 0)  # Parent death did not establish child completion.
                with patch.object(backup, 'observe') as observe, self.assertRaises(ValueError):
                    backup.backup(root)
                observe.assert_not_called()
                self.assertTrue((root/backup.PENDING).is_file())
            finally:
                if process.poll() is None: process.kill(); process.wait(timeout=5)
                if child_pid is not None:
                    try: os.kill(child_pid, signal.SIGTERM)
                    except ProcessLookupError: pass

    def test_private_directory_and_lock_reject_symlink_or_public_permissions(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            alias = root/'alias'; alias.symlink_to(root, target_is_directory=True)
            with self.assertRaises(ValueError): backup.backup(alias)
            root.chmod(0o755)
            with self.assertRaises(ValueError): backup.backup(root)
            root.chmod(0o700)
            lock = root/'backup.lock'; lock.symlink_to(root/'unrelated')
            with self.assertRaises(OSError): backup.backup(root)
            self.assertFalse((root/'unrelated').exists())

    def test_overlapping_backup_rejected_before_observing_database(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            fd = os.open(root/'backup.lock', os.O_CREAT|os.O_RDWR, 0o600)
            try:
                backup.fcntl.flock(fd, backup.fcntl.LOCK_EX|backup.fcntl.LOCK_NB)
                with patch.object(backup, 'observe') as observe, self.assertRaises(BlockingIOError):
                    backup.backup(root)
                observe.assert_not_called()
            finally: os.close(fd)

    def test_invalid_storage_rejected_before_archive_creation(self):
        for mutation in ['child', 'pgdata', 'effective']:
            value = fixtures.container('db')
            if mutation=='child': value['Mounts'].append({'Destination':'/var/lib/postgresql/data/base','Type':'volume','Name':'foreign'})
            if mutation=='pgdata': value['Config']['Env'] = ['PGDATA=/alternate']
            answers = ['a'*64, json.dumps([value]), 'f' if mutation=='effective' else 't']
            with tempfile.TemporaryDirectory() as temporary, \
                 patch.object(backup.runtime, 'capture', side_effect=answers), \
                 patch.object(backup, 'archive') as archive, self.assertRaises(ValueError):
                backup.backup(Path(temporary))
            archive.assert_not_called()

    def test_archive_failure_empty_output_and_overwrite_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary)/'archive'
            with patch.object(backup.subprocess, 'run', return_value=SimpleNamespace(returncode=1)), self.assertRaises(ValueError):
                backup.archive(['unused'], path)
            with self.assertRaises(FileExistsError): backup.archive(['unused'], path)
            empty = Path(temporary)/'empty'
            with patch.object(backup.subprocess, 'run', return_value=SimpleNamespace(returncode=0)), self.assertRaises(ValueError):
                backup.archive(['unused'], empty)

    def test_replacing_source_during_dump_cannot_publish_completion(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary); code = root/'code'; code.mkdir()
            for name in ['backup-postgres.py', 'inspect-runtime.py']:
                shutil.copyfile(ROOT/'ops/hetzner'/name, code/name)
            isolated = load('isolated_backup', code/'backup-postgres.py')
            def archive(command, destination):
                destination.write_bytes(b'synthetic'); destination.chmod(0o600)
                (code/'inspect-runtime.py').write_text('# replaced during the dump\n')
            with patch.object(isolated, 'observe', return_value=SOURCE), \
                 patch.object(isolated, 'archive', side_effect=archive), \
                 patch.object(isolated.subprocess, 'run', return_value=SimpleNamespace(returncode=0)), \
                 self.assertRaises(ValueError):
                isolated.backup(root)
            run = self.only_run_directory(root)
            self.assertFalse((run/'complete.json').exists())
            self.assertEqual(json.loads((run/'failure.json').read_text())['stage'], 'receipt')


if __name__ == '__main__': unittest.main()
