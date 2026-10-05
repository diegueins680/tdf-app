#!/usr/bin/env python3
from contextlib import ExitStack
import json
import os
import subprocess
import tempfile
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch, MagicMock

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('restore', ROOT / 'ops/hetzner/rehearse-postgres-restore.py')
restore = importlib.util.module_from_spec(spec)
spec.loader.exec_module(restore)
SOURCE, TARGET, NONCE = 'a' * 64, 'b' * 64, 'c' * 32
IMAGE, IMAGE_ID = 'pgvector/pgvector@sha256:' + 'd' * 64, 'sha256:' + 'e' * 64


def container():
    return {'Id': TARGET, 'Image': IMAGE_ID, 'Config': {'Image': IMAGE, 'Labels': {restore.LABEL: NONCE}},
            'HostConfig': {'NetworkMode': 'none', 'PortBindings': {}, 'ReadonlyRootfs': True,
                'Memory': restore.MEMORY_LIMIT, 'MemorySwap': restore.MEMORY_LIMIT, 'NanoCpus': 500000000,
                'PidsLimit': 64, 'IpcMode': 'private', 'SecurityOpt': ['no-new-privileges:true'],
                'Tmpfs': {key: '' for key in (restore.DATA, '/var/run/postgresql', '/tmp')}}, 'Mounts': []}


class RestoreBoundaryTests(unittest.TestCase):
    def make(self):
        return restore.IsolatedRestore(SOURCE, IMAGE, IMAGE_ID, NONCE)

    def test_target_must_be_distinct_and_owned(self):
        for key, value in [('Id', SOURCE), ('Id', 'db'), ('Image', 'sha256:' + 'f' * 64)]:
            data = container(); data[key] = value
            with self.subTest(key=key, value=value), self.assertRaises(ValueError):
                self.make().admit(data)
        data = container(); data['Config']['Labels'][restore.LABEL] = 'someone-else'
        with self.assertRaises(ValueError): self.make().admit(data)

    def test_all_unsafe_target_controls_reject(self):
        cases = {'NetworkMode': 'host', 'PortBindings': {'5432/tcp': []}, 'ReadonlyRootfs': False,
                 'Memory': 0, 'MemorySwap': -1, 'NanoCpus': 0, 'PidsLimit': 0, 'Privileged': True,
                 'Binds': ['/opt/tdf:/data'], 'VolumesFrom': ['production'], 'Devices': ['/dev/sda'],
                 'CapAdd': ['SYS_ADMIN'], 'PidMode': 'host', 'UTSMode': 'host', 'IpcMode': 'host',
                 'SecurityOpt': [], 'Tmpfs': {}}
        for key, value in cases.items():
            data = container(); data['HostConfig'][key] = value
            with self.subTest(key=key), self.assertRaises(ValueError): self.make().admit(data)
        for mount in [{'Type': 'volume', 'Destination': restore.DATA}, {'Type': 'bind', 'Destination': restore.DATA},
                      {'Type': 'tmpfs', 'Destination': '/unexpected'}]:
            data = container(); data['Mounts'] = [mount]
            with self.subTest(mount=mount), self.assertRaises(ValueError): self.make().admit(data)

    def test_creation_has_no_production_mount_ports_or_environment(self):
        command = self.make().create_command()
        self.assertIn('--network=none', command)
        self.assertIn('--pull=never', command)
        for bad in ('--volume', '-v', '--publish', '-p', '--env-file', '--privileged'):
            self.assertNotIn(bad, command)
        self.assertNotIn(SOURCE, command)
        self.assertIn(IMAGE, command)

    def test_mutations_and_cleanup_cannot_target_source(self):
        target = self.make()
        with self.assertRaises(ValueError): target.write_command('pg_restore', [])
        target.admit(container())
        self.assertIn(TARGET, target.write_command('pg_restore', []))
        self.assertNotIn(SOURCE, target.write_command('pg_restore', []))
        target.target = SOURCE
        with self.assertRaises(ValueError): target.write_command('pg_restore', [])
        with patch.object(restore, 'execute', return_value='[' + __import__('json').dumps(container()) + ']') as run:
            with self.assertRaises(ValueError): target.cleanup()
            self.assertEqual(len(run.call_args_list), 1)  # Inspection only, no removal.

    def test_source_commands_pin_read_only_socket_and_clear_routing(self):
        command = restore.psql(SOURCE, read_only=True)
        self.assertIn('PGOPTIONS=-c statement_timeout=60000 -c lock_timeout=5000 -c default_transaction_read_only=on -c idle_in_transaction_session_timeout=300000', command)
        for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE'): self.assertIn(key, command)
        self.assertIn('/var/run/postgresql', command)
        with self.assertRaises(ValueError): restore.local_database(SOURCE, 'pg_restore', [], read_only=True)

    def test_lost_create_response_recovers_only_owned_isolate(self):
        target = self.make(); target.creation_attempted = True
        with patch.object(restore, 'execute', return_value=json.dumps([container()])) as run:
            target.cleanup()
            self.assertEqual(run.call_args_list[-1].args[0], restore.DOCKER + ['rm', '--force', TARGET])
            self.assertTrue(all(call.kwargs['timeout'] == 10 for call in run.call_args_list))
            self.assertIsNone(target.target)
        target = self.make(); target.creation_attempted = True
        foreign = container(); foreign['Id'] = SOURCE
        with patch.object(restore, 'execute', return_value=json.dumps([foreign])) as run:
            with self.assertRaises(ValueError): target.cleanup()
            self.assertEqual(run.call_count, 1)

    def test_docker_routing_overrides_are_removed_in_real_child_environment(self):
        with tempfile.TemporaryDirectory() as temporary:
            executable = Path(temporary) / 'docker'
            executable.write_text('#!/bin/sh\nfor key in DOCKER_HOST DOCKER_CONTEXT DOCKER_TLS_VERIFY DOCKER_CERT_PATH; do printenv "$key" && exit 97; done\nprintf "%s\\n" "$@"\n')
            executable.chmod(0o700)
            environment = dict(os.environ, PATH=temporary + os.pathsep + os.environ['PATH'])
            environment.update({key: 'hostile-routing' for key in ('DOCKER_HOST', 'DOCKER_CONTEXT', 'DOCKER_TLS_VERIFY', 'DOCKER_CERT_PATH')})
            result = subprocess.run(restore.DOCKER + ['version'], env=environment, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0)
            self.assertEqual(result.stdout.splitlines(), ['--host', 'unix:///var/run/docker.sock', 'version'])
            inspection_spec = importlib.util.spec_from_file_location('inspector', ROOT / 'ops/hetzner/inspect-runtime.py')
            inspector = importlib.util.module_from_spec(inspection_spec); inspection_spec.loader.exec_module(inspector)
            self.assertEqual(restore.DOCKER, inspector.DOCKER)

    def test_rehearsals_exclude_each_other_and_release_on_failure(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            with self.assertRaisesRegex(RuntimeError, 'injected'):
                with restore.rehearsal_lock(directory):
                    with self.assertRaises(BlockingIOError):
                        with restore.rehearsal_lock(directory): self.fail('concurrent admission')
                    raise RuntimeError('injected')
            with restore.rehearsal_lock(directory): pass
            self.assertEqual((directory / 'restore-rehearsal.lock').stat().st_mode & 0o777, 0o600)

    def test_lock_rejects_symlinks_shared_permissions_and_hardlinks(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            lock = directory / 'restore-rehearsal.lock'
            lock.symlink_to(directory / 'other')
            with self.assertRaises(OSError):
                with restore.rehearsal_lock(directory): self.fail('symlink admitted')
            lock.unlink()
            lock.touch(mode=0o600)
            os.link(lock, directory / 'alias')
            with self.assertRaises(ValueError):
                with restore.rehearsal_lock(directory): self.fail('hardlink admitted')
            (directory / 'alias').unlink()
            lock.chmod(0o644)
            with self.assertRaises(ValueError):
                with restore.rehearsal_lock(directory): self.fail('shared lock admitted')
            lock.chmod(0o600)
            directory.chmod(0o755)
            with self.assertRaises(ValueError):
                with restore.rehearsal_lock(directory): self.fail('shared directory admitted')

    def test_count_receipts_reject_missing_negative_or_nonintegral_data(self):
        self.assertEqual(restore.validate_counts({'public.fixture': 2}), {'public.fixture': 2})
        for value in ({}, [], {'x': -1}, {'x': True}, {'x': 1.5}, {'x': '2'}):
            with self.subTest(value=value), self.assertRaises(ValueError): restore.validate_counts(value)


class RestoreOrchestrationTests(unittest.TestCase):
    def exercise(self, failure=None):
        with tempfile.TemporaryDirectory() as temporary, ExitStack() as stack:
            base = Path(temporary)
            snapshot = {'containers': {'db': {'containerId': SOURCE, 'image': IMAGE, 'imageId': IMAGE_ID}},
                        'database': {'migrations': ['synthetic-ledger']}, 'publicBackend': {'commit': 'f' * 40}}
            runtime = MagicMock()
            runtime.inspect.side_effect = [snapshot, snapshot]
            runtime.database_command.return_value = ['synthetic-inventory']
            runtime.summarize_database.return_value = {'migrations': ['synthetic-ledger']}
            held = MagicMock()
            held.query.side_effect = ['00000001-00000002-1', '{"public.fixture":2}']
            target = MagicMock()
            target.create_command.return_value = ['synthetic-create']
            target.write_command.side_effect = lambda program, args: ['synthetic-write', program, *args]
            real_path = Path
            def translated_path(value):
                if value == '/opt/tdf/backups': return base
                if value == '/proc/meminfo':
                    memory = MagicMock(); memory.read_text.return_value = 'MemAvailable: 2097152 kB'
                    return memory
                return real_path(value)
            stack.enter_context(patch.object(restore, 'Path', side_effect=translated_path))
            stack.enter_context(patch.object(restore.os, 'statvfs', return_value=type('Disk', (), {'f_bavail': 3*1024**3, 'f_frsize': 1})()))
            # Remote helper requires a root-owned archive parent; this local test
            # validates orchestration independently of the separately tested lock.
            info = base.stat()
            stack.enter_context(patch.object(Path, 'stat', autospec=True, side_effect=lambda path, **kw:
                type('Info', (), {'st_uid': 0, 'st_mode': info.st_mode})() if path == base else original_stat(path, **kw)))
            stack.enter_context(patch.object(restore, 'HeldSnapshot', return_value=held))
            stack.enter_context(patch.object(restore, 'IsolatedRestore', return_value=target))
            def archive(command, destination):
                if failure == 'dump': raise RuntimeError('injected dump failure')
                destination.write_bytes(b'CREATE ROLE postgres;\n' if destination.name == 'roles.sql' else b'synthetic archive')
            stack.enter_context(patch.object(restore, 'bounded_archive', side_effect=archive))
            def execute(command, **kw):
                if kw.get('input') == restore.CAPACITY_SQL: return '100'
                if command == ['synthetic-create']: return TARGET
                if command[:2] == ['synthetic-write', 'psql']:
                    if failure == 'roles': raise RuntimeError('injected role failure')
                    self.assertNotIn('CREATE ROLE postgres;', kw['input'])
                    return ''
                if kw.get('input') == restore.COUNTS_SQL:
                    return '{"public.fixture":3}' if failure == 'counts' else '{"public.fixture":2}'
                return '{}'
            stack.enter_context(patch.object(restore, 'execute', side_effect=execute))
            run = stack.enter_context(patch.object(restore.subprocess, 'run'))
            run.side_effect = [type('Ready', (), {'returncode': 0})(), RuntimeError('Rehearsal interrupted') if failure == 'interrupt' else type('Restore', (), {'returncode': 1 if failure == 'restore' else 0})()]
            if failure == 'cleanup': target.cleanup.side_effect = RuntimeError('injected cleanup failure')
            if failure == 'ledger': runtime.summarize_database.return_value = {'migrations': ['changed-ledger']}
            if failure:
                with self.assertRaises((ValueError, RuntimeError)): restore.rehearse_locked(runtime)
                self.assertEqual(list(base.glob('*/receipt.json')), [])
                self.assertEqual(len(list(base.glob('*/failure.json'))), 1)
            else:
                result = restore.rehearse_locked(runtime)
                self.assertEqual(result['status'], 'isolated-database-restore-passed')
                self.assertTrue(result['isolateRemoved'])
                self.assertFalse(result['productionDatabaseWritten'])
            held.close.assert_called_once()
            self.assertGreaterEqual(target.cleanup.call_count, 1)

    def test_success_and_injected_failures_never_skip_cleanup_or_emit_false_pass(self):
        for failure in [None, 'dump', 'roles', 'restore', 'counts', 'ledger', 'cleanup', 'interrupt']:
            with self.subTest(failure=failure): self.exercise(failure)


original_stat = Path.stat


if __name__ == '__main__': unittest.main()
