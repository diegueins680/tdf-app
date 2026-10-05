#!/usr/bin/env python3
import copy
import hashlib
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
    return {'Id': TARGET, 'Image': IMAGE_ID, 'Config': {'Image': IMAGE, 'Cmd': restore.POSTGRES_COMMAND, 'Labels': {restore.LABEL: NONCE}},
            'HostConfig': {'NetworkMode': 'none', 'PortBindings': {}, 'ReadonlyRootfs': True,
                'Memory': restore.MEMORY_LIMIT, 'MemorySwap': restore.MEMORY_LIMIT, 'NanoCpus': 500000000,
                'PidsLimit': 64, 'IpcMode': 'private', 'SecurityOpt': ['no-new-privileges:true'],
                'Tmpfs': {key: '' for key in (restore.DATA, '/var/run/postgresql', '/tmp')}}, 'Mounts': []}


class CollectorCompatibilityTests(unittest.TestCase):
    def test_restored_database_uses_actual_collector_with_optional_event_table(self):
        fixture_spec = importlib.util.spec_from_file_location('inspection_fixture', ROOT/'scripts/test-hetzner-inspection.py')
        fixtures = importlib.util.module_from_spec(fixture_spec)
        fixture_spec.loader.exec_module(fixtures)
        runtime = fixtures.module
        raw = fixtures.database()
        raw.pop('eventOperationFlags')  # Main SQL does not query this optional table.
        with self.assertRaises(KeyError):
            runtime.summarize_database(raw)  # Previous restore integration fails.
        for observed, expected in [(None, None), ([], []),
                ([{'feature_code': 'event.operations.api', 'enabled': False}],
                 [{'flag': 'event.operations.api', 'enabled': False}])]:
            with self.subTest(observed=observed), patch.object(restore, 'execute', return_value=json.dumps(raw)) as query, \
                    patch.object(runtime, 'optional_event_flags', return_value=observed) as optional:
                result = restore.observe_database(runtime, TARGET)
                self.assertEqual(result['eventOperationFlags'], expected)
                self.assertIn(TARGET, query.call_args.args[0])
                self.assertNotIn(SOURCE, query.call_args.args[0])
                optional.assert_called_once_with(TARGET)
        with patch.object(restore, 'execute', return_value=json.dumps(raw)), \
                patch.object(runtime, 'optional_event_flags', side_effect=ValueError('synthetic optional query rejection')):
            with self.assertRaises(ValueError): restore.observe_database(runtime, TARGET)


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
        data = container(); data['Config']['Cmd'] = ['postgres']
        with self.assertRaises(ValueError): self.make().admit(data)
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
    def test_uncertain_creation_blocks_even_with_no_visible_container(self):
        runtime = MagicMock()
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            restore.reserve_creation(directory, NONCE, IMAGE)
            self.assertEqual((directory / restore.PENDING_NAME).stat().st_mode & 0o777, 0o600)
            with patch.object(restore, 'Path', return_value=directory), patch.object(restore, 'execute') as run:
                with self.assertRaises(ValueError): restore.rehearse_locked(runtime)
                runtime.inspect.assert_not_called()
                run.assert_not_called()
            with self.assertRaises(FileExistsError): restore.reserve_creation(directory, 'f'*32, IMAGE)
            with self.assertRaises(ValueError): restore.release_creation(directory, 'f'*32, IMAGE)
            restore.release_creation(directory, NONCE, IMAGE)
            self.assertFalse((directory / restore.PENDING_NAME).exists())
            (directory / restore.PENDING_NAME).symlink_to(directory / 'missing')
            with patch.object(restore, 'Path', return_value=directory), patch.object(restore, 'execute') as run:
                with self.assertRaises(ValueError): restore.rehearse_locked(runtime)
                run.assert_not_called()

    def test_orphan_blocks_before_source_inspection_or_backup(self):
        runtime = MagicMock()
        for observed in [TARGET + '\n', 'unexpected-output']:
            with self.subTest(observed=observed), patch.object(restore, 'execute', return_value=observed) as run, \
                    patch.object(restore, 'HeldSnapshot') as held, patch.object(restore, 'IsolatedRestore') as target:
                with self.assertRaises(ValueError): restore.rehearse_locked(runtime)
                run.assert_called_once_with(restore.DOCKER + ['ps', '--all', '--quiet', '--filter', 'label=' + restore.LABEL], timeout=10)
                runtime.inspect.assert_not_called()
                held.assert_not_called()
                target.assert_not_called()

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
            target.creation_attempted = False
            target.cleanup.side_effect = lambda: setattr(target, 'creation_attempted', False)
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
                if command == restore.DOCKER + ['ps', '--all', '--quiet', '--filter', 'label=' + restore.LABEL]: return ''
                if kw.get('input') == restore.CAPACITY_SQL: return '100'
                if command == ['synthetic-create']:
                    self.assertTrue((base / restore.PENDING_NAME).is_file())
                    if failure == 'uncertain-create': raise RuntimeError('injected uncertain create')
                    return TARGET
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
            if failure == 'uncertain-create': target.cleanup.side_effect = RuntimeError('no visible container yet')
            if failure == 'ledger': runtime.summarize_database.return_value = {'migrations': ['changed-ledger']}
            if failure:
                with self.assertRaises((ValueError, RuntimeError)): restore.rehearse_locked(runtime)
                self.assertEqual(list(base.glob('*/receipt.json')), [])
                self.assertEqual(len(list(base.glob('*/failure.json'))), 1)
                self.assertEqual((base / restore.PENDING_NAME).exists(), failure in ('cleanup', 'uncertain-create'))
            else:
                result = restore.rehearse_locked(runtime)
                self.assertEqual(result['status'], 'isolated-database-restore-passed')
                self.assertTrue(result['isolateRemoved'])
                self.assertFalse(result['productionDatabaseWritten'])
                self.assertFalse((base / restore.PENDING_NAME).exists())
            held.close.assert_called_once()
            self.assertGreaterEqual(target.cleanup.call_count, 1)

    def test_success_and_injected_failures_never_skip_cleanup_or_emit_false_pass(self):
        for failure in [None, 'dump', 'roles', 'restore', 'counts', 'ledger', 'cleanup', 'interrupt', 'uncertain-create']:
            with self.subTest(failure=failure): self.exercise(failure)


class CandidateMigrationTests(unittest.TestCase):
    def candidate(self):
        sql = 'SELECT 1;'
        return {'schemaVersion': 1, 'sourceRevision': 'f'*40, 'manifestSha256': 'a'*64,
                'sql': sql, 'sqlSha256': hashlib.sha256(sql.encode()).hexdigest(),
                'migrations': [{'id': 'old', 'checksum': 'b'*64, 'compatibleAppliedChecksums': []},
                               {'id': 'new', 'checksum': 'c'*64, 'compatibleAppliedChecksums': []}]}

    def ledger(self, complete=False):
        rows = [{'migration_id': 'old', 'checksum': 'b'*64, 'source_commit': 'd'*40}]
        if complete: rows.append({'migration_id': 'new', 'checksum': 'c'*64, 'source_commit': 'f'*40})
        return rows

    def test_candidate_correspondence_rejects_changed_unknown_duplicate_missing_history(self):
        candidate = self.candidate()
        self.assertEqual(restore.validate_candidate(candidate, self.ledger()), 1)
        self.assertEqual(restore.validate_candidate(candidate, self.ledger(True), complete=True), 0)
        for key, value in [('sql', 'SELECT 2;'), ('sourceRevision', 'main'), ('manifestSha256', 'invalid'),
                           ('migrations', candidate['migrations']*2)]:
            broken = copy.deepcopy(candidate); broken[key] = value
            with self.subTest(key=key), self.assertRaises(ValueError): restore.validate_candidate(broken, self.ledger())
        for ledger in [self.ledger()*2, [{'migration_id': 'unknown', 'checksum': 'b'*64, 'source_commit': 'd'*40}],
                       [{'migration_id': 'old', 'checksum': 'c'*64, 'source_commit': 'd'*40}]]:
            with self.subTest(ledger=ledger), self.assertRaises(ValueError): restore.validate_candidate(candidate, ledger)
        with self.assertRaises(ValueError): restore.validate_candidate(candidate, self.ledger(), complete=True)

    def test_two_applications_preserve_history_and_report_control_changes(self):
        for fault in [None, 'execution', 'missing', 'history', 'second-control-change']:
            with self.subTest(fault=fault), tempfile.TemporaryDirectory() as temporary:
                before = {'migrations': self.ledger(), **{key: None for key in restore.DATABASE_CONTROLS}}
                after = {**before, 'migrations': self.ledger(True), **{key: [{'enabled': True}] for key in restore.DATABASE_CONTROLS}}
                second = copy.deepcopy(after)
                if fault == 'missing': after['migrations'] = self.ledger()
                if fault == 'history': after['migrations'][0]['source_commit'] = 'e'*40
                if fault == 'second-control-change': second['revenueFlags'] = []
                runtime = MagicMock(); runtime.summarize_database.side_effect = [after, second]
                target = restore.IsolatedRestore(SOURCE, IMAGE, IMAGE_ID, NONCE); target.admit(container())
                with patch.object(target, 'inspect'), patch.object(restore, 'execute', return_value='{}'), \
                     patch.object(restore.subprocess, 'run', return_value=type('Result', (), {'returncode': 1 if fault == 'execution' else 0})()) as run:
                    if fault:
                        with self.assertRaises(ValueError): restore.rehearse_candidate(runtime, target, Path(temporary), self.candidate(), before)
                    else:
                        result = restore.rehearse_candidate(runtime, target, Path(temporary), self.candidate(), before)
                        self.assertEqual(result['applications'], 2)
                        self.assertFalse(result['deploymentAuthorized'])
                        self.assertEqual(set(result['controlChanges']), set(restore.DATABASE_CONTROLS))
                        self.assertEqual(result['controlChanges']['revenueFlags']['after'], [{'enabled': True}])
                        self.assertEqual(run.call_count, 2)
                    for call in run.call_args_list:
                        self.assertIn(TARGET, call.args[0]); self.assertNotIn(SOURCE, call.args[0])


original_stat = Path.stat


if __name__ == '__main__': unittest.main()
