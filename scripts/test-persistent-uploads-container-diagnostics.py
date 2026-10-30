#!/usr/bin/env python3
"""Simulated subprocess tests of the actual hosted runner; never invoke Docker."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import traceback
import types
import unittest
from unittest.mock import patch


SOURCE = Path(__file__).with_name('test-persistent-uploads-container.py')
SPEC = importlib.util.spec_from_file_location('persistent_upload_container_runner', SOURCE)
RUNNER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUNNER)
NONCE = 'simulated-diagnostic-nonce'
NAME = f'tdf-upload-test-{NONCE}-0'


class SimulatedDocker:
    """Every subprocess boundary is mocked; unexpected calls fail the test."""

    def __init__(self, inspect='absent', primary_failure=False, remove_failure=False):
        self.inspect = inspect
        self.primary_failure = primary_failure
        self.remove_failure = remove_failure
        self.calls = []
        self.names = []

    def __call__(self, command, *, env, capture_output, text, timeout):
        if command[:3] != ['docker', '--host', 'unix:///var/run/docker.sock']:
            raise AssertionError(f'Unexpected subprocess: {command!r}')
        if any(key.startswith('DOCKER_') for key in env):
            raise AssertionError('Inherited Docker routing was not removed')
        if not capture_output or not text:
            raise AssertionError('Runner must capture text output')
        args = command[3:]
        self.calls.append(args)
        status, stdout, stderr = 0, '', ''
        if args == ['pull', RUNNER.IMAGE]:
            if timeout != 180:
                raise AssertionError('Image pull timeout changed')
        elif args[0] == 'run':
            if timeout != 90:
                raise AssertionError('Container timeout changed')
            name = args[args.index('--name') + 1]
            if name != f'tdf-upload-test-{NONCE}-{len(self.names)}':
                raise AssertionError('Unexpected synthetic container identity')
            if args[args.index('--label') + 1] != f'net.tdf.upload-test={NONCE}':
                raise AssertionError('Synthetic ownership label changed')
            self.names.append(name)
            if self.primary_failure:
                status, stderr = 71, 'simulated primary failure'
            elif len(self.names) >= 3:
                status, stderr = 78, 'persistent mount at /app/uploads is required'
        elif args[0] == 'inspect' and len(args) == 2 and args[1] in self.names:
            if timeout != 90:
                raise AssertionError('Inspect timeout changed')
            if args[1] != NAME or self.inspect == 'absent':
                status, stderr = 1, f'Error: No such object: {args[1]}'
            elif self.inspect == 'unexpected':
                status, stdout, stderr = 42, 'simulated stdout', 'simulated inspect failure'
            elif self.inspect == 'long':
                status, stdout, stderr = 43, 'O' * 6000, 'E' * 7000
            elif self.inspect == 'empty':
                status = 44
            elif self.inspect == 'malformed':
                stdout = 'invalid synthetic JSON'
            elif self.inspect in ('owned', 'foreign'):
                label = NONCE if self.inspect == 'owned' else 'foreign-nonce'
                stdout = json.dumps([{'Id': 'simulated-container-id',
                                      'Config': {'Labels': {'net.tdf.upload-test': label}}}])
            else:
                raise AssertionError('Unknown simulated inspect response')
        elif args == ['rm', '-f', 'simulated-container-id']:
            if timeout != 90:
                raise AssertionError('Remove timeout changed')
            if self.remove_failure:
                status, stderr = 45, 'simulated removal failure'
        else:
            raise AssertionError(f'Unexpected Docker operation: {args!r}')
        return subprocess.CompletedProcess(command, status, stdout, stderr)


class CleanupDiagnosticsTest(unittest.TestCase):
    def invoke(self, docker):
        output = io.StringIO()
        with patch.dict(RUNNER.os.environ, {
                'GITHUB_ACTIONS': 'true', 'RUNNER_ENVIRONMENT': 'github-hosted',
                'DOCKER_HOST': 'tcp://must-not-be-inherited'}, clear=True), \
                patch.object(RUNNER.uuid, 'uuid4', return_value=types.SimpleNamespace(hex=NONCE)), \
                patch.object(RUNNER.subprocess, 'run', side_effect=docker), \
                patch.object(RUNNER.subprocess, 'check_output', return_value='simulated-revision\n'), \
                contextlib.redirect_stdout(output):
            RUNNER.main()
        return json.loads(output.getvalue())

    def cleanup_error(self, docker):
        with self.assertRaises(RuntimeError) as caught:
            self.invoke(docker)
        error = caught.exception
        self.assertIsInstance(error, RUNNER.ContainerCleanupError)
        return error

    def test_hosted_gate_refuses_before_any_subprocess(self):
        with patch.dict(RUNNER.os.environ, {}, clear=True), \
                patch.object(RUNNER.subprocess, 'run') as run, \
                patch.object(RUNNER.subprocess, 'check_output') as check_output:
            with self.assertRaises(SystemExit):
                RUNNER.main()
            run.assert_not_called()
            check_output.assert_not_called()

    def test_existing_absence_and_five_case_success_are_preserved(self):
        docker = SimulatedDocker()
        result = self.invoke(docker)
        self.assertEqual(len(result['checks']), 5)
        self.assertEqual([row['actualExit'] for row in result['checks']], [0, 0, 78, 78, 78])
        self.assertEqual(len([args for args in docker.calls if args[0] == 'inspect']), 5)
        self.assertFalse(any(args[0] == 'rm' for args in docker.calls))

    def test_unexpected_inspect_retains_command_result_and_completed_cases(self):
        error = self.cleanup_error(SimulatedDocker('unexpected'))
        data = error.diagnostic
        self.assertEqual(data['operation'], 'inspect')
        self.assertEqual(data['containerName'], NAME)
        self.assertEqual(data['returncode'], 42)
        self.assertEqual(data['stdout'], {'text': 'simulated stdout', 'truncated': False})
        self.assertEqual(data['stderr'], {'text': 'simulated inspect failure', 'truncated': False})
        self.assertEqual(len(data['completedCases']), 5)
        self.assertEqual([row['actualExit'] for row in data['completedCases']], [0, 0, 78, 78, 78])
        self.assertEqual(json.loads(str(error).split(': ', 1)[1]), data)

    def test_stream_context_is_bounded_and_marks_truncation(self):
        error = self.cleanup_error(SimulatedDocker('long'))
        for stream, marker in [('stdout', 'O'), ('stderr', 'E')]:
            self.assertEqual(error.diagnostic[stream], {'text': marker * 2000, 'truncated': True})
        self.assertLess(len(str(error)), 7000)

    def test_empty_unexpected_response_does_not_become_absence(self):
        error = self.cleanup_error(SimulatedDocker('empty'))
        self.assertEqual(error.diagnostic['returncode'], 44)
        self.assertEqual(error.diagnostic['stderr'], {'text': '', 'truncated': False})

    def test_primary_failure_remains_chained_with_cleanup_failure(self):
        error = self.cleanup_error(SimulatedDocker('unexpected', primary_failure=True))
        self.assertEqual(error.diagnostic['completedCases'], [])
        self.assertIsInstance(error.__context__, RuntimeError)
        self.assertIn('expected 0, got 71: simulated primary failure', str(error.__context__))
        rendered = ''.join(traceback.format_exception(type(error), error, error.__traceback__))
        self.assertIn('simulated primary failure', rendered)
        self.assertIn('simulated inspect failure', rendered)

    def test_only_verified_owned_container_is_removed_by_id(self):
        docker = SimulatedDocker('owned')
        self.invoke(docker)
        self.assertEqual([args for args in docker.calls if args[0] == 'rm'],
                         [['rm', '-f', 'simulated-container-id']])

    def test_foreign_container_is_never_removed(self):
        docker = SimulatedDocker('foreign')
        with self.assertRaisesRegex(RuntimeError, 'Refusing cleanup of a foreign container'):
            self.invoke(docker)
        self.assertFalse(any(args[0] == 'rm' for args in docker.calls))

    def test_malformed_inspect_does_not_remove_a_container(self):
        docker = SimulatedDocker('malformed')
        with self.assertRaises(json.JSONDecodeError):
            self.invoke(docker)
        self.assertFalse(any(args[0] == 'rm' for args in docker.calls))

    def test_removal_error_still_fails(self):
        with self.assertRaisesRegex(RuntimeError, 'expected 0, got 45: simulated removal failure'):
            self.invoke(SimulatedDocker('owned', remove_failure=True))


if __name__ == '__main__':
    unittest.main(verbosity=2)
