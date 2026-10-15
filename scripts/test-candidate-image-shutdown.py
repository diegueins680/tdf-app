#!/usr/bin/env python3
"""Fail-closed candidate binding controls; actual Docker execution runs in Build Image."""
import hashlib
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('candidate_shutdown', Path(__file__).with_name('qualify-candidate-image-shutdown.py'))
m = importlib.util.module_from_spec(spec); spec.loader.exec_module(m)
IMAGE = 'diegueins680/tdf-hq@sha256:'+'a'*64
REVISION = 'b'*40


class CandidateTests(unittest.TestCase):
    def fixture(self, directory):
        root = Path(directory); output = root/'output'; output.mkdir(mode=0o700)
        binary = root/'binary'; binary.write_bytes(b'qualified synthetic artifact')
        with patch.object(m.base.sys, 'platform', 'linux'), \
                patch.object(m.subprocess, 'check_output', return_value=REVISION+'\n'), \
                patch.object(m.subprocess, 'run', return_value=subprocess.CompletedProcess([], 0, b'canonical SQL', b'')) as render:
            fixture = m.CandidateFixture(output, IMAGE, REVISION, binary)
        return fixture, render.call_args

    def test_mutable_foreign_or_malformed_image_rejects_before_effects(self):
        for image in ['diegueins680/tdf-hq:latest', 'foreign/tdf-hq@sha256:'+'a'*64,
                      IMAGE+'\nextra', 'tcp://production.invalid', IMAGE[:-1]]:
            with self.subTest(image=image), tempfile.TemporaryDirectory() as directory, \
                    patch.object(m.subprocess, 'check_output') as git, patch.object(m.base.Fixture, '__init__') as create:
                with self.assertRaises(ValueError):
                    m.CandidateFixture(Path(directory), image, REVISION, Path(directory)/'missing')
                git.assert_not_called(); create.assert_not_called()

    def test_wrong_source_empty_or_symlinked_artifact_rejects_before_fixture(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); binary = root/'binary'; binary.write_bytes(b'artifact')
            with patch.object(m.subprocess, 'check_output', return_value='c'*40+'\n'), \
                    patch.object(m.base.Fixture, '__init__') as create:
                with self.assertRaises(ValueError): m.CandidateFixture(root, IMAGE, REVISION, binary)
                create.assert_not_called()
            alias = root/'alias'; alias.symlink_to(binary)
            empty = root/'empty'; empty.touch()
            for invalid in (alias, empty):
                with self.subTest(invalid=invalid), patch.object(m.base.Fixture, '__init__') as create:
                    with self.assertRaises(ValueError): m.CandidateFixture(root, IMAGE, REVISION, invalid)
                    create.assert_not_called()

    def test_renderer_receives_only_source_bound_environment(self):
        with tempfile.TemporaryDirectory() as directory, patch.dict(m.os.environ, {'NODE_OPTIONS': '--unsafe', 'DB_PASS': 'host-secret'}):
            fixture, call = self.fixture(directory)
            self.assertEqual(set(call.kwargs['env']), {'PATH', 'LANG', 'SOURCE_COMMIT'})
            self.assertEqual(call.kwargs['env']['SOURCE_COMMIT'], REVISION)
            self.assertEqual(call.kwargs['cwd'], m.ROOT)
            self.assertEqual(fixture.expected_binary, hashlib.sha256(b'qualified synthetic artifact').hexdigest())
            self.assertFalse(fixture.report['releaseAuthorized'])

    def test_packaged_sql_must_match_canonical_render_exactly(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture, _ = self.fixture(directory)
            fixture.validate_migration_bundle(b'canonical SQL')
            for sql in (b'canonical SQL\n', b'different SQL', b''):
                with self.subTest(sql=sql), self.assertRaises(ValueError): fixture.validate_migration_bundle(sql)

    def test_artifact_change_invalidates_source_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture, _ = self.fixture(directory)
            with patch.object(m.base.Fixture, 'source_snapshot', return_value={'revision': REVISION}):
                snapshot = fixture.source_snapshot()
                self.assertEqual(snapshot['testedBinarySha256'], fixture.expected_binary)
                fixture.binary.write_bytes(b'changed artifact')
                with self.assertRaises(ValueError): fixture.source_snapshot()

    def test_sigkill_nonzero_or_late_quiescent_stop_cannot_qualify(self):
        for code, elapsed in [(137, 10), (255, .1), (0, 7.1), (0, .2)]:
            with self.subTest(code=code, elapsed=elapsed), tempfile.TemporaryDirectory() as directory:
                fixture, _ = self.fixture(directory)
                def observe(_self, signal):
                    _self.report['observations'].append({'exitCode': code, 'elapsedSeconds': elapsed})
                with patch.object(m.base.Fixture, 'observe', observe), patch.object(fixture, 'drain_request') as drain:
                    if code or elapsed >= 7:
                        with self.assertRaises(ValueError): fixture.observe('SIGTERM')
                        drain.assert_not_called()
                    else:
                        fixture.observe('SIGTERM'); drain.assert_called_once_with('SIGTERM')

    def test_incomplete_or_cleanup_missing_observations_never_become_qualified(self):
        for complete, removed in [(False, True), (True, False), (True, True)]:
            with self.subTest(complete=complete, removed=removed), tempfile.TemporaryDirectory() as directory:
                fixture, _ = self.fixture(directory)
                def exercise(_self):
                    _self.report['ownedResourcesRemoved'] = removed
                    _self.report['observations'] = ([{'signal': signal, 'kind': kind}
                        for signal in ('SIGTERM', 'SIGINT') for kind in ('quiescent', 'witnessed-blocked-public-course-read')]
                        if complete else [])
                with patch.object(m.base.Fixture, 'exercise', exercise):
                    if complete and removed:
                        fixture.exercise()
                        self.assertEqual(fixture.report['status'], 'candidate-image-http-shutdown-qualified')
                        self.assertFalse(fixture.report['releaseAuthorized'])
                    else:
                        with self.assertRaises(ValueError): fixture.exercise()
                        self.assertFalse(fixture.report['activeRequestDrainVerified'])

    def test_listener_closure_needs_explicit_refusal_not_timeout_or_cli_failure(self):
        error = b'bash: line 1: /dev/tcp/127.0.0.1/8080: Connection refused'
        for code, stderr, expected in [(1, error, True), (0, error, False),
                (124, error, False), (125, error, False), (1, b'Docker unavailable', False)]:
            with self.subTest(code=code, stderr=stderr):
                self.assertEqual(m.listener_refused(subprocess.CompletedProcess([], code, b'', stderr)), expected)

    def test_complete_expected404_response_with_length_or_eof_framing(self):
        body = b'Curso no encontrado'
        for headers in (b'', b'Content-Length: 19\r\n'):
            response = b'HTTP/1.0 404 Not Found\r\n'+headers+b'\r\n'+body
            self.assertEqual(m.completed_missing_course_response(response), body)

    def test_header_only_truncated_or_invalidly_framed404_never_counts_as_completion(self):
        body = b'Curso no encontrado'
        responses = [b'HTTP/1.0 404 Not Found\r\n\r\n',
            b'HTTP/1.0 404 Not Found\r\n\r\nCurso no',
            b'HTTP/1.0 404 Not Found\r\nContent-Length: 19\r\n\r\nCurso no',
            b'HTTP/1.0 404 Not Found\r\nContent-Length: 1\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\nContent-Length: 19\r\nContent-Length: 20\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\nTransfer-Encoding: chunked\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\nContent-Length: -1\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\n folded: invalid\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\nX-Debug: invalid\nheader\r\n\r\n'+body,
            b'HTTP/1.0 404 Not Found\r\n\r\n'+body+b'extra',
            b'HTTP/1.0 200 OK\r\n\r\n'+body]
        for response in responses:
            with self.subTest(response=response), self.assertRaises(ValueError):
                m.completed_missing_course_response(response)

    def test_historical_diagnostic_stays_fixed_and_cannot_take_candidate_arguments(self):
        self.assertEqual(m.base.Fixture.image, m.base.IMAGE)
        self.assertEqual(m.base.Fixture.revision, m.base.REVISION)
        self.assertEqual(m.base.Fixture.migration_count, 188)
        with self.assertRaises(ValueError): m.base.main([IMAGE, REVISION, 'binary', 'output'])


if __name__ == '__main__': unittest.main()
