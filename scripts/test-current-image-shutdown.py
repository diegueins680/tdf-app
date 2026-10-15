#!/usr/bin/env python3
"""Negative boundary controls for the fixed-image synthetic diagnostic."""
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('image_shutdown', Path(__file__).with_name('qualify-current-image-shutdown.py'))
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


def completed(output=b'', code=0, error=b''):
    return subprocess.CompletedProcess([], code, stdout=output, stderr=error)


class BoundaryTests(unittest.TestCase):
    def fixture(self, directory):
        with patch.object(m.sys, 'platform', 'linux'):
            return m.Fixture(Path(directory))

    def test_shared_nonempty_symlink_and_nonlinux_outputs_reject_before_effects(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            root.chmod(0o755)
            with self.assertRaises(ValueError): self.fixture(root)
            root.chmod(0o700)
            (root/'existing').touch()
            with self.assertRaises(ValueError): self.fixture(root)
            with patch.object(m.sys, 'platform', 'darwin'), self.assertRaises(ValueError):
                m.Fixture(root)
            with tempfile.TemporaryDirectory() as other:
                alias = Path(other)/'alias'; alias.symlink_to(root)
                with self.assertRaises(ValueError): self.fixture(alias)

    def test_actual_cli_does_not_dereference_output_symlink(self):
        with tempfile.TemporaryDirectory() as directory, tempfile.TemporaryDirectory() as other:
            alias = Path(other)/'alias'; alias.symlink_to(directory)
            with patch.object(m.sys, 'platform', 'linux'), patch.object(m.Fixture, 'exercise') as exercise:
                with self.assertRaises(ValueError): m.main([str(alias)])
                exercise.assert_not_called()

    def test_host_environment_cannot_route_docker_or_seed_application_secrets(self):
        with tempfile.TemporaryDirectory() as directory, patch.dict(m.os.environ, {
            'DOCKER_HOST': 'tcp://production.invalid:2375', 'DB_PASS': 'host-secret',
            'STRIPE_SECRET_KEY': 'host-secret', 'DATABASE_URL': 'postgres://production.invalid'}):
            fixture = self.fixture(directory)
            self.assertEqual(set(fixture.environment), {'PATH', 'LANG'})
            with patch.object(fixture, 'run', return_value=completed()) as run:
                fixture.docker(['version'])
                self.assertEqual(run.call_args.args[0][:2], ['docker', '--host=unix:///var/run/docker.sock'])
            self.assertNotIn('STRIPE_SECRET_KEY', m.APP_ENV)
            self.assertNotIn('DATABASE_URL', m.APP_ENV)
            self.assertEqual(m.APP_ENV['AUTO_APPLY_PRODUCTION_MIGRATIONS'], 'false')

    def test_wrong_image_revision_entrypoint_or_secret_reject_before_creation(self):
        valid = {'RepoDigests': [m.IMAGE], 'Architecture': 'amd64', 'Os': 'linux', 'Id': 'sha256:'+'a'*64,
                 'Config': {'Env': ['SOURCE_COMMIT='+m.REVISION, 'GIT_SHA='+m.REVISION], 'Entrypoint': None}}
        for mutation in ('digest', 'revision', 'entrypoint', 'secret'):
            row = json.loads(json.dumps(valid))
            if mutation == 'digest': row['RepoDigests'] = []
            if mutation == 'revision': row['Config']['Env'][0] = 'SOURCE_COMMIT='+'b'*40
            if mutation == 'entrypoint': row['Config']['Entrypoint'] = ['/unexpected']
            if mutation == 'secret': row['Config']['Env'].append('STRIPE_SECRET_KEY=synthetic-baked-secret')
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as directory:
                fixture = self.fixture(directory)
                with patch.object(fixture, 'docker', return_value=completed(json.dumps([row]).encode())) as docker:
                    with self.assertRaises(ValueError): fixture.prepare()
                    self.assertEqual(docker.call_count, 1)
                    self.assertEqual(fixture.containers, [])

    def test_missing_historical_fixture_rejects_before_any_docker_request(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.fixture(directory)
            with patch.object(fixture, 'run', side_effect=ValueError('missing historical object')), \
                    patch.object(fixture, 'docker') as docker:
                with self.assertRaises(ValueError): fixture.prepare()
                docker.assert_not_called()
                self.assertEqual(fixture.containers, [])

    def test_foreign_resource_is_never_removed(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.fixture(directory); fixture.containers = ['foreign']
            raw = json.dumps([{'Config': {'Labels': {m.LABEL: 'someone-else'}}}]).encode()
            with patch.object(fixture, 'docker', return_value=completed(raw)) as docker:
                with self.assertRaises(ValueError): fixture.cleanup()
                self.assertTrue(all(call.args[0][0] == 'inspect' for call in docker.call_args_list))

    def test_lost_inspect_response_does_not_authorize_cleanup(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.fixture(directory); fixture.containers = ['owned-intent']
            with patch.object(fixture, 'docker', return_value=completed(code=1, error=b'daemon unavailable')) as docker:
                with self.assertRaises(ValueError): fixture.cleanup()
                docker.assert_called_once()

    def test_dirty_qualification_source_rejects_before_docker(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.fixture(directory)
            revision = ('a'*40+'\n'+'b'*40+'\n').encode()
            with patch.object(fixture, 'run', side_effect=[completed(revision), completed(b'M scripts/runner.py\n')]), \
                    patch.object(fixture, 'docker') as docker:
                with self.assertRaises(ValueError): fixture.source_snapshot()
                docker.assert_not_called()

    def test_shutdown_sigkill_is_observed_and_never_becomes_release_authority(self):
        with tempfile.TemporaryDirectory() as directory:
            fixture = self.fixture(directory); fixture.app = 'synthetic'
            with patch.object(fixture, 'docker', return_value=completed()), \
                    patch.object(fixture, 'inspect', return_value={'State': {'Running': False, 'OOMKilled': False, 'ExitCode': 137}}), \
                    patch.object(fixture, 'healthy', return_value={'commit': m.REVISION}), \
                    patch.object(fixture, 'sql', return_value=b'committed-before-shutdown\n'):
                fixture.observe('SIGTERM')
            self.assertFalse(fixture.report['observations'][0]['quiescentStopAvoidedSigkill'])
            self.assertFalse(fixture.report['releaseAuthorized'])
            self.assertFalse(fixture.report['activeRequestDrainVerified'])

    def test_malformed_or_nonmetadata_http_response_rejects(self):
        for raw in (b'invalid', b'HTTP/1.1 200 OK\r\n\r\n{"token":"never-allowed"}',
                    b'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n{}'):
            with self.subTest(raw=raw), tempfile.TemporaryDirectory() as directory:
                fixture = self.fixture(directory); fixture.app = 'synthetic'
                with patch.object(fixture, 'docker', return_value=completed(raw)), self.assertRaises(ValueError):
                    fixture.probe('/health')


if __name__ == '__main__':
    unittest.main()
