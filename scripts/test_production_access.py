import contextlib
import io
import json
import pathlib
import subprocess
import sys
import unittest
from types import SimpleNamespace
from unittest.mock import patch
import production_access as access


class AccessTests(unittest.TestCase):
    def test_connection_uses_existing_key_and_strict_identity(self):
        args = access.connection_args({})
        for required in ['BatchMode=yes', 'StrictHostKeyChecking=yes', 'IdentitiesOnly=yes', 'root@178.105.93.101']:
            self.assertIn(required, args)
        with self.assertRaises(ValueError):
            access.connection_args({'TDF_PRODUCTION_SSH_HOST': '-oProxyCommand=bad'})
        with self.assertRaises(ValueError):
            access.connection_args({'TDF_PRODUCTION_SSH_KEY': 'relative/key'})

    def test_credentials_are_allowlisted_and_errors_do_not_echo_secrets(self):
        with patch.object(access, 'remote', return_value=json.dumps({'SMTP_USERNAME': 'u', 'SMTP_PASSWORD': 'p'})):
            self.assertEqual(access.mail_credentials()['SMTP_PASSWORD'], 'p')
        for value in [{'SMTP_PASSWORD': 'secret-canary'}, {'SMTP_USERNAME': 'u', 'SMTP_PASSWORD': 'p', 'other': 'secret-canary'}]:
            with patch.object(access, 'remote', return_value=json.dumps(value)), self.assertRaisesRegex(RuntimeError, '^Production mail configuration unavailable$'):
                access.mail_credentials()
        with patch.object(access.subprocess, 'run', return_value=SimpleNamespace(returncode=1, stderr='secret-canary', stdout='secret-canary')):
            with self.assertRaisesRegex(RuntimeError, '^Production access unavailable$'):
                access.remote('credentials')

    def test_cli_never_prints_credentials(self):
        result = subprocess.run([sys.executable, access.__file__, 'credentials'], capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, '')

    def remote_fixture(self, mode, mutate=None, permissions=0o600):
        def container(service):
            return {'Id': service, 'Image': 'sha256:abc', 'State': {'Running': True},
                    'Config': {'Labels': {'com.docker.compose.project': 'tdf-production',
                        'com.docker.compose.service': service, 'com.docker.compose.project.working_dir': '/opt/tdf/production'},
                        'Env': ['DB_HOST=db', 'DB_NAME=tdf_hq', 'DB_PORT=5432', 'SMTP_USERNAME=u', 'SMTP_PASSWORD=p']},
                    'NetworkSettings': {'Networks': {'tdf-production_database': {'NetworkID': 'n', 'IPAddress': '172.1.1.2', 'Aliases': ['db']}}}}
        api, db = container('api'), container('db')
        if mutate:
            mutate(api, db)
        calls = []
        def run(args, **kw):
            calls.append((args, kw))
            text = json.dumps([api if args[-1] == 'tdf-production-api-1' else db]) if args[:2] == ['docker', 'inspect'] else '{"kind":"metadata"}\n'
            return SimpleNamespace(returncode=0, stdout=text)
        def read(path, *args, **kw):
            return 'TDF_IMAGE=registry/image@sha256:abc\n' if str(path).endswith('/.env') else 'SMTP_USERNAME=u\nSMTP_PASSWORD=p\n'
        output = io.StringIO()
        with patch.object(sys, 'argv', ['remote', mode]), patch.object(sys, 'stdin', io.StringIO('BEGIN READ ONLY; ROLLBACK;')), \
             patch.object(subprocess, 'run', side_effect=run), patch.object(pathlib.Path, 'read_text', read), \
             patch.object(pathlib.Path, 'stat', return_value=SimpleNamespace(st_mode=permissions)), contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()):
            exec(access.REMOTE, {})
        return output.getvalue(), calls

    def test_remote_inventory_forces_readonly_before_psql_and_checks_target(self):
        output, calls = self.remote_fixture('inventory')
        args, kw = calls[-1]
        self.assertIn('PGOPTIONS=-c default_transaction_read_only=on', args)
        self.assertIn('tdf-production-db-1', args)
        self.assertEqual(args[-1], 'tdf_hq')
        self.assertIn('BEGIN READ ONLY', kw['input'])
        self.assertIn('metadata', output)
        mutations = [lambda a, d: a['State'].update(Running=False),
                     lambda a, d: a['Config']['Labels'].update({'com.docker.compose.project': 'tdf-restore'}),
                     lambda a, d: a['Config']['Env'].append('DB_NAME=trader'),
                     lambda a, d: d['NetworkSettings']['Networks']['tdf-production_database'].update(NetworkID='other'),
                     lambda a, d: a.update(Image='sha256:other')]
        for mutation in mutations:
            with self.subTest(mutation=mutation), self.assertRaises(SystemExit):
                self.remote_fixture('inventory', mutation)

    def test_world_readable_secret_file_is_rejected(self):
        with self.assertRaises(SystemExit):
            self.remote_fixture('credentials', permissions=0o644)

    def test_protected_config_must_match_runtime(self):
        output, _ = self.remote_fixture('credentials')
        self.assertEqual(json.loads(output), {'SMTP_USERNAME': 'u', 'SMTP_PASSWORD': 'p'})
        with self.assertRaises(SystemExit):
            self.remote_fixture('credentials', lambda a, d: a['Config']['Env'].append('SMTP_PASSWORD=different'))


if __name__ == '__main__':
    unittest.main()
