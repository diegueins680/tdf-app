import contextlib
import io
import json
import os
import urllib.request
import pathlib
import subprocess
import stat
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

    def remote_fixture(self, mode, mutate=None, permissions=0o600, repo_digests=None, sql_override=None, postgres_image='postgres@sha256:manifest', database_repo_digests=None, owner=0, file_type=stat.S_IFREG, open_error=None):
        def container(service):
            return {'Id': service, 'Image': 'sha256:local-config', 'State': {'Running': True},
                    'Config': {'Image': 'registry/image@sha256:manifest', 'Labels': {'com.docker.compose.project': 'tdf-production',
                        'com.docker.compose.service': service, 'com.docker.compose.project.working_dir': '/opt/tdf/production'},
                        'Env': ['DB_HOST=db', 'DB_NAME=tdf_hq', 'DB_PORT=5432', 'SMTP_USERNAME=u', 'SMTP_PASSWORD=p']},
                    'NetworkSettings': {'Networks': {'tdf-production_database': {'NetworkID': 'n', 'IPAddress': '172.1.1.2', 'Aliases': ['db']}}}}
        api, db = container('api'), container('db')
        db['Image'] = 'sha256:database-config'
        db['Config']['Image'] = 'postgres@sha256:manifest'
        db['Mounts'] = [{'Type': 'volume', 'Name': 'tdf_production_postgres_data', 'Destination': '/var/lib/postgresql/data'}]
        if mutate:
            mutate(api, db)
        calls = []
        def run(args, **kw):
            calls.append((args, kw))
            text = (json.dumps([api if args[-1] == 'tdf-production-api-1' else db]) if args[:2] == ['docker', 'inspect'] else json.dumps([{'RepoDigests': (database_repo_digests if args[-1] == 'sha256:database-config' else repo_digests) or []}]) if args[:3] == ['docker', 'image', 'inspect'] else 't\n' if '-c' in args else '{"kind":"metadata"}\n')
            return SimpleNamespace(returncode=0, stdout=text)
        def read(path, *args, **kw):
            return ('TDF_IMAGE=registry/image@sha256:manifest\n' + (f'POSTGRES_IMAGE={postgres_image}\n' if postgres_image is not None else '')) if str(path).endswith('/.env') else 'SMTP_USERNAME=u\nSMTP_PASSWORD=p\n'
        output = io.StringIO()
        credential_stream = io.StringIO('SMTP_USERNAME=u\nSMTP_PASSWORD=p\n')
        credential_stream.fileno = lambda: 42
        with patch.object(sys, 'argv', ['remote', mode]), patch.object(sys, 'stdin', io.StringIO(sql_override if sql_override is not None else pathlib.Path(__file__).with_name('production-catalog-inventory.mjs').read_text().split('const inventorySql = String.raw`', 1)[1].split('`;', 1)[0])), \
             patch.object(subprocess, 'run', side_effect=run), patch.object(pathlib.Path, 'read_text', read), \
             patch.object(os, 'open', return_value=42, side_effect=open_error), \
             patch.object(os, 'fdopen', return_value=credential_stream), \
             patch.object(os, 'fstat', return_value=SimpleNamespace(st_mode=file_type | permissions, st_uid=owner)), \
             patch.dict(os.environ, {'SSH_CONNECTION': '192.0.2.10 50000 178.105.93.101 22'}), \
             patch.object(urllib.request, 'urlopen', side_effect=lambda *a, **kw: io.StringIO('{}')), contextlib.redirect_stdout(output), contextlib.redirect_stderr(io.StringIO()):
            exec(access.REMOTE, {})
        return output.getvalue(), calls

    def test_metadata_origin_comes_from_authenticated_ssh_connection(self):
        output, _ = self.remote_fixture('metadata')
        self.assertEqual(json.loads(output)['sshServerAddress'], '178.105.93.101')

    def test_remote_inventory_forces_readonly_before_psql_and_checks_target(self):
        output, calls = self.remote_fixture('inventory')
        args, kw = calls[-1]
        self.assertIn('PGOPTIONS=-c default_transaction_read_only=on', args)
        self.assertIn('tdf-production-db-1', args)
        self.assertEqual(args[-1], 'tdf_hq')
        self.assertIn('BEGIN TRANSACTION READ ONLY', kw['input'])
        self.assertIn('tdf_catalog_inventory', args)
        self.assertNotIn('postgres', args)
        self.assertIn('metadata', output)
        mutations = [lambda a, d: a['State'].update(Running=False),
                     lambda a, d: a['Config']['Labels'].update({'com.docker.compose.project': 'tdf-restore'}),
                     lambda a, d: a['Config']['Env'].append('DB_NAME=trader'),
                     lambda a, d: d['NetworkSettings']['Networks']['tdf-production_database'].update(NetworkID='other'),
                     lambda a, d: a['Config'].update(Image='registry/image@sha256:other')]
        for mutation in mutations:
            with self.subTest(mutation=mutation), self.assertRaises(SystemExit):
                self.remote_fixture('inventory', mutation)

    def test_wrong_missing_bind_or_shadowing_database_store_is_rejected(self):
        for mode in ['metadata', 'inventory', 'credentials']:
            for mounts in [[],
                [{'Type': 'volume', 'Name': 'restore_data', 'Destination': '/var/lib/postgresql/data'}],
                [{'Type': 'bind', 'Name': 'tdf_production_postgres_data', 'Destination': '/var/lib/postgresql/data'}],
                [{'Type': 'volume', 'Name': 'tdf_production_postgres_data', 'Destination': '/somewhere/else'}],
                [{'Type': 'volume', 'Name': 'tdf_production_postgres_data', 'Destination': '/var/lib/postgresql/data'},
                 {'Type': 'bind', 'Destination': '/var/lib/postgresql/data/base'}]]:
                with self.subTest(mode=mode, mounts=mounts), self.assertRaises(SystemExit):
                    self.remote_fixture(mode, lambda a, d: d.update(Mounts=mounts))
        output, _ = self.remote_fixture('metadata')
        self.assertEqual(json.loads(output)['databaseVolume'], 'tdf_production_postgres_data')

    def test_registry_digest_is_distinct_from_local_image_id(self):
        output, _ = self.remote_fixture('inventory',
            lambda a, d: a['Config'].update(Image='sha256:local-config'),
            repo_digests=['registry/image@sha256:manifest'])
        self.assertIn('metadata', output)
        with self.assertRaises(SystemExit):
            self.remote_fixture('inventory',
                lambda a, d: a['Config'].update(Image='sha256:local-config'),
                repo_digests=['registry/image@sha256:wrong'])

    def test_database_image_must_match_the_configured_digest_in_every_mode(self):
        for mode in ['metadata', 'inventory', 'credentials']:
            for configured in [None, '', 'postgres:17', 'postgres@sha256:other']:
                with self.subTest(mode=mode, configured=configured), self.assertRaises(SystemExit):
                    self.remote_fixture(mode, postgres_image=configured)
            with self.subTest(mode=mode), self.assertRaises(SystemExit):
                self.remote_fixture(mode, lambda a, d: d['Config'].update(Image='postgres@sha256:stale'))
        output, _ = self.remote_fixture('metadata')
        self.assertEqual(json.loads(output)['configuredDatabaseImage'], 'postgres@sha256:manifest')
        output, _ = self.remote_fixture('metadata',
            lambda a, d: d['Config'].update(Image='sha256:database-config'),
            database_repo_digests=['postgres@sha256:manifest'])
        self.assertEqual(json.loads(output)['databaseImage'], 'sha256:database-config')
        with self.assertRaises(SystemExit):
            self.remote_fixture('metadata',
                lambda a, d: d['Config'].update(Image='sha256:database-config'),
                database_repo_digests=['postgres@sha256:wrong'])

    def test_unreviewed_sql_and_readwrite_override_are_rejected(self):
        for sql in ['SET default_transaction_read_only=off; SELECT 1;', '\\! echo unsafe', 'BEGIN READ WRITE; UPDATE country SET name=name;']:
            with self.subTest(sql=sql), self.assertRaises(SystemExit):
                self.remote_fixture('inventory', sql_override=sql)

    def test_world_readable_secret_file_is_rejected(self):
        with self.assertRaises(SystemExit):
            self.remote_fixture('credentials', permissions=0o644)

    def test_nonroot_and_nonregular_secret_files_are_rejected(self):
        for args in [{'owner': 1000}, {'file_type': stat.S_IFLNK}, {'file_type': stat.S_IFDIR}, {'file_type': stat.S_IFIFO}, {'open_error': OSError('symlink rejected')}]:
            with self.subTest(args=args), self.assertRaises(SystemExit):
                self.remote_fixture('credentials', **args)

    def test_protected_config_must_match_runtime(self):
        output, _ = self.remote_fixture('credentials')
        self.assertEqual(json.loads(output), {'SMTP_USERNAME': 'u', 'SMTP_PASSWORD': 'p'})
        with self.assertRaises(SystemExit):
            self.remote_fixture('credentials', lambda a, d: a['Config']['Env'].append('SMTP_PASSWORD=different'))


if __name__ == '__main__':
    unittest.main()
