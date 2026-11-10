#!/usr/bin/env python3
"""Routing controls only: a sentinel psql never connects or executes SQL."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

parser = argparse.ArgumentParser(add_help=False)
parser.add_argument('--script', type=Path, default=Path(__file__).with_name('test-studio-gallery-migration.sh'))
args, remaining = parser.parse_known_args()
DRIVER = args.script.resolve()
VALID = 'postgresql://fixture:fixture@127.0.0.1:18792/tdf_studio_gallery_test'

class GalleryRunnerSafety(unittest.TestCase):
    def run_driver(self, url, extra=None, arguments=()):
        with tempfile.TemporaryDirectory(prefix='gallery-routing-') as directory:
            root = Path(directory)
            sentinel = root / 'psql-called.json'
            fake = root / 'psql'
            fake.write_text('#!/usr/bin/env python3\nimport json,os,sys\nfrom pathlib import Path\nPath(os.environ["TDF_GALLERY_PSQL_SENTINEL"]).write_text(json.dumps(sys.argv[1:]))\nsys.exit(73)\n')
            fake.chmod(0o700)
            env = os.environ.copy()
            for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE'):
                env.pop(key, None)
            env.update(PATH=str(root) + os.pathsep + env['PATH'],
                       TDF_GALLERY_TEST_DATABASE_URL=url,
                       TDF_GALLERY_PSQL_SENTINEL=str(sentinel))
            env.update(extra or {})
            result = subprocess.run(['bash', str(DRIVER), *arguments], env=env,
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=5)
            return result, json.loads(sentinel.read_text()) if sentinel.exists() else None

    def assert_no_connection(self, url=VALID, extra=None):
        result, call = self.run_driver(url, extra)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(call, 'Unsafe routing reached psql before rejection')

    def test_query_host_override_rejected_before_psql(self):
        self.assert_no_connection(VALID + '?host=outside.invalid')

    def test_query_hostaddr_override_rejected_before_psql(self):
        self.assert_no_connection(VALID + '?hostaddr=192.0.2.1')

    def test_query_service_override_rejected_before_psql(self):
        self.assert_no_connection(VALID + '?service=foreign')

    def test_uri_fragment_rejected_before_psql(self):
        self.assert_no_connection(VALID + '#unexpected')

    def test_environment_hostaddr_rejected_before_psql(self):
        self.assert_no_connection(extra={'PGHOSTADDR': '192.0.2.1'})

    def test_environment_service_rejected_before_psql(self):
        self.assert_no_connection(extra={'PGSERVICE': 'foreign'})

    def test_environment_servicefile_rejected_before_psql(self):
        self.assert_no_connection(extra={'PGSERVICEFILE': '/synthetic/nonexistent-service.conf'})

    def test_remote_host_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace('127.0.0.1', 'outside.invalid'))

    def test_encoded_host_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace('127.0.0.1', '%2Ftmp'))

    def test_missing_port_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace(':18792/', '/'))

    def test_zero_port_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace(':18792/', ':0/'))

    def test_out_of_range_port_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace(':18792/', ':65536/'))

    def test_other_database_rejected_before_psql(self):
        self.assert_no_connection(VALID.replace('tdf_studio_gallery_test', 'ordinary_database'))

    def test_valid_literal_loopback_reaches_only_sentinel_psql(self):
        result, call = self.run_driver(VALID)
        self.assertEqual(result.returncode, 2)  # Sentinel is deliberately not a database.
        self.assertIsNotNone(call)
        self.assertIn(VALID, call)
        self.assertIn('-X', call)
        self.assertIn('ON_ERROR_STOP=1', call)

    def test_valid_localhost_reaches_only_sentinel_psql(self):
        url = VALID.replace('127.0.0.1', 'localhost')
        result, call = self.run_driver(url)
        self.assertEqual(result.returncode, 2)
        self.assertIsNotNone(call)
        self.assertIn(url, call)

    def test_routing_only_mode_returns_without_psql(self):
        result, call = self.run_driver(VALID, arguments=('--check-routing-only',))
        self.assertEqual(result.returncode, 0)
        self.assertIsNone(call)
        self.assertIn(b'no SQL executed', result.stdout)

    def test_routing_only_mode_still_rejects_current_process_overrides(self):
        result, call = self.run_driver(VALID, {'PGHOSTADDR': '192.0.2.1'},
                                       arguments=('--check-routing-only',))
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(call)

    def test_unknown_mode_rejected_before_psql(self):
        result, call = self.run_driver(VALID, arguments=('--not-a-fixture-mode',))
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(call)

if __name__ == '__main__':
    unittest.main(argv=[sys.argv[0], *remaining])
