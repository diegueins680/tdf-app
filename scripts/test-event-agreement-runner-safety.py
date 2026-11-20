#!/usr/bin/env python3
"""Fail-closed routing controls; no database command is permitted by this suite."""
import importlib.util
import os
from pathlib import Path
import unittest
from unittest import mock

path = Path(__file__).with_name('test-event-agreements.py')
spec = importlib.util.spec_from_file_location('agreement_routing_fixture', path)
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)
VALID = 'host=127.0.0.1 port=5432 dbname=synthetic_agreements_test user=postgres'


class RunnerRoutingTests(unittest.TestCase):
    def denied_before_connect(self, dsn=VALID, **overrides):
        with mock.patch.dict(os.environ, {'EVENT_AGREEMENTS_TEST_DSN': dsn, **overrides}, clear=True):
            with mock.patch.object(fixture.psycopg, 'connect') as connect:
                with self.assertRaises((RuntimeError, fixture.psycopg.Error)):
                    fixture.AgreementDatabaseTests.setUpClass()
                connect.assert_not_called()

    def test_explicit_loopback_ports(self):
        with mock.patch.dict(os.environ, {}, clear=True):
            self.assertEqual(fixture.checked_test_dsn(VALID), VALID)
            other = 'host=localhost port=18747 dbname=synthetic_test user=postgres'
            self.assertEqual(fixture.checked_test_dsn(other), other)

    def test_forbidden_inherited_routing(self):
        for key in ('PGHOSTADDR','PGSERVICE','PGSERVICEFILE','PGOPTIONS','PGTARGETSESSIONATTRS','PGLOADBALANCEHOSTS'):
            with self.subTest(key=key):
                self.denied_before_connect(**{key: 'synthetic-override'})

    def test_remote_and_missing_hosts(self):
        for value in ('host=production.invalid dbname=synthetic_test', 'dbname=synthetic_test', 'host=127.0.0.1,production.invalid dbname=synthetic_test'):
            with self.subTest(dsn=value):
                self.denied_before_connect(value)

    def test_ports_and_database_scope(self):
        for value in ('port=0', 'port=65536', "port='5432@127.0.0.1'", 'port=1.0'):
            with self.subTest(port=value):
                self.denied_before_connect('host=127.0.0.1 dbname=synthetic_test ' + value)
        self.denied_before_connect('host=127.0.0.1 dbname=production')
        self.denied_before_connect("host=127.0.0.1 dbname='synthetic/test'")

    def test_missing_port_rejects_inherited_pgport_before_connect(self):
        self.denied_before_connect('host=127.0.0.1 dbname=synthetic_test user=postgres', PGPORT='6432')

    def test_dsn_service_hostaddr_and_options(self):
        for extra in ('service=synthetic', 'hostaddr=192.0.2.1', "options='-c search_path=public'"):
            with self.subTest(extra=extra):
                self.denied_before_connect(VALID + ' ' + extra)


if __name__ == '__main__':
    unittest.main(verbosity=2)
