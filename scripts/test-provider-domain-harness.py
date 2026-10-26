#!/usr/bin/env python3
"""Independent invalid-error/snapshot controls for the real admission harness.

Extract only denied(), avoiding database setup and provider network access. The
production database suite remains responsible for actual transaction behavior.
Run normally and with -O: compilation must preserve the rejection checks.
"""
import ast
from pathlib import Path
from types import SimpleNamespace
import unittest


class DatabaseError(Exception):
    def __init__(self, code, message):
        self.pgcode = code
        self.diag = SimpleNamespace(message_primary=message)


class AdmissionHarnessControls(unittest.TestCase):
    def setUp(self):
        path = Path(__file__).with_name('test-provider-domain-binding.py')
        tree = ast.parse(path.read_text(), filename=str(path))
        function = next(n for n in tree.body
                        if isinstance(n, ast.FunctionDef) and n.name == 'denied')
        self.code = compile(ast.Module(body=[function], type_ignores=[]),
                            str(path), 'exec')
        self.expected = ('Service booking checkout cannot become paid without '
                         'bound verified payment evidence')

    def invoke(self, code, message, snapshots=('before', 'before')):
        calls = []
        values = iter(snapshots)

        def settle(*_):
            calls.append('settle')
            raise DatabaseError(code, message)

        namespace = {
            'snapshot': lambda *_: next(values),
            'execute': lambda _, query: calls.append(query),
            'settle': settle,
            'psycopg2': SimpleNamespace(Error=DatabaseError),
            'DOMAINS': {'service_booking': 'Service booking'},
        }
        exec(self.code, namespace)
        namespace['denied'](None, 'service_booking', 'synthetic', 100)
        return calls

    def test_named_boundary_rejection_rolls_back_and_checks_snapshot(self):
        self.assertEqual(self.invoke('P0001', self.expected),
                         ['SAVEPOINT denied', 'settle',
                          'ROLLBACK TO SAVEPOINT denied'])

    def test_unrelated_sqlstate_is_not_boundary_evidence(self):
        with self.assertRaises(AssertionError):
            self.invoke('23503', self.expected)

    def test_wrong_guard_message_is_not_boundary_evidence(self):
        with self.assertRaises(AssertionError):
            self.invoke('P0001', 'Unrelated policy validation failed')

    def test_changed_snapshot_is_not_a_successful_rollback(self):
        with self.assertRaises(AssertionError):
            self.invoke('P0001', self.expected, ('before', 'corrupted'))


if __name__ == '__main__':
    unittest.main()
