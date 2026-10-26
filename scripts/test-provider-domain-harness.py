#!/usr/bin/env python3
"""Independent invalid-error/snapshot controls for the real admission harness.

Extract denied() and execute the actual shell orchestration with inert tools.
No database or provider is contacted by these controls. The production database
suite remains responsible for actual transaction and expiry behavior.
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


class ProviderRunnerGateControls(unittest.TestCase):
    """Execute the real shell runner with inert tools, checking fail-closed wiring."""

    def run_runner(self, expiry_exit):
        import os
        import subprocess
        import sys
        import tempfile

        with tempfile.TemporaryDirectory(prefix='tdf-provider-gate-') as directory:
            root = Path(directory)
            scripts = root / 'scripts'
            scripts.mkdir()
            (root / 'tdf-hq').mkdir()
            tools = root / 'bin'
            tools.mkdir()
            for name in ('test-provider-retry-runtime.sh', 'load-payment-test-schema.sh'):
                (scripts / name).write_bytes(Path(__file__).with_name(name).read_bytes())
            # Every database, Node, Python and Stack command is inert. No fixture
            # database or Docker daemon is contacted by this orchestration test.
            stub = '''#!PYTHON
import os, pathlib, sys
name = pathlib.Path(sys.argv[0]).name
args = sys.argv[1:]
trace = pathlib.Path(os.environ['GATE_TRACE'])
if name == 'python3' and args[0].endswith('/verify-payment-intent-expiry.py'):
    with trace.open('a') as output: output.write('expiry\\n')
    sys.exit(int(os.environ['GATE_EXPIRY_EXIT']))
if name == 'stack':
    with trace.open('a') as output: output.write('stack\\n')
if name == 'git': print('0' * 40)
if name == 'node' and args and args[0] == '-p': print('223')
if name == 'psql':
    if '-c' in args:
        query = args[args.index('-c') + 1]
        if 'current_database()' in query: print('tdf_provider_retry_test')
        elif 'tdf_schema_migration' in query: print('223')
        elif 'revenue_feature_flag' in query: print('1')
        elif 'pg_class' in query: print('0')
        else: print('1')
    if '-f' in args and args[args.index('-f') + 1].endswith('_rollback.sql'):
        if trace.exists() and 'stack' in trace.read_text(): sys.exit(1)
'''.replace('PYTHON', sys.executable, 1)
            for name in ('node', 'psql', 'python3', 'stack', 'git'):
                target = tools / name
                target.write_text(stub)
                target.chmod(0o700)
            trace = root / 'trace'
            environment = {
                'PATH': str(tools) + os.pathsep + os.environ['PATH'],
                'TDF_PROVIDER_RETRY_DATABASE_URL': 'postgresql://localhost/tdf_provider_retry_test',
                'GATE_TRACE': str(trace),
                'GATE_EXPIRY_EXIT': str(expiry_exit),
            }
            result = subprocess.run(['sh', str(scripts / 'test-provider-retry-runtime.sh')],
                                    env=environment, text=True, capture_output=True)
            return result, trace.read_text().splitlines() if trace.exists() else []

    def test_intent_expiry_verification_precedes_compiled_provider_suite(self):
        result, trace = self.run_runner(0)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(trace, ['expiry', 'stack'])

    def test_intent_expiry_verification_failure_blocks_compiled_suite(self):
        result, trace = self.run_runner(37)
        self.assertEqual(result.returncode, 37, result.stderr)
        self.assertEqual(trace, ['expiry'])


if __name__ == '__main__':
    unittest.main()
