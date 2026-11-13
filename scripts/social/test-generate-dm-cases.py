#!/usr/bin/env python3
"""Check that generated DM cases only update the runtime singleton."""
import json
import re
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from contextlib import closing
from pathlib import Path

ROOT = Path(__file__).resolve().parent


class DmCaseScopeTests(unittest.TestCase):
    def assert_scoped_updates(self, sql):
        updates = re.findall(r'UPDATE social_v2_runtime\b[^;]*;', sql)
        self.assertTrue(updates, 'No runtime updates tested')
        with closing(sqlite3.connect(':memory:')) as db:
            # Omit the production CHECK(singleton) so an unrelated sentinel can
            # detect an unbounded UPDATE even if the schema changes later.
            db.execute('CREATE TABLE social_v2_runtime '
                       '(singleton boolean PRIMARY KEY, enabled boolean, activated_once boolean)')
            for update in updates:
                with self.subTest(update=update):
                    db.execute('DELETE FROM social_v2_runtime')
                    db.execute('INSERT INTO social_v2_runtime VALUES (true, NULL, NULL)')
                    db.execute('INSERT INTO social_v2_runtime VALUES (false, NULL, NULL)')
                    self.assertEqual(db.execute(update).rowcount, 1)
                    self.assertEqual(db.execute('SELECT enabled, activated_once FROM '
                                                'social_v2_runtime WHERE NOT singleton').fetchone(),
                                     (None, None))
                    self.assertNotIn(None, db.execute('SELECT enabled, activated_once FROM '
                                                     'social_v2_runtime WHERE singleton').fetchone())

    def test_checked_cases_update_only_the_singleton(self):
        self.assert_scoped_updates((ROOT / 'dm-model-cases.sql').read_text())

    def test_generator_updates_only_the_singleton(self):
        before = '\n'.join('/\\ ' + key + ' = ' + value for key, value in {
            'enabled': 'FALSE', 'activated': 'FALSE', 'exists': 'FALSE',
            'blocked': 'FALSE', 'closed': 'FALSE', 'consent': '{}',
        }.items())
        with tempfile.TemporaryDirectory() as directory:
            graph = Path(directory) / 'traces.dot'
            output = Path(directory) / 'cases.sql'
            after = '/\\ observed = [allowed |-> TRUE]'
            graph.write_text(f'1 [label={json.dumps(before)}];\n'
                             f'2 [label={json.dumps(after)}];\n'
                             '1 -> 2 [label="Insert"];\n')
            subprocess.run([sys.executable, str(ROOT / 'generate-dm-cases.py'),
                            str(graph), str(output)], check=True, capture_output=True, text=True)
            self.assert_scoped_updates(output.read_text())


if __name__ == '__main__':
    unittest.main()
