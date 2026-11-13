#!/usr/bin/env python3
"""Check generated DM runtime updates without a database server."""
import re
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from contextlib import closing
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CASES = ROOT / 'scripts/social/dm-model-cases.sql'


class DmModelCasesTest(unittest.TestCase):
    def test_generated_cases_match_checked_fixture(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'cases.sql'
            subprocess.run([
                sys.executable, str(ROOT / 'scripts/social/generate-dm-cases.py'),
                str(ROOT / 'formal/social/LegacyDm.dot'), str(output),
            ], check=True, capture_output=True, text=True)
            self.assertEqual(output.read_text(), CASES.read_text())

    def test_runtime_updates_only_change_the_singleton(self):
        updates = re.findall(r'UPDATE social_v2_runtime\b[^;]*;', CASES.read_text())
        self.assertEqual(len(updates), 27)
        for update in updates:
            with self.subTest(update=update), closing(sqlite3.connect(':memory:')) as db:
                expected = tuple(int(value == 'true') for value in re.search(
                    r'enabled=(true|false),activated_once=(true|false)', update).groups())
                initial = tuple(1 - value for value in expected)
                # Omit the production CHECK(singleton) deliberately: a second row
                # makes an unbounded UPDATE observable rather than a silent no-op.
                db.execute('CREATE TABLE social_v2_runtime '
                           '(singleton boolean PRIMARY KEY, enabled boolean, activated_once boolean)')
                db.executemany('INSERT INTO social_v2_runtime VALUES (?, ?, ?)',
                               [(True, *initial), (False, *initial)])
                self.assertEqual(db.execute(update).rowcount, 1)
                self.assertEqual(db.execute('SELECT enabled, activated_once '
                                           'FROM social_v2_runtime WHERE singleton').fetchone(), expected)
                self.assertEqual(db.execute('SELECT enabled, activated_once '
                                           'FROM social_v2_runtime WHERE NOT singleton').fetchone(), initial)


if __name__ == '__main__':
    unittest.main()
