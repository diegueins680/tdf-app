#!/usr/bin/env python3
"""Check the row scope of the archived records-ingestion fixture update."""
from pathlib import Path
import re
import sqlite3
import unittest


class RecordsIngestionRecoverySqlTests(unittest.TestCase):
    def test_enablement_only_updates_singleton(self):
        fixture = Path(__file__).resolve().parent.parent / (
            "docs/records-ingestion/recovery-2026-09-25/new-files/"
            "tdf-hq/test/sql/records_ingestion_invariants.sql"
        )
        updates = re.findall(
            r"\bUPDATE\s+records_ingestion_control\b[^;]*;",
            fixture.read_text(),
            re.IGNORECASE,
        )
        self.assertEqual(len(updates), 1)
        db = sqlite3.connect(":memory:")
        self.addCleanup(db.close)
        # Deliberately relax the production singleton CHECK to expose row scope.
        db.execute("CREATE TABLE records_ingestion_control (singleton boolean PRIMARY KEY, enabled boolean)")
        db.executemany("INSERT INTO records_ingestion_control VALUES (?, false)", [(True,), (False,)])
        db.execute(updates[0])
        self.assertEqual(
            db.execute("SELECT singleton, enabled FROM records_ingestion_control ORDER BY singleton").fetchall(),
            [(0, 0), (1, 1)],
        )


if __name__ == "__main__":
    unittest.main()
