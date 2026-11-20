#!/usr/bin/env python3
"""Actual agreement/identity forward tests, only in the checked loopback test DB.

The negative-control switch installs the original agreement schema without the
new forward migration, then runs the SAME nine archived-insert assertions. It
must fail; it is not a skip, fallback, or successful CI mode.
"""
import argparse
import importlib.util
import unittest
import uuid
from pathlib import Path

import psycopg
from psycopg import sql

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('agreement_fixture', ROOT / 'scripts/test-event-agreements.py')
original = importlib.util.module_from_spec(spec)
spec.loader.exec_module(original)
TARGETS = (
    ('event_workspace_plan', 'recipient_party_id'), ('event_workspace_plan', 'created_by'),
    ('event_agreement', 'recipient_party_id'), ('event_agreement', 'created_by'),
    ('event_agreement_version', 'approved_by'), ('event_agreement_version', 'created_by'),
    ('event_agreement_acceptance', 'accepted_by'), ('event_workspace_action', 'actor_party_id'),
    ('event_workspace_command', 'actor_party_id'),
)
FORWARD = ROOT / 'tdf-hq/sql/2026-11-07_event_agreement_identity_guards.sql'
ROLLBACK = ROOT / 'tdf-hq/sql/2026-11-07_event_agreement_identity_guards_rollback.sql'


class IdentityGuardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        original.AgreementDatabaseTests.setUpClass()
        cls.db = original.AgreementDatabaseTests.db
        cls.schema = original.AgreementDatabaseTests.schema
        cls.dsn = original.AgreementDatabaseTests.dsn
        cls.db.execute("INSERT INTO party VALUES(9001,'Synthetic archived'),(9002,'Synthetic historical')")
        cls.db.execute('INSERT INTO identity_party_archive VALUES(9001)')

    @classmethod
    def tearDownClass(cls):
        original.AgreementDatabaseTests.tearDownClass()

    def graph(self, override=None, approver=3):
        override = override or {}
        value = lambda table, column, default: override.get((table, column), default)
        self.db.execute('BEGIN')
        try:
            event = self.db.execute("INSERT INTO social_event SELECT COALESCE(max(id),0)+1,'1' FROM social_event RETURNING id").fetchone()[0]
            agreement = uuid.uuid4()
            request = uuid.uuid4()
            self.db.execute('INSERT INTO event_workspace_plan(event_id,revision,recipient_party_id,content,document_hash,created_by) VALUES(%s,1,%s,%s::jsonb,%s,%s)',
                (event, value('event_workspace_plan','recipient_party_id',2), '{}', 'synthetic', value('event_workspace_plan','created_by',1)))
            self.db.execute('INSERT INTO event_agreement(id,event_id,recipient_party_id,current_version,state,created_by) VALUES(%s,%s,%s,1,%s,%s)',
                (agreement, event, value('event_agreement','recipient_party_id',2), 'accepted' if approver is not None else 'draft', value('event_agreement','created_by',1)))
            approved_by = value('event_agreement_version','approved_by',approver)
            self.db.execute('INSERT INTO event_agreement_version(agreement_id,version,content,document_hash,approved_by,approved_at,sent_at,created_by) VALUES(%s,1,%s::jsonb,%s,%s,CASE WHEN %s::bigint IS NULL THEN NULL ELSE clock_timestamp() END,CASE WHEN %s::bigint IS NULL THEN NULL ELSE clock_timestamp() END,%s)',
                (agreement, '{}', 'synthetic', approved_by, approved_by, approved_by, value('event_agreement_version','created_by',1)))
            if approver is not None:
                self.db.execute('INSERT INTO event_agreement_acceptance(agreement_id,version,accepted_by,accepted_name,document_hash) VALUES(%s,1,%s,%s,%s)',
                    (agreement, value('event_agreement_acceptance','accepted_by',2), 'Synthetic recipient', 'synthetic'))
            self.db.execute('INSERT INTO event_workspace_action(event_id,agreement_id,version,actor_party_id,action) VALUES(%s,%s,1,%s,%s)',
                (event, agreement, value('event_workspace_action','actor_party_id',1), 'synthetic'))
            actor = value('event_workspace_command','actor_party_id',1)
            self.db.execute('INSERT INTO event_workspace_command(event_id,actor_party_id,request_id,request_hash,response) VALUES(%s,%s,%s,%s,%s::jsonb)',
                (event, actor, request, 'synthetic', '{}'))
            self.db.execute('COMMIT')
            return event, actor, request
        except BaseException:
            self.db.execute('ROLLBACK')
            raise

    def guards(self, db=None):
        return (db or self.db).execute("SELECT c.relname,t.tgargs,t.tgenabled FROM pg_trigger t JOIN pg_class c ON c.oid=t.tgrelid JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname=current_schema() AND t.tgname='identity_archive_reference_guard' ORDER BY c.relname").fetchall()

    def test_active_accounts_and_nullable_approver(self):
        self.graph()
        self.graph(approver=None)
        self.assertEqual(len(self.guards()), 6)
        self.assertEqual(sum(len(bytes(row[1]).rstrip(b'\0').split(b'\0')) for row in self.guards()), 9)
        self.assertTrue(all(row[2] == 'O' for row in self.guards()))

    def test_forward_replay_preserves_arguments_and_archive_denial(self):
        before = self.guards()
        self.db.execute(FORWARD.read_text())
        self.db.execute(FORWARD.read_text())
        self.assertEqual(before, self.guards())
        for target in TARGETS:
            with self.subTest(target=target):
                with self.assertRaises(psycopg.Error) as error:
                    self.graph({target: 9001})
                self.assertEqual(error.exception.sqlstate, '55000')

    def test_unchanged_historical_command_remains_writable_but_reassignment_rejects(self):
        event, actor, request = self.graph({('event_workspace_command','actor_party_id'): 9002})
        self.db.execute('BEGIN')
        try:
            self.db.execute('INSERT INTO identity_party_archive VALUES(9002)')
            self.db.execute("UPDATE event_workspace_command SET response='{" + '"synthetic":true' + "}'::jsonb WHERE event_id=%s AND actor_party_id=%s AND request_id=%s", (event, actor, request))
            with self.assertRaises(psycopg.Error) as error:
                self.db.execute('UPDATE event_workspace_command SET actor_party_id=9001 WHERE event_id=%s AND actor_party_id=%s AND request_id=%s', (event, actor, request))
            self.assertEqual(error.exception.sqlstate, '55000')
        finally:
            self.db.execute('ROLLBACK')

    def test_populated_rollback_refuses_and_retains_evidence_and_guards(self):
        self.graph()
        before = self.guards()
        count = self.db.execute('SELECT count(*) FROM event_agreement_acceptance').fetchone()[0]
        try:
            with self.assertRaises(psycopg.Error) as error:
                self.db.execute(ROLLBACK.read_text())
            self.assertEqual(error.exception.sqlstate, '55000')
            self.assertIn('Event agreement evidence exists', str(error.exception))
        finally:
            self.db.execute('ROLLBACK')
        self.assertEqual(before, self.guards())
        self.assertEqual(count, self.db.execute('SELECT count(*) FROM event_agreement_acceptance').fetchone()[0])

    def test_empty_rollback_replays_and_forward_reinstalls_without_dropping_tables(self):
        schema = 'event_agreement_empty_' + uuid.uuid4().hex
        with psycopg.connect(self.dsn, autocommit=True) as other:
            try:
                other.execute(sql.SQL('CREATE SCHEMA {}').format(sql.Identifier(schema)))
                other.execute(sql.SQL('SET search_path TO {},public').format(sql.Identifier(schema)))
                other.execute('CREATE TABLE party(id BIGINT PRIMARY KEY,display_name TEXT)')
                other.execute('CREATE TABLE user_credential(party_id BIGINT,active BOOLEAN)')
                other.execute('CREATE TABLE social_event(id BIGINT PRIMARY KEY,organizer_party_id TEXT)')
                other.execute("CREATE TABLE notification(id BIGSERIAL PRIMARY KEY,recipient_party_id BIGINT,notif_type TEXT CHECK(notif_type='existing'),title TEXT,body TEXT,target_type TEXT,target_id BIGINT,target_key TEXT,is_read BOOL)")
                other.execute((ROOT / 'tdf-hq/sql/2026-11-03_event_agreements.sql').read_text())
                original.install_identity_guard(other)
                other.execute(ROLLBACK.read_text())
                other.execute(ROLLBACK.read_text())
                self.assertEqual(self.guards(other), [])
                other.execute(FORWARD.read_text())
                self.assertEqual(len(self.guards(other)), 6)
                self.assertEqual(other.execute("SELECT count(*) FROM information_schema.tables WHERE table_schema=current_schema() AND table_name IN ('event_workspace_plan','event_agreement','event_agreement_version','event_agreement_acceptance','event_workspace_action','event_workspace_command')").fetchone()[0], 6)
            finally:
                other.execute('ROLLBACK')
                other.execute(sql.SQL('DROP SCHEMA IF EXISTS {} CASCADE').format(sql.Identifier(schema)))


def archived_insert_case(target):
    def test(self):
        with self.assertRaises(psycopg.Error) as error:
            self.graph({target: 9001})
        self.assertEqual(error.exception.sqlstate, '55000')
    return test


for table, column in TARGETS:
    setattr(IdentityGuardTests, 'test_archived_insert_' + table + '_' + column, archived_insert_case((table, column)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--omit-forward-for-negative-control', action='store_true')
    args = parser.parse_args()
    if args.omit_forward_for_negative_control:
        original.FORWARD_IDENTITY_GUARDS = False
        names = [name for name in unittest.defaultTestLoader.getTestCaseNames(IdentityGuardTests) if name.startswith('test_archived_insert_')]
        suite = unittest.TestSuite(IdentityGuardTests(name) for name in names)
    else:
        suite = unittest.defaultTestLoader.loadTestsFromTestCase(IdentityGuardTests)
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    raise SystemExit(0 if result.wasSuccessful() else 1)
