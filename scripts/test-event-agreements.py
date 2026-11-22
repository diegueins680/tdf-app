#!/usr/bin/env python3
"""Exercise the actual migration in an isolated schema of a loopback test DB.

Requires psycopg 3 and EVENT_AGREEMENTS_TEST_DSN naming a database ending _test.
Never contacts a provider, creates users in production, or accepts a real agreement.
"""
import concurrent.futures
import copy
import json
import os
import re
from pathlib import Path
import unittest
import uuid

import psycopg
from psycopg import sql
from psycopg.conninfo import conninfo_to_dict

ROOT = Path(__file__).resolve().parent.parent
FORWARD_IDENTITY_GUARDS = True  # Only the explicit negative-control driver changes this.


def checked_test_dsn(value):
    forbidden = ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE', 'PGOPTIONS', 'PGTARGETSESSIONATTRS', 'PGLOADBALANCEHOSTS')
    if any(os.environ.get(key) for key in forbidden):
        raise RuntimeError('Inherited libpq routing overrides are not permitted')
    routing = conninfo_to_dict(value)
    if set(routing) - {'host', 'port', 'dbname', 'user', 'password', 'application_name'}:
        raise RuntimeError('Only explicit disposable database connection fields are permitted')
    port = routing.get('port', '')
    if (routing.get('host') not in ('127.0.0.1', 'localhost')
            or not re.fullmatch(r'[0-9]{1,5}', port) or not 1 <= int(port) <= 65535
            or not re.fullmatch(r'[a-zA-Z0-9_]+_test', routing.get('dbname', ''))):
        raise RuntimeError('An explicit loopback test database is required')
    return value


def install_identity_guard(db):
    db.execute('CREATE TABLE identity_party_archive(party_id BIGINT PRIMARY KEY REFERENCES party(id))')
    canonical = (ROOT / 'tdf-hq/sql/2026-09-17_identity_reconciliation.sql').read_text()
    start = canonical.index('CREATE OR REPLACE FUNCTION identity_reject_archived_reference()')
    end = canonical.index('CREATE OR REPLACE FUNCTION identity_reject_archived_party_write()', start)
    db.execute(canonical[start:end])
    if FORWARD_IDENTITY_GUARDS:
        db.execute((ROOT / 'tdf-hq/sql/2026-11-07_event_agreement_identity_guards.sql').read_text())


class AgreementDatabaseTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.dsn = checked_test_dsn(os.environ['EVENT_AGREEMENTS_TEST_DSN'])
        cls.schema = 'event_agreements_' + uuid.uuid4().hex
        cls.db = psycopg.connect(cls.dsn, autocommit=True)
        cls.db.execute(sql.SQL('CREATE SCHEMA {}').format(sql.Identifier(cls.schema)))
        cls.db.execute(sql.SQL('SET search_path TO {},public').format(sql.Identifier(cls.schema)))
        cls.db.execute('CREATE TABLE party(id BIGINT PRIMARY KEY,display_name TEXT)')
        cls.db.execute('CREATE TABLE user_credential(party_id BIGINT,active BOOLEAN)')
        cls.db.execute('CREATE TABLE social_event(id BIGINT PRIMARY KEY,organizer_party_id TEXT)')
        cls.db.execute("CREATE TABLE notification(id BIGSERIAL PRIMARY KEY,recipient_party_id BIGINT,notif_type TEXT CHECK(notif_type='existing'),title TEXT,body TEXT,target_type TEXT,target_id BIGINT,target_key TEXT,is_read BOOL)")
        # Match production's named, pre-existing notification allowlist.
        cls.db.execute((ROOT / 'tdf-hq/sql/2026-11-03_event_agreements.sql').read_text())
        install_identity_guard(cls.db)
        cls.db.execute("INSERT INTO party VALUES(1,'Organizer'),(2,'Synthetic recipient'),(3,'Unrelated account')")
        cls.db.execute('INSERT INTO user_credential VALUES(1,TRUE),(2,TRUE),(3,TRUE)')
        print('PostgreSQL', cls.db.execute('SHOW server_version').fetchone()[0], 'actual agreement migration')

    @classmethod
    def tearDownClass(cls):
        cls.db.execute(sql.SQL('DROP SCHEMA {} CASCADE').format(sql.Identifier(cls.schema)))
        cls.db.close()

    def setUp(self):
        self.ev = self.db.execute("INSERT INTO social_event SELECT COALESCE(max(id),0)+1,'1' FROM social_event RETURNING id").fetchone()[0]
        self.plan = {'title': 'Synthetic plan', 'overview': 'Synthetic scope', 'activities': [],
                     'budget': [{'label': 'Synthetic production', 'amountCents': 20000, 'basis': 'Approved synthetic fixture'}],
                     'budgetCapCents': 20000, 'currency': 'USD', 'deliverables': ['Session'], 'pending': []}
        self.content = {'title': 'Synthetic agreement', 'body': 'Complete synthetic agreement for isolated testing.',
                        'recipientLegalName': 'Synthetic recipient', 'pendingFields': []}

    def call(self, actor, payload, db=None):
        payload = {'requestId': str(uuid.uuid4()), **payload}
        return (db or self.db).execute('SELECT event_workspace_mutate(%s,%s,%s::jsonb)', (actor, self.ev, json.dumps(payload))).fetchone()[0]

    def read(self, actor):
        return self.db.execute('SELECT event_workspace_read(%s,%s)', (actor, self.ev)).fetchone()[0]

    def prepare(self, pending=False):
        if pending:
            self.plan['budget'][0]['amountCents'] = None
            self.plan['budgetCapCents'] = None
            self.content['pendingFields'] = ['Scope']
        result = self.call(1, {'action': 'plan.save', 'version': 0, 'recipientPartyId': '2', 'content': self.plan})
        self.assertNotIn('error', result)
        result = self.call(1, {'action': 'agreement.create', 'version': 0, 'content': self.content})
        self.assertNotIn('error', result)
        return result['agreements'][-1]

    @staticmethod
    def command(agreement, action, **extra):
        version = agreement['versions'][-1]
        return {'action': 'agreement.' + action, 'agreementId': agreement['id'],
                'version': version['version'], 'hash': version['document_hash'], **extra}

    def sent(self):
        agreement = self.prepare()
        result = self.call(1, self.command(agreement, 'approve'))
        self.assertNotIn('error', result)
        agreement = result['agreements'][-1]
        result = self.call(1, self.command(agreement, 'send'))
        self.assertNotIn('error', result)
        return result['agreements'][-1]

    def test_draft_scope_and_budget_block_approval_and_acceptance(self):
        agreement = self.prepare(pending=True)
        self.assertEqual(self.read(2)['agreements'], [])
        self.assertEqual(self.call(1, self.command(agreement, 'approve'))['status'], 409)
        self.assertEqual(self.call(1, self.command(agreement, 'send'))['status'], 409)
        before = self.db.execute('SELECT count(*) FROM notification').fetchone()[0]
        self.assertNotIn('error', self.call(1, self.command(agreement, 'comment', comment='Private draft note')))
        self.assertEqual(self.db.execute('SELECT count(*) FROM notification').fetchone()[0], before)
        self.assertIn('error', self.call(2, self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True)))

    def test_recipient_and_owner_permissions(self):
        agreement = self.sent()
        self.assertEqual(self.read(3)['status'], 403)
        for action in ['approve', 'send', 'withdraw', 'revise']:
            self.assertEqual(self.call(2, self.command(agreement, action))['status'], 403)
        self.assertEqual(self.call(1, self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True))['status'], 403)
        self.assertEqual(self.call(3, self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True))['status'], 403)
        self.assertEqual(self.call(2, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '2', 'content': self.plan})['status'], 403)
        self.assertNotIn('error', self.call(2, {'action': 'plan.comment', 'version': 1, 'comment': 'Requested schedule adjustment'}))

    def test_acceptance_is_exact_and_idempotent(self):
        agreement = self.sent()
        wrong = self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True)
        self.assertEqual(self.call(2, {**wrong, 'hash': 'stale'})['status'], 409)
        self.assertEqual(self.call(2, {**wrong, 'consent': False})['status'], 400)
        self.assertEqual(self.call(2, {**wrong, 'acceptedName': 'Someone else'})['status'], 400)
        request = {**wrong, 'requestId': str(uuid.uuid4())}
        first = self.call(2, request)
        self.assertEqual(first, self.call(2, request))
        self.assertEqual(first['agreements'][-1]['acceptance'], self.call(2, wrong)['agreements'][-1]['acceptance'])
        self.assertEqual(self.db.execute('SELECT count(*) FROM event_agreement_acceptance WHERE agreement_id=%s', (agreement['id'],)).fetchone()[0], 1)
        self.assertEqual(self.db.execute("SELECT count(*) FROM event_workspace_action WHERE agreement_id=%s AND action='agreement.accept'", (agreement['id'],)).fetchone()[0], 1)

    def test_internal_send_replay_and_acceptance_need_no_mail_transport(self):
        # No mail configuration, transport or generic contract tables exist in
        # this isolated schema. Sending means durable in-app availability only.
        agreement = self.prepare()
        self.assertNotIn('error', self.call(1, self.command(agreement, 'approve')))
        request = {**self.command(agreement, 'send'), 'requestId': str(uuid.uuid4())}
        before = self.db.execute('SELECT count(*) FROM notification').fetchone()[0]
        sent = self.call(1, request)
        self.assertEqual(sent, self.call(1, request))
        self.assertEqual(self.call(2, request)['status'], 403)
        self.assertEqual(self.db.execute('SELECT count(*) FROM notification').fetchone()[0], before + 1)
        self.assertEqual(self.db.execute("SELECT count(*) FROM event_workspace_action WHERE agreement_id=%s AND action='agreement.send'", (agreement['id'],)).fetchone()[0], 1)
        self.assertEqual(self.call(1, self.command(agreement, 'send'))['status'], 409)
        proposed = self.read(2)['agreements'][-1]
        self.assertEqual(proposed['state'], 'sent')
        accepted = self.call(2, self.command(proposed, 'accept', acceptedName='Synthetic recipient', consent=True))['agreements'][-1]
        receipt = accepted['acceptance']
        frozen = accepted['versions'][receipt['version'] - 1]
        self.assertEqual(receipt['document_hash'], frozen['document_hash'])
        self.assertEqual(receipt['accepted_by'], 2)
        self.assertTrue(receipt['accepted_at'])
        self.assertEqual(frozen['content']['plan']['content'], self.plan)
        self.assertEqual(self.read(1)['agreements'][-1]['acceptance'], receipt)
        self.assertEqual(self.read(3)['status'], 403)

    def test_revisions_hide_unsent_content_and_invalidate_old_acceptance(self):
        agreement = self.sent()
        changed = self.call(2, self.command(agreement, 'request_changes', comment='Adjust the deliverables'))['agreements'][-1]
        revised = self.call(1, self.command(changed, 'revise', content={**self.content, 'body': 'New private draft'}))['agreements'][-1]
        self.assertEqual(revised['current_version'], 2)
        self.assertIsNone(revised['versions'][-1]['approved_at'])
        self.assertEqual(len(self.read(2)['agreements'][-1]['versions']), 1)
        self.assertEqual(self.call(2, self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True))['status'], 409)
        self.assertEqual(self.call(1, self.command(revised, 'send'))['status'], 409)
        self.assertNotIn('error', self.call(1, self.command(revised, 'approve')))
        sent = self.call(1, self.command(revised, 'send'))['agreements'][-1]
        rejected = self.call(2, self.command(sent, 'reject', comment='Decline this proposal'))['agreements'][-1]
        self.assertEqual(rejected['state'], 'rejected')
        self.assertEqual(self.call(2, self.command(rejected, 'accept', acceptedName='Synthetic recipient', consent=True))['status'], 409)
        replacement = self.call(1, self.command(rejected, 'revise', content=self.content))['agreements'][-1]
        self.assertEqual(replacement['current_version'], 3)
        self.assertIsNone(replacement['versions'][-1]['approved_at'])


    def test_accepted_evidence_and_annex_are_immutable(self):
        agreement = self.sent()
        accepted = self.call(2, self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True))['agreements'][-1]
        for statement in ["UPDATE event_agreement_acceptance SET accepted_name='tampered' WHERE agreement_id=%s", "UPDATE event_agreement_version SET content='{}'::jsonb WHERE agreement_id=%s", "UPDATE event_agreement SET state='draft' WHERE id=%s"]:
            with self.assertRaises(psycopg.errors.CheckViolation):
                self.db.execute(statement, (agreement['id'],))
        updated = copy.deepcopy(self.plan); updated['overview'] = 'Updated operational plan'
        self.assertNotIn('error', self.call(1, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '2', 'content': updated}))
        self.assertEqual(self.read(2)['agreements'][-1]['versions'][-1]['content']['plan']['revision'], 1)
        self.assertEqual(self.call(1, self.command(accepted, 'revise', content=self.content))['status'], 409)
        amendment = self.call(1, {'action': 'agreement.create', 'version': 0, 'parentId': agreement['id'], 'content': self.content})
        self.assertEqual(amendment['agreements'][-1]['parent_id'], agreement['id'])

    def test_stale_plan_and_unknown_fields_fail_without_writing(self):
        self.prepare()
        for bad in [{'approvedBy': 1}, {'createdAt': 'yesterday'}]:
            self.assertEqual(self.call(1, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '2', 'content': self.plan, **bad})['status'], 400)
        self.assertEqual(self.call(1, {'action': 'plan.save', 'version': 0, 'recipientPartyId': '2', 'content': self.plan})['status'], 409)
        for invalid in [-1, 0.5, 9007199254740992, '20000']:
            plan = copy.deepcopy(self.plan); plan['budget'][0]['amountCents'] = invalid
            self.assertEqual(self.call(1, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '2', 'content': plan})['status'], 400)

    def test_replay_content_conflict_and_recipient_reassignment(self):
        request = {'action': 'plan.save', 'version': 0, 'recipientPartyId': '2', 'content': self.plan, 'requestId': str(uuid.uuid4())}
        first = self.call(1, request)
        self.assertEqual(first, self.call(1, request))
        self.assertEqual(self.call(1, {**request, 'version': 1})['status'], 409)
        comment = {'action': 'plan.comment', 'version': 1, 'comment': 'Private comment', 'requestId': str(uuid.uuid4())}
        self.assertNotIn('error', self.call(2, comment))
        self.assertNotIn('error', self.call(1, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '3', 'content': self.plan}))
        self.assertEqual(self.call(2, comment)['status'], 403)
        self.assertFalse(any(item.get('comment') == 'Private comment' for item in self.read(3)['planComments']))

    def test_inactive_recipient_blocks_assignment_and_send(self):
        self.db.execute('UPDATE user_credential SET active=FALSE WHERE party_id=2')
        try:
            self.assertEqual(self.call(1, {'action': 'plan.save', 'version': 0, 'recipientPartyId': '2', 'content': self.plan})['status'], 400)
        finally:
            self.db.execute('UPDATE user_credential SET active=TRUE WHERE party_id=2')
        agreement = self.prepare()
        self.assertNotIn('error', self.call(1, self.command(agreement, 'approve')))
        self.db.execute('UPDATE user_credential SET active=FALSE WHERE party_id=2')
        try:
            self.assertEqual(self.call(1, self.command(agreement, 'send'))['status'], 409)
        finally:
            self.db.execute('UPDATE user_credential SET active=TRUE WHERE party_id=2')

    def test_irrelevant_fields_and_inconsistent_cap_are_rejected(self):
        agreement = self.prepare()
        self.assertEqual(self.call(1, self.command(agreement, 'approve', consent=True))['status'], 400)
        self.plan['budgetCapCents'] = 20001
        self.assertNotIn('error', self.call(1, {'action': 'plan.save', 'version': 1, 'recipientPartyId': '2', 'content': self.plan}))
        revised = self.call(1, self.command(agreement, 'revise', content=self.content))['agreements'][-1]
        self.assertEqual(self.call(1, self.command(revised, 'approve'))['status'], 409)
        self.assertIsNone(self.read(1)['agreements'][-1]['versions'][-1]['approved_at'])

    def test_parallel_accept_and_withdraw_have_one_terminal_result(self):
        agreement = self.sent()
        def run(actor, action):
            with psycopg.connect(self.dsn, autocommit=True) as connection:
                connection.execute(sql.SQL('SET search_path TO {},public').format(sql.Identifier(self.schema)))
                return self.call(actor, action, connection)
        accept = self.command(agreement, 'accept', acceptedName='Synthetic recipient', consent=True)
        withdraw = self.command(agreement, 'withdraw')
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as workers:
            results = list(workers.map(lambda pair: run(*pair), [(2, accept), (1, withdraw)]))
        self.assertEqual(sum('error' not in value for value in results), 1)
        final = self.read(1)['agreements'][-1]
        self.assertIn(final['state'], ['accepted', 'withdrawn'])
        self.assertEqual(final['acceptance'] is not None, final['state'] == 'accepted')


if __name__ == '__main__':
    unittest.main(verbosity=2)
