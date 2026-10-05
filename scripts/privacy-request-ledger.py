#!/usr/bin/env python3
"""Private operator ledger. Records verified evidence; never deletes customer data.

The trust boundary is the authorized local OS account and its protected evidence
store. Hashes bind artifacts; they do not prove the artifacts' assertions true.
"""
import argparse
from contextlib import closing, contextmanager
from datetime import datetime, timedelta, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import stat
import sys
import uuid

ROOT = Path(__file__).resolve().parent.parent
POLICY_PATH = ROOT / 'ops/privacy/request-workflow.json'
POLICY = json.loads(POLICY_PATH.read_text())
POLICY_HASH = hashlib.sha256(POLICY_PATH.read_bytes()).hexdigest()
TOKEN = re.compile(r'^[a-zA-Z0-9_-]{8,100}$')
HEX = re.compile(r'^[a-f0-9]{64}$')


class LedgerError(ValueError):
    pass


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':'), ensure_ascii=True)


def digest(value):
    return hashlib.sha256(canonical(value).encode()).hexdigest()


def utc(value):
    parsed = datetime.fromisoformat(value.replace('Z', '+00:00'))
    if parsed.tzinfo is None or parsed.utcoffset() != timedelta(0):
        raise ValueError('Timestamp must use UTC')
    return parsed.astimezone(timezone.utc)


def stamp(value):
    return value.isoformat().replace('+00:00', 'Z')


def require(condition, message):
    if not condition:
        raise LedgerError(message)


def private_file(path):
    info = path.lstat()
    require(stat.S_ISREG(info.st_mode) and info.st_uid == os.getuid(), 'File must be owned regular file, not a symlink')
    require(info.st_mode & 0o077 == 0, 'Private file must not permit group/other access')


def location(path):
    path = Path(os.path.abspath(path))
    parent = path.parent
    require(not path.is_relative_to(ROOT.resolve()), 'Operational ledger must be outside the repository')
    require(parent.resolve() == parent, 'Ledger directory must not traverse symlinks')
    info = parent.stat()
    require(stat.S_ISDIR(info.st_mode) and info.st_uid == os.getuid() and info.st_mode & 0o077 == 0,
            'Ledger directory must be owned and private (0700)')
    return path


def initialize(path):
    path = location(path)
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    os.close(fd)
    with closing(sqlite3.connect(path)) as db:
        db.executescript('''
            CREATE TABLE metadata (policy_hash TEXT NOT NULL);
            CREATE TABLE event (
              seq INTEGER PRIMARY KEY, case_id TEXT NOT NULL, version INTEGER NOT NULL,
              key TEXT NOT NULL UNIQUE, request_hash TEXT NOT NULL, payload TEXT NOT NULL,
              previous_hash TEXT NOT NULL, event_hash TEXT NOT NULL,
              UNIQUE(case_id,version));
            CREATE TRIGGER no_event_update BEFORE UPDATE ON event BEGIN SELECT RAISE(ABORT,'append only'); END;
            CREATE TRIGGER no_event_delete BEFORE DELETE ON event BEGIN SELECT RAISE(ABORT,'append only'); END;
        ''')
        db.execute('INSERT INTO metadata VALUES (?)', (POLICY_HASH,))
        db.commit()
    return {'status': 'initialized', 'policyHash': POLICY_HASH}


@contextmanager
def transaction(path):
    path = location(path)
    private_file(path)
    db = sqlite3.connect(path, timeout=15, isolation_level=None)
    db.row_factory = sqlite3.Row
    try:
        db.execute('PRAGMA synchronous=FULL')
        db.execute('BEGIN IMMEDIATE')
        require(db.execute('SELECT policy_hash FROM metadata').fetchone()[0] == POLICY_HASH,
                'Policy changed; reviewed ledger migration required')
        previous = '0' * 64
        for row in db.execute('SELECT * FROM event ORDER BY seq'):
            payload = json.loads(row['payload'])
            require(row['previous_hash'] == previous and
                    row['event_hash'] == digest({'previous': previous, 'case': row['case_id'],
                        'version': row['version'], 'key': row['key'], 'request': row['request_hash'], 'payload': payload}),
                    'Ledger integrity failure')
            previous = row['event_hash']
        yield db
        db.execute('COMMIT')
    except BaseException:
        if db.in_transaction:
            db.execute('ROLLBACK')
        raise
    finally:
        db.close()


def evidence(path, kind, case_id=None):
    path = Path(path)
    private_file(path)
    require(path.stat().st_size <= 1024 * 1024, 'Evidence receipt too large')
    raw = path.read_bytes()
    receipt = json.loads(raw)
    allowed = {'kind', 'caseId', 'artifacts', 'surfaces'}
    require(set(receipt) <= allowed and receipt.get('kind') == kind, 'Wrong evidence kind or unsupported receipt fields')
    require(receipt.get('caseId') == case_id, 'Evidence case binding mismatch')
    artifacts = receipt.get('artifacts')
    require(isinstance(artifacts, list) and artifacts and all(isinstance(x, str) and HEX.fullmatch(x) for x in artifacts),
            'Receipt requires private artifact SHA256 references')
    # The receipt itself is read and hashed. Artifact storage/review remains an
    # explicit operator obligation; digest references are not delivery receipts.
    return receipt, hashlib.sha256(raw).hexdigest()


def mutate(path, request, evidence_path, now=None):
    now = now or datetime.now(timezone.utc)
    require(TOKEN.fullmatch(request['key']), 'Use an opaque non-PII idempotency key')
    action = request['action']
    case_id = request.get('caseId')
    if case_id:
        require(str(uuid.UUID(case_id)) == case_id, 'Case id must be canonical UUID')
    kind = 'received_request' if action == 'open' else POLICY['transitions'][action]['evidence']
    receipt, receipt_hash = evidence(evidence_path, kind, case_id)
    request_hash = digest({**request, 'evidenceHash': receipt_hash, 'actorUid': os.getuid()})
    with transaction(path) as db:
        replay = db.execute('SELECT * FROM event WHERE key=?', (request['key'],)).fetchone()
        if replay:
            require(replay['request_hash'] == request_hash, 'Idempotency key conflicts with actor/request/evidence')
            return {'caseId': replay['case_id'], 'version': replay['version'], 'replayed': True,
                    'state': json.loads(replay['payload'])['state']}
        if action == 'open':
            require(request['channel'] in POLICY['channels'], 'Unknown intake channel')
            received = utc(request['receivedAt'])
            require(received <= now, 'Received timestamp cannot be in the future')
            case_id = str(uuid.uuid4())
            snapshot = {'state': 'received', 'receivedAt': stamp(received),
                        'dueAt': stamp(received + timedelta(days=POLICY['deadlineDays'])),
                        'channel': request['channel'], 'surfaces': {}}
            version = 0
        else:
            latest = db.execute('SELECT * FROM event WHERE case_id=? ORDER BY version DESC LIMIT 1', (case_id,)).fetchone()
            require(latest is not None, 'Unknown case')
            require(latest['version'] == request['expectedVersion'], 'Stale expected version')
            snapshot = json.loads(latest['payload'])
            require(now >= utc(snapshot['recordedAt']), 'Clock regression cannot advance a case')
            rule = POLICY['transitions'][action]
            require(snapshot['state'] in rule['from'], 'Forbidden transition')
            if action in ['plan', 'replan', 'verify_effects', 'review_retention']:
                surfaces = receipt.get('surfaces', {})
                require(isinstance(surfaces, dict) and set(surfaces) == set(POLICY['surfaces']), 'Every data surface requires explicit disposition')
                for name, item in surfaces.items():
                    require(isinstance(item, dict) and set(item) <= {'disposition', 'proofHash', 'reviewAt'}, 'Invalid surface evidence')
                    require(item.get('disposition') in POLICY['dispositions'] and isinstance(item.get('proofHash'), str) and HEX.fullmatch(item['proofHash']), 'Disposition/proof required')
                    if item['disposition'] == 'retain':
                        require(utc(item.get('reviewAt', '')) > now, 'Retention requires future review deadline and rationale in referenced proof')
                    else:
                        require('reviewAt' not in item, 'Only retained data has reviewAt')
                    if action == 'review_retention':
                        prior = snapshot['surfaces'][name]
                        if prior['disposition'] == 'retain':
                            require(item['disposition'] in ['erase', 'anonymize', 'retain'], 'Retained data cannot become uninspected or not applicable')
                        else:
                            require(item == prior, 'Retention review cannot rewrite other surface dispositions')
                    if action == 'verify_effects':
                        require(item['disposition'] == snapshot['surfaces'][name]['disposition'], 'Execution must match admitted scope; failures require remediation')
                snapshot['surfaces'] = surfaces
            if action == 'close':
                snapshot['closedAt'] = stamp(now)
            snapshot['state'] = rule['to']
            version = latest['version'] + 1
        snapshot.update(recordedAt=stamp(now), action=action, evidenceHash=receipt_hash, actorUid=os.getuid())
        previous = db.execute('SELECT event_hash FROM event ORDER BY seq DESC LIMIT 1').fetchone()
        previous = previous[0] if previous else '0' * 64
        event_hash = digest({'previous': previous, 'case': case_id, 'version': version,
                             'key': request['key'], 'request': request_hash, 'payload': snapshot})
        db.execute('INSERT INTO event(case_id,version,key,request_hash,payload,previous_hash,event_hash) VALUES(?,?,?,?,?,?,?)',
                   (case_id, version, request['key'], request_hash, canonical(snapshot), previous, event_hash))
        return {'caseId': case_id, 'version': version, 'state': snapshot['state'], 'replayed': False}


def report(path, now=None):
    now = now or datetime.now(timezone.utc)
    with transaction(path) as db:
        rows = db.execute('SELECT e.* FROM event e JOIN (SELECT case_id,MAX(version) v FROM event GROUP BY case_id) x ON e.case_id=x.case_id AND e.version=x.v').fetchall()
        result = []
        for row in rows:
            snapshot = json.loads(row['payload'])
            closed = snapshot['state'] in ['closed', 'withdrawn']
            due = utc(snapshot['dueAt'])
            reviews = [utc(x['reviewAt']) for x in snapshot['surfaces'].values() if x['disposition'] == 'retain']
            result.append({'caseId': row['case_id'], 'version': row['version'], 'state': snapshot['state'],
                'dueAt': snapshot['dueAt'], 'completedLate': bool(snapshot.get('closedAt') and utc(snapshot['closedAt']) > due), 'overdue': not closed and now > due,
                'dueSoon': not closed and now <= due <= now + timedelta(days=POLICY['warningDays']),
                'retentionReviewOverdue': any(now > review for review in reviews),
                'retainedSurfaces': [name for name, item in snapshot['surfaces'].items() if item['disposition'] == 'retain']})
        return {'cases': result, 'attentionRequired': any(r['overdue'] or r['dueSoon'] or r['retentionReviewOverdue'] for r in result),
                'limitation': 'Operator evidence ledger; no claim of automated deletion, mailbox coverage, provider revocation or independently validated evidence truth.'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--ledger', required=True)
    sub = parser.add_subparsers(dest='command', required=True)
    sub.add_parser('init')
    sub.add_parser('report')
    opening = sub.add_parser('open')
    opening.add_argument('--channel', choices=POLICY['channels'], required=True)
    opening.add_argument('--received-at', required=True)
    opening.add_argument('--key', required=True)
    opening.add_argument('--evidence', required=True)
    step = sub.add_parser('step')
    step.add_argument('--case-id', required=True)
    step.add_argument('--action', choices=POLICY['transitions'], required=True)
    step.add_argument('--expected-version', type=int, required=True)
    step.add_argument('--key', required=True)
    step.add_argument('--evidence', required=True)
    args = parser.parse_args()
    if args.command == 'init':
        result = initialize(args.ledger)
    elif args.command == 'report':
        result = report(args.ledger)
    else:
        request = {'action': 'open', 'receivedAt': args.received_at, 'channel': args.channel, 'key': args.key} if args.command == 'open' else {
            'action': args.action, 'caseId': args.case_id, 'expectedVersion': args.expected_version, 'key': args.key}
        result = mutate(args.ledger, request, args.evidence)
    print(json.dumps(result, sort_keys=True))
    if result.get('attentionRequired'):
        return 2
    return 0


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, KeyError, OSError, sqlite3.Error) as error:
        # Do not print file paths, raw receipts or data-bearing exception messages.
        print(json.dumps({'status': 'rejected', 'category': type(error).__name__, 'reason': str(error) if isinstance(error, LedgerError) else 'Invalid input or storage failure'}), file=sys.stderr)
        sys.exit(1)
