#!/usr/bin/env python3
"""Durable ordered intent journal; callers implement and verify external effects.

No production command, rollback, evidence authentication or recovery is provided.
An interrupted intent always needs separately reviewed recovery.
"""
from contextlib import contextmanager
import fcntl
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat
import threading

_spec = importlib.util.spec_from_file_location('recovery_files', Path(__file__).with_name('recovery-files.py'))
files = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(files)
STAGES = ('maintenance', 'stop-writers', 'stop-database', 'capture', 'encrypt',
          'retrieve-off-host', 'restore-isolate', 'start-database', 'migrate',
          'start-candidate', 'verify-candidate', 'reopen')
MAX_RECORD = 16384
RELEASE_RECORDS = 1 + 2*len(STAGES)
# Bound admission work without deleting any history. Exhaustion needs a separately
# reviewed storage/coordinator change, never rotation into a different directory.
MAX_RELEASES = 128
PLAN_KEYS = {'sourceRevision', 'mobileRevision', 'runtimeHash', 'manifestHash',
             'candidateImage', 'recoveryImage', 'recipientHash'}


def require(condition):
    if not condition:
        raise ValueError('Release journal boundary rejected')


def sha(value):
    return hashlib.sha256(value).hexdigest()


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'), ensure_ascii=True) + '\n').encode()


def hash_value(value, length=64):
    return isinstance(value, str) and re.fullmatch('[0-9a-f]{%d}' % length, value) is not None


def validate_plan(plan):
    require(isinstance(plan, dict) and set(plan) == PLAN_KEYS)
    for key, value in plan.items():
        if key.endswith('Revision'):
            require(hash_value(value, 40))
        elif key.endswith('Image'):
            require(isinstance(value, str) and value.startswith('sha256:') and hash_value(value[7:]))
        else:
            require(hash_value(value))


def private_file(fd):
    info = os.fstat(fd)
    require(stat.S_ISREG(info.st_mode) and info.st_uid == os.geteuid()
            and info.st_nlink == 1 and stat.S_IMODE(info.st_mode) == 0o600)
    return info


class Journal:
    def __init__(self, directory_fd, lock_fd):
        self.directory_fd, self.lock_fd = directory_fd, lock_fd
        self.pid = os.getpid()
        self.closed = False
        self._append_lock = threading.Lock()

    def guard(self):
        require(not self.closed and self.pid == os.getpid())
        locked = private_file(self.lock_fd)
        named = os.stat('release.lock', dir_fd=self.directory_fd, follow_symlinks=False)
        require((locked.st_dev, locked.st_ino) == (named.st_dev, named.st_ino))

    def records(self):
        self.guard()
        names = set(os.listdir(self.directory_fd))
        require('release.lock' in names)
        names.remove('release.lock')
        require(len(names) <= RELEASE_RECORDS*MAX_RELEASES)
        expected = {'%03d.json' % i for i in range(len(names))}
        # A .pending file, hole, alias, unrelated file or extra record rejects.
        require(names == expected)
        records, previous, nonces = [], None, set()
        for index in range(len(names)):
            fd = os.open('%03d.json' % index, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=self.directory_fd)
            try:
                info = private_file(fd)
                require(0 < info.st_size <= MAX_RECORD)
                raw = os.read(fd, MAX_RECORD + 1)
                require(len(raw) == info.st_size)
                record = json.loads(raw)
                require(canonical(record) == raw and isinstance(record, dict)
                        and set(record) == {'schemaVersion', 'releaseNonce', 'planHash', 'sequence', 'previousHash', 'event'}
                        and type(record['schemaVersion']) is int and record['schemaVersion'] == 1
                        and hash_value(record['releaseNonce'], 32) and hash_value(record['planHash'])
                        and type(record['sequence']) is int and record['sequence'] == index
                        and record['previousHash'] == previous)
                event = record['event']
                require(isinstance(event, dict))
                release_index = index % RELEASE_RECORDS
                if release_index == 0:
                    require(set(event) == {'kind', 'plan'} and event['kind'] == 'plan')
                    validate_plan(event['plan'])
                    require(record['planHash'] == sha(canonical(event['plan']))
                            and record['releaseNonce'] not in nonces)
                    nonces.add(record['releaseNonce'])
                else:
                    plan_record = records[index-release_index]
                    require(record['releaseNonce'] == plan_record['releaseNonce']
                            and record['planHash'] == plan_record['planHash'])
                    stage = STAGES[(release_index-1)//2]
                    kind = 'intent' if release_index % 2 else 'observed'
                    keys = {'kind', 'stage', 'targetsHash', 'operationId'} if kind == 'intent' else {'kind', 'stage', 'operationId', 'evidenceHash'}
                    require(set(event) == keys and event['kind'] == kind and event['stage'] == stage)
                    if kind == 'observed':
                        require(hash_value(event['evidenceHash'])
                                and event['operationId'] == records[-1]['event']['operationId'])
                    else:
                        require(hash_value(event['targetsHash'])
                                and event['operationId'] == sha(canonical({
                                    'releaseNonce': record['releaseNonce'], 'planHash': record['planHash'],
                                    'stage': stage, 'targetsHash': event['targetsHash']})))
                records.append(record)
                previous = sha(raw)
            finally:
                os.close(fd)
        return records

    def _append(self, event, *, release_nonce=None, expected_tip_hash=None):
        # Reject inherited handles before waiting on a possibly inherited mutex.
        self.guard()
        with self._append_lock:
            self._append_locked(event, release_nonce=release_nonce,
                                expected_tip_hash=expected_tip_hash)

    def _append_locked(self, event, *, release_nonce=None, expected_tip_hash=None):
        records = self.records()
        # Recheck the caller's observed prefix at publication. The live handle
        # can be shared by threads; flock alone does not serialize its callers.
        require((not records and expected_tip_hash is None)
                or (records and hash_value(expected_tip_hash)
                    and sha(canonical(records[-1])) == expected_tip_hash))
        new_plan = event['kind'] == 'plan'
        if new_plan:
            require(len(records) % RELEASE_RECORDS == 0
                    and all(record['releaseNonce'] != release_nonce for record in records))
        record = {'schemaVersion': 1,
                  'releaseNonce': release_nonce if new_plan else records[-1]['releaseNonce'],
                  'planHash': sha(canonical(event['plan'])) if new_plan else records[-1]['planHash'],
                  'sequence': len(records), 'previousHash': sha(canonical(records[-1])) if records else None,
                  'event': event}
        raw = canonical(record)
        require(len(raw) <= MAX_RECORD and len(records) < RELEASE_RECORDS*MAX_RELEASES)
        name = '%03d.json' % len(records)
        pending = name + '.pending'
        fd = os.open(pending, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600,
                     dir_fd=self.directory_fd)
        try:
            private_file(fd)
            with os.fdopen(fd, 'wb', closefd=False) as output:
                output.write(raw); output.flush(); os.fsync(fd)
            os.link(pending, name, src_dir_fd=self.directory_fd, dst_dir_fd=self.directory_fd,
                    follow_symlinks=False)
            os.fsync(self.directory_fd)
            os.unlink(pending, dir_fd=self.directory_fd)
            os.fsync(self.directory_fd)
        except BaseException:
            # No further admission through this handle after uncertain IO. A
            # fresh open durably syncs the directory before reading any prefix.
            self.closed = True
            raise
        finally:
            os.close(fd)

    def initialize(self, plan, release_nonce):
        validate_plan(plan)
        require(hash_value(release_nonce, 32))
        require(not self.records())
        self._append({'kind': 'plan', 'plan': plan}, release_nonce=release_nonce)

    def initialize_next(self, plan, release_nonce, expected_tip_hash):
        """Append a new plan after a complete release under the permanent lock.

        The caller must freshly admit the fixed directory, runtime, fences and
        exact plan. A matching historical tip is only a compare-and-set token;
        it does not authenticate approval or validate any external effect.
        """
        validate_plan(plan)
        require(hash_value(release_nonce, 32) and hash_value(expected_tip_hash))
        records = self.records()
        require(records and len(records) % RELEASE_RECORDS == 0
                and len(records) < RELEASE_RECORDS*MAX_RELEASES
                and sha(canonical(records[-1])) == expected_tip_hash
                and all(record['releaseNonce'] != release_nonce for record in records))
        self._append({'kind': 'plan', 'plan': plan}, release_nonce=release_nonce,
                     expected_tip_hash=expected_tip_hash)

    def _current_records(self, records):
        require(records)
        return records[((len(records)-1)//RELEASE_RECORDS)*RELEASE_RECORDS:]

    def status(self):
        records = self.records()
        current = self._current_records(records)
        completed = (len(current)-1)//2
        pending = len(current) % 2 == 0
        return {'releaseNonce': current[0]['releaseNonce'], 'planHash': current[0]['planHash'],
                'completedStages': list(STAGES[:completed]),
                'pendingStage': STAGES[completed] if pending else None,
                'sequenceComplete': completed == len(STAGES),
                'priorReleases': (len(records)-1)//RELEASE_RECORDS,
                'latestRecordHash': sha(canonical(records[-1])),
                # DB startup, migrations and the candidate may all write before traffic.
                # Keep this conservative across succession; history is not reset.
                'newWritesPossible': len(records) >= 2 + 2*STAGES.index('start-database')}

    def perform(self, stage, targets_hash, effect):
        records = self.records()
        current = self._current_records(records)
        require(len(current) % 2 == 1)
        next_index = (len(current)-1)//2
        require(next_index < len(STAGES) and stage == STAGES[next_index])
        require(hash_value(targets_hash))
        context = {'releaseNonce': current[0]['releaseNonce'], 'planHash': current[0]['planHash'],
                   'stage': stage, 'targetsHash': targets_hash}
        operation_id = sha(canonical(context))
        context['operationId'] = operation_id
        self._append({'kind': 'intent', 'stage': stage, 'targetsHash': targets_hash, 'operationId': operation_id},
                     expected_tip_hash=sha(canonical(records[-1])))
        intent_records = self.records()
        # Only this invocation can complete its intent. An exception, process
        # death, invalid observation or failed fsync leaves it non-retryable.
        observation = effect(dict(context))
        self.guard()
        require(isinstance(observation, dict) and set(observation) == set(context) | {'evidenceHash'}
                and all(observation[key] == value for key, value in context.items())
                and hash_value(observation['evidenceHash']))
        require(self.records() == intent_records)
        self._append({'kind': 'observed', 'stage': stage, 'operationId': operation_id,
                      'evidenceHash': observation['evidenceHash']},
                     expected_tip_hash=sha(canonical(intent_records[-1])))
        return self.status()


@contextmanager
def open_journal(path):
    """Use the one fixed global control directory, durably created by the caller.

    All cooperating operators must use this same configured directory/lock for
    every release nonce. No per-release directory, rotation or reset is admitted.
    A completed prefix can only gain a new plan through initialize_next; never
    unlink/replace the lock, overwrite a terminal plan, or delete an interrupted journal.
    """
    with files.directory(path, private=True) as directory_fd:
        lock_fd = os.open('release.lock', os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW,
                          0o600, dir_fd=directory_fd)
        journal = None
        try:
            private_file(lock_fd)
            fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            os.fsync(lock_fd); os.fsync(directory_fd)
            journal = Journal(directory_fd, lock_fd)
            journal.records()
            yield journal
        finally:
            if journal is not None: journal.closed = True
            os.close(lock_fd)
