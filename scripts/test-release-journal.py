#!/usr/bin/env python3
"""Real filesystem/process controls for the release ordering boundary."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('journal', ROOT/'ops/hetzner/release-journal.py')
journal = importlib.util.module_from_spec(spec); spec.loader.exec_module(journal)
PLAN = {key: ('sha256:'+'a'*64 if key.endswith('Image') else 'a'*(40 if key.endswith('Revision') else 64))
        for key in journal.PLAN_KEYS}
NONCE = 'b'*32
TARGETS = 'c'*64

def observed(context): return {**context, 'evidenceHash': 'd'*64}


class ReleaseJournalTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()

    def initialize(self):
        with journal.open_journal(self.root) as current: current.initialize(PLAN, NONCE)

    def complete(self, current):
        for stage in journal.STAGES:
            current.perform(stage, TARGETS, observed)
        return current.status()['latestRecordHash']

    def test_next_release_preserves_legacy_bytes_lock_and_global_hash_chain(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            first_tip = self.complete(current)
            original = {p.name: p.read_bytes() for p in self.root.glob('*.json')}
            lock_inode = (self.root/'release.lock').stat().st_ino
            for number, nonce in enumerate(['e'*32, 'f'*32], 1):
                plan = {**PLAN, 'sourceRevision': str(number)*40}
                tip = current.status()['latestRecordHash']
                current.initialize_next(plan, nonce, tip)
                state = current.status()
                self.assertEqual(state['releaseNonce'], nonce)
                self.assertEqual(state['priorReleases'], number)
                self.assertFalse(state['sequenceComplete'])
                self.assertEqual(state['completedStages'], [])
                self.assertTrue(state['newWritesPossible'])
                records = current.records()
                self.assertEqual(records[number*journal.RELEASE_RECORDS]['previousHash'], tip)
                contexts = []
                for stage in journal.STAGES:
                    current.perform(stage, TARGETS,
                                    lambda context: contexts.append(context) or observed(context))
                self.assertTrue(current.status()['sequenceComplete'])
                self.assertTrue(all(c['releaseNonce'] == nonce for c in contexts))
                self.assertTrue(all(c['planHash'] == journal.sha(journal.canonical(plan)) for c in contexts))
            self.assertNotEqual(first_tip, current.status()['latestRecordHash'])
            self.assertEqual((self.root/'release.lock').stat().st_ino, lock_inode)
            self.assertEqual({name: (self.root/name).read_bytes() for name in original}, original)
            self.assertEqual(len(current.records()), 3*journal.RELEASE_RECORDS)
        with journal.open_journal(self.root) as fresh:
            self.assertTrue(fresh.status()['sequenceComplete'])
            self.assertEqual(fresh.status()['priorReleases'], 2)
            with self.assertRaises(ValueError): fresh.initialize(PLAN, '1'*32)

    def test_next_release_requires_terminal_tip_and_unused_nonce(self):
        with journal.open_journal(self.root) as current:
            with self.assertRaises(ValueError): current.initialize_next(PLAN, 'e'*32, '0'*64)
            current.initialize(PLAN, NONCE)
            before = current.records()
            with self.assertRaises(ValueError):
                current.initialize_next(PLAN, 'e'*32, current.status()['latestRecordHash'])
            self.assertEqual(current.records(), before)
            tip = self.complete(current)
            for nonce, expected in [(NONCE, tip), ('e'*32, '0'*64), ('e'*32, True), ('bad', tip)]:
                with self.assertRaises(ValueError): current.initialize_next(PLAN, nonce, expected)
            with self.assertRaises(ValueError): current.initialize_next({**PLAN, 'secret': 'no'}, 'e'*32, tip)
            current.initialize_next(PLAN, 'e'*32, tip)
            with self.assertRaises(ValueError): current.initialize_next(PLAN, 'f'*32, tip)
            second_tip = self.complete(current)
            with self.assertRaises(ValueError): current.initialize_next(PLAN, NONCE, second_tip)
            with self.assertRaises(ValueError): current.initialize_next(PLAN, 'e'*32, second_tip)
            with self.assertRaises(ValueError): current.initialize_next(PLAN, 'f'*32, tip)
            current.initialize_next(PLAN, 'f'*32, second_tip)

    def test_next_release_io_uncertainty_never_overwrites_prior_history(self):
        for failure in (1, 2, 3):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory(dir=self.root) as path:
                root = Path(path)
                with journal.open_journal(root) as current:
                    current.initialize(PLAN, NONCE)
                    tip = self.complete(current)
                    before = {p.name: p.read_bytes() for p in root.glob('*.json')}
                    original = journal.os.fsync
                    calls = []
                    def sync(fd):
                        calls.append(fd)
                        if len(calls) == failure: raise OSError('synthetic next-plan sync failure')
                        return original(fd)
                    with patch.object(journal.os, 'fsync', side_effect=sync), self.assertRaises(OSError):
                        current.initialize_next(PLAN, 'e'*32, tip)
                    with self.assertRaises(ValueError): current.status()
                    with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)
                self.assertEqual({name: (root/name).read_bytes() for name in before}, before)
                if failure < 3:
                    with self.assertRaises(ValueError):
                        with journal.open_journal(root): pass
                else:
                    # The complete new plan is visible; a fresh successful sync
                    # admits it without repeating or overwriting its publication.
                    with patch.object(journal.os, 'fsync', side_effect=OSError), self.assertRaises(OSError):
                        with journal.open_journal(root): self.fail('unsynced admission')
                    with journal.open_journal(root) as fresh:
                        self.assertEqual(fresh.status()['releaseNonce'], 'e'*32)
                        self.assertEqual(fresh.status()['completedStages'], [])
                        with self.assertRaises(ValueError): fresh.initialize_next(PLAN, 'f'*32, tip)
                        fresh.perform('maintenance', TARGETS, observed)

    def test_pending_successor_and_stale_competing_operator_remain_blocked(self):
        self.initialize()
        with journal.open_journal(self.root) as first:
            tip = self.complete(first)
            with self.assertRaises(BlockingIOError):
                with journal.open_journal(self.root): self.fail('second operator acquired lock')
            first.initialize_next(PLAN, 'e'*32, tip)
            with self.assertRaises(RuntimeError):
                first.perform('maintenance', TARGETS,
                              lambda _: (_ for _ in ()).throw(RuntimeError('synthetic crash')))
        with journal.open_journal(self.root) as stale:
            self.assertEqual(stale.status()['pendingStage'], 'maintenance')
            with self.assertRaises(ValueError): stale.initialize_next(PLAN, 'f'*32, tip)
            with self.assertRaises(ValueError): stale.initialize_next(PLAN, 'f'*32, stale.status()['latestRecordHash'])
            with self.assertRaises(ValueError): stale.perform('maintenance', TARGETS, observed)

    def test_history_corruption_or_nonce_replay_rejects_successor_admission(self):
        for kind in ('old-corruption', 'reused-nonce', 'pending', 'hole', 'symlink', 'public-mode'):
            with self.subTest(kind=kind), tempfile.TemporaryDirectory(dir=self.root) as path:
                root = Path(path)
                with journal.open_journal(root) as current:
                    current.initialize(PLAN, NONCE)
                    tip = self.complete(current)
                    current.initialize_next(PLAN, 'e'*32, tip)
                first = root/'000.json'
                if kind == 'old-corruption':
                    value=json.loads(first.read_bytes()); value['event']['plan']['runtimeHash']='0'*64
                    first.write_bytes(journal.canonical(value))
                elif kind == 'reused-nonce':
                    successor=root/'025.json'; value=json.loads(successor.read_bytes())
                    value['releaseNonce']=NONCE; successor.write_bytes(journal.canonical(value))
                elif kind == 'pending': (root/'026.json.pending').touch(mode=0o600)
                elif kind == 'hole': (root/'024.json').rename(root/'026.json')
                elif kind == 'symlink':
                    first.rename(root/'old'); first.symlink_to(root/'old')
                else: first.chmod(0o644)
                with self.assertRaises((ValueError, OSError)):
                    with journal.open_journal(root): self.fail('corrupt history admitted')

    def test_succession_limit_rejects_before_writing_without_rotation(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            tip = self.complete(current)
            before = current.records()
            with patch.object(journal, 'MAX_RELEASES', 1), self.assertRaises(ValueError):
                current.initialize_next(PLAN, 'e'*32, tip)
            self.assertEqual(current.records(), before)

    def test_inherited_handle_cannot_initialize_next(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            tip = self.complete(current)
            child = os.fork()
            if child == 0:
                try: current.initialize_next(PLAN, 'e'*32, tip)
                except ValueError: os._exit(0)
                os._exit(1)
            _, status = os.waitpid(child, 0)
            self.assertEqual(os.waitstatus_to_exitcode(status), 0)
            self.assertTrue(current.status()['sequenceComplete'])

    def test_two_real_operators_with_same_tip_admit_only_one_successor(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            tip = self.complete(current)
        # Both candidates observe the same completed tip before taking the lock.
        code = ('import importlib.util,sys\n'
                + 's=importlib.util.spec_from_file_location("journal",%r);m=importlib.util.module_from_spec(s);s.loader.exec_module(m)\n' % str(ROOT/'ops/hetzner/release-journal.py')
                + 'try:\n with m.open_journal(%r) as j:j.initialize_next(%r,sys.argv[1],%r)\n' % (str(self.root), PLAN, tip)
                + 'except (ValueError,BlockingIOError):sys.exit(4)\n')
        processes = [subprocess.Popen([sys.executable, '-c', code, nonce],
                                      stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                     for nonce in ['e'*32, 'f'*32]]
        try:
            for process in processes:
                output, error = process.communicate(timeout=8)
                self.assertEqual(output, b'')
                self.assertEqual(error, b'')
            self.assertEqual(sorted(p.returncode for p in processes), [0, 4])
            with journal.open_journal(self.root) as fresh:
                self.assertEqual(len(fresh.records()), journal.RELEASE_RECORDS+1)
                self.assertEqual(fresh.status()['priorReleases'], 1)
        finally:
            for process in processes:
                if process.poll() is None: process.kill(); process.communicate(timeout=5)

    def outer_stale(self, kind):
        with journal.open_journal(self.root) as current:
            if kind == 'successor':
                current.initialize(PLAN, NONCE)
                tip = self.complete(current)
            elif kind == 'intent':
                current.initialize(PLAN, 'b'*32)
            before_append, proceed = threading.Event(), threading.Event()
            original = current._append
            outcomes, effects = {}, []
            def paused(event, **kwargs):
                if threading.current_thread().name == 'stale':
                    before_append.set()
                    if not proceed.wait(5):
                        raise RuntimeError('test watchdog')
                return original(event, **kwargs)
            current._append = paused
            def call(label):
                try:
                    if kind == 'initial':
                        current.initialize(PLAN, ('e' if label == 'stale' else 'f')*32)
                    elif kind == 'successor':
                        current.initialize_next(PLAN, ('e' if label == 'stale' else 'f')*32, tip)
                    else:
                        current.perform('maintenance', '2'*64,
                                        lambda context: effects.append(label) or observed(context))
                    outcomes[label] = 'admitted'
                except ValueError:
                    outcomes[label] = 'rejected'
            worker = threading.Thread(target=call, args=('stale',), name='stale')
            worker.start()
            try:
                self.assertTrue(before_append.wait(5), 'stale caller did not reach test barrier')
                call('winner')
            finally:
                proceed.set()
                worker.join(5)
            self.assertFalse(worker.is_alive(), 'test watchdog')
            self.assertEqual(outcomes, {'winner':'admitted', 'stale':'rejected'})
            self.assertFalse(current.closed)
            self.assertEqual(list(self.root.glob('*.pending')), [])
            state = current.status()
            if kind == 'intent':
                self.assertEqual(len(current.records()), 3)
                self.assertEqual(effects, ['winner'])
                self.assertEqual(state['completedStages'], ['maintenance'])
            else:
                self.assertEqual(len(current.records()), 1 if kind == 'initial' else 26)
                self.assertEqual(state['releaseNonce'], 'f'*32)


    def test_stale_initial_plan_is_rejected_at_append(self):
        self.outer_stale('initial')


    def test_stale_successor_cas_is_rejected_at_append(self):
        self.outer_stale('successor')


    def test_stale_intent_never_writes_or_calls_effect(self):
        self.outer_stale('intent')


    def test_inner_publication_is_serialized_without_pending_residue(self):
        with journal.open_journal(self.root) as current:
            current.initialize(PLAN, NONCE)
            tip = self.complete(current)
            prefix = {p.name:p.read_bytes() for p in self.root.glob('*.json')}
            lock_inode = (self.root/'release.lock').stat().st_ino
            actual_lock = current._append_lock
            first_inside, second_attempted, proceed = (threading.Event() for _ in range(3))
            outcomes = {}
            class ObservedLock:
                def __enter__(self):
                    if threading.current_thread().name == 'second':
                        second_attempted.set()
                    actual_lock.acquire()
                def __exit__(self, *_):
                    actual_lock.release()
            current._append_lock = ObservedLock()
            original_open = journal.os.open
            def held_open(name, flags, *args, **kwargs):
                if threading.current_thread().name == 'first' and name == '025.json.pending':
                    first_inside.set()
                    if not proceed.wait(5):
                        raise RuntimeError('test watchdog')
                return original_open(name, flags, *args, **kwargs)
            def call(label, nonce):
                try:
                    current.initialize_next(PLAN, nonce, tip)
                    outcomes[label] = 'admitted'
                except ValueError:
                    outcomes[label] = 'rejected'
            first = threading.Thread(target=call, args=('first', 'e'*32), name='first')
            second = threading.Thread(target=call, args=('second', 'f'*32), name='second')
            with patch.object(journal.os, 'open', side_effect=held_open):
                first.start()
                try:
                    self.assertTrue(first_inside.wait(5), 'first caller did not reach publication barrier')
                    second.start()
                    self.assertTrue(second_attempted.wait(5), 'second caller did not enter append serialization')
                    # This immediate nonblocking acquisition probes the real lock.
                    # Event deadlines are watchdogs, not timing evidence of exclusion.
                    unexpectedly_acquired = actual_lock.acquire(blocking=False)
                    if unexpectedly_acquired:
                        actual_lock.release()
                    self.assertFalse(unexpectedly_acquired, 'publication did not hold the real mutex')
                finally:
                    proceed.set()
                    first.join(5)
                    if second.ident is not None:
                        second.join(5)
            self.assertFalse(first.is_alive() or second.is_alive(), 'test watchdog')
            self.assertEqual(outcomes, {'first':'admitted', 'second':'rejected'})
            self.assertEqual(len(current.records()), 26)
            self.assertEqual(current.status()['releaseNonce'], 'e'*32)
            self.assertEqual(list(self.root.glob('*.pending')), [])
            self.assertFalse(current.closed)
            self.assertEqual({n:(self.root/n).read_bytes() for n in prefix}, prefix)
            self.assertEqual((self.root/'release.lock').stat().st_ino, lock_inode)
        with journal.open_journal(self.root) as fresh:
            self.assertEqual(fresh.status()['releaseNonce'], 'e'*32)


    def test_process_death_during_successor_publication_preserves_blocking_evidence(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            tip = self.complete(current)
        marker = self.root.parent/(self.root.name+'-next-plan.ready')
        self.addCleanup(lambda: marker.unlink(missing_ok=True))
        code = ('import importlib.util,pathlib,time\n'
                + 's=importlib.util.spec_from_file_location("journal",%r);m=importlib.util.module_from_spec(s);s.loader.exec_module(m)\n' % str(ROOT/'ops/hetzner/release-journal.py')
                + 'with m.open_journal(%r) as j:\n' % str(self.root)
                + ' original=m.os.fsync\n def interrupted(fd):\n  original(fd)\n  pathlib.Path(%r).touch()\n  time.sleep(30)\n' % str(marker)
                + ' m.os.fsync=interrupted\n j.initialize_next(%r,%r,%r)\n' % (PLAN, 'e'*32, tip))
        process = subprocess.Popen([sys.executable, '-c', code], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            deadline = time.monotonic()+8
            while not marker.exists() and time.monotonic()<deadline and process.poll() is None:
                time.sleep(.02)
            self.assertTrue(marker.exists(), 'owned next-plan writer did not reach fsync')
            process.kill(); process.communicate(timeout=5)
            self.assertTrue((self.root/'025.json.pending').is_file())
            self.assertFalse((self.root/'025.json').exists())
            with self.assertRaises(ValueError):
                with journal.open_journal(self.root): self.fail('interrupted plan admitted')
        finally:
            if process.poll() is None: process.kill(); process.communicate(timeout=5)

    def test_ordered_completion_is_not_reusable_or_external_verification(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            for index, stage in enumerate(journal.STAGES):
                def effect(context):
                    state = current.status()
                    self.assertEqual(state['pendingStage'], stage)
                    self.assertFalse(state['sequenceComplete'])
                    self.assertEqual(state['newWritesPossible'], index >= journal.STAGES.index('start-database'))
                    return observed(context)
                state = current.perform(stage, TARGETS, effect)
                self.assertEqual(state['completedStages'], list(journal.STAGES[:index+1]))
                self.assertIsNone(state['pendingStage'])
            self.assertTrue(state['sequenceComplete'])
            with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)
            with self.assertRaises(ValueError): current.initialize(PLAN, 'e'*32)
        with self.assertRaises(ValueError): current.status()  # Closed handle.

    def test_exception_or_invalid_observation_blocks_retry_and_new_nonce(self):
        for kind in ['exception', 'boolean', 'wrong-plan', 'wrong-target', 'wrong-operation', 'wrong-nonce']:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory(dir=self.root) as path:
                with journal.open_journal(path) as current:
                    current.initialize(PLAN, NONCE)
                    def effect(context):
                        if kind == 'exception': raise RuntimeError('synthetic-private-message')
                        if kind == 'boolean': return True
                        field = {'wrong-plan':'planHash', 'wrong-target':'targetsHash',
                                 'wrong-operation':'operationId', 'wrong-nonce':'releaseNonce'}[kind]
                        return {**observed(context), field:'0'*64}
                    with self.assertRaises((ValueError, RuntimeError)):
                        current.perform('maintenance', TARGETS, effect)
                with journal.open_journal(path) as current:
                    self.assertEqual(current.status()['pendingStage'], 'maintenance')
                    with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)
                    with self.assertRaises(ValueError): current.initialize(PLAN, 'e'*32)
                self.assertNotIn('synthetic-private-message', ''.join(p.read_text() for p in Path(path).glob('*.json')))

    def test_fsync_failure_never_calls_effect_and_keeps_uncertainty(self):
        for failure in range(1, 4):  # file, publish directory, unlink directory
            with self.subTest(failure=failure), tempfile.TemporaryDirectory(dir=self.root) as path:
                with journal.open_journal(path) as current:
                    current.initialize(PLAN, NONCE)
                    original = journal.os.fsync; calls = []
                    def sync(fd):
                        calls.append(fd)
                        if len(calls) == failure: raise OSError('synthetic fsync failure')
                        return original(fd)
                    effects = []
                    with patch.object(journal.os, 'fsync', side_effect=sync), self.assertRaises(OSError):
                        current.perform('maintenance', TARGETS, lambda c: effects.append(c))
                    self.assertFalse(effects)
                try:
                    with journal.open_journal(path) as current:
                        with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)
                        with self.assertRaises(ValueError): current.initialize(PLAN, 'e'*32)
                except ValueError:
                    pass  # A partial publication prevents even admission.

    def test_concurrent_lock_and_forked_handle_cannot_operate(self):
        self.initialize()
        with journal.open_journal(self.root) as current:
            with self.assertRaises(BlockingIOError):
                with journal.open_journal(self.root): pass
            child = os.fork()
            if child == 0:
                try: current.status()
                except ValueError: os._exit(0)
                os._exit(1)
            _, status = os.waitpid(child, 0)
            self.assertEqual(os.waitstatus_to_exitcode(status), 0)

    def test_completion_sync_failure_poisoning_and_fresh_durable_admission(self):
        for failure in (4, 5, 6):  # Three intent syncs precede completion syncs.
            with self.subTest(failure=failure), tempfile.TemporaryDirectory(dir=self.root) as path:
                with journal.open_journal(path) as current:
                    current.initialize(PLAN, NONCE)
                    original = journal.os.fsync; calls = []; effects = []
                    def sync(fd):
                        calls.append(fd)
                        if len(calls) == failure: raise OSError('synthetic completion sync failure')
                        return original(fd)
                    def effect(context):
                        effects.append(context)
                        return observed(context)
                    with patch.object(journal.os, 'fsync', side_effect=sync), self.assertRaises(OSError):
                        current.perform('maintenance', TARGETS, effect)
                    self.assertEqual(len(effects), 1)
                    with self.assertRaises(ValueError): current.status()
                    with self.assertRaises(ValueError): current.perform('stop-writers', TARGETS, observed)
                if failure < 6:
                    with self.assertRaises(ValueError):
                        with journal.open_journal(path): pass  # Partial publication stays blocked.
                else:
                    # Unlink happened, so the full canonical observation is visible.
                    # Failed admission fsync cannot turn it into execution authority.
                    with patch.object(journal.os, 'fsync', side_effect=OSError), self.assertRaises(OSError):
                        with journal.open_journal(path): self.fail('admitted without sync')
                    with journal.open_journal(path) as fresh:
                        self.assertEqual(fresh.status()['completedStages'], ['maintenance'])
                        fresh.perform('stop-writers', TARGETS, observed)
                    self.assertEqual(len(effects), 1)  # No maintenance replay occurred.

    def test_skips_replays_wrong_targets_and_invalid_plan_rejected_before_effect(self):
        with journal.open_journal(self.root) as current:
            bad = {**PLAN, 'secret': 'must-not-persist'}
            with self.assertRaises(ValueError): current.initialize(bad, NONCE)
            self.assertEqual(os.listdir(self.root), ['release.lock'])
            current.initialize(PLAN, NONCE)
            for stage, target in [('stop-writers', TARGETS), ('maintenance', True), ('rollback', TARGETS)]:
                with self.assertRaises(ValueError): current.perform(stage, target, lambda c: self.fail('effect called'))
            current.perform('maintenance', TARGETS, observed)
            with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)

    def test_partial_hole_extra_links_permissions_and_corruption_reject(self):
        for kind in ['partial', 'hole', 'extra', 'symlink', 'hardlink', 'mode', 'hash', 'duplicate-json', 'changed-plan']:
            with self.subTest(kind=kind), tempfile.TemporaryDirectory(dir=self.root) as path:
                root = Path(path)
                with journal.open_journal(root) as current:
                    current.initialize(PLAN, NONCE)
                    current.perform('maintenance', TARGETS, observed)
                first = root/'000.json'
                if kind == 'partial': (root/'003.json.pending').write_bytes(b'')
                elif kind == 'hole': (root/'001.json').rename(root/'004.json')
                elif kind == 'extra': (root/'unrelated').write_bytes(b'')
                elif kind == 'symlink': first.rename(root/'original'); first.symlink_to(root/'original')
                elif kind == 'hardlink': os.link(first, root/'alias')
                elif kind == 'mode': first.chmod(0o644)
                elif kind == 'hash':
                    value = json.loads((root/'001.json').read_bytes()); value['previousHash']='0'*64
                    (root/'001.json').write_bytes(journal.canonical(value))
                elif kind == 'duplicate-json': first.write_bytes(first.read_bytes().replace(b'"sequence":0', b'"sequence":0,"sequence":0'))
                else:
                    value=json.loads(first.read_bytes()); value['event']['plan']['sourceRevision']='0'*40
                    first.write_bytes(journal.canonical(value))
                with self.assertRaises((ValueError, OSError)):
                    with journal.open_journal(root): pass

    def test_lock_replacement_is_detected(self):
        with journal.open_journal(self.root) as current:
            current.initialize(PLAN, NONCE)
            (self.root/'release.lock').rename(self.root/'old-lock')
            replacement = self.root/'release.lock'; replacement.touch(mode=0o600)
            with self.assertRaises(ValueError): current.status()

    def test_parent_death_does_not_authorize_second_release_while_child_survives(self):
        self.initialize()
        pidfile = self.root.parent/(self.root.name+'-child.pid')
        self.addCleanup(lambda: pidfile.unlink(missing_ok=True))
        child_code = 'import pathlib,os,time;pathlib.Path(%r).write_text(str(os.getpid()));time.sleep(30)' % str(pidfile)
        parent_code = ('import importlib.util,pathlib,subprocess,sys,time\n'
            + 's=importlib.util.spec_from_file_location("journal",%r);m=importlib.util.module_from_spec(s);s.loader.exec_module(m)\n' % str(ROOT/'ops/hetzner/release-journal.py')
            + 'def effect(context):\n subprocess.Popen(%r);time.sleep(30)\n' % [sys.executable, '-c', child_code]
            + 'with m.open_journal(pathlib.Path(%r)) as j:j.perform("maintenance",%r,effect)\n' % (str(self.root), TARGETS))
        process = subprocess.Popen([sys.executable,'-c',parent_code], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        child_pid = None
        try:
            deadline = time.monotonic()+8
            while not pidfile.exists() and time.monotonic()<deadline and process.poll() is None: time.sleep(.02)
            self.assertTrue(pidfile.exists(), 'owned synthetic child did not start')
            child_pid = int(pidfile.read_text()); process.kill(); process.wait(timeout=5)
            os.kill(child_pid, 0)
            with journal.open_journal(self.root) as current:
                self.assertEqual(current.status()['pendingStage'], 'maintenance')
                with self.assertRaises(ValueError): current.perform('maintenance', TARGETS, observed)
                with self.assertRaises(ValueError): current.initialize(PLAN, 'e'*32)
        finally:
            if process.poll() is None: process.kill(); process.wait(timeout=5)
            if child_pid:
                try: os.kill(child_pid, signal.SIGTERM)
                except ProcessLookupError: pass


if __name__ == '__main__': unittest.main()
