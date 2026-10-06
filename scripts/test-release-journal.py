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
        self.addCleanup(lambda: pidfile.with_suffix('.pending').unlink(missing_ok=True))
        child_code = 'import pathlib,os,time;p=pathlib.Path(%r);t=p.with_suffix(".pending");t.write_text(str(os.getpid()));t.rename(p);time.sleep(30)' % str(pidfile)
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
