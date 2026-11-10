#!/usr/bin/env python3
"""Real recovery journal/reservation and synthetic unit/Docker observations."""
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    s=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m

r=load('timer_recovery_test',ROOT/'ops/hetzner/original-timer-recovery.py')
base=load('timer_recovery_fixture',ROOT/'scripts/test-original-database-recovery.py')
base.r=r.d
base.SAVED={**base.SAVED,'units':{'timerEnabled':True,'timerActive':True,'fileHashes':{}}}


class TimerTests(unittest.TestCase):
    def setUp(self):
        base.DatabaseRecoveryTests.setUp(self)
        for stage in ('recover-db','recover-api','recover-edge'):
            self.journal.perform(stage,'d'*64,lambda c:{**c,'evidenceHash':'e'*64})
        self.adapter=r.OriginalTimer(self.journal,self.root,self.reservation)
        self.stopped=True
        def observation(saved):
            return {'running':dict.fromkeys(('db','api','edge'),True),'units':{
                'timerStopped':self.stopped,'backupServiceInactive':True,'unitConfigurationSha256':'c'*64}}
        self.observation=observation
        self.enterContext(patch.object(r.d,'observe',side_effect=observation))
        def execute(command):self.commands.append(command);self.stopped=False;return ''
        self.enterContext(patch.object(r.d.o.fence,'execute',side_effect=execute))

    def test_only_original_timer_starts_once_after_edge_recovery(self):
        state=self.adapter.recover()
        self.assertEqual(self.commands,[['systemctl','start',r.d.o.fence.TIMER]])
        self.assertEqual(state['completedStages'],['remove-disposables','recover-db','recover-api','recover-edge','restore-timer'])
        self.assertFalse(state['originalDeploymentRecoverySequenceComplete'])
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)

    def test_already_active_timer_is_observed_without_start(self):
        self.stopped=False;self.adapter.recover();self.assertEqual(self.commands,[])

    def test_changed_unit_or_busy_backup_prevents_start(self):
        with patch.object(r.d,'observe',side_effect=ValueError('unit admission rejected')):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_missing_original_service_prevents_timer_start(self):
        bad=self.observation(None);bad['running']['edge']=False
        with patch.object(r.d,'observe',return_value=bad):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_changed_ledger_prevents_timer_start(self):
        with patch.object(r.d.o,'database_identity',return_value={}):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_lost_start_response_remains_pending_without_retry(self):
        def lost(command):self.commands.append(command);raise TimeoutError('lost timer reply')
        with patch.object(r.d.o.fence,'execute',side_effect=lost):
            with self.assertRaises(TimeoutError):self.adapter.recover()
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)
        self.assertEqual(self.journal.current()['pendingStage'],'restore-timer')

    def test_active_backup_at_closing_sample_prevents_successful_observation(self):
        with patch.object(r.d,'observe',side_effect=[self.observation(None),ValueError('backup active at closing sample')]):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'restore-timer')
        self.assertEqual(len(self.commands),1)

    def test_initially_disabled_timer_is_not_implicitly_enabled(self):
        self.adapter.saved['units']['timerEnabled']=False
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])
        self.assertIsNone(self.journal.current()['pendingStage'])


if __name__=='__main__':unittest.main(verbosity=2)
