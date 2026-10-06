#!/usr/bin/env python3
"""Real private journal/lock; synthetic runtime and redacted probe observations."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
def load(name,path):
    s=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m

r=load('completion_recovery_test',ROOT/'ops/hetzner/original-recovery-completion.py')
base=load('completion_recovery_fixture',ROOT/'scripts/test-original-database-recovery.py')
base.r=r.a.d
base.SAVED={**base.SAVED,'containers':{'api':{'Config':{'Env':['SOURCE_COMMIT='+'a'*40,'GIT_SHA='+'a'*40,'APP_PORT=8080']}}},
            'units':{'timerEnabled':True,'timerActive':True,'fileHashes':{}}}

def probe(saved,path,service):
    assert service in ('api','edge')
    metadata={'name':'tdf-hq','commit':'a'*40,'version':'0.1.0.0','buildTime':'2026-10-06T00:00:00Z'} if path=='/version' else {}
    return {'code':401 if path=='/bookings' else 200,'valid':True,'metadata':metadata,'transportUnavailable':False}


class CompletionTests(unittest.TestCase):
    def setUp(self):
        base.DatabaseRecoveryTests.setUp(self)
        for stage in ('recover-db','recover-api','recover-edge','restore-timer'):
            self.journal.perform(stage,'d'*64,lambda c:{**c,'evidenceHash':'e'*64})
        self.adapter=r.OriginalRecoveryCompletion(self.journal,self.root,self.reservation)
        self.value={'running':dict.fromkeys(('db','api','edge'),True),'units':{
            'timerStopped':False,'backupServiceInactive':True,'unitConfigurationSha256':'c'*64}}
        self.enterContext(patch.object(r.a.d,'observe',side_effect=lambda saved:copy.deepcopy(self.value)))
        self.enterContext(patch.object(r.a,'probe',side_effect=probe))
        self.enterContext(patch.object(r.a.d.o.sources.inspector,'capture',return_value=''))

    def test_fresh_eight_probes_seal_sequence_without_runtime_mutation(self):
        with patch.object(r.a,'probe',side_effect=probe) as calls:state=self.adapter.recover()
        self.assertTrue(state['originalDeploymentRecoverySequenceComplete'])
        self.assertFalse(state['releaseContinuationAllowed'])
        self.assertTrue(state['newWritesPossible'])
        self.assertEqual([(c.args[1],c.args[2]) for c in calls.call_args_list],[(p,s) for s in ('api','edge') for p in r.a.PATHS])
        self.assertEqual(self.commands,[])
        with self.assertRaises(ValueError):self.adapter.recover()
        with self.assertRaises(ValueError):self.journal.request_next_reboot(lambda:self.fail('reboot after completion'))

    def rejected(self):
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'complete-abort')
        self.assertFalse(self.journal.current()['originalDeploymentRecoverySequenceComplete'])
        self.assertEqual(self.commands,[])

    def test_missing_original_service_is_not_repaired(self):
        self.value['running']['api']=False;self.rejected()

    def test_stopped_timer_is_not_restarted(self):
        self.value['units']['timerStopped']=True;self.rejected()

    def test_pending_disposable_marker_blocks_completion(self):
        (self.root/r.a.d.restore.PENDING_NAME).touch(mode=0o600);self.rejected()

    def test_surviving_stopped_disposable_blocks_completion(self):
        with patch.object(r.a.d.o.sources.inspector,'capture',return_value='f'*64+'\n'):self.rejected()

    def test_active_backup_prevents_completion(self):
        self.value['units']['backupServiceInactive']=False;self.rejected()

    def test_database_drift_at_closing_sample_prevents_completion(self):
        with patch.object(r.a.d.o,'database_identity',side_effect=[copy.deepcopy(base.SAVED['database']),{}]):self.rejected()

    def test_wrong_revision_on_either_transport_prevents_completion(self):
        def wrong(saved,path,service):
            value=probe(saved,path,service)
            if path=='/version' and service=='edge':value['metadata']['commit']='b'*40
            return value
        with patch.object(r.a,'probe',side_effect=wrong):self.rejected()

    def test_tls_failure_cannot_complete_or_retry(self):
        def wrong(saved,path,service):
            return {'code':None,'valid':False,'metadata':{},'transportUnavailable':False} if service=='edge' else probe(saved,path,service)
        with patch.object(r.a,'probe',side_effect=wrong) as calls:
            self.rejected();count=calls.call_count
            with self.assertRaises(ValueError):self.adapter.recover()
            self.assertEqual(calls.call_count,count)

    def test_anonymous_booking_success_prevents_completion(self):
        def wrong(saved,path,service):
            return {**probe(saved,path,service),'code':200} if path=='/bookings' else probe(saved,path,service)
        with patch.object(r.a,'probe',side_effect=wrong):self.rejected()

    def test_closing_timer_change_prevents_completion(self):
        after=copy.deepcopy(self.value);after['units']['timerStopped']=True
        with patch.object(r.a.d,'observe',side_effect=[self.value,after]):self.rejected()

    def test_observation_failure_leaves_permanent_pending_stage(self):
        with patch.object(r.a,'probe',side_effect=TimeoutError('lost observation')):
            with self.assertRaises(TimeoutError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'complete-abort')
        self.assertFalse(self.journal.current()['originalDeploymentRecoverySequenceComplete'])


if __name__=='__main__':unittest.main(verbosity=2)
