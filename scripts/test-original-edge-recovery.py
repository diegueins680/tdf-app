#!/usr/bin/env python3
"""Real recovery journal and reservation; synthetic Docker, DB and HTTP observations."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    s=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m

edge=load('edge_recovery_test',ROOT/'ops/hetzner/original-edge-recovery.py')
base=load('edge_recovery_fixture',ROOT/'scripts/test-original-database-recovery.py')
REAL_PROBE=edge.a.probe
base.r=edge.a.d
base.SAVED={**base.SAVED,'containers':{'api':{'Config':{'Env':['SOURCE_COMMIT='+'a'*40,'GIT_SHA='+'a'*40,'APP_PORT=8080']}}},
            'expected':{**base.SAVED['expected'],'api':{'containerId':'a'*64},'edge':{'containerId':'e'*64}}}


def probe(saved,path,service):
    assert service=='edge'
    metadata={'status':'ok','db':'ok'} if path=='/health' else {'name':'tdf-hq','commit':'a'*40,'version':'0.1.0.0','buildTime':'2026-10-06T00:00:00Z'} if path=='/version' else {}
    return {'code':401 if path=='/bookings' else 200,'valid':True,'metadata':metadata,'transportUnavailable':False}


class EdgeTests(unittest.TestCase):
    def setUp(self):
        base.DatabaseRecoveryTests.setUp(self)
        for stage in ('recover-db','recover-api'):
            self.journal.perform(stage,'d'*64,lambda c:{**c,'evidenceHash':'e'*64})
        self.adapter=edge.OriginalEdge(self.journal,self.root,self.reservation)
        self.enterContext(patch.object(edge.a.d,'observe',side_effect=lambda saved:{'running':{'db':True,'api':True,'edge':self.running}}))
        self.enterContext(patch.object(edge.a,'probe',side_effect=probe))
        def execute(command):self.commands.append(command);self.running=True;return 'e'*64+'\n'
        self.enterContext(patch.object(edge.a.d.o.fence,'execute',side_effect=execute))

    def test_exact_start_requires_all_probes_through_original_edge(self):
        with patch.object(edge.a,'probe',side_effect=probe) as calls:state=self.adapter.recover()
        self.assertEqual(self.commands,[edge.a.d.o.sources.inspector.DOCKER+['start','e'*64]])
        self.assertEqual([(c.args[1],c.args[2]) for c in calls.call_args_list],[(p,'edge') for p in edge.a.PATHS])
        self.assertEqual(state['completedStages'],['remove-disposables','recover-db','recover-api','recover-edge'])
        self.assertFalse(state['originalDeploymentRecoverySequenceComplete'])
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)

    def test_running_original_never_receives_duplicate_start(self):
        self.running=True;self.adapter.recover();self.assertEqual(self.commands,[])

    def test_missing_api_prevents_edge_start(self):
        with patch.object(edge.a.d,'observe',return_value={'running':{'db':True,'api':False,'edge':False}}):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_changed_database_prevents_edge_start(self):
        with patch.object(edge.a.d.o,'database_identity',return_value={}):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_tls_failure_is_not_retried_as_transient_absence(self):
        with patch.object(edge.a,'probe',return_value={'code':None,'valid':False,'metadata':{},'transportUnavailable':False}) as calls:
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(calls.call_count,1)
        self.assertEqual(self.journal.current()['pendingStage'],'recover-edge')

    def test_redirect_is_rejected(self):
        with patch.object(edge.a,'probe',return_value={'code':302,'valid':False,'metadata':{},'transportUnavailable':False}):
            with self.assertRaises(ValueError):self.adapter.recover()

    def test_wrong_revision_is_rejected(self):
        def wrong(saved,path,service):
            value=probe(saved,path,service)
            if path=='/version':value['metadata']['commit']='b'*40
            return value
        with patch.object(edge.a,'probe',side_effect=wrong):
            with self.assertRaises(ValueError):self.adapter.recover()

    def test_anonymous_success_is_rejected(self):
        def wrong(saved,path,service):
            return {**probe(saved,path,service),'code':200,'valid':False} if path=='/bookings' else probe(saved,path,service)
        with patch.object(edge.a,'probe',side_effect=wrong):
            with self.assertRaises(ValueError):self.adapter.recover()

    def test_lost_start_response_cannot_replay_in_same_epoch(self):
        def lost(command):self.commands.append(command);raise TimeoutError('lost edge start response')
        with patch.object(edge.a.d.o.fence,'execute',side_effect=lost):
            with self.assertRaises(TimeoutError):self.adapter.recover()
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)

    def test_closing_database_drift_retains_pending_intent(self):
        with patch.object(edge.a.d.o,'database_identity',side_effect=[copy.deepcopy(base.SAVED['database']),{}]):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'recover-edge')

    def test_unknown_service_denied_before_runtime_inspection(self):
        for service in ('db','https://foreign.example',None):
            with self.assertRaises(ValueError):REAL_PROBE({},'/health',service)


if __name__=='__main__':unittest.main(verbosity=2)
