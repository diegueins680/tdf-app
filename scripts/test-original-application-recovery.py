#!/usr/bin/env python3
"""Real journal/lock controls; API transport and Docker observations are synthetic."""
import copy
import contextlib
import http.client
import io
import json
import sys
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m);return m

app=load('app_recovery_test',ROOT/'ops/hetzner/original-application-recovery.py')
base=load('app_recovery_fixture',ROOT/'scripts/test-original-database-recovery.py')
base.r=app.d
base.SAVED={**base.SAVED,'containers':{'api':{'Config':{'Env':['SOURCE_COMMIT='+'a'*40,'GIT_SHA='+'a'*40,'APP_PORT=8080']}}},
            'expected':{**base.SAVED['expected'],'api':{'containerId':'a'*64}}}


def probes(saved,path):
    metadata={'status':'ok','db':'ok'} if path=='/health' else {'name':'tdf-hq','commit':'a'*40,'version':'0.1.0.0','buildTime':'2026-10-06T00:00:00Z'} if path=='/version' else {}
    return {'code':401 if path=='/bookings' else 200,'valid':True,'metadata':metadata,'transportUnavailable':False}


class ApplicationTests(unittest.TestCase):
    def setUp(self):
        base.DatabaseRecoveryTests.setUp(self)
        self.journal.perform('recover-db','d'*64,lambda c:{**c,'evidenceHash':'e'*64})
        self.adapter=app.OriginalApplication(self.journal,self.root,self.reservation)
        self.enterContext(patch.object(app.d,'observe',side_effect=lambda saved:{'running':{'db':True,'api':self.running}}))
        self.enterContext(patch.object(app,'probe',side_effect=probes))
        def execute(command):self.commands.append(command);self.running=True;return 'a'*64+'\n'
        self.enterContext(patch.object(app.d.o.fence,'execute',side_effect=execute))

    def test_exact_original_start_requires_all_four_probes(self):
        with patch.object(app,'probe',side_effect=probes) as calls:status=self.adapter.recover()
        self.assertEqual(self.commands,[app.d.o.sources.inspector.DOCKER+['start','a'*64]])
        self.assertEqual([c.args[1] for c in calls.call_args_list],list(app.PATHS))
        self.assertEqual(status['completedStages'],['remove-disposables','recover-db','recover-api'])
        self.assertFalse(status['originalDeploymentRecoverySequenceComplete'])

    def test_running_original_receives_no_start(self):
        self.running=True;self.adapter.recover();self.assertEqual(self.commands,[])

    def test_health_success_cannot_hide_public_database_read_failure(self):
        def fail(saved,path):return {**probes(saved,path),'code':503,'valid':False} if path=='/rooms/public' else probes(saved,path)
        with patch.object(app,'probe',side_effect=fail),self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'recover-api')

    def test_unauthorized_endpoint_success_is_rejected(self):
        def fail(saved,path):return {**probes(saved,path),'code':200,'valid':False} if path=='/bookings' else probes(saved,path)
        with patch.object(app,'probe',side_effect=fail),self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.journal.current()['pendingStage'],'recover-api')

    def test_wrong_revision_is_not_recovery(self):
        def fail(saved,path):
            result=probes(saved,path)
            if path=='/version':result['metadata']['commit']='b'*40
            return result
        with patch.object(app,'probe',side_effect=fail),self.assertRaises(ValueError):self.adapter.recover()

    def test_changed_ledger_denies_before_api_start(self):
        with patch.object(app.d.o,'database_identity',return_value={}),self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_lost_start_response_blocks_same_boot_retry(self):
        def lost(command):self.commands.append(command);raise TimeoutError('lost API start')
        with patch.object(app.d.o.fence,'execute',side_effect=lost):
            with self.assertRaises(TimeoutError):self.adapter.recover()
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)

    def test_conflicting_metadata_environment_rejected_without_start(self):
        self.adapter.saved=copy.deepcopy(self.adapter.saved)
        self.adapter.saved['containers']['api']['Config']['Env'].append('GIT_COMMIT='+'b'*40)
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])


class ProbeControls(unittest.TestCase):
    def execute(self,path,code,body):
        class Response:
            status=code
            def read(self,maximum):return body[:maximum]
        calls=[]
        class Connection:
            def __init__(self,*args,**kwargs):calls.append((args,kwargs))
            def request(self,*args,**kwargs):calls.append((args,kwargs))
            def getresponse(self):return Response()
            def close(self):pass
        output=io.StringIO()
        with patch.object(http.client,'HTTPConnection',Connection),patch.object(sys,'argv',['probe',path]),contextlib.redirect_stdout(output):
            exec(compile(app.PROBE,'fixed-original-probe','exec'),{})
        self.assertEqual(calls[0],(('127.0.0.1',8080),{'timeout':5}))
        self.assertEqual(calls[1][0],('GET',path))
        return json.loads(output.getvalue()),output.getvalue()

    def test_room_data_is_validated_and_never_emitted(self):
        row={'roomId':'fixture-id','rName':'fixture-private-output-marker','rBookable':True}
        value,raw=self.execute('/rooms/public',200,json.dumps([row]).encode())
        self.assertTrue(value['valid']);self.assertEqual(value['metadata'],{})
        self.assertNotIn(row['rName'],raw);self.assertNotIn(row['roomId'],raw)

    def test_wrong_room_schema_and_nonbookable_values_fail(self):
        for body in ({},[{'roomId':'x','rName':'x','rBookable':False}],[{'roomId':'x','rName':'x','rBookable':True,'extra':'data'}]):
            value,_=self.execute('/rooms/public',200,json.dumps(body).encode());self.assertFalse(value['valid'])

    def test_redirect_and_malformed_or_oversized_bodies_fail(self):
        for code,body in ((302,b'[]'),(200,b'not-json'),(200,b' '*65537)):
            value,_=self.execute('/rooms/public',code,body);self.assertFalse(value['valid'])

    def test_successful_booking_response_is_never_accepted_or_emitted(self):
        for code,valid in ((200,False),(401,True),(403,False),(302,False)):
            value,raw=self.execute('/bookings',code,b'fixture-sensitive-body')
            self.assertEqual(value['valid'],valid);self.assertNotIn('fixture-sensitive-body',raw)

    def test_version_payload_requires_exact_bounded_string_fields(self):
        for body in ({'commit':'a'*40},{'name':'tdf-hq','commit':'a'*40,'version':'0.1','buildTime':7},
                     {'name':'tdf-hq','commit':'a'*40,'version':'0.1','buildTime':'x'*257}):
            value,_=self.execute('/version',200,json.dumps(body).encode());self.assertFalse(value['valid'])



if __name__=='__main__':unittest.main(verbosity=2)
