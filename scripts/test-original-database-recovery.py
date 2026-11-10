#!/usr/bin/env python3
"""Real durable journal/lock controls; Docker, host boot and SQL are synthetic."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,path);m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m);return m

r=load('db_recovery',ROOT/'ops/hetzner/original-database-recovery.py')
s=load('db_service_journal',ROOT/'ops/hetzner/abort-service-journal.py')
a=s.a;j=a.j
PLAN={k:('sha256:'+'a'*64 if k.endswith('Image') else 'a'*(40 if k.endswith('Revision') else 64)) for k in j.PLAN_KEYS}
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
NEW={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}
SAVED={'runtimeConfigurationSha256':'e'*64,'expected':{'db':{'containerId':'d'*64}},
       'database':{'database':'tdf_hq','systemIdentifier':'123','migrations':[]}}


class DatabaseRecoveryTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve();self.control=self.root/'control';self.control.mkdir(mode=0o700)
        with j.open_journal(self.control) as q:q.initialize(PLAN,'b'*32)
        self.admission={'schemaVersion':1,'releaseNonce':'b'*32,'planHash':j.sha(j.canonical(PLAN)),
                        'host':HOST,'originalDeployment':copy.deepcopy(SAVED)}
        with a.open_abort(self.control) as q,patch.object(a,'boot_identity',return_value=HOST):
            q.latch(self.admission,a.sha(a.canonical(self.admission)));q.request_reboot(lambda:None)
        self.q=self.enterContext(a.open_abort(self.control))
        self.enterContext(patch.object(a,'boot_identity',return_value=NEW))
        self.journal=s.ServiceJournal(self.q);self.journal.begin_epoch()
        self.journal.perform('remove-disposables','a'*64,lambda c:{**c,'evidenceHash':'b'*64})
        fd=self.enterContext(r.restore.rehearsal_lock(self.root))
        self.reservation=r.Reservation(self.root,fd)
        self.enterContext(patch.object(r.o,'read_prepared',return_value=self.admission))
        self.adapter=r.OriginalDatabase(self.journal,self.root,self.reservation)
        self.running=False;self.commands=[]
        self.enterContext(patch.object(r,'observe',side_effect=lambda saved:{'running':{'db':self.running}}))
        self.enterContext(patch.object(r.o,'database_identity',return_value=copy.deepcopy(SAVED['database'])))
        def execute(command):
            self.commands.append(command);self.running=True;return 'd'*64+'\n'
        self.enterContext(patch.object(r.o.fence,'execute',side_effect=execute))

    def test_exact_start_then_sql_readiness_records_completion(self):
        status=self.adapter.recover()
        self.assertEqual(self.commands,[r.o.sources.inspector.DOCKER+['start','d'*64]])
        self.assertEqual(status['completedStages'],['remove-disposables','recover-db'])
        self.assertFalse(status['originalDeploymentRecoverySequenceComplete'])
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1)

    def test_already_running_does_not_submit_start(self):
        self.running=True;self.adapter.recover();self.assertEqual(self.commands,[])

    def test_failed_original_admission_submits_no_start_and_retains_intent(self):
        with patch.object(r,'observe',side_effect=ValueError('changed original')):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[]);self.assertEqual(self.journal.current()['pendingStage'],'recover-db')

    def test_lost_start_response_blocks_same_boot_replay(self):
        def lost(command):self.commands.append(command);self.running=True;raise TimeoutError('lost start response')
        with patch.object(r.o.fence,'execute',side_effect=lost):
            with self.assertRaises(TimeoutError):self.adapter.recover()
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1);self.assertEqual(self.journal.current()['pendingStage'],'recover-db')

    def test_wrong_system_id_or_migration_ledger_never_completes(self):
        changed={**SAVED['database'],'systemIdentifier':'999','migrations':[{'changed':'history'}]}
        with patch.object(r.o,'database_identity',return_value=changed):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1);self.assertEqual(self.journal.current()['pendingStage'],'recover-db')

    def test_lost_readiness_repeats_only_reads_and_eventually_fails(self):
        with patch.object(r.o,'database_identity',side_effect=ValueError('not ready')),patch.object(r.time,'monotonic',side_effect=[0,61]):
            with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(len(self.commands),1);self.assertEqual(self.journal.current()['pendingStage'],'recover-db')

    def test_closed_or_replaced_reservation_prevents_effect(self):
        self.reservation.closed=True
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])
        self.reservation.closed=False
        lock=self.root/'restore-rehearsal.lock';lock.rename(self.root/'retained-lock')
        lock.touch(mode=0o600)
        with self.assertRaises(ValueError):self.adapter.recover()
        self.assertEqual(self.commands,[])

    def test_reservation_excludes_other_restore_user(self):
        with self.assertRaises(BlockingIOError):
            with r.restore.rehearsal_lock(self.root):self.fail('parallel restore acquired lock')


if __name__=='__main__':unittest.main(verbosity=2)
