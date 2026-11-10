#!/usr/bin/env python3
"""Real durable journal prefixes; synthetic Docker observations and host identity."""
import copy
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('cold',ROOT/'ops/hetzner/cold-container-admission.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
j=m.j
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
PLAN={k:('sha256:'+'a'*64 if k.endswith('Image') else 'a'*(40 if k.endswith('Revision') else 64)) for k in j.PLAN_KEYS}


def inspect(cid):
    return {'Id':cid,'Image':'sha256:'+'f'*64,'Config':{'Env':['SYNTHETIC=value']},
            'HostConfig':{'RestartPolicy':{'Name':'unless-stopped','MaximumRetryCount':0}},'Mounts':[]}


def row(cid,pid):
    return {'id':cid,'configurationSha256':m.d.configuration_fingerprint(inspect(cid)),
            'running':True,'pid':pid,'paused':False,'restarting':False,'dead':False,'status':'running',
            'startedAt':'2026-10-06T00:00:00Z','restartCount':0,
            'restartPolicy':{'Name':'unless-stopped','MaximumRetryCount':0},'networkMode':'owned',
            'networkIds':['e'*64],'metadata':None}


def stop(value):
    value.update(running=False,pid=0,status='exited',metadata={'id':value['id'],'sha256':'c'*64,
                 'manuallyStopped':True,'startedBefore':True})


class StageTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve();self.control=self.root/'journal';self.control.mkdir(mode=0o700)
        self.directory=self.root/'prepared';self.directory.mkdir(mode=0o700)
        self.bindings=dict(zip(('api','db','edge'),('1'*64,'2'*64,'3'*64)))
        self.snapshot={'schemaVersion':1,'version':'29.1.3','socket':'/run/docker.sock','dataRoot':'/var/lib/docker',
          'daemon':{'sha256':'d'*64,'bytes':100},'daemonProcess':{'pid':90,'startTicks':200},
          'containers':[row(cid,100+i) for i,cid in enumerate(self.bindings.values())]+[row('4'*64,200)]}
        stop(self.snapshot['containers'][-1])
        self.policy={'schemaVersion':1,'qualification':{'sourceRevision':'e'*40,
          'daemonRestartEvidenceSha256':'f'*64,'rebootEvidenceSha256':'1'*64},'snapshot':copy.deepcopy(self.snapshot)}
        saved={'containers':{s:inspect(cid) for s,cid in self.bindings.items()},
               'expected':{s:{'containerId':cid} for s,cid in self.bindings.items()},
               'runtimeConfigurationSha256':PLAN['runtimeHash']}
        self.host=self.enterContext(patch.object(m.o.abort,'boot_identity',return_value=HOST))
        self.sampler=self.enterContext(patch.object(m.d,'observe',side_effect=lambda *_:copy.deepcopy(self.snapshot)))
        self.current=self.enterContext(j.open_journal(self.control));self.current.initialize(PLAN,'b'*32)
        with patch.object(m.o,'observe',return_value=saved):m.o.prepare(self.current,self.directory,{}, {})
        self.guard=m.ColdContainerAdmission(self.current,self.directory,self.policy)

    def advance(self,index):
        def effect(context):
            if index<3:stop(next(r for r in self.snapshot['containers'] if r['id']==self.bindings[('edge','api','db')[index]]))
            return {**context,'evidenceHash':'d'*64}
        self.current.perform(j.STAGES[index],'c'*64,effect)

    def test_all_acknowledged_cold_stages_and_original_configuration(self):
        for index in range(7):
            result=self.guard.observe()
            self.assertEqual(result['completedStages'],index)
            self.assertEqual(result['runningOriginalServices'],sorted(m.RUNNING[index]))
            for key in ('stopAuthorized','recoveryAuthorized','qualificationReferencesVerified','hostBypassAdmissionVerified'):
                self.assertIs(result[key],False)
            if index<6:self.advance(index)

    def test_pending_intent_is_not_an_acknowledged_stop_or_resumable_authority(self):
        def uncertain(_):
            stop(self.snapshot['containers'][2]);raise RuntimeError('lost reply')
        with self.assertRaises(RuntimeError):self.current.perform('maintenance','c'*64,uncertain)
        with self.assertRaises(ValueError):self.guard.observe()
        with self.assertRaises(ValueError):m.ColdContainerAdmission(self.current,self.directory,self.policy)

    def test_no_early_stop_or_restart_and_no_new_constructor_after_stage(self):
        original=copy.deepcopy(self.snapshot);stop(self.snapshot['containers'][0])
        with self.assertRaises(ValueError):self.guard.observe()
        self.snapshot=original;self.advance(0)
        self.snapshot['containers'][2]=copy.deepcopy(self.policy['snapshot']['containers'][2])
        with self.assertRaises(ValueError):self.guard.observe()
        with self.assertRaises(ValueError):m.ColdContainerAdmission(self.current,self.directory,self.policy)

    def test_restarted_running_original_cannot_reuse_its_old_pid(self):
        initial=copy.deepcopy(self.snapshot)
        for field,value in (('startedAt','2026-10-06T01:00:00Z'),('restartCount',1)):
            self.snapshot=copy.deepcopy(initial);self.snapshot['containers'][0][field]=value
            with self.assertRaises(ValueError):self.guard.observe()

    def test_reboot_or_same_binary_daemon_restart_rejects(self):
        self.host.return_value={**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}
        with self.assertRaises(ValueError):self.guard.observe()
        self.host.return_value=HOST;self.snapshot['daemonProcess']['startTicks']+=1
        with self.assertRaises(ValueError):self.guard.observe()

    def test_existing_dormant_configuration_metadata_or_membership_cannot_change(self):
        initial=copy.deepcopy(self.snapshot)
        for field,value in (('configurationSha256','f'*64),('networkIds',['f'*64]),('pid',12)):
            self.snapshot=copy.deepcopy(initial);self.snapshot['containers'][-1][field]=value
            with self.assertRaises(ValueError):self.guard.observe()
        self.snapshot=copy.deepcopy(initial);self.snapshot['containers'][-1]['metadata']['sha256']='f'*64
        with self.assertRaises(ValueError):self.guard.observe()
        self.snapshot=copy.deepcopy(initial);self.snapshot['containers'].pop()
        with self.assertRaises(ValueError):self.guard.observe()
        self.snapshot=copy.deepcopy(initial);extra=row('5'*64,300);stop(extra);self.snapshot['containers'].append(extra)
        with self.assertRaises(ValueError):self.guard.observe()

    def test_stopped_original_retains_config_network_and_literal_manual_flags(self):
        self.advance(0);initial=copy.deepcopy(self.snapshot)
        for field,value in (('configurationSha256','f'*64),('networkIds',['f'*64]),('status','created'),('paused',True),('startedAt','2026-10-06T01:00:00Z'),('restartCount',1)):
            self.snapshot=copy.deepcopy(initial);self.snapshot['containers'][2][field]=value
            with self.assertRaises(ValueError):self.guard.observe()
        for field,value in (('manuallyStopped',False),('manuallyStopped',1),('startedBefore',False),('sha256','invalid')):
            self.snapshot=copy.deepcopy(initial);self.snapshot['containers'][2]['metadata'][field]=value
            with self.assertRaises(ValueError):self.guard.observe()

    def test_changed_prepared_original_or_cross_release_policy_rejects(self):
        changed=copy.deepcopy(self.guard.original);changed['releaseNonce']='f'*32
        with patch.object(m.o,'read_prepared',return_value=changed),self.assertRaises(ValueError):self.guard.observe()
        changed=copy.deepcopy(self.guard.original);changed['originalDeployment']['containers']['api']['Config']['Env']=['DIFFERENT=value']
        with patch.object(m.o,'read_prepared',return_value=changed),self.assertRaises(ValueError):
            m.ColdContainerAdmission(self.current,self.directory,self.policy)

    def test_between_sample_drift_and_process_transfer_reject(self):
        changed=copy.deepcopy(self.snapshot);changed['daemonProcess']['pid']+=1
        with patch.object(m.d,'observe',side_effect=[self.snapshot,changed]),self.assertRaises(ValueError):self.guard.observe()
        with patch.object(m.os,'getpid',return_value=self.guard.owner+1),self.assertRaises(ValueError):self.guard.observe()

    def test_disposable_phase_is_not_covered_by_original_only_inventory(self):
        for index in range(7):self.advance(index)
        with self.assertRaises(ValueError):self.guard.observe()

    def test_changed_journal_stage_order_cannot_reinterpret_record_counts(self):
        changed=('stop-writers','maintenance',*j.STAGES[2:])
        with patch.object(j,'STAGES',changed),self.assertRaises(ValueError):self.guard.observe()

    def test_policy_mutation_after_construction_does_not_redefine_authority(self):
        self.policy['snapshot']['daemonProcess']['startTicks']=999
        self.assertEqual(self.guard.observe()['completedStages'],0)


if __name__=='__main__':unittest.main()
