#!/usr/bin/env python3
"""Negative controls for persisted manual-stop admission; no Docker mutations."""
import copy
import importlib.util
from pathlib import Path
import unittest
import tempfile
from unittest.mock import patch
from types import SimpleNamespace

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('dormant',ROOT/'ops/hetzner/dormant-container-admission.py')
d=importlib.util.module_from_spec(spec);spec.loader.exec_module(d)
spec=importlib.util.spec_from_file_location('dormant_fixture',ROOT/'scripts/test-dormant-container-reboot-linux.py')
fixture=importlib.util.module_from_spec(spec);spec.loader.exec_module(fixture)


class AdmissionTests(unittest.TestCase):
    def setUp(self):
        cid='a'*64
        self.row={'id':cid,'configurationSha256':'9'*64,'running':False,'pid':0,'paused':False,'restarting':False,
                  'dead':False,'status':'exited','startedAt':'2026-10-06T00:00:00Z','restartCount':0,'restartPolicy':{'Name':'unless-stopped','MaximumRetryCount':0},
                  'networkMode':'legacy','networkIds':['b'*64],
                  'metadata':{'id':cid,'sha256':'c'*64,'manuallyStopped':True,'startedBefore':True}}
        self.snapshot={'schemaVersion':1,'version':'29.1.3','socket':'/run/docker.sock',
                       'dataRoot':'/var/lib/docker','daemon':{'sha256':'d'*64,'bytes':100},'daemonProcess':{'pid':47,'startTicks':100},'containers':[self.row]}
        self.policy={'schemaVersion':1,'qualification':{'sourceRevision':'e'*40,
                     'daemonRestartEvidenceSha256':'f'*64,'rebootEvidenceSha256':'1'*64},
                     'snapshot':copy.deepcopy(self.snapshot)}

    def test_same_binary_daemon_restart_or_pid_reuse_between_samples_rejects(self):
        for incarnation in ({'pid':48,'startTicks':200},{'pid':47,'startTicks':200}):
            changed=copy.deepcopy(self.snapshot);changed['daemonProcess']=incarnation
            with patch.object(d,'observe',side_effect=[self.snapshot,changed]),self.assertRaises(ValueError):
                d.observe_qualified(self.policy)
        for incarnation in ({'pid':0,'startTicks':100},{'pid':True,'startTicks':100},
                            {'pid':47,'startTicks':0},{'pid':47}, {'pid':47,'startTicks':True}):
            changed=copy.deepcopy(self.snapshot);changed['daemonProcess']=incarnation
            with self.assertRaises(ValueError):d.admit(changed,{**self.policy,'snapshot':changed})

    def test_configuration_fingerprint_preserves_all_authority_but_not_runtime_state(self):
        value={'Id':'a'*64,'Image':'sha256:'+'b'*64,
               'Config':{'Image':'synthetic/image','Env':['PRIVATE=never-in-receipt'],'Labels':{'role':'original'}},
               'HostConfig':{'RestartPolicy':{'Name':'unless-stopped'},'Privileged':False},
               'Mounts':[{'Destination':'/second','Source':'/owned/two','RW':True},
                         {'Destination':'/first','Source':'/owned/one','RW':False}],
               'State':{'Pid':1,'StartedAt':'first'}}
        digest=d.configuration_fingerprint(value)
        changed=copy.deepcopy(value);changed['Mounts'].reverse();changed['State']={'Pid':2,'StartedAt':'second'}
        self.assertEqual(d.configuration_fingerprint(changed),digest)
        mutations=(lambda v:v.update(Id='c'*64),lambda v:v.update(Image='sha256:'+'c'*64),
                   lambda v:v['Config']['Env'].append('PRIVATE=changed'),
                   lambda v:v['Config']['Labels'].update(role='other'),
                   lambda v:v['HostConfig'].update(Privileged=True),
                   lambda v:v['Mounts'][0].update(Source='/other'),
                   lambda v:v['Mounts'][0].update(RW=False))
        for mutate in mutations:
            changed=copy.deepcopy(value);mutate(changed)
            self.assertNotEqual(d.configuration_fingerprint(changed),digest)
        self.assertNotIn('never-in-receipt',digest)
        changed=copy.deepcopy(value);changed['Mounts'].append(copy.deepcopy(changed['Mounts'][0]))
        with self.assertRaises(ValueError):d.configuration_fingerprint(changed)

    def test_missing_or_changed_configuration_binding_rejects(self):
        changed=copy.deepcopy(self.snapshot);changed['containers'][0]['configurationSha256']='f'*64
        with self.assertRaises(ValueError):d.admit(changed,self.policy)
        for value in (None,True,'not-a-hash'):
            changed=copy.deepcopy(self.snapshot);changed['containers'][0]['configurationSha256']=value
            with self.assertRaises(ValueError):d.admit(changed,{**self.policy,'snapshot':changed})

    def test_manual_stop_does_not_claim_network_or_release_admission(self):
        result=d.admit(self.snapshot,self.policy)
        self.assertTrue(result['dormantContainersMatchReviewedQualification'])
        self.assertFalse(result['hostBypassAdmissionVerified'])

    def test_flag_false_missing_or_nonboolean_rejects_even_in_policy(self):
        for key in ('manuallyStopped','startedBefore'):
            for value in (False,None,1,'true'):
                with self.subTest(key=key,value=value):
                    changed=copy.deepcopy(self.snapshot);changed['containers'][0]['metadata'][key]=value
                    self.policy['snapshot']=changed
                    with self.assertRaises(ValueError):d.admit(changed,self.policy)

    def test_always_or_on_failure_cannot_be_treated_as_manual_stop(self):
        for name in ('always','on-failure',''):
            changed=copy.deepcopy(self.snapshot);changed['containers'][0]['restartPolicy']['Name']=name
            self.policy['snapshot']=changed
            with self.assertRaises(ValueError):d.admit(changed,self.policy)

    def test_no_restart_accepts_created_container_without_manual_stop(self):
        self.row['restartPolicy']['Name']='no';self.row['status']='created'
        self.row['metadata']['manuallyStopped']=False;self.row['metadata']['startedBefore']=False
        self.policy['snapshot']=copy.deepcopy(self.snapshot)
        d.admit(self.snapshot,self.policy)

    def test_nonzero_pid_or_transitional_state_rejects(self):
        for key,value in (('pid',7),('paused',True),('restarting',True),('dead',True),('status','removing')):
            changed=copy.deepcopy(self.snapshot);changed['containers'][0][key]=value
            self.policy['snapshot']=changed
            with self.subTest(key=key),self.assertRaises(ValueError):d.admit(changed,self.policy)

    def test_unobserved_container_policy_network_and_daemon_drift_rejects(self):
        for mutate in (lambda s:s['containers'].append(copy.deepcopy(self.row)),
                       lambda s:s['containers'][0]['networkIds'].append('3'*64),
                       lambda s:s['containers'][0]['restartPolicy'].update(Name='no'),
                       lambda s:s['daemon'].update(sha256='4'*64),
                       lambda s:s.update(dataRoot='/other'),lambda s:s.update(socket='/other.sock'),
                       lambda s:s['containers'][0]['metadata'].update(sha256='5'*64)):
            changed=copy.deepcopy(self.snapshot);mutate(changed)
            with self.assertRaises(ValueError):d.admit(changed,self.policy)

    def test_unqualified_daemon_version_rejects_even_with_matching_policy(self):
        self.snapshot['version']='28.4.0';self.policy['snapshot']=copy.deepcopy(self.snapshot)
        with self.assertRaises(ValueError):d.admit(self.snapshot,self.policy)

    def test_missing_qualification_rejects(self):
        for key in self.policy['qualification']:
            changed=copy.deepcopy(self.policy);del changed['qualification'][key]
            with self.assertRaises(ValueError):d.admit(self.snapshot,changed)

    def test_change_during_observation_rejects(self):
        changed=copy.deepcopy(self.snapshot);changed['containers'][0]['metadata']['manuallyStopped']=False
        with patch.object(d,'observe',side_effect=[self.snapshot,changed]):
            with self.assertRaises(ValueError):d.observe_qualified(self.policy)

    def test_invalid_container_identifier_rejects(self):
        for value in ('../escape','a'*63,'A'*64,None):
            with self.assertRaises(ValueError):d.identifier(value)

    def test_get_boundary_allows_no_mutation_or_other_endpoint(self):
        client=object.__new__(d.Docker)
        for path in ('/containers/'+('a'*64)+'/start','/images/json','/info?anything=1'):
            with self.assertRaises(ValueError):client.get(path)

    def test_pid1_socket_activation_binds_inherited_listener(self):
        result=SimpleNamespace(returncode=0,stdout='47\n')
        unix=('Num RefCount Protocol Flags Type St Inode Path\n0000: 2 0 00010000 0001 01 98 /fixture.sock\n'
              '0000: 2 0 00000000 0001 03 99 /fixture.sock\n')
        with patch.object(d.subprocess,'run',return_value=result),patch.object(d.Path,'read_text',return_value=unix),\
             patch.object(d.Path,'iterdir',return_value=iter([Path('/proc/47/fd/3')])),\
             patch.object(d.os,'readlink',return_value='socket:[98]'):
            self.assertEqual(d.serving_pid(1,'/fixture.sock'),47)

    def test_wrong_missing_or_connected_socket_cannot_bind_pid1(self):
        result=SimpleNamespace(returncode=0,stdout='47\n')
        for rows,target in (('0000: 2 0 00010000 0001 01 98 /fixture.sock','socket:[99]'),
                            ('0000: 2 0 00010000 0001 01 98 /wrong.sock','socket:[98]'),
                            ('0000: 2 0 00000000 0001 03 98 /fixture.sock','socket:[98]')):
            with patch.object(d.subprocess,'run',return_value=result),\
                 patch.object(d.Path,'read_text',return_value='header\n'+rows+'\n'),\
                 patch.object(d.Path,'iterdir',return_value=iter([Path('/proc/47/fd/3')])),\
                 patch.object(d.os,'readlink',return_value=target),self.assertRaises(ValueError):
                d.serving_pid(1,'/fixture.sock')

    def test_missing_or_changed_unit_pid_cannot_bind_listener(self):
        for value in ('0\n','1\n','invalid','47\n48\n'):
            with patch.object(d.subprocess,'run',return_value=SimpleNamespace(returncode=0,stdout=value)),\
                 self.assertRaises(ValueError):d.serving_pid(1,'/fixture.sock')

    def test_process_change_after_http_response_rejects(self):
        client=object.__new__(d.Docker);client.peer=(47,100)
        response=SimpleNamespace(status=200,read=lambda limit:b'{}')
        with patch.object(client,'request'),patch.object(client,'getresponse',return_value=response),\
             patch.object(d,'proc_start',return_value=101),self.assertRaises(ValueError):client.get('/info')

    def test_abandoned_fixture_roots_reject_before_any_command(self):
        for name in ('data','exec','docker.sock','dockerd.pid'):
            with tempfile.TemporaryDirectory() as directory:
                root=Path(directory);(root/name).symlink_to(root/'absent')
                with patch.object(fixture,'ROOT',root),patch.object(fixture,'STATE',root/'fixture.json'),\
                     patch.object(fixture,'UNIT_PATH',root/'fixture.service'),\
                     patch.object(fixture,'ownership',return_value=('machine',{})),\
                     patch.object(fixture.sys,'argv',['fixture','prepare']),\
                     patch.object(fixture,'run') as command:
                    with self.assertRaises(ValueError):fixture.main()
                    command.assert_not_called()


if __name__=='__main__':unittest.main()
