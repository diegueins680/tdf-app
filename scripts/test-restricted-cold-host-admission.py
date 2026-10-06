#!/usr/bin/env python3
"""Real journal/policy/prepared files and source hashes; synthetic host samplers."""
import copy
import importlib.util
from pathlib import Path
import tempfile
import shutil
import subprocess
import sys
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,path)
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

m=load('restricted',ROOT/'ops/hetzner/restricted-cold-host-admission.py')
f=load('cold_fixture',ROOT/'scripts/test-cold-container-admission.py')
l=load('legacy_fixture',ROOT/'scripts/test-legacy-stop-policy.py')
j=m.j


class LoaderTests(unittest.TestCase):
    def run_copy(self,mode):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory).resolve()
            for name in m.SOURCE_FILES:shutil.copyfile(ROOT/'ops/hetzner'/name,root/name)
            script=r"""
import builtins,importlib.util,os,pathlib,py_compile,sys
root=pathlib.Path(sys.argv[1]);mode=sys.argv[2];bpf=root/'bpf-recovery-admission.py'
def load(name,path):
 spec=importlib.util.spec_from_file_location(name,path)
 module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module
if mode=='stale-pyc':
 raw=bpf.read_bytes();bpf.write_bytes(raw+b"\nSTALE_MARKER = 'OLD'\n")
 stamp=bpf.stat();py_compile.compile(str(bpf),doraise=True)
 bpf.write_bytes(raw+b"\nSTALE_MARKER = 'NEW'\n");os.utime(bpf,ns=(stamp.st_atime_ns,stamp.st_mtime_ns))
 assert load('timestamp_control',bpf).STALE_MARKER=='OLD'
 guarded=load('isolated_guard',root/'restricted-cold-host-admission.py')
 assert guarded.bpf.STALE_MARKER=='NEW'
 assert guarded.source_hashes()==guarded.LOADED_SOURCE_HASHES
elif mode=='replace-during-import':
 original=builtins.exec;replaced=[]
 def execute(code,globals=None,locals=None,**kwargs):
  result=original(code,globals,locals,**kwargs)
  if globals and globals.get('__name__')=='restricted_bpf':
   assert 'REPLACEMENT_MARKER' not in globals
   bpf.write_bytes(bpf.read_bytes()+b'\nREPLACEMENT_MARKER = True\n');replaced.append(True)
  return result
 builtins.exec=execute
 try:
  load('isolated_guard',root/'restricted-cold-host-admission.py')
 except ValueError as error:
  assert replaced==[True] and str(error)=='Restricted host admission rejected'
 else:raise AssertionError('Replacement enrolled as loaded source')
 finally:builtins.exec=original
else:raise AssertionError(mode)
print('isolated loader control passed')
"""
            result=subprocess.run([sys.executable,'-I','-c',script,str(root),mode],capture_output=True,text=True,timeout=30)
            self.assertEqual(result.returncode,0,result.stderr)
            self.assertEqual(result.stdout.strip(),'isolated loader control passed')

    def test_source_replacement_after_dependency_execution_is_rejected(self):
        self.run_copy('replace-during-import')

    def test_valid_timestamp_cache_cannot_substitute_old_dependency_code(self):
        self.run_copy('stale-pyc')


class CompositeTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve();self.control=self.root/'journal';self.control.mkdir(mode=0o700)
        self.directory=self.root/'prepared';self.directory.mkdir(mode=0o700)
        self.ids=dict(zip(('api','db','edge'),('1'*64,'2'*64,'3'*64)))
        containers={s:f.inspect(cid) for s,cid in self.ids.items()}
        containers['api']['Image']=l.BINDING['imageId']
        containers['api']['Config']={'Image':l.m.IMAGE,
          'Env':['SOURCE_COMMIT='+l.m.REVISION,'GIT_SHA='+l.m.REVISION]}
        self.snapshot={'schemaVersion':1,'version':'29.1.3','socket':'/run/docker.sock','dataRoot':'/var/lib/docker',
          'daemon':{'sha256':'d'*64,'bytes':100},'daemonProcess':{'pid':90,'startTicks':200},
          'containers':[f.row(cid,100+i) for i,cid in enumerate(self.ids.values())]}
        for service,cid in self.ids.items():
            next(r for r in self.snapshot['containers'] if r['id']==cid)['configurationSha256']=m.cold.d.configuration_fingerprint(containers[service])
        bridges=['br-'+'e'*12]
        self.restriction={'persistentConfigurationSha256':'7'*64,'bridges':bridges}
        self.units={'restriction':self.restriction,'timerStopped':False}
        self.policy={'schemaVersion':1,'sourceRevision':l.BASE['sourceRevision'],'sources':m.source_hashes(),
          'persistentConfigurationSha256':'7'*64,'packetEvidenceSha256':'8'*64,'rebootEvidenceSha256':'9'*64,
          'dormant':{'schemaVersion':1,'qualification':{'sourceRevision':'e'*40,
            'daemonRestartEvidenceSha256':'f'*64,'rebootEvidenceSha256':'1'*64},'snapshot':copy.deepcopy(self.snapshot)},
          'ufw':{'synthetic':'ufw'},'bpf':{'synthetic':'bpf'},'processes':{'synthetic':'processes'},
          'schedulers':{'synthetic':'schedulers'},'network':{'admittedNetworkIds':['e'*64],'uplink':'eth0'}}
        self.legacy={**l.POLICY,'restrictionPolicySha256':j.sha(j.canonical(self.policy))}
        self.plan={**l.PLAN,'legacyStopPolicyHash':j.sha(j.canonical(self.legacy))}
        self.saved={'runtimeConfigurationSha256':self.plan['runtimeHash'],'containers':containers,
          'expected':{s:({'containerId':cid,**{k:v for k,v in l.BINDING.items() if k!='containerId'}} if s=='api' else {'containerId':cid}) for s,cid in self.ids.items()},
          'units':{'fileHashes':{'synthetic':'hash'},'admission':self.units}}
        self.host=self.enterContext(patch.object(m.o.abort,'boot_identity',return_value=f.HOST))
        self.current=self.enterContext(j.open_journal(self.control));self.current.initialize(self.plan,'b'*32)
        with patch.object(m.o,'observe',return_value=self.saved) as sampled:
            m.o.prepare_restricted(self.current,self.directory,{}, {},'7'*64)
            self.assertEqual(sampled.call_count,2)
            self.assertEqual(sampled.call_args.kwargs,{'restriction_hash':'7'*64})
        m.legacy.prepare(self.current,self.directory,self.legacy)
        self.mock={}
        def sampler(module,name,value):
            mocked=self.enterContext(patch.object(module,name,return_value=value));self.mock[name]=mocked;return mocked
        sampler(m.cold.d,'observe',None).side_effect=lambda *_:copy.deepcopy(self.snapshot)
        sampler(m.o.fence.quarantine,'observe_persistent',self.restriction)
        self.units_mock=sampler(m.o.fence,'observe_restricted_units',self.units)
        self.source_mock=sampler(m.o.sources,'observe',{'runtimeConfigurationSha256':self.plan['runtimeHash']})
        self.ufw_mock=sampler(m.ufw,'observe_qualified',{'ufw':'sampled'})
        self.bpf_mock=sampler(m.bpf,'observe',{'bpf':'sampled'})
        sampler(m.bpf,'admit',{'bpf':'admitted'})
        self.network_mock=sampler(m.network,'observe',{'result':{'admittedBridges':bridges},'snapshot':{}})
        self.process_mock=sampler(m.process,'observe',{'processes':'sampled'})
        self.scheduler_mock=sampler(m.scheduler,'observe',{'schedulers':'sampled'})
        self.guard=m.RestrictedColdHostAdmission(self.current,self.directory,self.policy)

    def test_real_bound_policy_composes_samples_without_granting_permission(self):
        result=self.guard.observe()
        self.assertEqual(result['sampledComponents'],['bpf','containers','network','processes','restriction','schedulers','sources','ufw','units'])
        self.assertEqual(result['policySha256'],j.sha(j.canonical(self.policy)))
        for key in ('stopAuthorized','recoveryAuthorized','qualificationReferencesVerified','continuousIsolationVerified','hostBypassAdmissionVerified'):
            self.assertFalse(result[key])
        self.assertEqual(len(self.current.records()),1)

    def test_stage_derives_container_process_network_and_timer_policy(self):
        for index,service in enumerate(('edge','api','db')):
            def effect(context):
                f.stop(next(r for r in self.snapshot['containers'] if r['id']==self.ids[service]))
                return {**context,'evidenceHash':'f'*64}
            self.current.perform(j.STAGES[index],'c'*64,effect)
            self.units['timerStopped']=True
            self.guard.observe()
            running=m.cold.RUNNING[index+1]
            ids=frozenset(self.ids[s] for s in running)
            self.assertEqual(self.network_mock.call_args.args[0]['runningContainerIds'],sorted(ids))
            self.assertEqual(self.process_mock.call_args.args[1],ids)
            self.assertEqual(self.source_mock.call_args.kwargs['stopped_services'],frozenset(self.ids)-running)
            self.assertIs(self.units_mock.call_args.kwargs['timer_stopped'],True)

    def test_pending_intent_never_runs_component_samplers(self):
        def fail(_):raise RuntimeError('lost')
        with self.assertRaises(RuntimeError):self.current.perform('maintenance','c'*64,fail)
        self.network_mock.reset_mock()
        with self.assertRaises(ValueError):self.guard.observe()
        self.network_mock.assert_not_called()

    def test_wrong_bundle_source_or_saved_restriction_binding_rejects(self):
        for field,value in (('persistentConfigurationSha256','f'*64),('packetEvidenceSha256','f'*64),('sourceRevision','f'*40)):
            changed={**self.policy,field:value}
            with self.assertRaises(ValueError):m.RestrictedColdHostAdmission(self.current,self.directory,changed)
        changed=copy.deepcopy(self.policy);changed['sources']['release-journal.py']='f'*64
        with self.assertRaises(ValueError):m.RestrictedColdHostAdmission(self.current,self.directory,changed)
        changed=copy.deepcopy(self.guard.original);changed['originalDeployment']['units']['admission']['restriction']['persistentConfigurationSha256']='f'*64
        with patch.object(m.o,'read_prepared',return_value=changed),self.assertRaises(ValueError):self.guard.observe()

    def test_any_component_failure_prevents_composite_receipt(self):
        for observer in (self.source_mock,self.ufw_mock,self.bpf_mock,self.network_mock,self.process_mock,self.scheduler_mock,self.units_mock):
            observer.side_effect=ValueError('synthetic rejection')
            with self.assertRaises(ValueError):self.guard.observe()
            observer.side_effect=None
        self.assertEqual(len(self.current.records()),1)

    def test_closing_component_or_persistent_policy_drift_rejects(self):
        for observer in (self.source_mock,self.ufw_mock,self.bpf_mock,self.network_mock,self.process_mock,self.scheduler_mock,self.units_mock):
            initial=observer.return_value
            observer.side_effect=[initial,{**initial,'changed':True}]
            with self.assertRaises(ValueError):self.guard.observe()
            observer.side_effect=None
        with patch.object(m.o.fence.quarantine,'observe_persistent',side_effect=[self.restriction,{**self.restriction,'changed':True}]),self.assertRaises(ValueError):self.guard.observe()

    def test_network_bridge_policy_cannot_disagree_with_packet_guard(self):
        self.network_mock.return_value={'result':{'admittedBridges':['br-'+'f'*12]},'snapshot':{}}
        with self.assertRaises(ValueError):self.guard.observe()

    def test_changed_code_or_caller_process_rejects_before_host_sampling(self):
        changed={**self.policy['sources'],'release-journal.py':'f'*64}
        self.network_mock.reset_mock()
        with patch.object(m,'source_hashes',return_value=changed),self.assertRaises(ValueError):self.guard.observe()
        with patch.object(m.os,'getpid',return_value=self.guard.owner+1),self.assertRaises(ValueError):self.guard.observe()
        self.network_mock.assert_not_called()

    def test_legacy_binding_file_cannot_be_replaced_after_constructor(self):
        path=self.directory/m.legacy.NAME;path.write_bytes(path.read_bytes()+b' ')
        with self.assertRaises(ValueError):self.guard.observe()

    def test_mutable_internal_policy_cannot_redefine_sealed_authority(self):
        self.guard.policy['packetEvidenceSha256']='f'*64
        with self.assertRaises(ValueError):self.guard.observe()

    def test_loaded_code_fingerprint_cannot_be_reenrolled_after_import(self):
        changed={**self.policy['sources'],'release-journal.py':'f'*64}
        policy={**self.policy,'sources':changed}
        with patch.object(m,'source_hashes',return_value=changed),self.assertRaises(ValueError):
            m.RestrictedColdHostAdmission(self.current,self.directory,policy)

    def test_current_self_source_is_in_closed_hash_inventory(self):
        hashes=m.source_hashes()
        self.assertEqual(set(hashes),set(m.SOURCE_FILES))
        self.assertEqual(hashes['restricted-cold-host-admission.py'],m.hashlib.sha256((ROOT/'ops/hetzner/restricted-cold-host-admission.py').read_bytes()).hexdigest())


if __name__=='__main__':unittest.main()
