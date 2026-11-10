#!/usr/bin/env python3
"""Compose sampled host predicates at completed same-boot release boundaries.

No installer, stop, capture, reboot, restart or recovery authority. Qualification
references require external review; privileged host/kernel behavior is trusted.
The legacy interrupted-stop path remains rejected by ordinary source admission.
"""
import copy
import hashlib
import importlib.util
import os
from pathlib import Path
import stat


SOURCE_FILES=tuple(sorted(('restricted-cold-host-admission.py','cold-container-admission.py',
    'dormant-container-admission.py','original-deployment-admission.py','legacy-stop-policy.py',
    'network-recovery-admission.py','bpf-recovery-admission.py','ufw-recovery-admission.py',
    'host-process-admission.py','host-scheduler-admission.py','production-writer-fence.py',
    'outbound-quarantine.py','production-recovery-sources.py','inspect-runtime.py',
    'interrupted-release-recovery.py','release-journal.py','recovery-files.py')))
SOURCE_DIRECTORY=Path(__file__).resolve().parent


def require(value):
    if not value:raise ValueError('Restricted host admission rejected')


def captured_sources():
    """Capture all local dependency bytes before executing any of them.

    The verified launcher, Python/stdlib and this bootstrap remain trusted.
    Local .pyc files are never used by the dependency loader.
    """
    def identity(info):
        return (info.st_dev,info.st_ino,info.st_size,info.st_mtime_ns,info.st_ctime_ns)
    result={}
    parent=os.open(SOURCE_DIRECTORY,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW)
    try:
        parent_info=os.fstat(parent)
        for name in SOURCE_FILES:
            fd=os.open(name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
            try:
                before=os.fstat(fd)
                require(stat.S_ISREG(before.st_mode) and before.st_nlink==1 and not before.st_mode&0o022
                        and 0<before.st_size<=1024**2)
                raw=os.read(fd,1024**2+1)
                require(len(raw)==before.st_size and identity(os.fstat(fd))==identity(before)
                        and identity(os.stat(name,dir_fd=parent,follow_symlinks=False))==identity(before))
                result[name]=raw
            finally:os.close(fd)
        named=os.stat(SOURCE_DIRECTORY,follow_symlinks=False)
        require((named.st_dev,named.st_ino)==(parent_info.st_dev,parent_info.st_ino))
    finally:os.close(parent)
    return result


def hashes(sources):
    return {name:hashlib.sha256(raw).hexdigest() for name,raw in sources.items()}


def source_hashes():
    return hashes(captured_sources())


CAPTURED_SOURCES=captured_sources()
LOADED_SOURCE_HASHES=hashes(CAPTURED_SOURCES)


class CapturedLoader:
    def __init__(self,path):self.path=path
    def create_module(self,spec):return None
    def exec_module(self,module):
        module.__file__=str(self.path)
        exec(compile(CAPTURED_SOURCES[self.path.name],str(self.path),'exec'),module.__dict__)


def captured_spec(name,path,*args,**kwargs):
    path=Path(path)
    require(not args and not kwargs and path.parent==SOURCE_DIRECTORY and path.name in CAPTURED_SOURCES)
    return importlib.util.spec_from_loader(name,CapturedLoader(path))


def load(name,filename):
    spec=captured_spec(name,SOURCE_DIRECTORY/filename)
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


# All transitive local loaders use spec_from_file_location. Restrict that hook
# only during this single-threaded bootstrap, restoring it even on failure.
import threading
require(threading.active_count()==1)
_original_spec=importlib.util.spec_from_file_location
try:
    importlib.util.spec_from_file_location=captured_spec
    cold=load('restricted_cold','cold-container-admission.py')
    legacy=load('restricted_legacy','legacy-stop-policy.py')
    network=load('restricted_network','network-recovery-admission.py')
    bpf=load('restricted_bpf','bpf-recovery-admission.py')
    ufw=load('restricted_ufw','ufw-recovery-admission.py')
    process=load('restricted_process','host-process-admission.py')
    scheduler=load('restricted_scheduler','host-scheduler-admission.py')
finally:importlib.util.spec_from_file_location=_original_spec
require(source_hashes()==LOADED_SOURCE_HASHES)
o=cold.o;j=cold.j
KEYS={'schemaVersion','sourceRevision','sources','persistentConfigurationSha256',
      'packetEvidenceSha256','rebootEvidenceSha256','ufw','dormant','bpf','processes',
      'schedulers','network'}


class RestrictedColdHostAdmission:
    def __init__(self,journal,directory,policy):
        records=journal.records();require(len(records)==1)
        self.policy=copy.deepcopy(policy);policy=self.policy
        require(isinstance(policy,dict) and set(policy)==KEYS
                and type(policy['schemaVersion']) is int and policy['schemaVersion']==1
                and j.hash_value(policy['sourceRevision'],40))
        for name in ('persistentConfigurationSha256','packetEvidenceSha256','rebootEvidenceSha256'):
            require(j.hash_value(policy[name]))
        require(set(policy['network'])=={'admittedNetworkIds','uplink'})
        require(policy['sources']==LOADED_SOURCE_HASHES==source_hashes())
        self.journal,self.directory,self.owner=journal,directory,os.getpid()
        self.original=o.read_prepared(directory,records[0]['releaseNonce'],records[0]['planHash'])
        plan=records[0]['event']['plan']
        require(policy['sourceRevision']==plan['sourceRevision'])
        self.legacy_policy=legacy.read(directory,plan,self.original)
        self.policy_hash=j.sha(j.canonical(policy))
        require(self.legacy_policy['restrictionPolicySha256']==self.policy_hash)
        saved=self.original['originalDeployment']
        require(saved['units']['admission']['restriction']['persistentConfigurationSha256']==policy['persistentConfigurationSha256'])
        self.containers=cold.ColdContainerAdmission(journal,directory,policy['dormant'])
        self.observe()

    def observe(self):
        require(os.getpid()==self.owner and source_hashes()==self.policy['sources']==LOADED_SOURCE_HASHES
                and j.sha(j.canonical(self.policy))==self.policy_hash)
        records,running=cold.expected_running(self.journal,self.original)
        require(o.read_prepared(self.directory,self.original['releaseNonce'],self.original['planHash'])==self.original)
        require(legacy.read(self.directory,records[0]['event']['plan'],self.original)==self.legacy_policy)
        saved=self.original['originalDeployment'];expected=saved['expected'];policy=self.policy
        first=o.fence.quarantine.observe_persistent()
        require(first['persistentConfigurationSha256']==policy['persistentConfigurationSha256'])
        container_receipt=self.containers.observe()
        require(container_receipt['journalPrefixSha256']==j.sha(j.canonical(records)))
        units=o.fence.observe_restricted_units(saved['units']['fileHashes'],policy['persistentConfigurationSha256'],
                                               timer_stopped=len(records)>1)
        require(units['restriction']==first)
        sources=o.sources.observe(expected,stopped_services=frozenset(expected)-running)
        require(sources['runtimeConfigurationSha256']==saved['runtimeConfigurationSha256'])
        ufw_receipt=ufw.observe_qualified(policy['ufw'])
        bpf_snapshot=bpf.observe();bpf_receipt=bpf.admit(bpf_snapshot,policy['bpf'])
        ids=frozenset(expected[service]['containerId'] for service in running)
        topology=network.observe({**policy['network'],'runningContainerIds':sorted(ids)})
        require(topology['result']['admittedBridges']==first['bridges'])
        processes=process.observe(policy['processes'],ids)
        schedulers=scheduler.observe(policy['schedulers'])
        # Close the same sampled authorities, with the persistent restriction
        # enclosing the entire composite observation. Nothing here issues intent.
        require(self.containers.observe()==container_receipt
                and o.sources.observe(expected,stopped_services=frozenset(expected)-running)==sources
                and ufw.observe_qualified(policy['ufw'])==ufw_receipt
                and bpf.observe()==bpf_snapshot
                and network.observe({**policy['network'],'runningContainerIds':sorted(ids)})==topology
                and process.observe(policy['processes'],ids)==processes
                and scheduler.observe(policy['schedulers'])==schedulers
                and o.fence.observe_restricted_units(saved['units']['fileHashes'],policy['persistentConfigurationSha256'],
                                                     timer_stopped=len(records)>1)==units
                and o.fence.quarantine.observe_persistent()==first)
        require(self.journal.records()==records and source_hashes()==policy['sources']==LOADED_SOURCE_HASHES
                and j.sha(j.canonical(policy))==self.policy_hash
                and o.read_prepared(self.directory,self.original['releaseNonce'],self.original['planHash'])==self.original
                and legacy.read(self.directory,records[0]['event']['plan'],self.original)==self.legacy_policy
                and o.abort.boot_identity()==self.original['host'])
        evidence={'restriction':first,'containers':container_receipt,'units':units,'sources':sources,
                  'ufw':ufw_receipt,'bpf':bpf_receipt,'network':topology['result'],
                  'processes':processes,'schedulers':schedulers}
        return {'schemaVersion':1,'policySha256':self.policy_hash,
                'originalAdmissionSha256':j.sha(j.canonical(self.original)),
                'journalPrefixSha256':j.sha(j.canonical(records)),'evidenceSha256':j.sha(j.canonical(evidence)),
                'sampledComponents':sorted(evidence),'qualificationReferencesVerified':False,
                'continuousIsolationVerified':False,'hostBypassAdmissionVerified':False,
                'stopAuthorized':False,'recoveryAuthorized':False}
