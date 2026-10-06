#!/usr/bin/env python3
"""Journaled six-root capture after caller-established shutdown.

No shutdown, key custody, decryption, database startup or deployment entrypoint.
Captured data and full manifests remain private. OS/kernel/root trust is explicit.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat


def load(name,filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


bundle=load('capture_bundle','coordinated-recovery-bundle.py')
capacity=load('capture_capacity','offline-recovery-capacity.py')
schedulers=load('capture_schedulers','host-scheduler-admission.py')
processes=load('capture_processes','host-process-admission.py')
storage=load('capture_storage','stopped-application-storage.py')
files=bundle.files
UNIT_DIRECTORY=Path('/etc/systemd/system')


def require(value):
    if not value:raise ValueError('Coordinated capture rejected')


def source_mounts(text,roots):
    capacity.physical.admit_mounts(text)
    paths=[bundle.normalized(value) for value in roots.values()]
    for line in text.splitlines():
        fields=line.split();require(len(fields)>=10 and '-' in fields[6:])
        name=re.sub(r'\\([0-7]{3})',lambda match:chr(int(match[1],8)),fields[4])
        require('\\' not in name)
        mount=bundle.normalized(name)
        if name=='/':continue
        require(all(mount!=root and mount not in root.parents and root not in mount.parents for root in paths))


def upper_bound(manifests):
    require(len(manifests)==6)
    for manifest in manifests:files.validate_manifest(manifest)
    entries=sum(len(value['entries']) for value in manifests)
    # Same PAX overhead bound as the file recovery primitive, plus maximum index
    # and outer-archive entries/padding. A too-large upper bound rejects early.
    size=sum(value['bytes'] for value in manifests)+(entries+8)*8192+bundle.MAX_INDEX+7*10240
    require(size<=capacity.MAX_BUNDLE)
    return size,entries


class Capture:
    def __init__(self,fence,clone,binding,scheduler_policy,process_policy):
        bundle.validate_binding(binding)
        self.fence,self.clone,self.binding=fence,clone,dict(binding)
        self.scheduler_policy=json.loads(json.dumps(scheduler_policy))
        self.process_policy=json.loads(json.dumps(process_policy))
        self.owner=os.getpid();self.used=False
        self.output=clone.directory/'coordinated-plain.tar'
        self.workspace=clone.directory/'coordinated-components'
        self.units=clone.directory/'staged-units'
        self.legacy=clone.directory/'staged-legacy-uploads'
        self.legacy_archive=clone.directory/'legacy-uploads.tar'
        self.receipt_path=clone.directory/'coordinated-capture-receipt.json'
        self.receipt=None;self.unit_identities={}

    def guard(self,*,pending=False,stage='capture'):
        prefixes={'capture':['maintenance','stop-writers','stop-database'],
                  'encrypt':['maintenance','stop-writers','stop-database','capture'],
                  'retrieve-off-host':['maintenance','stop-writers','stop-database','capture','encrypt']}
        require(stage in prefixes)
        require(os.getpid()==self.owner and self.clone.reservation_pid==self.owner
                and self.clone.target is None and not self.clone.creation_attempted and not self.clone.start_attempted)
        journal=self.fence.journal;journal.guard();status=journal.status()
        require(status['releaseNonce']==self.clone.nonce==self.binding['releaseNonce']
                and status['completedStages']==prefixes[stage]
                and status['pendingStage']==(stage if pending else None)
                and status['newWritesPossible'] is False
                and self.clone.source==self.fence.expected['db']['containerId']
                and self.clone.system_id==self.binding['databaseSystemIdentifier'])
        plan=journal.records()[0]['event']['plan']
        for key,other in [('sourceRevision','sourceRevision'),('mobileRevision','mobileRevision'),
                          ('runtimeHash','runtimeSha256'),('manifestHash','migrationManifestSha256')]:
            require(plan[key]==self.binding[other])
        with files.directory(str(self.clone.directory),private=True):pass
        sampled=self.fence.observe();require(sampled['sources']['runtimeConfigurationSha256']==self.binding['runtimeSha256']
                and sampled['sources']['dockerWritersStopped'] is True
                and sampled['units']['timerStopped'] is True and sampled['units']['backupServiceInactive'] is True)
        roots=sampled['sources']['roots'];require(set(roots)=={'database','production','edge-data','edge-config'})
        source_mounts(Path('/proc/self/mountinfo').read_text(),roots)
        schedulers.observe(self.scheduler_policy)
        processes.observe(self.process_policy,frozenset(v['containerId'] for v in self.fence.expected.values()))
        require(self.fence.observe()==sampled)
        return sampled

    def legacy_manifest(self,legacy):
        retained=self.fence.legacy_root
        require(retained is not None and retained.target==self.fence.expected['api']['containerId'])
        retained.guard();retained.inspect(running=False)
        storage.require_empty_legacy_contracts(retained.root_fd)
        if not legacy:return None
        fd=os.dup(retained.root_fd)
        try:
            for name in ('app','uploads'):
                try:child=os.open(name,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=fd)
                except FileNotFoundError:return None
                os.close(fd);fd=child
            return files.walk(fd)
        finally:
            os.close(fd);retained.guard();retained.inspect(running=False)

    def stage_units(self):
        require(set(self.fence.unit_hashes)=={schedulers.TDF_SERVICE,schedulers.TDF_TIMER})
        bundle.private_new_directory(str(self.units))
        for name,expected in sorted(self.fence.unit_hashes.items()):
            require(name in (schedulers.TDF_SERVICE,schedulers.TDF_TIMER))
            with files.directory(str(UNIT_DIRECTORY)) as parent:
                fd=os.open(name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
                try:
                    before=os.fstat(fd)
                    require(stat.S_ISREG(before.st_mode) and before.st_uid==0 and before.st_nlink==1
                            and stat.S_IMODE(before.st_mode)==0o644 and 0<before.st_size<=16384)
                    files.no_extended_attributes(fd)
                    data=os.read(fd,16385)
                    require(len(data)==before.st_size and hashlib.sha256(data).hexdigest()==expected
                            and files.identity(os.fstat(fd))==files.identity(before)
                            and files.identity(os.stat(name,dir_fd=parent,follow_symlinks=False))==files.identity(before))
                finally:os.close(fd)
            self.unit_identities[name]=files.identity(before)
            with files.directory(str(self.units),private=True) as parent:
                fd=os.open(name,os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW,0o600,dir_fd=parent)
                try:
                    with os.fdopen(fd,'wb',closefd=False) as output:output.write(data);output.flush()
                    os.fchown(fd,before.st_uid,before.st_gid);os.fchmod(fd,stat.S_IMODE(before.st_mode))
                    os.utime(fd,ns=(before.st_atime_ns,before.st_mtime_ns));os.fsync(fd)
                finally:os.close(fd)
                os.fsync(parent)

    def verify_units(self):
        require(set(self.unit_identities)=={schedulers.TDF_SERVICE,schedulers.TDF_TIMER})
        with files.directory(str(UNIT_DIRECTORY)) as parent:
            for name,expected in self.unit_identities.items():
                fd=os.open(name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
                try:
                    require(files.identity(os.fstat(fd))==expected)
                    files.no_extended_attributes(fd)
                    data=os.read(fd,16385)
                    require(hashlib.sha256(data).hexdigest()==self.fence.unit_hashes[name]
                            and files.identity(os.fstat(fd))==expected
                            and files.identity(os.stat(name,dir_fd=parent,follow_symlinks=False))==expected)
                finally:os.close(fd)

    def stage_legacy(self,legacy,expected):
        evidence={'presence':'not-used-persistent-storage','sourceContainer':self.fence.expected['api']['containerId']}
        if legacy:
            evidence=self.fence.legacy_root.capture_uploads(str(self.legacy_archive))
            require(evidence['sourceContainer']==self.fence.expected['api']['containerId']
                    and evidence['manifest']==expected
                    and evidence['presence']==('absent' if expected is None else 'present'))
        if expected is not None:
            files.restore(str(self.legacy_archive),expected,str(self.legacy))
        else:
            # Explicit staging of an absent/unneeded component. This is new
            # empty storage, not fabricated restoration of source metadata.
            bundle.private_new_directory(str(self.legacy))
            with files.directory(str(self.legacy),private=True) as fd:
                os.fchown(fd,1000,1000);os.fchmod(fd,0o700);os.fsync(fd)
        return evidence

    def capture(self):
        require(not self.used);self.used=True
        observed=self.guard();roots=observed['sources']['roots'];legacy=observed['sources']['legacyUploads']
        manifests={}
        for name,path in roots.items():
            with files.directory(path) as fd:manifests[name]=files.walk(fd)
        legacy_manifest=self.legacy_manifest(legacy)
        # Budget known staging before publishing capture intent or creating it.
        empty={'schemaVersion':1,'entries':[{'path':'','kind':'directory','uid':1000,'gid':1000,'mode':0o700,'mtimeNs':0}],'bytes':0}
        # Host-unit bound includes two <=16KiB files plus its staging root.
        units={'schemaVersion':1,'entries':[{'path':'','kind':'directory','uid':0,'gid':0,'mode':0o700,'mtimeNs':0}], 'bytes':0}
        for i in range(2):
            units['entries'].append({'path':'unit-'+str(i),'kind':'file','uid':0,'gid':0,'mode':0o644,'mtimeNs':0,'bytes':16384,'sha256':'0'*64})
            units['bytes']+=16384
        size,entries=upper_bound(list(manifests.values())+[legacy_manifest or empty,units])
        capacity_receipt=capacity.observe(self.fence,self.clone,size,entries)
        target_hash=hashlib.sha256(bundle.canonical({'binding':self.binding,'roots':roots,
                                                   'legacyUploads':legacy,'upperBoundBytes':size,'upperBoundEntries':entries})).hexdigest()
        def effect(context):
            require(context['releaseNonce']==self.clone.nonce and context['stage']=='capture')
            require(self.guard(pending=True)==observed)
            self.stage_units();legacy_evidence=self.stage_legacy(legacy,legacy_manifest)
            all_roots={**roots,'host-units':str(self.units),'legacy-uploads':str(self.legacy)}
            captured=bundle.capture(all_roots,str(self.workspace),str(self.output),self.binding)
            require(captured['archive']['bytes']<=size)
            for name,path in roots.items():
                with files.directory(path) as fd:require(files.walk(fd)==manifests[name])
            require(self.legacy_manifest(legacy)==legacy_manifest)
            self.verify_units()
            require(self.guard(pending=True)==observed)
            self.receipt={'schemaVersion':1,'context':dict(context),'bundle':captured,'legacyStorage':legacy_evidence,'capacity':capacity_receipt,
                          'encryptionVerified':False,'offHostVerified':False,'databaseRecoveryVerified':False}
            # Exclusive private publication and file/directory fsync precede the
            # journal observation. On any failure, preserve the pending intent
            # and partial artifacts; there is no cleanup/retry/rollback here.
            bundle.write_index(self.receipt_path,self.receipt)
            require(self.guard(pending=True)==observed)
            return {**context,'evidenceHash':hashlib.sha256(bundle.canonical(self.receipt)).hexdigest()}
        self.fence.journal.perform('capture',target_hash,effect)
        return self.receipt
