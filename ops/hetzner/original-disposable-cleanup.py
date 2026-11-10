#!/usr/bin/env python3
"""Abort-only disposal using durable pre-create identities after a recorded boot.

Never starts/unpauses a container or removes files, bind data, volumes or images.
Unknown inventory, legacy/partial records and uncertain effects remain blocked.
The service journal forbids same-epoch retry after any failed removal.
"""
import importlib.util
import json
import os
from pathlib import Path


def load(name,file):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(file))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

r=load('cleanup_records','durable-disposable-records.py')
d=load('cleanup_original','original-database-recovery.py')
require,canonical,sha=d.require,d.canonical,d.sha


class OriginalDisposableCleanup(d.OriginalDatabase):
    def __init__(self,journal,prepared,reservation):
        super().__init__(journal,prepared,reservation)
        self.prepared=Path(prepared)
        self.admission=journal.guard()[0]['admission']
        self.binding=r.binding(self.admission)
        # The abort reader authenticates the frozen normal journal prefix.
        raw,_=d.o.abort.read_file(journal.abort.parent,'000.json',maximum=d.o.abort.j.MAX_RECORD)
        first=json.loads(raw);require(canonical(first)==raw and first['planHash']==self.binding['planHash'])
        self.plan=first['event']['plan']
        self.marker=None;self.marker_identity=None

    def retained(self):
        self.guard()
        with d.o.abort.j.files.directory(str(self.reservation.directory),private=True) as fd:
            # Interrupted hard-link publication is never promoted to authority.
            require(not os.path.lexists(self.reservation.directory/(d.restore.PENDING_NAME+'.pending')))
            raw,info=d.o.abort.read_file(fd,d.restore.PENDING_NAME,maximum=32768)
        value=json.loads(raw)
        require(canonical(value)==raw and set(value)=={'schemaVersion','binding','recoveryImage','candidateImage','descriptorDirectory'}
                and type(value['schemaVersion']) is int and value['schemaVersion']==2
                and value['binding']==self.binding
                and value['recoveryImage']==self.saved['expected']['db']['image']
                and value['recoveryImage'].endswith('@'+self.plan['recoveryImage'])
                and value['candidateImage']==self.plan['candidateImage']
                and value['descriptorDirectory']==r.o.directory_identity(self.prepared/r.DIRECTORY))
        identity=d.o.abort.j.files.identity(info)
        if self.marker is None:self.marker=value;self.marker_identity=identity
        else:require(value==self.marker and identity==self.marker_identity)
        records=r.read_records(self.prepared,self.admission)
        for role,row in records.items():
            spec=row['specification']
            if role=='physical-database':
                require(spec['image']==value['recoveryImage'] and spec['imageId']==self.saved['expected']['db']['imageId'])
            else:
                require(spec['image']=='diegueins680/tdf-hq@'+self.plan['candidateImage']
                        and spec['revision']==self.plan['sourceRevision'])
        self.guard()
        return records

    def inventory(self,records):
        self.guard();d.observe(self.saved);self.guard()
        capture=d.o.sources.inspector.capture;docker=d.o.sources.inspector.DOCKER
        ids=capture(docker+['ps','--all','--quiet','--no-trunc']).split()
        require(3<=len(ids)<=5 and len(set(ids))==len(ids) and all(d.o.abort.j.hash_value(cid) for cid in ids))
        rows=json.loads(capture(docker+['inspect',*ids]))
        require(isinstance(rows,list) and len(rows)==len(ids) and {row['Id'] for row in rows}==set(ids))
        originals=set(self.binding['originalContainerIds'].values());require(originals<=set(ids))
        found={}
        for row in rows:
            if row['Id'] in originals:continue
            matches=[role for role,record in records.items() if row['Name']=='/'+record['specification']['name']]
            require(len(matches)==1 and matches[0] not in found)
            role=matches[0]
            found[role]=r.s.admit(records[role]['specification'],row,self.binding['originalContainerIds'])
            # These restart=no containers cannot legitimately resume on reboot.
            require(row['State']['Running'] is False and row['State'].get('Paused',False) is False)
        if 'application-canary' in found and 'physical-database' in found:
            require(records['application-canary']['specification']['dependentDatabase']==found['physical-database'])
        require(sorted(capture(docker+['ps','--all','--quiet','--no-trunc']).split())==sorted(ids))
        self.guard();require(self.retained()==records)
        return found

    def recover(self):
        self.guard()
        def effect(context):
            records=self.retained();found=self.inventory(records);removed=[]
            for role in ('application-canary','physical-database'):
                current=self.inventory(records)
                require(current==found)
                if role not in current:continue
                target=current[role]
                self.guard();require(target not in self.binding['originalContainerIds'].values())
                output=d.o.fence.execute(d.o.sources.inspector.DOCKER+['rm','--force',target]).strip()
                require(output==target)
                # A failed/lost reply exits before observing success or retrying.
                removed.append(target);found={key:cid for key,cid in found.items() if key!=role}
                require(self.inventory(records)==found)
            require(not self.inventory(records));self.guard();self.retained()
            with d.o.abort.j.files.directory(str(self.reservation.directory),private=True) as fd:
                require(d.o.abort.j.files.identity(os.stat(d.restore.PENDING_NAME,dir_fd=fd,follow_symlinks=False))==self.marker_identity)
                os.unlink(d.restore.PENDING_NAME,dir_fd=fd);os.fsync(fd)
            self.guard()
            evidence={'removedContainerIds':removed,'onlyRecordedDisposablesRemoved':True,
                      'originalContainerIds':self.binding['originalContainerIds'],
                      'retainedDataDeleted':False,'markerReleased':True,
                      'freshBootBoundByJournal':True,'releaseContinuationAllowed':False,
                      'continuousPrivilegedWriterExclusionVerified':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('remove-disposables',self.targets_hash,effect)
