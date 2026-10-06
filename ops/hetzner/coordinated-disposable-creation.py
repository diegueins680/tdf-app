#!/usr/bin/env python3
"""Journal-bound disposable creation reservation; no production release entrypoint.

The physical clone owns the common restore lock. Uncertain publication or cleanup
retains the versioned marker. This module never adopts a legacy marker or deletes
a container; abort cleanup must independently authenticate all retained evidence.
"""
import importlib.util
import json
import os
from pathlib import Path


def load(name,file):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(file))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module

r=load('coordinated_creation_records','durable-disposable-records.py')
d=load('coordinated_creation_reservation','original-database-recovery.py')
o=r.o
require,canonical,sha=r.require,r.canonical,r.sha
HOST_ROOT=Path("/opt/tdf/backups")


class CoordinatedCreation:
    def __init__(self,journal,prepared):
        self.journal=journal;self.prepared=Path(prepared);self.owner=os.getpid()
        first=journal.records()[0]
        self.admission=o.read_prepared(self.prepared,first['releaseNonce'],first['planHash'])
        self.plan=first['event']['plan'];self.binding=r.binding(self.admission)
        self.reservation=None;self.writer=None;self.marker=None;self.closed=False

    def base_guard(self):
        require(not self.closed and self.owner==os.getpid() and self.reservation is not None)
        self.reservation.guard();first=self.journal.records()[0]
        require(first['releaseNonce']==self.binding['releaseNonce']
                and first['planHash']==self.binding['planHash'] and first['event']['plan']==self.plan
                and o.read_prepared(self.prepared,first['releaseNonce'],first['planHash'])==self.admission)

    def marker_guard(self):
        self.base_guard();require(self.marker is not None)
        with o.abort.j.files.directory(str(self.reservation.directory),private=True) as fd:
            raw,info=o.abort.read_file(fd,d.restore.PENDING_NAME,maximum=32768)
            require(raw==canonical(self.marker) and o.abort.j.files.identity(info)==self.marker_identity)
        return info

    def reserve(self,clone,descriptor):
        require(self.reservation is None and not self.closed)
        try:
            self.reservation=d.Reservation(HOST_ROOT,descriptor)
            self.base_guard()
            original=self.admission['originalDeployment']['expected']['db']
            require(clone.nonce==self.binding['releaseNonce'] and clone.source==original['containerId']
                    and clone.image==original['image'] and clone.image_id==original['imageId']
                    and clone.image.endswith('@'+self.plan['recoveryImage'])
                    and clone.target is None and not clone.creation_attempted)
            self.writer=r.Writer(self.prepared,self.admission,self.base_guard)
            self.marker={'schemaVersion':2,'binding':self.binding,'recoveryImage':clone.image,
                         'candidateImage':self.plan['candidateImage'],
                         'descriptorDirectory':self.writer.identity}
            with o.abort.j.files.directory(str(self.reservation.directory),private=True) as fd:
                o.abort.publish(fd,d.restore.PENDING_NAME,self.marker)
                raw,info=o.abort.read_file(fd,d.restore.PENDING_NAME,maximum=32768)
                require(raw==canonical(self.marker));self.marker_identity=o.abort.j.files.identity(info)
            self.marker_guard()
        except BaseException:
            self.close();raise

    def publish(self,role,obj):
        self.marker_guard()
        state=self.journal.status()
        require(state['pendingStage']=='restore-isolate' and not state['newWritesPossible'])
        if role=='physical-database':
            original=self.admission['originalDeployment']['expected']['db']
            require(obj.reservation_pid==os.getpid() and obj.source==original['containerId']
                    and obj.image==original['image'] and obj.image_id==original['imageId'])
        else:
            require(role=='application-canary' and obj.revision==self.plan['sourceRevision']
                    and obj.image=='diegueins680/tdf-hq@'+self.plan['candidateImage'])
            rows=r.read_records(self.prepared,self.admission)
            require('physical-database' in rows)
            target=obj.database.target
            require(o.abort.j.hash_value(target) and target not in self.binding['originalContainerIds'].values())
            current=json.loads(d.restore.execute(d.restore.DOCKER+['inspect',target],timeout=10))
            require(isinstance(current,list) and len(current)==1)
            require(r.s.admit(rows['physical-database']['specification'],current[0],
                              self.binding['originalContainerIds'])==target)
        receipt=self.writer.publish(role,obj)
        self.marker_guard();require(self.journal.status()==state)
        return receipt

    def release(self,clone):
        self.marker_guard()
        require(self.writer is not None and not self.writer.closed)
        r.read_records(self.prepared,self.admission)
        require(clone.target is None and not clone.creation_attempted and clone.active_application is None)
        # Successful full inventory is required. No absent-name inference from a
        # failed inspect call, and no deletion or start is dispatched here.
        ids=d.restore.execute(d.restore.DOCKER+['ps','--all','--quiet','--no-trunc'],timeout=10).split()
        require(0<len(ids)<=128 and len(ids)==len(set(ids)) and all(o.abort.j.hash_value(cid) for cid in ids))
        rows=json.loads(d.restore.execute(d.restore.DOCKER+['inspect',*ids],timeout=10))
        require(isinstance(rows,list) and len(rows)==len(ids) and {row['Id'] for row in rows}==set(ids))
        nonce=self.binding['releaseNonce']
        for row in rows:
            labels=row['Config'].get('Labels') or {}
            require(row['Name'] not in ('/tdf-audit-restore-'+nonce,'/tdf-audit-canary-'+nonce)
                    and labels.get(d.restore.LABEL)!=nonce
                    and labels.get('net.tdf.application-canary')!=nonce)
        require(sorted(d.restore.execute(d.restore.DOCKER+['ps','--all','--quiet','--no-trunc'],timeout=10).split())==sorted(ids))
        self.marker_guard()
        with o.abort.j.files.directory(str(self.reservation.directory),private=True) as fd:
            require(o.abort.j.files.identity(os.stat(d.restore.PENDING_NAME,dir_fd=fd,follow_symlinks=False))==self.marker_identity)
            os.unlink(d.restore.PENDING_NAME,dir_fd=fd);os.fsync(fd)
        self.close()

    def close(self):
        self.closed=True
        if self.reservation is not None:self.reservation.closed=True
        if self.writer is not None:self.writer.closed=True
